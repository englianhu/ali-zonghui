library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(randomForestSRC)
library(BTYD)
library(PerformanceAnalytics)
library(xts)          # 显式加载 xts，确保 maxDrawdown 等函数可用
library(data.table)
library(conflicted)

conflicted::conflicts_prefer(dplyr::arrange)
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::filter)

# ---------------------------- 数据模拟函数 ----------------------------
simulate_user_data <- function(n_users = 200, max_days = 90, seed = 2026) {
  set.seed(seed)
  user_data <- data.frame(
    user_id = 1:n_users,
    reg_date = as.Date("2026-01-01") + runif(n_users, 0, 30)
  )
  user_data$frailty <- exp(rnorm(n_users, mean = -1.2, sd = 0.6))
  
  daily_log <- list()
  for (i in 1:n_users) {
    uid <- user_data$user_id[i]
    lambda0 <- exp(rnorm(1, mean = 1.2, sd = 0.6))
    day_seq <- 1:max_days
    logins <- rpois(max_days, lambda = lambda0 * exp(-0.02 * day_seq))
    purchase_prob <- plogis(-2.2 + 0.35 * logins)
    purchase <- rbinom(max_days, 1, purchase_prob)
    revenue <- ifelse(purchase == 1, rgamma(max_days, shape = 2, scale = 35), 0)
    
    event_day <- NA_integer_
    current_surv <- 1
    u <- runif(1)
    for (day in day_seq) {
      hazard_t <- 0.008 * exp(-0.15 * logins[day]) * user_data$frailty[i]
      surv_t <- exp(-hazard_t)
      current_surv <- current_surv * surv_t
      if (current_surv < u) {
        event_day <- day
        break
      }
    }
    
    event <- ifelse(is.na(event_day), 0, 1)
    if (is.na(event_day)) event_day <- max_days
    
    user_daily <- data.frame(
      user_id = uid,
      day = day_seq,
      logins = logins,
      purchase = purchase,
      revenue = revenue,
      event = ifelse(day_seq == event_day & event == 1, 1, 0),
      start_time = day_seq - 1,
      stop_time = day_seq,
      inactive_cost = ifelse(purchase == 0, 0.8 + 0.05 * logins, 0.2)
    )
    daily_log[[i]] <- user_daily
  }
  bind_rows(daily_log)
}

# ---------------------------- RSF 模型 ----------------------------
fit_rsf_model <- function(data) {
  randomForestSRC::rfsrc(
    Surv(stop_time, event) ~ logins + purchase + revenue + inactive_cost,
    data = data,
    ntree = 500,
    importance = TRUE,
    forest = TRUE,
    block.size = 1,
    na.action = "na.impute"
  )
}

# ---------------------------- 用户特征汇总 ----------------------------
get_user_features <- function(daily_df) {
  daily_df %>%
    group_by(user_id) %>%
    summarise(
      last_day = max(day),
      avg_logins = mean(logins),
      sum_logins = sum(logins),
      purchase_days = sum(purchase),
      total_revenue = sum(revenue),
      churn_event = max(event),
      last_purchase = ifelse(any(purchase == 1), max(day[purchase == 1]), 0),
      inactive_cost = sum(inactive_cost),
      .groups = "drop"
    )
}

# ---------------------------- BTYD ----------------------------
fit_btyd_model <- function(user_df) {
  if (nrow(user_df) < 5) return(NULL)
  # 构建符合 BTYD 要求的数据框
  clv <- user_df %>%
    transmute(
      customer_id = as.character(user_id),
      x = pmax(purchase_days - 1, 0),
      t.x = pmax(last_day - last_purchase, 0),
      T.cal = pmax(last_day, 1),
      monetary_value = ifelse(purchase_days > 0, total_revenue / purchase_days, 0.01)
    )
  tryCatch({
    # 修正点：直接传入数据框
    BTYD::bgnbd.EstimateParameters(clv)
  }, error = function(e) NULL)
}

# ---------------------------- 收益风险指标 ----------------------------
calc_portfolio_metrics <- function(df) {
  daily_net <- df %>%
    group_by(day) %>%
    summarise(net = sum(revenue) - sum(inactive_cost), .groups = "drop") %>%
    arrange(day)
  
  net_vals <- daily_net$net
  if (length(net_vals) < 2) net_vals <- c(0, 0)
  
  # 转换为 xts 以保证 PerformanceAnalytics 正常运行
  net_xts <- xts(net_vals, order.by = as.Date("2026-01-01") + daily_net$day - 1)
  
  list(
    sharpe = as.numeric(SharpeRatio.annualized(net_xts, geometric = FALSE, scale = 365)),
    sortino = as.numeric(SortinoRatio(net_xts, MAR = 0)),
    maxdd = as.numeric(maxDrawdown(net_xts)),
    equity = cumsum(net_vals),
    daily_net = daily_net
  )
}

# ---------------------------- 预测 ----------------------------
predict_user_rsf <- function(user_id, history_days, future_days, daily_df, rsf_fit) {
  hist <- daily_df %>% filter(user_id == !!user_id, day <= history_days)
  if (nrow(hist) == 0) return(NULL)
  
  last <- hist %>% slice_tail(n = 1)
  future_days_seq <- (history_days + 1):(history_days + future_days)
  
  future <- data.frame(
    user_id = user_id,
    day = future_days_seq,
    logins = rep(last$logins, future_days),
    purchase = rep(last$purchase, future_days),
    revenue = rep(last$revenue, future_days),
    inactive_cost = rep(last$inactive_cost, future_days),
    event = 0,
    start_time = future_days_seq - 1,
    stop_time = future_days_seq
  )
  
  pred <- predict(rsf_fit, newdata = future)
  surv <- as.data.frame(pred$survival)
  
  surv_prob <- if (ncol(surv) >= future_days) {
    as.numeric(surv[1, seq_len(future_days)])
  } else {
    rep(tail(as.numeric(surv[1, ]), 1), future_days)
  }
  
  data.frame(
    stop_time = future_days_seq,
    surv_prob = surv_prob,
    churn_prob = 1 - surv_prob
  )
}

get_history_survival <- function(user_id, history_days, daily_df, rsf_fit) {
  hist <- daily_df %>% filter(user_id == !!user_id, day <= history_days)
  if (nrow(hist) == 0) return(NULL)
  
  pred <- predict(rsf_fit, newdata = hist)
  surv <- as.data.frame(pred$survival)
  
  surv_prob <- if (nrow(surv) >= 1) {
    pmin(pmax(as.numeric(surv[1, seq_len(nrow(hist))]), 0), 1)
  } else {
    rep(NA, nrow(hist))
  }
  
  data.frame(stop_time = hist$stop_time, surv_prob = surv_prob)
}

make_feature_importance <- function(rsf_fit) {
  imp <- rsf_fit$importance
  if (is.null(imp)) return(data.frame(variable = character(), importance = numeric()))
  data.frame(variable = names(imp), importance = as.numeric(imp)) %>% arrange(desc(importance))
}

# ---------------------------- UI ----------------------------
ui <- fluidPage(
  titlePanel("RSF + BTYD + 风险调整版用户流失预测"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_users", "模拟用户数量", value = 200, min = 50, max = 1000, step = 50),
      numericInput("max_days", "最大观察天数", value = 90, min = 30, max = 365, step = 10),
      numericInput("history_days", "历史天数（用于预测）", value = 30, min = 7, max = 180, step = 1),
      numericInput("future_days", "预测未来天数", value = 30, min = 7, max = 180, step = 1),
      numericInput("seed", "随机种子", value = 2026, min = 1, max = 99999, step = 1),
      actionButton("run", "开始分析", class = "btn-primary"),
      hr(),
      h4("选择用户"),
      uiOutput("user_select"),
      br(),
      helpText("RSF 用于流失预测，BTYD 用于复购/CLV，风险指标基于每日净收益计算。")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("模型摘要", verbatimTextOutput("model_summary")),
        tabPanel("特征重要性", plotOutput("importance_plot"), DTOutput("importance_table")),
        tabPanel("生存曲线", plotOutput("survival_plot")),
        tabPanel("未来流失概率", plotOutput("churn_plot")),
        tabPanel("收益风险", verbatimTextOutput("risk_summary"), plotOutput("equity_plot")),
        tabPanel("预测数据表", DTOutput("pred_table")),
        tabPanel("原始数据",
                 br(),
                 downloadButton("download_raw", "下载原始数据 (CSV)"),
                 br(), br(),
                 DTOutput("raw_data_table"))
      )
    )
  )
)

# ---------------------------- Server ----------------------------
server <- function(input, output, session) {
  
  analysis_ready <- eventReactive(input$run, {
    withProgress(message = "模拟数据...", value = 0.2, {
      daily_df <- simulate_user_data(input$n_users, input$max_days, input$seed)
    })
    withProgress(message = "拟合RSF模型...", value = 0.6, {
      rsf_fit <- fit_rsf_model(daily_df)
    })
    user_feat <- get_user_features(daily_df)
    btyd_fit <- fit_btyd_model(user_feat)
    risk <- calc_portfolio_metrics(daily_df)
    list(daily = daily_df, rsf = rsf_fit, user_feat = user_feat, btyd = btyd_fit, risk = risk)
  })
  
  output$user_select <- renderUI({
    req(analysis_ready())
    users <- sort(unique(analysis_ready()$daily$user_id))
    selectInput("user_id", "用户ID", choices = users, selected = users[1])
  })
  
  output$model_summary <- renderPrint({
    req(analysis_ready())
    print(analysis_ready()$rsf)
    cat("\n--- 用户特征概览 ---\n")
    print(summary(analysis_ready()$user_feat))
    if (!is.null(analysis_ready()$btyd)) {
      cat("\n--- BTYD 参数 ---\n")
      print(analysis_ready()$btyd)
    }
  })
  
  output$importance_table <- renderDT({
    req(analysis_ready())
    make_feature_importance(analysis_ready()$rsf) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$importance_plot <- renderPlot({
    req(analysis_ready())
    imp <- make_feature_importance(analysis_ready()$rsf)
    validate(need(nrow(imp) > 0, "暂无特征重要性结果"))
    ggplot(imp, aes(x = reorder(variable, importance), y = importance)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = "随机生存森林特征重要性", x = NULL, y = "重要性")
  })
  
  pred_future <- reactive({
    req(analysis_ready(), input$user_id)
    predict_user_rsf(input$user_id, input$history_days, input$future_days, analysis_ready()$daily, analysis_ready()$rsf)
  })
  
  history_surv <- reactive({
    req(analysis_ready(), input$user_id)
    get_history_survival(input$user_id, input$history_days, analysis_ready()$daily, analysis_ready()$rsf)
  })
  
  output$survival_plot <- renderPlot({
    req(pred_future(), history_surv())
    hist_df <- history_surv() %>% mutate(type = "历史")
    fut_df <- pred_future() %>% select(stop_time, surv_prob) %>% mutate(type = "未来预测")
    plot_df <- bind_rows(hist_df, fut_df)
    
    ggplot(plot_df, aes(x = stop_time, y = surv_prob, color = type)) +
      geom_line(linewidth = 1.1) +
      theme_minimal() +
      labs(title = paste("用户", input$user_id, "生存概率曲线"), x = "时间 (天)", y = "生存概率") +
      scale_color_manual(values = c("历史" = "blue", "未来预测" = "orange"))
  })
  
  output$churn_plot <- renderPlot({
    req(pred_future())
    ggplot(pred_future(), aes(x = stop_time, y = churn_prob)) +
      geom_line(color = "darkred", linewidth = 1.1) +
      theme_minimal() +
      labs(title = paste("用户", input$user_id, "未来流失概率"), x = "时间 (天)", y = "流失概率")
  })
  
  output$risk_summary <- renderPrint({
    req(analysis_ready())
    r <- analysis_ready()$risk
    cat("Sharpe Ratio:", round(r$sharpe, 4), "\n")
    cat("Sortino Ratio:", round(r$sortino, 4), "\n")
    cat("Max Drawdown:", round(r$maxdd, 4), "\n")
    cat("说明：以每日净收益(revenue - inactive_cost)作为现金流序列计算。\n")
  })
  
  output$equity_plot <- renderPlot({
    req(analysis_ready())
    daily_net <- analysis_ready()$risk$daily_net
    daily_net$equity <- cumsum(daily_net$net)
    ggplot(daily_net, aes(x = day, y = equity)) +
      geom_line(color = "darkgreen", linewidth = 1.1) +
      theme_minimal() +
      labs(title = "净收益累计曲线", x = "时间 (天)", y = "累计净收益")
  })
  
  output$pred_table <- renderDT({
    req(pred_future())
    pred_future() %>%
      transmute(时间 = stop_time, 生存概率 = round(surv_prob, 4), 流失概率 = round(churn_prob, 4)) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$raw_data_table <- renderDT({
    req(analysis_ready())
    analysis_ready()$daily %>%
      select(user_id, day, logins, purchase, revenue, event, start_time, stop_time, inactive_cost) %>%
      datatable(options = list(pageLength = 15, scrollX = TRUE),
                caption = "用户每日行为数据（计数过程格式）")
  })
  
  output$download_raw <- downloadHandler(
    filename = function() paste0("user_daily_data_", Sys.Date(), ".csv"),
    content = function(file) {
      req(analysis_ready())
      write.csv(analysis_ready()$daily, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
