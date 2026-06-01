# app.R
# 用户流失预测 - 时变 Cox 模型 + Shiny 可视化
# 新增：原始数据展示与下载

library(shiny)
library(survival)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(randomForestSRC)
library(cmprsk)

# ---------------------------- 数据模拟函数（内置） ----------------------------
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
    
    event_day <- NULL
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
    if (is.null(event_day)) {
      event_day <- max_days
      event <- 0
    } else {
      event <- 1
    }
    
    user_daily <- data.frame(
      user_id = uid,
      day = day_seq,
      logins = logins,
      event = ifelse(day == event_day & event == 1, 1, 0),
      start_time = day - 1,
      stop_time = day
    )
    daily_log[[i]] <- user_daily
  }
  bind_rows(daily_log)
}

# ---------------------------- 模型拟合函数 ----------------------------
fit_cox_model <- function(data) {
  coxph(Surv(start_time, stop_time, event) ~ logins + cluster(user_id), data = data)
}

# ---------------------------- 获取基线风险 ----------------------------
get_baseline_hazard <- function(cox_fit, event_data) {
  basehaz_obj <- basehaz(cox_fit, centered = FALSE)
  if (nrow(basehaz_obj) < 2) {
    event_times <- sort(unique(event_data$stop_time[event_data$event == 1]))
    if (length(event_times) == 0) {
      basehaz_df <- data.frame(time = c(0, 1), hazard = c(0, 0.001))
    } else {
      basehaz_df <- data.frame(
        time = c(0, event_times),
        hazard = c(0, seq_along(event_times) * 0.002)
      )
    }
  } else {
    basehaz_df <- basehaz_obj
  }
  basehaz_df %>%
    arrange(time) %>%
    mutate(baseline_inc = hazard - lag(hazard, default = 0))
}

# ---------------------------- 预测单个用户未来流失概率 ----------------------------
predict_user_future <- function(user_id, history_days = 30, future_days = 30,
                                daily_df, cox_fit, basehaz_df) {
  user_hist <- daily_df %>% filter(user_id == !!user_id, day <= history_days)
  if (nrow(user_hist) == 0) return(NULL)
  last_day <- max(user_hist$day)
  last_logins <- user_hist$logins[user_hist$day == last_day]
  
  future_stop <- (last_day + 1):(last_day + future_days)
  future_df <- data.frame(
    user_id = user_id,
    start_time = future_stop - 1,
    stop_time = future_stop,
    logins = rep(last_logins, future_days),
    event = 0
  )
  
  future_df <- future_df %>%
    rowwise() %>%
    mutate(baseline_inc = approx(basehaz_df$time, basehaz_df$baseline_inc,
                                 xout = stop_time, rule = 2)$y) %>%
    ungroup()
  
  future_df$relative_risk <- exp(predict(cox_fit, newdata = future_df, type = "lp"))
  future_df$daily_hazard <- future_df$baseline_inc * future_df$relative_risk
  future_df$cond_surv <- exp(-future_df$daily_hazard)
  future_df$cum_surv <- cumprod(future_df$cond_surv)
  future_df$churn_prob <- 1 - future_df$cum_surv
  future_df
}

# ---------------------------- 历史生存概率 ----------------------------
get_history_survival <- function(user_id, history_days, daily_df, cox_fit, basehaz_df) {
  user_hist <- daily_df %>% filter(user_id == !!user_id, day <= history_days)
  if (nrow(user_hist) == 0) return(NULL)
  hist_df <- user_hist %>%
    rowwise() %>%
    mutate(baseline_inc = approx(basehaz_df$time, basehaz_df$baseline_inc,
                                 xout = stop_time, rule = 2)$y) %>%
    ungroup()
  hist_df$relative_risk <- exp(predict(cox_fit, newdata = hist_df, type = "lp"))
  hist_df$daily_hazard <- hist_df$baseline_inc * hist_df$relative_risk
  hist_df$cond_surv <- exp(-hist_df$daily_hazard)
  hist_df$cum_surv <- cumprod(hist_df$cond_surv)
  hist_df %>% select(stop_time, surv_prob = cum_surv)
}

# ---------------------------- Shiny UI ----------------------------
ui <- fluidPage(
  titlePanel("时变Cox模型：用户流失预测（登录次数为时变协变量）"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_users", "模拟用户数量", value = 200, min = 50, max = 500, step = 50),
      numericInput("max_days", "最大观察天数", value = 90, min = 30, max = 180, step = 10),
      numericInput("history_days", "历史天数（用于预测）", value = 30, min = 7, max = 90, step = 1),
      numericInput("future_days", "预测未来天数", value = 30, min = 7, max = 90, step = 1),
      actionButton("run", "开始分析", class = "btn-primary"),
      hr(),
      h4("选择用户"),
      uiOutput("user_select"),
      br(),
      helpText("模型基于模拟数据，实际应用请替换为真实数据。")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("模型摘要", verbatimTextOutput("model_summary")),
        tabPanel("生存曲线", plotOutput("survival_plot")),
        tabPanel("未来流失概率", plotOutput("churn_plot")),
        tabPanel("预测数据表", DT::DTOutput("pred_table")),
        tabPanel("原始数据",              # 新增标签页
                 br(),
                 downloadButton("download_raw", "下载原始数据 (CSV)"),
                 br(), br(),
                 DT::DTOutput("raw_data_table")
        )
      )
    )
  )
)

# ---------------------------- Shiny Server ----------------------------
server <- function(input, output, session) {
  
  # 反应式数据：当用户点击“开始分析”时重新模拟并建模
  analysis_ready <- eventReactive(input$run, {
    withProgress(message = "模拟数据...", {
      daily_df <- simulate_user_data(input$n_users, input$max_days)
    })
    withProgress(message = "拟合Cox模型...", {
      cox_fit <- fit_cox_model(daily_df)
    })
    basehaz_df <- get_baseline_hazard(cox_fit, daily_df)
    list(daily = daily_df, cox = cox_fit, basehaz = basehaz_df)
  })
  
  # 用户选择下拉菜单
  output$user_select <- renderUI({
    req(analysis_ready())
    users <- sort(unique(analysis_ready()$daily$user_id))
    selectInput("user_id", "用户ID", choices = users, selected = users[1])
  })
  
  # 模型摘要
  output$model_summary <- renderPrint({
    req(analysis_ready())
    summary(analysis_ready()$cox)
  })
  
  # 预测结果
  pred_future <- reactive({
    req(analysis_ready(), input$user_id)
    predict_user_future(
      user_id = input$user_id,
      history_days = input$history_days,
      future_days = input$future_days,
      daily_df = analysis_ready()$daily,
      cox_fit = analysis_ready()$cox,
      basehaz_df = analysis_ready()$basehaz
    )
  })
  
  history_surv <- reactive({
    req(analysis_ready(), input$user_id)
    get_history_survival(
      user_id = input$user_id,
      history_days = input$history_days,
      daily_df = analysis_ready()$daily,
      cox_fit = analysis_ready()$cox,
      basehaz_df = analysis_ready()$basehaz
    )
  })
  
  # 生存曲线图
  output$survival_plot <- renderPlot({
    req(pred_future(), history_surv())
    hist_df <- history_surv() %>% mutate(type = "历史 (模型拟合)")
    fut_df <- pred_future() %>% 
      select(stop_time, surv_prob = cum_surv) %>% 
      mutate(type = "未来预测")
    plot_df <- bind_rows(hist_df, fut_df)
    
    true_event <- analysis_ready()$daily %>%
      filter(user_id == input$user_id, event == 1) %>%
      pull(stop_time)
    if (length(true_event) == 0) true_event <- NA
    
    ggplot(plot_df, aes(x = stop_time, y = surv_prob, color = type)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = true_event, linetype = "dashed", color = "red", linewidth = 0.8) +
      labs(
        title = paste("用户", input$user_id, "生存概率曲线"),
        x = "时间 (天)", y = "生存概率",
        caption = ifelse(is.na(true_event), "无事件发生", "红色虚线 = 实际流失时间")
      ) +
      theme_minimal() +
      scale_color_manual(values = c("历史 (模型拟合)" = "blue", "未来预测" = "orange"))
  })
  
  # 未来流失概率图
  output$churn_plot <- renderPlot({
    req(pred_future())
    ggplot(pred_future(), aes(x = stop_time, y = churn_prob)) +
      geom_line(color = "darkred", linewidth = 1.2) +
      labs(
        title = paste("用户", input$user_id, "未来", input$future_days, "天累积流失概率"),
        x = "时间 (天)", y = "流失概率"
      ) +
      theme_minimal()
  })
  
  # 预测数据表
  output$pred_table <- DT::renderDT({
    req(pred_future())
    pred_future() %>%
      select(时间 = stop_time, 每日风险 = daily_hazard, 
             条件生存 = cond_surv, 累积生存 = cum_surv, 流失概率 = churn_prob) %>%
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---------- 新增：原始数据展示 ----------
  output$raw_data_table <- DT::renderDT({
    req(analysis_ready())
    analysis_ready()$daily %>%
      select(user_id, day, logins, event, start_time, stop_time) %>%
      DT::datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        caption = "用户每日行为数据（计数过程格式）"
      )
  })
  
  # 下载原始数据
  output$download_raw <- downloadHandler(
    filename = function() {
      paste0("user_daily_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(analysis_ready())
      write.csv(analysis_ready()$daily, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)
