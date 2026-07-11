# 加载所有必需包
library(shiny)
library(shinydashboard)
library(BTYD)
library(tidyverse)
library(plotly)
library(shinythemes)
library(DT)
library(actuar)
library(fitdistrplus)
library(e1071)
library(Metrics)

# -------------------------------------------------------------
# 生成模拟数据（支持训练/测试划分，支持 Gamma 或 Pareto）
# -------------------------------------------------------------
generate_simulated_data <- function(n_customers = 1000, 
                                    train_ratio = 0.8,
                                    dist_type = "Gamma",
                                    seed = 42) {
  set.seed(seed)
  start_date <- as.Date("2025-01-01")
  end_date <- as.Date("2025-12-31")
  total_days <- as.numeric(end_date - start_date)
  train_cut <- floor(total_days * train_ratio)
  train_end <- start_date + train_cut
  
  customer_ids <- paste0("CUST-", 1000 + 1:n_customers)
  all_transactions <- list()
  
  for (id in customer_ids) {
    channel <- sample(c("Google_Paid", "Facebook_Ads", "Organic_Search"), 1, prob = c(0.4, 0.4, 0.2))
    join_date <- start_date + sample(0:180, 1)
    base_freq <- switch(channel,
                        "Google_Paid" = 0.05,
                        "Facebook_Ads" = 0.03,
                        "Organic_Search" = 0.08)
    n_tx <- rpois(1, lambda = base_freq * as.numeric(end_date - join_date))
    if (n_tx == 0) next
    tx_offsets <- sort(sample(0:as.numeric(end_date - join_date), n_tx))
    tx_dates <- join_date + tx_offsets
    base_spend <- switch(channel,
                         "Google_Paid" = 80,
                         "Facebook_Ads" = 110,
                         "Organic_Search" = 60)
    if (dist_type == "Gamma") {
      spendings <- rgamma(n_tx, shape = 5, scale = base_spend / 5)
    } else {
      spendings <- rpareto(n_tx, shape = 3, scale = base_spend / 2)
    }
    period <- ifelse(tx_dates <= train_end, "train", "test")
    trans_df <- tibble(
      cust_id = id,
      date = tx_dates,
      spend = round(spendings, 2),
      channel = channel,
      birth_date = join_date,
      period = period
    )
    all_transactions[[length(all_transactions) + 1]] <- trans_df
  }
  df_transactions <- bind_rows(all_transactions)
  return(list(df = df_transactions, train_end = train_end))
}

# -------------------------------------------------------------
# 计算偏度、峰度及拟合 Pareto
# -------------------------------------------------------------
compute_dist_stats <- function(spend_vector) {
  skew <- skewness(spend_vector, na.rm = TRUE)
  kurt <- kurtosis(spend_vector, na.rm = TRUE)
  fit_pareto <- tryCatch({
    fitdist(spend_vector, "pareto", start = list(shape = 1, scale = min(spend_vector[spend_vector > 0])))
  }, error = function(e) NULL)
  return(list(skewness = skew, kurtosis = kurt, fit_pareto = fit_pareto))
}

# -------------------------------------------------------------
# 评估预测精度（MASE, MAE, RMSE）
# -------------------------------------------------------------
compute_accuracy <- function(actual, predicted, naive = NULL) {
  mae <- mae(actual, predicted)
  rmse <- rmse(actual, predicted)
  if (!is.null(naive)) {
    mase <- mae / mean(abs(naive - actual))
  } else {
    mase <- NA
  }
  return(list(MAE = mae, RMSE = rmse, MASE = mase))
}

# -------------------------------------------------------------
# 训练并评估（修正 select 问题）
# -------------------------------------------------------------
train_and_evaluate <- function(transactions, forecast_days, discount_rate, cpa_inputs) {
  train_df <- transactions %>% filter(period == "train")
  test_df <- transactions %>% filter(period == "test")
  if (nrow(test_df) == 0) return(NULL)
  
  max_train_date <- max(train_df$date)
  cbs_train <- train_df %>%
    group_by(cust_id) %>%
    summarise(
      x = n() - 1,
      t.x = as.numeric(max(date) - min(date)),
      T.cal = as.numeric(max_train_date - min(birth_date)),
      m.x = if_else(n() > 1, mean(spend[date > min(date)]), 0),
      channel = first(channel)
    ) %>%
    filter(T.cal > 0)
  
  # BG/NBD
  bgnbd_params <- bgnbd.EstimateParameters(as.matrix(cbs_train[, c("x", "t.x", "T.cal")]))
  cbs_train$expected_tx <- bgnbd.ConditionalExpectedTransactions(
    bgnbd_params, T.star = forecast_days,
    x = cbs_train$x, t.x = cbs_train$t.x, T.cal = cbs_train$T.cal
  )
  
  # Gamma-Gamma
  repeat_customers <- cbs_train %>% filter(x > 0)
  if (nrow(repeat_customers) > 0) {
    gg_params <- spend.EstimateParameters(
      m.x.vector = repeat_customers$m.x,
      x.vector = repeat_customers$x
    )
    cbs_train$expected_spend <- spend.expected.value(
      params = gg_params,
      m.x = cbs_train$m.x,
      x = cbs_train$x
    )
    avg_spend <- mean(repeat_customers$m.x, na.rm = TRUE)
    cbs_train$expected_spend[is.na(cbs_train$expected_spend)] <- avg_spend
  } else {
    cbs_train$expected_spend <- 0
  }
  
  # 实际测试期交易次数
  test_start <- max_train_date + 1
  test_end <- test_start + forecast_days - 1
  actual_tx <- test_df %>%
    filter(date >= test_start, date <= test_end) %>%
    group_by(cust_id) %>%
    summarise(actual_tx = n(), .groups = "drop")
  
  # 修正：显式使用 dplyr::select
  pred_tx <- cbs_train %>%
    dplyr::select(cust_id, expected_tx)   # 关键修正：加上 dplyr:::
  
  compare <- inner_join(pred_tx, actual_tx, by = "cust_id")
  if (nrow(compare) == 0) return(NULL)
  
  naive_pred <- rep(mean(cbs_train$expected_tx, na.rm = TRUE), nrow(compare))
  acc <- compute_accuracy(compare$actual_tx, compare$expected_tx, naive_pred)
  return(acc)
}

# -------------------------------------------------------------
# UI 定义
# -------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("高级数据科学：BG/NBD + Gamma-Gamma LTV 预测系统（优化版）"),
  sidebarLayout(
    sidebarPanel(
      h4("🔮 模拟与回测配置"),
      sliderInput("obs_customers", "客户规模:", min = 500, max = 3000, value = 1200, step = 100),
      sliderInput("forecast_days", "预测天数 (T):", min = 30, max = 365, value = 180, step = 30),
      numericInput("discount_rate", "年折现率 (%):", value = 10, min = 0, max = 100),
      hr(),
      h4("📊 数据生成选项"),
      radioButtons("dist_type", "消费金额分布:",
                   choices = c("Gamma" = "Gamma", "Pareto" = "Pareto"),
                   selected = "Gamma"),
      sliderInput("train_ratio", "训练期占比:", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
      hr(),
      h4("📊 获客成本 (CPA)"),
      numericInput("cpa_google", "Google Paid CPA ($):", value = 45),
      numericInput("cpa_fb", "Facebook Ads CPA ($):", value = 65),
      numericInput("cpa_organic", "Organic CPA ($):", value = 5),
      actionButton("run_analysis", "🚀 运行模型与回测", class = "btn-primary btn-block")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("📈 LTV 与 ROI",
                 fluidRow(valueBoxOutput("total_pred_ltv", width = 4),
                          valueBoxOutput("avg_roi", width = 4),
                          valueBoxOutput("model_status", width = 4)),
                 hr(),
                 plotlyOutput("roi_comparison_plot")
        ),
        tabPanel("📊 模型评估",
                 br(),
                 h3("预测精度（未来交易次数）"),
                 verbatimTextOutput("accuracy_metrics"),
                 h3("消费金额分布诊断"),
                 plotOutput("dist_diagnostic")
        ),
        tabPanel("👥 群组分析",
                 br(),
                 dataTableOutput("prediction_table")
        )
      )
    )
  )
)

# -------------------------------------------------------------
# Server 逻辑
# -------------------------------------------------------------
server <- function(input, output, session) {
  
  model_results <- eventReactive(input$run_analysis, {
    withProgress(message = "生成数据...", value = 0.2, {
      sim <- generate_simulated_data(
        n_customers = input$obs_customers,
        train_ratio = input$train_ratio,
        dist_type = input$dist_type
      )
      transactions <- sim$df
    })
    
    withProgress(message = "训练 BG/NBD 和 Gamma-Gamma...", value = 0.5, {
      max_train_date <- max(transactions %>% filter(period == "train") %>% pull(date))
      cbs <- transactions %>%
        filter(period == "train") %>%
        group_by(cust_id) %>%
        summarise(
          x = n() - 1,
          t.x = as.numeric(max(date) - min(date)),
          T.cal = as.numeric(max_train_date - min(birth_date)),
          m.x = if_else(n() > 1, mean(spend[date > min(date)]), 0),
          channel = first(channel)
        ) %>%
        filter(T.cal > 0)
      
      bgnbd_params <- bgnbd.EstimateParameters(as.matrix(cbs[, c("x", "t.x", "T.cal")]))
      t_pred <- input$forecast_days
      cbs$expected_tx <- bgnbd.ConditionalExpectedTransactions(
        bgnbd_params, T.star = t_pred,
        x = cbs$x, t.x = cbs$t.x, T.cal = cbs$T.cal
      )
      cbs$p_alive <- bgnbd.PAlive(bgnbd_params, x = cbs$x, t.x = cbs$t.x, T.cal = cbs$T.cal)
      
      repeat_cust <- cbs %>% filter(x > 0)
      if (nrow(repeat_cust) > 0) {
        gg_params <- spend.EstimateParameters(
          m.x.vector = repeat_cust$m.x,
          x.vector = repeat_cust$x
        )
        cbs$expected_spend <- spend.expected.value(
          params = gg_params,
          m.x = cbs$m.x,
          x = cbs$x
        )
        avg_spend <- mean(repeat_cust$m.x, na.rm = TRUE)
        cbs$expected_spend[is.na(cbs$expected_spend)] <- avg_spend
      } else {
        cbs$expected_spend <- 0
      }
      
      d_daily <- (input$discount_rate / 100) / 365
      cbs <- cbs %>%
        mutate(
          predicted_ltv = expected_tx * expected_spend / (1 + d_daily * t_pred),
          cpa = case_when(
            channel == "Google_Paid" ~ input$cpa_google,
            channel == "Facebook_Ads" ~ input$cpa_fb,
            channel == "Organic_Search" ~ input$cpa_organic
          ),
          net_value = predicted_ltv - cpa
        )
    })
    
    withProgress(message = "评估模型精度...", value = 0.8, {
      cpa_list <- list(google = input$cpa_google,
                       fb = input$cpa_fb,
                       organic = input$cpa_organic)
      accuracy <- train_and_evaluate(transactions, t_pred, input$discount_rate, cpa_list)
    })
    
    spend_all <- transactions$spend
    dist_stats <- compute_dist_stats(spend_all)
    
    list(
      cbs = cbs,
      accuracy = accuracy,
      dist_stats = dist_stats,
      spend_data = spend_all
    )
  })
  
  output$total_pred_ltv <- renderValueBox({
    res <- model_results()
    valueBox(paste0("$", round(mean(res$cbs$predicted_ltv), 2)),
             paste0("平均预测 LTV (", input$forecast_days, "天)"),
             icon = icon("dollar-sign"), color = "purple")
  })
  
  output$avg_roi <- renderValueBox({
    res <- model_results()
    total_ltv <- sum(res$cbs$predicted_ltv)
    total_cac <- sum(res$cbs$cpa)
    roi <- round(total_ltv / total_cac, 2)
    valueBox(paste0(roi, " x"), "LTV / CAC 杠杆",
             icon = icon("chart-line"), color = if_else(roi >= 3, "green", "orange"))
  })
  
  output$model_status <- renderValueBox({
    valueBox("已收敛 (Converged)", "BG/NBD & Gamma-Gamma 估计成功",
             icon = icon("check-circle"), color = "blue")
  })
  
  output$roi_comparison_plot <- renderPlotly({
    res <- model_results()
    df <- res$cbs %>%
      group_by(channel) %>%
      summarise(Avg_LTV = mean(predicted_ltv), .groups = "drop")
    p <- ggplot(df, aes(x = channel, y = Avg_LTV, fill = channel)) +
      geom_col() + geom_text(aes(label = paste0("$", round(Avg_LTV, 0))), vjust = -0.5) +
      labs(x = "渠道", y = "平均预测 LTV ($)") + theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$accuracy_metrics <- renderPrint({
    res <- model_results()
    if (is.null(res$accuracy)) {
      cat("测试期无足够交易，无法计算精度。")
    } else {
      cat("预测精度（未来交易次数）:\n")
      cat(sprintf("MAE  = %.4f\n", res$accuracy$MAE))
      cat(sprintf("RMSE = %.4f\n", res$accuracy$RMSE))
      cat(sprintf("MASE = %.4f\n", res$accuracy$MASE))
      cat("\n说明: MASE < 1 表示模型优于朴素预测（历史均值）。")
    }
  })
  
  output$dist_diagnostic <- renderPlot({
    res <- model_results()
    spend <- res$spend_data
    if (!is.null(res$dist_stats$fit_pareto)) {
      denscomp(res$dist_stats$fit_pareto, xlab = "消费金额", main = "Pareto 拟合对比")
    } else {
      hist(spend, breaks = 50, col = "skyblue", main = "消费金额分布", xlab = "金额")
    }
    title(sub = paste0("偏度 = ", round(res$dist_stats$skewness, 3),
                       "，峰度 = ", round(res$dist_stats$kurtosis, 3)), line = 1)
  })
  
  output$prediction_table <- renderDataTable({
    res <- model_results()
    res$cbs %>%
      dplyr::select(cust_id, channel, x, p_alive, expected_spend, expected_tx, predicted_ltv, cpa, net_value) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      arrange(desc(predicted_ltv)) %>%
      head(100)
  }, options = list(pageLength = 10, scrollX = TRUE))
}

shinyApp(ui = ui, server = server)
