library(shiny)
library(BTYD)
library(tidyverse)
library(plotly)
library(fitdistrplus)
library(actuar) # 必须载入，提供 "pareto" 分布的密度与分布函数支持

# ==============================================================================
# 1. 高级仿真环境：生成包含“巨鲸用户”的非对称长尾消费流水
# ==============================================================================
generate_advanced_data <- function(n_customers = 1000) {
  set.seed(42)
  start_date <- as.Date("2025-01-01")
  end_date <- as.Date("2025-12-31")
  customer_ids <- paste0("CUST-", 1000 + 1:n_customers)
  
  df_transactions <- map_df(customer_ids, function(id) {
    channel <- sample(c("Google_Paid", "Facebook_Ads", "Organic_Search"), 1, prob = c(0.4, 0.4, 0.2))
    join_date <- start_date + sample(0:180, 1)
    
    base_freq <- switch(channel, "Google_Paid" = 0.05, "Facebook_Ads" = 0.03, "Organic_Search" = 0.08)
    n_tx <- rpois(1, lambda = base_freq * as.numeric(end_date - join_date))
    if (n_tx == 0) return(NULL)
    
    tx_offsets <- sort(sample(0:as.numeric(end_date - join_date), n_tx))
    tx_dates <- join_date + tx_offsets
    
    # 模拟真实商业世界：85% 是 Gamma 常规消费，15% 是爆发性的帕累托“巨鲸”大额消费（Pareto Power Law）
    is_whale <- sample(c(FALSE, TRUE), n_tx, replace = TRUE, prob = c(0.85, 0.15))
    spendings <- numeric(n_tx)
    
    # 常规消费群体（中度右偏）
    spendings[!is_whale] <- rgamma(sum(!is_whale), shape = 4.5, scale = 12) 
    # 巨鲸大额消费（幂律分布极端长尾：形参为 2.5，最小起步消费 100 元）
    spendings[is_whale] <- rpareto(sum(is_whale), shape = 2.5, scale = 100)
    
    tibble(
      cust_id = id,
      date = tx_dates,
      spend = round(spendings, 2),
      channel = channel,
      birth_date = join_date
    )
  })
  return(df_transactions)
}

# ==============================================================================
# 2. 用户界面设计 (Shiny UI)
# ==============================================================================
ui <- fluidPage(
  titlePanel("高级数据科学：基于多分布拟合（Gamma vs Pareto）与 LTV 仿真系统"),
  br(),
  sidebarLayout(
    sidebarPanel(
      h4("🔮 模拟与回测配置"),
      sliderInput("obs_customers", "历史客户样本规模:", min = 500, max = 2500, value = 1000, step = 100),
      sliderInput("forecast_days", "未来 LTV 预测天数 (T):", min = 30, max = 365, value = 180, step = 30),
      numericInput("discount_rate", "年度折现率 (%):", value = 10, min = 0, max = 100),
      hr(),
      h4("📊 获客成本 (CPA) 假设"),
      numericInput("cpa_google", "Google Paid CPA ($):", value = 45),
      numericInput("cpa_fb", "Facebook Ads CPA ($):", value = 65),
      numericInput("cpa_organic", "Organic CPA ($):", value = 5),
      actionButton("run_analysis", "🚀 执行 MLE 拟合与 LTV 回测", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("🔬 真实流水客单价分布拟合诊断", 
                 br(),
                 h3("最大似然估计 (MLE) 概率密度分布对撞"),
                 p("下图将公司真实历史流水（阴影区域）与经 MLE 估计出的 Gamma 分布、Pareto 分布曲线进行交叉比对："),
                 plotlyOutput("distribution_fit_plot"),
                 br(),
                 h4("📊 模型统计学拟合优度对比（Goodness-of-Fit）"),
                 p("提示：对数似然 (Log-Likelihood) 越大越好，AIC 和 BIC 指标越小说明拟合越精准。"),
                 tableOutput("fit_metrics_table")
        ),
        tabPanel("📈 全盘渠道 LTV & ROI 看板", 
                 fluidRow(
                   br(),
                   valueBoxOutput("total_pred_ltv", width = 4),
                   valueBoxOutput("avg_roi", width = 4),
                   valueBoxOutput("best_model_box", width = 4)
                 ),
                 hr(),
                 h3("不同渠道的 净 LTV 相比 获客成本 (CPA) 的边际表现"),
                 plotlyOutput("roi_comparison_plot")
        ),
        tabPanel("👥 微观预测数据集", 
                 br(),
                 dataTableOutput("prediction_table")
        )
      )
    )
  )
)

# ==============================================================================
# 3. 服务端数学计算与模型训练逻辑 (Shiny Server)
# ==============================================================================
server <- function(input, output, session) {
  
  # 监听动作按钮，点火高能计算引擎
  analysis_core <- eventReactive(input$run_analysis, {
    
    # Step 1: 提取或生成具有巨鲸用户特征的真实流水
    raw_data <- generate_advanced_data(input$obs_customers)
    max_date <- max(raw_data$date)
    
    # 过滤出所有严格大于 0 的有效消费单据，用于分布拟合
    valid_spends <- raw_data$spend[raw_data$spend > 0]
    
    # Step 2: 使用 fitdistrplus 执行最大似然估计拟合 (MLE)
    # 拟合 Gamma 分布
    fit_gamma <- fitdist(valid_spends, "gamma", method = "mle")
    # 拟合 Pareto 分布 (底层调用 actuar 包的 dpareto/ppareto)
    fit_pareto <- fitdist(valid_spends, "pareto", method = "mle")
    
    # Step 3: 特征工程降维转化为 BTYD 专用的 CBS 矩阵
    cbs_summary <- raw_data %>%
      group_by(cust_id) %>%
      summarise(
        x = n() - 1,
        t.x = as.numeric(max(date) - min(date)),
        T.cal = as.numeric(max_date - min(birth_date)),
        m.x = if_else(n() > 1, mean(spend[date > min(date)]), 0),
        channel = first(channel)
      ) %>%
      filter(T.cal > 0)
    
    # Step 4: 训练 BG/NBD 模型预测购买频次与存活概率
    bgnbd_params <- bgnbd.EstimateParameters(as.matrix(cbs_summary[, c("x", "t.x", "T.cal")]))
    t_pred <- input$forecast_days
    cbs_summary$expected_tx <- bgnbd.ConditionalExpectedTransactions(
      bgnbd_params, T.star = t_pred, x = cbs_summary$x, t.x = cbs_summary$t.x, T.cal = cbs_summary$T.cal
    )
    cbs_summary$p_alive <- bgnbd.PAlive(bgnbd_params, x = cbs_summary$x, t.x = cbs_summary$t.x, T.cal = cbs_summary$T.cal)
    
    # Step 5: 训练 Gamma-Gamma 预测基础个体的预期单笔消费
    repeat_customers <- cbs_summary %>% filter(x > 0)
    gg_params <- gg.EstimateParameters(repeat_customers$x, repeat_customers$m.x)
    cbs_summary$expected_value <- gg.ConditionalExpectedTransactionValue(
      gg_params, x = cbs_summary$x, m.x = cbs_summary$m.x
    )
    
    # Step 6: 结合资金时间价值折现率合成最终指标
    d_daily <- (input$discount_rate / 100) / 365
    cbs_summary <- cbs_summary %>%
      mutate(
        predicted_ltv = expected_tx * expected_value / (1 + d_daily * t_pred),
        cpa = case_when(
          channel == "Google_Paid" ~ input$cpa_google,
          channel == "Facebook_Ads" ~ input$cpa_fb,
          channel == "Organic_Search" ~ input$cpa_organic
        ),
        net_value = predicted_ltv - cpa
      )
    
    # 返回一个包含丰富计算成果的命名列表
    list(
      cbs = cbs_summary,
      fit_gamma = fit_gamma,
      fit_pareto = fit_pareto,
      spends = valid_spends
    )
  })
  
  # ---- 标签页 1 渲染：双分布拟合诊断曲线图 ----
  output$distribution_fit_plot <- renderPlotly({
    data_pack <- analysis_core()
    spends <- data_pack$spends
    
    # 提取经 MLE 估计出的最佳模型超参数
    g_shape <- data_pack$fit_gamma$estimate["shape"]
    g_rate  <- data_pack$fit_gamma$estimate["rate"]
    p_shape <- data_pack$fit_pareto$estimate["shape"]
    p_scale <- data_pack$fit_pareto$estimate["scale"]
    
    # 创建平滑的 X 轴画线区间（限制在 98% 的数据内，防止超级异常值挤压图表）
    x_seq <- seq(min(spends), quantile(spends, 0.98), length.out = 300)
    
    # 依据估计出来的超参数生成理论上的概率密度曲线数据
    theoretical_df <- tibble(
      Amount = x_seq,
      Gamma = dgamma(x_seq, shape = g_shape, rate = g_rate),
      Pareto = dpareto(x_seq, shape = p_shape, scale = p_scale)
    ) %>% pivot_longer(cols = c(Gamma, Pareto), names_to = "Distribution", values_to = "Density")
    
    # 绘制真实流水直方图与两条理论分布曲线的重叠诊断图
    p <- ggplot() +
      geom_density(data = tibble(spend = spends), aes(x = spend, fill = "真实历史流水数据"), alpha = 0.2, color = NA) +
      geom_line(data = theoretical_df, aes(x = Amount, y = Density, color = Distribution), size = 1) +
      xlim(0, quantile(spends, 0.98)) +
      labs(x = "单笔交易花费金额 (USD)", y = "概率密度 (Density)", fill = "", color = "MLE 估计曲线") +
      scale_color_manual(values = c("Gamma" = "#1f77b4", "Pareto" = "#d62728")) +
      scale_fill_manual(values = c("真实历史流水数据" = "#7f7f7f")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ---- 标签页 1 渲染：优度指标对比表 ----
  output$fit_metrics_table <- renderTable({
    data_pack <- analysis_core()
    fg <- data_pack$fit_gamma
    fp <- data_pack$fit_pareto
    
    # 抽取三大核心模型筛选统计指标进行横向比对
    tibble(
      `统计学诊断指标` = c("对数似然 (Log-Likelihood)", "赤池信息准则 (AIC)", "贝叶斯信息准则 (BIC)"),
      `Gamma 分布模型` = c(fg$loglik, fg$aic, fg$bic),
      `Pareto 帕累托分布` = c(fp$loglik, fp$aic, fp$bic)
    )
  }, digits = 2, striped = TRUE, hover = TRUE)
  
  # ---- 标签页 2 渲染：指标看板 (Value Boxes) ----
  output$total_pred_ltv <- renderValueBox({
    res <- analysis_core()$cbs
    valueBox(value = paste0("$", round(mean(res$predicted_ltv), 2)), 
             subtitle = paste0("群体平均预测 ", input$forecast_days, "天 LTV"), icon = icon("dollar-sign"), color = "purple")
  })
  
  output$avg_roi <- renderValueBox({
    res <- analysis_core()$cbs
    roi_ratio <- round(sum(res$predicted_ltv) / sum(res$cpa), 2)
    valueBox(value = paste0(roi_ratio, " x"), subtitle = "大盘综合 LTV / CAC 杠杆乘数", 
             icon = icon("chart-line"), color = if_else(roi_ratio >= 3, "green", "orange"))
  })
  
  output$best_model_box <- renderValueBox({
    data_pack <- analysis_core()
    # 自动读取统计学红线：寻找决策 AIC 最小的获胜分布
    winner <- if_else(data_pack$fit_pareto$aic < data_pack$fit_gamma$aic, "Pareto (帕累托)", "Gamma (伽马)")
    valueBox(value = winner, subtitle = "根据最低 AIC 判定的大盘最佳拟合分布", icon = icon("award"), color = "blue")
  })
  
  # ---- 标签页 2 渲染：渠道边际效益柱状图 ----
  output$roi_comparison_plot <- renderPlotly({
    res <- analysis_core()$cbs
    summary_df <- res %>% group_by(channel) %>% 
      summarise(Avg_LTV = mean(predicted_ltv), CPA = first(cpa), Net_LTV = mean(net_value))
    
    p % layout(showlegend = FALSE)
  })
  
  # ---- 标签页 3 渲染：动态明细大表 ----
  output$prediction_table <- renderDataTable({
    analysis_core()$cbs %>% 
      select(cust_id, channel, x, p_alive, expected_value, predicted_ltv, cpa, net_value) %>%
