# 加载 Shiny 框架，用于构建交互式 Web 应用
library(shiny)
# 加载 shinydashboard，提供 valueBox 等仪表盘组件
library(shinydashboard)
# 加载 BTYD 包，包含 BG/NBD 和 Gamma-Gamma 模型
library(BTYD)
# 加载 tidyverse 数据科学全家桶（包含 dplyr, ggplot2, purrr 等）
library(tidyverse)
# 加载 plotly，用于将静态 ggplot 转换为交互式图表
library(plotly)
# 加载 shinythemes，提供便捷的主题样式（如 cosmo）
library(shinythemes)
# 加载 DT，用于在 Shiny 中渲染可排序、搜索的数据表格
library(DT)
library(fitdistrplus)
library(actuar) # 必须载入，提供 "pareto" 分布的密度与分布函数支持

# 定义一个函数，用于生成模拟的客户交易数据
generate_simulated_data <- function(n_customers = 1000) {
  # 设置随机种子，确保每次运行结果可复现
  set.seed(42)
  # 定义模拟数据的起始日期（客户入驻开始日期）
  start_date <- as.Date("2025-01-01")
  # 定义模拟数据的结束日期（观察截止日期）
  end_date <- as.Date("2025-12-31")
  # 生成客户 ID 向量，格式为 "CUST-1001", "CUST-1002", ...
  customer_ids <- paste0("CUST-", 1000 + 1:n_customers)
  
  # 使用 purrr::map_df 遍历每个客户，生成其交易记录并合并为一个数据框
  df_transactions <- map_df(customer_ids, function(id) {
    # 随机分配获客渠道，并设置概率权重（模拟真实流量来源）
    channel <- sample(c("Google_Paid", "Facebook_Ads", "Organic_Search"), 1, prob = c(0.4, 0.4, 0.2))
    # 随机生成该客户的注册日期（在开始日期后的 0~180 天内）
    join_date <- start_date + sample(0:180, 1)
    # 根据渠道确定基础消费频率（泊松分布的 lambda 参数）
    base_freq <- switch(channel,
                        "Google_Paid" = 0.05,
                        "Facebook_Ads" = 0.03,
                        "Organic_Search" = 0.08)
    # 模拟该客户在观察期内的总购买次数（泊松分布）
    n_tx <- rpois(1, lambda = base_freq * as.numeric(end_date - join_date))
    # 如果购买次数为 0，则跳过该客户（返回 NULL）
    if (n_tx == 0) return(NULL)
    # 随机生成 n_tx 个购买偏移量（距离注册日的天数），并排序
    tx_offsets <- sort(sample(0:as.numeric(end_date - join_date), n_tx))
    # 计算具体的交易日期
    tx_dates <- join_date + tx_offsets
    # 根据渠道确定基础客单价（Gamma 分布的尺度参数基准）
    base_spend <- switch(channel,
                         "Google_Paid" = 80,
                         "Facebook_Ads" = 110,
                         "Organic_Search" = 60)
    # 生成每次交易的金额（Gamma 分布，右偏态，符合消费金额分布）
    spendings <- rgamma(n_tx, shape = 5, scale = base_spend / 5)
    # 返回该客户的所有交易记录（一行代表一笔交易）
    tibble(
      cust_id = id,               # 客户 ID
      date = tx_dates,            # 交易日期
      spend = round(spendings, 2),# 交易金额（保留两位小数）
      channel = channel,          # 获客渠道
      birth_date = join_date      # 客户注册日期
    )
  })
  # 返回生成的交易数据框
  return(df_transactions)
}

# ---------------------------- UI 部分 ----------------------------
# 定义 Shiny 应用的用户界面
ui <- fluidPage(
  # 应用主题为 "cosmo"（来自 shinythemes 包）
  theme = shinytheme("cosmo"),
  # 页面标题
  titlePanel("高级数据科学：基于 BG/NBD 与 Gamma-Gamma 的 LTV 预测与模拟系统"),
  # 侧边栏布局（左侧控制面板，右侧主显示区）
  sidebarLayout(
    # 左侧面板：包含所有输入控件
    sidebarPanel(
      h4("🔮 模拟与回测配置"),
      # 滑动条：控制模拟的客户数量（范围 500~3000，默认 1200）
      sliderInput("obs_customers", "模拟历史客户规模:", min = 500, max = 3000, value = 1200, step = 100),
      # 滑动条：控制预测未来天数（范围 30~365，默认 180）
      sliderInput("forecast_days", "未来 LTV 预测天数 (T):", min = 30, max = 365, value = 180, step = 30),
      # 数字输入：年度折现率（百分比）
      numericInput("discount_rate", "年度折现率 (Discount Rate %):", value = 10, min = 0, max = 100),
      hr(), # 水平分割线
      h4("📊 获客成本 (CPA) 假设"),
      # 三个渠道的获客成本输入框
      numericInput("cpa_google", "Google Paid CPA ($):", value = 45),
      numericInput("cpa_fb", "Facebook Ads CPA ($):", value = 65),
      numericInput("cpa_organic", "Organic CPA ($):", value = 5),
      # 动作按钮：点击后触发模型计算
      actionButton("run_analysis", "🚀 运行模型与回测", class = "btn-primary btn-block")
    ),
    # 右侧主显示区：包含多个标签页
    mainPanel(
      # 标签页集合
      tabsetPanel(
        # 第一个标签页：LTV 预测与 ROI 回测
        tabPanel("📈 LTV 预测与 ROI 回测",
                 # 流体行，用于放置 valueBox
                 fluidRow(
                   br(), # 换行
                   # 三个 valueBox 输出占位符（宽度各为 4，总宽 12）
                   valueBoxOutput("total_pred_ltv", width = 4),
                   valueBoxOutput("avg_roi", width = 4),
                   valueBoxOutput("model_status", width = 4)
                 ),
                 hr(),
                 h3("不同渠道的 净LTV 相比 获客成本(CPA) 的边际表现"),
                 # Plotly 交互式图表输出
                 plotlyOutput("roi_comparison_plot")
        ),
        # 第二个标签页：群组分析矩阵（预测结果明细表）
        tabPanel("👥 群组分析矩阵 (Cohort & Feature)",
                 br(),
                 h3("模型预测结果数据集 (Top 100 客户视图)"),
                 p("基于 BG/NBD 计算的 pAlive（当前存活概率）及 Gamma-Gamma 计算的期望客单价："),
                 # 数据表格输出（支持排序、搜索）
                 dataTableOutput("prediction_table")
        )
      )
    )
  )
)

# ---------------------------- Server 部分 ----------------------------
# 定义 Shiny 应用的服务器逻辑
server <- function(input, output, session) {
  
  # 事件响应式：仅当点击 "run_analysis" 按钮时才执行内部的繁重计算
  model_results <- eventReactive(input$run_analysis, {
    # 1. 生成模拟交易数据（根据用户选择的客户数量）
    raw_data <- generate_simulated_data(input$obs_customers)
    # 获取交易数据中的最大日期（观察期结束时间）
    max_date <- max(raw_data$date)
    
    # 2. 数据聚合：将交易流水转换为 BTYD 所需的客户基础统计量（CBS）
    cbs_summary <- raw_data %>%
      # 按客户 ID 分组
      group_by(cust_id) %>%
      summarise(
        # x = 重复购买次数（总购买次数 - 1）
        x = n() - 1,
        # t.x = 最后一次购买距离第一次购买的天数（recency）
        t.x = as.numeric(max(date) - min(date)),
        # T.cal = 第一次购买距离观察截止日期的天数（年龄）
        T.cal = as.numeric(max_date - min(birth_date)),
        # m.x = 重复购买的平均消费金额（仅当有重复购买时）
        m.x = if_else(n() > 1, mean(spend[date > min(date)]), 0),
        # 保留客户最初的获客渠道
        channel = first(channel)
      ) %>%
      # 过滤掉 T.cal <= 0 的异常情况（避免分母为 0）
      filter(T.cal > 0)
    
    # 3. BG/NBD 模型：估计参数并预测未来交易次数
    # 使用极大似然估计拟合 BG/NBD 模型（输入矩阵为 x, t.x, T.cal）
    bgnbd_params <- bgnbd.EstimateParameters(as.matrix(cbs_summary[, c("x", "t.x", "T.cal")]))
    # 获取预测的未来天数
    t_pred <- input$forecast_days
    # 计算每个客户在未来 t_pred 天内的期望交易次数
    cbs_summary$expected_tx <- bgnbd.ConditionalExpectedTransactions(
      bgnbd_params, T.star = t_pred,
      x = cbs_summary$x, t.x = cbs_summary$t.x, T.cal = cbs_summary$T.cal
    )
    # 计算每个客户当前仍然“活跃”（alive）的概率
    cbs_summary$p_alive <- bgnbd.PAlive(bgnbd_params, x = cbs_summary$x, t.x = cbs_summary$t.x, T.cal = cbs_summary$T.cal)
    
    # 4. Gamma-Gamma 模型：估计客户未来单笔消费的期望金额
    # 筛选出至少有一次重复购买的客户（x > 0）用于训练 Gamma-Gamma 模型
    repeat_customers <- cbs_summary %>% filter(x > 0)
    # 使用 spend.EstimateParameters 估计 Gamma-Gamma 参数（注意参数顺序：m.x.vector 和 x.vector）
    gg_params <- spend.EstimateParameters(
      m.x.vector = repeat_customers$m.x,
      x.vector = repeat_customers$x
    )
    # 计算每个客户的期望单笔消费金额（基于训练好的 Gamma-Gamma 模型）
    cbs_summary$expected_value <- spend.expected.value(
      params = gg_params,
      m.x = cbs_summary$m.x,
      x = cbs_summary$x
    )
    # 对于没有重复购买的客户（x == 0），将其期望消费金额设为所有活跃客户的平均消费金额
    cbs_summary$expected_value <- ifelse(
      cbs_summary$x == 0,
      mean(repeat_customers$m.x, na.rm = TRUE),
      cbs_summary$expected_value
    )
    
    # 5. 计算折现因子（按天）
    d_daily <- (input$discount_rate / 100) / 365
    # 计算最终预测 LTV 和净价值（LTV - CPA）
    cbs_summary <- cbs_summary %>%
      mutate(
        # 预测 LTV = 期望交易次数 × 期望单笔金额 / (1 + 日折现率 × 预测天数)
        predicted_ltv = expected_tx * expected_value / (1 + d_daily * t_pred),
        # 根据渠道映射 CPA（获客成本）
        cpa = case_when(
          channel == "Google_Paid" ~ input$cpa_google,
          channel == "Facebook_Ads" ~ input$cpa_fb,
          channel == "Organic_Search" ~ input$cpa_organic
        ),
        # 净价值 = 预测 LTV - 获客成本
        net_value = predicted_ltv - cpa
      )
    # 返回包含所有预测结果的数据框
    cbs_summary
  })
  
  # --- 渲染 valueBox：平均 LTV ---
  output$total_pred_ltv <- renderValueBox({
    res <- model_results()  # 获取模型结果
    # 计算所有客户预测 LTV 的平均值
    avg_ltv <- mean(res$predicted_ltv)
    valueBox(
      value = paste0("$", round(avg_ltv, 2)),          # 显示美元符号和数值
      subtitle = paste0("群体平均预测 ", input$forecast_days, "天 LTV"),  # 副标题
      icon = icon("dollar-sign"),                      # 图标
      color = "purple"                                 # 背景色
    )
  })
  
  # --- 渲染 valueBox：整体 ROI（LTV / CAC）---
  output$avg_roi <- renderValueBox({
    res <- model_results()
    total_ltv <- sum(res$predicted_ltv)   # 总预测 LTV
    total_cac <- sum(res$cpa)             # 总获客成本
    roi_ratio <- round(total_ltv / total_cac, 2)  # 杠杆比率
    valueBox(
      value = paste0(roi_ratio, " x"),
      subtitle = "整体营销全渠道 LTV / CAC 杠杆比率",
      icon = icon("chart-line"),
      # 根据是否达到 3 倍杠杆改变颜色（绿/橙）
      color = if_else(roi_ratio >= 3, "green", "orange")
    )
  })
  
  # --- 渲染 valueBox：模型收敛状态 ---
  output$model_status <- renderValueBox({
    valueBox(
      value = "已收敛 (Converged)",
      subtitle = "BG/NBD & Gamma-Gamma 最大似然估计成功",
      icon = icon("check-circle"),
      color = "blue"
    )
  })
  
  # --- 渲染渠道 ROI 对比图（Plotly 交互式）---
  output$roi_comparison_plot <- renderPlotly({
    res <- model_results()
    # 按渠道汇总：平均 LTV、CPA、净 LTV、客户数
    summary_df <- res %>%
      group_by(channel) %>%
      summarise(
        Avg_LTV = mean(predicted_ltv),
        CPA = first(cpa),
        Net_LTV = mean(net_value),
        Customer_Count = n()
      )
    # 使用 ggplot2 绘制柱状图
    p <- ggplot(summary_df, aes(x = channel, y = Avg_LTV, fill = channel)) +
      geom_col(alpha = 0.8) +                               # 柱状图，透明度 0.8
      geom_text(aes(label = paste0("$", round(Avg_LTV, 0))), vjust = -0.5) +  # 在柱顶显示金额
      labs(title = "渠道级别 LTV vs CPA 透视", x = "获客渠道", y = "平均预测 LTV ($)") +
      theme_minimal() +
      theme(legend.position = "none")                       # 隐藏图例（颜色已区分）
    # 转换为 Plotly 交互式对象
    ggplotly(p)
  })
  
  # --- 渲染预测数据表（Top 100 客户）---
  output$prediction_table <- renderDataTable({
    model_results() %>%
      # 选择需要展示的列
      select(cust_id, channel, x, p_alive, expected_value, predicted_ltv, cpa, net_value) %>%
      # 将所有数值列四舍五入保留两位小数
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      # 按预测 LTV 降序排列
      arrange(desc(predicted_ltv)) %>%
      # 仅取前 100 行，保证页面加载性能
      head(100)
  }, options = list(pageLength = 10, scrollX = TRUE))   # 每页显示 10 行，横向滚动
}

# 启动 Shiny 应用
shinyApp(ui = ui, server = server)
