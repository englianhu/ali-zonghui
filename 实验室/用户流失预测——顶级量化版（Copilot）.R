# ============================================================
# 顶级量化科技 Shiny 应用
# 功能：用户流失预测 + 贝叶斯生存模型 + 随机森林生存模型
#       + BTYD 客户价值预测 + 风险收益指标
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# 生存分析与机器学习
library(randomForestSRC)       # 随机森林生存模型
library(BTYD)                  # 客户生命周期价值预测
library(rstanarm)              # 贝叶斯生存模型 (stan_surv)
library(PerformanceAnalytics)  # 夏普率、索替诺率、最大回撤

# ---------------- 数据模拟 ----------------
# 模拟用户数据，包括登录次数、购买次数、充值情况、流失标签
simulate_user_data <- function(n_users = 200, max_days = 90, seed = 2026) {
  set.seed(seed)
  data.frame(
    user_id = 1:n_users,
    reg_date = as.Date("2026-01-01") + runif(n_users, 0, 30),
    logins = rpois(n_users, lambda = 5),       # 登录次数
    purchases = rpois(n_users, lambda = 2),    # 购买次数
    topup = rbinom(n_users, 1, 0.4),           # 是否充值
    churn = rbinom(n_users, 1, 0.2)            # 是否流失
  )
}

# ---------------- 风险收益指标 ----------------
# 计算夏普率、索替诺率、最大回撤
calc_risk_metrics <- function(returns) {
  sharpe <- SharpeRatio(returns, Rf = 0.01, p = 0.95)
  sortino <- SortinoRatio(returns, Rf = 0.01)
  mdd <- maxDrawdown(returns)
  list(sharpe = sharpe, sortino = sortino, mdd = mdd)
}

# ---------------- Shiny UI ----------------
ui <- fluidPage(
  titlePanel("量化科技计数尤物：用户流失与金融风险分析"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_users", "模拟用户数量", value = 200),
      actionButton("run", "开始分析", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("随机森林生存模型", verbatimTextOutput("rf_summary")),
        tabPanel("贝叶斯生存模型", verbatimTextOutput("bayes_summary")),
        tabPanel("客户价值预测 (BTYD)", DTOutput("btyd_table")),
        tabPanel("风险收益指标", verbatimTextOutput("risk_metrics")),
        tabPanel("原始数据", DTOutput("raw_data"))
      )
    )
  )
)

# ---------------- Shiny Server ----------------
server <- function(input, output, session) {
  # 当点击“开始分析”时生成模拟数据
  analysis_ready <- eventReactive(input$run, {
    simulate_user_data(input$n_users)
  })
  
  # 随机森林生存模型
  output$rf_summary <- renderPrint({
    df <- analysis_ready()
    rf_fit <- rfsrc(Surv(logins, churn) ~ purchases + topup, data = df)
    rf_fit
  })
  
  # 贝叶斯生存模型 (修正：使用 stan_surv 正确调用)
  output$bayes_summary <- renderPrint({
    df <- analysis_ready()
    # stan_surv 用法：stan_surv(Surv(time, event) ~ covariates, data = ...)
    # 这里用 logins 作为时间，churn 作为事件
    bayes_fit <- stan_surv(
      formula = Surv(logins, churn) ~ purchases + topup,
      data = df,
      chains = 2, iter = 500   # 设置较小迭代数，避免运行过慢
    )
    summary(bayes_fit)
  })
  
  # BTYD 客户价值预测
  output$btyd_table <- DT::renderDT({
    df <- analysis_ready()
    # 构造交易数据矩阵 (x, t.x, T.cal)
    x <- df$purchases
    t.x <- pmin(df$logins, 30)
    T.cal <- rep(30, nrow(df))
    
    params <- bgnbd.EstimateParameters(cbind(x, t.x, T.cal))
    
    data.frame(
      Parameter = c("r", "alpha", "a", "b"),
      Value = round(params, 4)
    ) %>%
      DT::datatable(options = list(pageLength = 5))
  })
  
  # 风险收益指标
  output$risk_metrics <- renderPrint({
    returns <- rnorm(100, mean = 0.01, sd = 0.02)
    metrics <- calc_risk_metrics(returns)
    metrics
  })
  
  # 原始数据展示
  output$raw_data <- renderDT({
    analysis_ready()
  })
}

shinyApp(ui, server)
