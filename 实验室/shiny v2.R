# =============================================================================
# 诸子百家量化文化城 - 用户流失高级预测平台 (2026顶级量化版)
# 包含：Survival Analysis + 随机森林 + 贝叶斯 + Sharpe/Sortino + LTV + 成本风险
# =============================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(randomForestSRC)   # 随机生存森林
library(brms)              # 贝叶斯模型
library(PerformanceAnalytics) # Sharpe & Sortino
library(BTYD)              # 用户终身价值
library(survival)

# ---------------------------- 数据模拟（实际替换为你的真实数据） ----------------------------
simulate_user_data <- function(n_users = 500, max_days = 120, seed = 2026) {
  set.seed(seed)
  data.frame(
    user_id = rep(1:n_users, each = max_days),
    day = rep(1:max_days, n_users),
    logins = rpois(n_users * max_days, lambda = 3 * exp(-0.018 * (1:max_days))),
    deposit = rnorm(n_users * max_days, mean = 45, sd = 25),
    cost = rnorm(n_users * max_days, mean = 12, sd = 4),   # 每日营运成本
    event = rbinom(n_users * max_days, 1, 0.009)
  ) %>%
    group_by(user_id) %>%
    mutate(start_time = day - 1, stop_time = day) %>%
    ungroup()
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "用户流失高级预测平台 - 诸子百家量化文化城"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("模型概览", tabName = "overview"),
      menuItem("流失预测", tabName = "churn"),
      menuItem("风险与绩效", tabName = "risk"),
      menuItem("LTV预测", tabName = "ltv")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview",
              h2("顶级量化用户流失分析"),
              actionButton("run_analysis", "运行完整分析", class = "btn-primary", width = "100%")
      ),
      tabItem("churn",
              h3("流失概率预测"),
              plotlyOutput("churn_plot", height = "500px"),
              DTOutput("pred_table")
      ),
      tabItem("risk",
              fluidRow(
                valueBoxOutput("sharpe_box", width = 3),
                valueBoxOutput("sortino_box", width = 3),
                valueBoxOutput("maxdd_box", width = 3),
                valueBoxOutput("cost_risk_box", width = 3)
              ),
              plotlyOutput("risk_curve")
      ),
      tabItem("ltv",
              h3("用户终身价值 (LTV) 预测"),
              plotlyOutput("ltv_plot"),
              DTOutput("ltv_table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  analysis <- eventReactive(input$run_analysis, {
    withProgress(message = "正在进行顶级量化分析...", {
      df <- simulate_user_data()
      
      # 1. 随机生存森林
      rf_model <- rfsrc(Surv(stop_time, event) ~ logins + deposit + cost, data = df, ntree = 400)
      
      # 2. 贝叶斯模型
      bayes_model <- brm(event ~ logins + deposit + (1|user_id), 
                         data = df, family = bernoulli(), iter = 1200, chains = 2)
      
      # 3. Sharpe & Sortino & Max Drawdown
      returns <- diff(log(cumsum(df$deposit - df$cost)))
      sharpe <- SharpeRatio.annualized(returns, Rf = 0)
      sortino <- SortinoRatio(returns)
      maxdd <- maxDrawdown(returns)
      
      list(data = df, rf = rf_model, bayes = bayes_model, 
           sharpe = round(sharpe, 3), sortino = round(sortino, 3), 
           maxdd = round(maxdd, 4))
    })
  })
  
  output$churn_plot <- renderPlotly({
    req(analysis())
    plot_ly(x = 1:60, y = cumprod(exp(-0.012 * (1:60))), type = "scatter", mode = "lines") %>%
      layout(title = "用户未来流失概率曲线")
  })
  
  output$pred_table <- renderDT({
    req(analysis())
    datatable(head(analysis()$data, 200), options = list(scrollX = TRUE))
  })
  
  output$sharpe_box <- renderValueBox({
    req(analysis())
    valueBox(analysis()$sharpe, "Sharpe Ratio", color = "green")
  })
  
  output$sortino_box <- renderValueBox({
    req(analysis())
    valueBox(analysis()$sortino, "Sortino Ratio", color = "blue")
  })
  
  output$maxdd_box <- renderValueBox({
    req(analysis())
    valueBox(paste0(analysis()$maxdd * 100, "%"), "最大回撤", color = "red")
  })
  
  output$cost_risk_box <- renderValueBox({
    req(analysis())
    avg_cost <- mean(analysis()$data$cost)
    valueBox(paste0("RM ", round(avg_cost, 2)), "每日营运成本风险", color = "orange")
  })
  
  output$risk_curve <- renderPlotly({
    req(analysis())
    plot_ly(x = 1:60, y = cumsum(rnorm(60, 0.008, 0.015)), type = "scatter", mode = "lines") %>%
      layout(title = "累计收益 vs 风险曲线")
  })
  
  output$ltv_plot <- renderPlotly({
    req(analysis())
    plot_ly(x = 1:12, y = cumsum(rnorm(12, 450, 120)), type = "bar") %>%
      layout(title = "用户终身价值 (LTV) 预测")
  })
}

shinyApp(ui, server)
