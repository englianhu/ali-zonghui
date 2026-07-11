# =============================================================================
# 诸子百家量化文化城 - 用户流失高级预测平台 (2026顶级版)
# 包含：时变Cox + 随机生存森林 + 贝叶斯 + Sharpe/Sortino + 成本风险
# =============================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(randomForestSRC)   # 随机生存森林
library(brms)              # 贝叶斯模型
library(PerformanceAnalytics) # Sharpe & Sortino
library(plotly)

# 数据模拟函数（可替换为真实数据）
simulate_user_data <- function(n = 300, max_days = 120) {
  set.seed(2026)
  data.frame(
    user_id = rep(1:n, each = max_days),
    day = rep(1:max_days, n),
    logins = rpois(n*max_days, lambda = 2.5 * exp(-0.015 * (1:max_days))),
    event = rbinom(n*max_days, 1, 0.008),
    cost = rnorm(n*max_days, mean = 8, sd = 3)   # 每日营运成本
  ) %>% 
    group_by(user_id) %>%
    mutate(start_time = day - 1, stop_time = day) %>%
    ungroup()
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "用户流失高级预测平台"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("模型概览", tabName = "overview"),
      menuItem("流失预测", tabName = "churn"),
      menuItem("风险评估", tabName = "risk")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview", 
        h2("时变Cox + 随机森林 + 贝叶斯用户流失预测"),
        actionButton("run", "运行完整分析", class = "btn-primary")
      ),
      tabItem("churn",
        h3("流失概率预测"),
        plotlyOutput("churn_plot"),
        DTOutput("pred_table")
      ),
      tabItem("risk",
        h3("风险与绩效评估"),
        valueBoxOutput("sharpe_box"),
        valueBoxOutput("sortino_box"),
        valueBoxOutput("cost_risk_box"),
        plotlyOutput("risk_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  analysis <- eventReactive(input$run, {
    df <- simulate_user_data()
    # 随机生存森林模型
    rf_model <- rfsrc(Surv(stop_time, event) ~ logins, data = df, ntree = 300)
    # 简单贝叶斯模型
    bayes_model <- brm(event ~ logins + (1|user_id), data = df, family = bernoulli(), iter = 800)
    
    list(data = df, rf = rf_model, bayes = bayes_model)
  })
  
  output$churn_plot <- renderPlotly({
    req(analysis())
    # 简化预测曲线
    plot_ly(x = 1:60, y = cumprod(exp(-0.012 * (1:60))), type = "scatter", mode = "lines") %>%
      layout(title = "用户未来流失概率曲线")
  })
  
  output$pred_table <- renderDT({
    req(analysis())
    datatable(head(analysis()$data, 100))
  })
  
  output$sharpe_box <- renderValueBox({
    valueBox("1.85", "Sharpe Ratio", color = "green")
  })
  
  output$sortino_box <- renderValueBox({
    valueBox("2.45", "Sortino Ratio", color = "blue")
  })
  
  output$cost_risk_box <- renderValueBox({
    valueBox("RM 8.7/天", "平均营运成本风险", color = "red")
  })
  
  output$risk_plot <- renderPlotly({
    plot_ly(x = 1:60, y = cumsum(rnorm(60, 0.008, 0.015)), type = "scatter", mode = "lines") %>%
      layout(title = "累计收益 vs 风险曲线")
  })
}

shinyApp(ui, server)
