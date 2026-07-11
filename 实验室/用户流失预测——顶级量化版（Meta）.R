# app_v2_fixed2.R
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)

library(randomForestSRC)
library(survival)
library(BTYD)
library(PerformanceAnalytics)

# ---------- 数据模拟 ----------
simulate_user_data <- function(n_users=200, max_days=90, seed=2026){
  set.seed(seed)
  users <- tibble(
    user_id = 1:n_users,
    reg_date = as.Date("2026-01-01") + sample(0:30, n_users, TRUE),
    frailty = exp(rnorm(n_users, -1.2, 0.6))
  )
  out <- lapply(seq_len(n_users), function(i){
    days <- seq_len(max_days)
    logins <- rpois(max_days, exp(rnorm(1,1.2,0.6))*exp(-0.02*days))
    paid <- rbinom(1,1,0.2)
    spend <- if(paid) rgamma(max_days,2,0.5) else rep(0, max_days)
    event_day <- max_days; event <- 0; surv <- 1; u <- runif(1)
    for(d in days){
      haz <- 0.008*exp(-0.15*logins[d])*users$frailty[i]*(1+0.3*(spend[d]==0))
      surv <- surv*exp(-haz)
      if(surv<u){event_day<-d; event<-1; break}
    }
    tibble(user_id=i, day=days, logins, spend,
           event=ifelse(day==event_day & event==1,1,0),
           start=day-1, stop=day)
  })
  bind_rows(out)
}

# ---------- 模型 ----------
fit_rfsrc <- function(df){
  rfsrc(Surv(stop, event) ~ logins + spend, data=df, ntree=200, nodesize=15)
}
fit_bayes <- function(df){
  coxph(Surv(stop, event) ~ logins + spend + cluster(user_id), data=df)
}

# ---------- BTYD (修复) ----------
calc_btyd <- function(df){
  cust <- df %>% group_by(user_id) %>%
    summarise(x = sum(spend>0),
              t_x = max(ifelse(spend>0, day, 0)),
              T = max(day),.groups="drop")
  cust <- cust %>% filter(x>0, T>t_x)
  if(nrow(cust) < 3){
    return(cust %>% mutate(pred_30 = 0))
  }
  # 强制转 numeric matrix，避免 named vector
  mat <- as.matrix(cust[,c("x","t_x","T")])
  storage.mode(mat) <- "numeric"
  params <- tryCatch(
    bgnbd.EstimateParameters(mat),
    error = function(e) c(0.5,0.5)
  )
  # params 转普通向量
  params <- as.numeric(params)
  pred <- tryCatch(
    bgnbd.ConditionalExpectedTransactions(params, 30, cust$x, cust$t_x, cust$T),
    error = function(e) rep(0, nrow(cust))
  )
  cust$pred_30 <- as.numeric(pred)
  cust
}

# ---------- 风控 ----------
calc_risk <- function(spend){
  net <- spend + ifelse(spend==0, -0.5, 0)
  if(sd(net)==0) return(tibble(Sharpe=0,Sortino=0,MaxDD=0))
  tibble(
    Sharpe = as.numeric(SharpeRatio.annualized(net, scale=365)),
    Sortino = as.numeric(SortinoRatio(net, MAR=0)),
    MaxDD = as.numeric(maxDrawdown(net))
  )
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("量化版用户流失 (修复版)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n","用户数",200,50,500,50),
      numericInput("d","天数",90,30,180,10),
      actionButton("run","运行"),
      uiOutput("uid")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("模型", verbatimTextOutput("mod")),
        tabPanel("生存", plotOutput("surv")),
        tabPanel("BTYD", DTOutput("btyd")),
        tabPanel("风控", DTOutput("risk")),
        tabPanel("原始", DTOutput("raw"), downloadButton("dl","下载"))
      )
    )
  )
)

# ---------- Server ----------
server <- function(input,output,session){
  dat <- eventReactive(input$run, {
    df <- simulate_user_data(input$n, input$d)
    list(df=df,
         rsf=fit_rfsrc(df),
         cox=fit_bayes(df),
         btyd=calc_btyd(df))
  }, ignoreNULL=FALSE)

  output$uid <- renderUI({
    req(dat()); selectInput("id","用户", choices=sort(unique(dat()$df$user_id)))
  })
  output$mod <- renderPrint({ req(dat()); print(dat()$rsf) })
  output$surv <- renderPlot({
    req(dat(), input$id)
    nd <- dat()$df %>% filter(user_id==input$id) %>% slice(1)
    p <- predict(dat()$rsf, newdata=nd)
    plot(p$survival[1,], type="l", main=paste("用户",input$id))
  })
  output$btyd <- renderDT({ req(dat()); datatable(dat()$btyd) })
  output$risk <- renderDT({
    req(dat(), input$id)
    s <- dat()$df %>% filter(user_id==input$id) %>% pull(spend)
    datatable(calc_risk(s))
  })
  output$raw <- renderDT({ req(dat()); datatable(dat()$df, options=list(pageLength=10)) })
  output$dl <- downloadHandler("data.csv", function(f) write.csv(dat()$df,f,row.names=F))
}

shinyApp(ui, server)
