# ==============================================================================
# 📊 顶级量化产品矩阵与利润象限分析系统 (完美运行版)
# ==============================================================================

# 1. 载入核心数据分析包
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

# 2. 定义用户界面 (UI)
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"), # 科技感主题
  titlePanel("📊 顶级量化产品矩阵与利润象限分析系统"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "第一步：上传彩种报表文件", accept = c(".xls", ".xlsx", ".csv")),
      hr(),
      selectInput("xAxis", "第二步：选择动态 X 轴 (规模指标)", 
                  choices = c("投注人数", "投注金额", "中奖金额"),
                  selected = "投注金额"),
      selectInput("yAxis", "第三步：选择动态 Y 轴 (效率指标)", 
                  choices = c("盈利", "盈率_数值"),
                  selected = "盈利"),
      hr(),
      p("💡 量化提示：系统已修复图像渲染引擎，并对人数指标进行了整数化极简处理。")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📈 利润象限走势图", plotOutput("quantPlot", height = "500px")),
        tabPanel("🏆 高价值产品看板", tableOutput("topProductsTable"))
      )
    )
  )
)

# 3. 定义服务器逻辑 (Server)
server <- function(input, output) {
  
  # 核心数据反应链：动态读取并清洗数据
  processedData <- reactive({
    req(input$file)
    
    df_raw <- readxl::read_excel(input$file$datapath)
    
    df <- as.data.frame(df_raw)
    colnames(df) <- gsub("^[^a-zA-Z0-9\u4e00-\u9fa5]+", "", colnames(df))
    
    # 顶级量化数据清洗
    df_clean <- df %>%
      filter(!!sym(colnames(df)[1]) != "小计") %>% 
      mutate(
        盈率_数值 = if(is.character(盈率)) as.numeric(gsub("%", "", 盈率)) / 100 else as.numeric(盈率),
        投注人数 = as.integer(投注人数), # 🛠️ 修正：强制转换为整数，去掉内部小数点
        投注金额 = as.numeric(投注金额),
        盈利 = as.numeric(盈利)
      )
    return(df_clean)
  })
  
  # 渲染动态产品象限图
  output$quantPlot <- renderPlot({
    df <- processedData()
    label_col <- colnames(df)[1] 
    
    ggplot(df, aes(x = .data[[input$xAxis]], y = .data[[input$yAxis]], label = .data[[label_col]])) +
      geom_point(aes(size = 投注金额, color = 盈利), alpha = 0.7) +
      geom_text(vjust = -1, size = 3.5, check_overlap = TRUE) + 
      # 🛠️ 修正：将 midp = 0 更正为正确的 midpoint = 0
      scale_color_gradient2(low = "#cf1020", mid = "#999999", high = "#107c41", midpoint = 0) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
      theme_minimal() +
      labs(
        title = paste("产品定位矩阵:", input$xAxis, "vs", input$yAxis),
        size = "总资金池流水", color = "平台净利润"
      )
  })
  
  # 渲染最赚钱的前 5 个核心资产看板
  output$topProductsTable <- renderTable({
    df <- processedData()
    label_col <- colnames(df)[1]
    
    df %>%
      arrange(desc(盈利)) %>%
      head(5) %>%
      select(!!sym(label_col), 投注人数, 投注金额, 盈利, 盈率) %>%
      # 🛠️ 修正：在最终展示环节，将整数转为纯字符，确保表格中绝不出现 .0
      mutate(投注人数 = as.character(投注人数)) 
  })
}

# 4. 运行闪霓应用
shinyApp(ui = ui, server = server)
