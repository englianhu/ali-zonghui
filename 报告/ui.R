library(shiny)
library(shinythemes) #闪霓主题
library(bslib) #闪霓主题
library(sass) #层叠样式表个性化
library(shinyWidgets) #彩色渐层
library(corazon) #彩色渐层
library(shinydashboard) #闪霓控板
library(flexdashboard) #灵活性控板
library(shinyBS)
library(shinyjs)
library(shinycssloaders)

thematic::thematic_shiny(font = "auto")

# Define UI for application that draws a histogram
shinyUI(
  fluidPage( ## https://rstudio.github.io/shinythemes/ 可以选择设置闪霓主题
    theme = bs_theme(version = 5, bootswatch = 'sketchy'), 
    shinythemes::themeSelector(), 
    
    # use a gradient in background
    shinyWidgets::setBackgroundColor(
      color = c('#002C54', '#4CB5F5'),
      gradient = 'radial',
      direction = c('top', 'left')), 
    
    ## https://github.com/englianhu/corazon
    ## #https://www.colorffy.com/gradients
    corazon::corazon_gradient(colorName = 'SWAMP', txtColor = 'yellow'), 
    
    # Application title
    titlePanel('一键生成报表'),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput('bins',
                        'Number of bins:',
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel('Tab 1',
                     plotOutput('distPlot')), 
            tabPanel('Tab 2')
          )
        )
    )
))
