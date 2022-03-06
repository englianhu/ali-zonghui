library(shiny)
library(shinythemes) #theme
library(bslib) #theme
library(sass) #stylist css
library(shinyWidgets) #color gradient
library(corazon) #color gradient
library(shinydashboard)
library(flexdashboard)

thematic::thematic_shiny(font = "auto")

# Define UI for application that draws a histogram
shinyUI(
  fluidPage( ## https://rstudio.github.io/shinythemes/
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
    titlePanel('Hello Shiny!'),

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
            plotOutput('distPlot')
        )
    )
))
