#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 200*1024^2)

library(shiny)
library(ggplot2)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux

library(tidyr)

# We'll use a subset of the mtcars data set, with fewer columns
# so that it prints nicely

appName <- "MoistureBalanceInterctive"
appVersion <- "18.03"

#---------------------------------------------------------------------
# Martin duSaire modification of RStudio gallery Plot Interactions
# example.
# Added the side panel controls, data import and subsampling code.
#
# Still to do: file and plot save.
#              display click data not working
#              save zoomed plot and data
#---------------------------------------------------------------------


ui <- fluidPage(
  sidebarPanel(
    radioButtons("dataFormat", "Data Format",
                 c("MB18.xx model data" = "MB18.xx", "Ohaus MB45 data" = "MB45", "XYZ data" = "XYZ"),
                 selected = "MB45"
    ),
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    tags$hr(),
    # checkboxGroupInput("modelID", "Model ID",
    #                    c("Item A", "Item B", "Item C")),
    radioButtons("sampleNumber", "Select sample set", c(1)),
    
    
    sliderInput("sampleRate", "Sample Rate", min = 1, max = 20, value = 1, step = 1), 
    tags$hr(),
    
    selectInput('xcol', 'X variable', NULL),
    selectInput('ycol', 'Y variable', NULL),
    selectInput('modelSelect', 'Model Selection', c("Exponential","Henderson-Pabis", "Lewis", "Linear", "Logarithm", "Logarithmic", "Modified Henderson-Pabis",
                                                    "Modified Page", "Modified Page II", "Page", "Simplified Fick Diffusion", "Two-term", "Wang-Singh")),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Exponential'",
      sliderInput('aEx', "a parameter", min = -10, max = 10, value = -1.0, step = 0.01),
      sliderInput('bEx', "b parameter", min = -10.0, max = 10.0, value = 0, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Henderson-Pabis'",
      sliderInput('aHP', "a parameter", min = -10, max = 10, value = 0.8, step = 0.01),
      sliderInput('kHP', "k parameter", min = -10.0, max = 10.0, value = 0.5, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Lewis'",
      sliderInput('kLe', "k parameter", min = -10, max = 10, value = 0.5, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Linear'",
      sliderInput('mLi', "slope", min = -100, max = 100, value = 1, step = 0.1),
      sliderInput('bLi', "y-intercept", min = 0, max = 1, value = 0.5, step = 0.01)      
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Logarithm'",
      sliderInput('aLo', "slope", min = -100, max = 100, value = 1, step = 0.01),
      sliderInput('bLo', "y-intercept", min = -10, max = 10, value = 1, step = 0.1)
    ), 
    
    conditionalPanel(
      condition = "input.modelSelect == 'Logarithmic'",
      sliderInput('aLoga', "a parameter", min = -10, max = 10, value = 0.4, step = 0.01),
      sliderInput('kLoga', "k parameter", min = -10, max = 10, value = 0.5, step = 0.01),
      sliderInput('cLoga', "c parameter", min = -10, max = 10, value = 0.4, step = 0.01)
    ), 
    
    conditionalPanel(
      condition = "input.modelSelect == 'Modified Henderson-Pabis'",
      sliderInput('aMHP', "a parameter", min = -10, max = 10, value = 0.3, step = 0.01),
      sliderInput('bMHP', "b parameter", min = -10, max = 10, value = 0.2, step = 0.01),
      sliderInput('cMHP', "c parameter", min = -10, max = 10, value = 0.4, step = 0.01),
      sliderInput('kaMHP', "ka parameter", min = -10, max = 10, value = 0.2, step = 0.01),
      sliderInput('kbMHP', "kb parameter", min = -10, max = 10, value = 0.5, step = 0.01),
      sliderInput('kcMHP', "kc parameter", min = -10, max = 10, value = 0.3, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Modified Page'",
      sliderInput('kMP', "k parameter", min = -10.0, max = 10.0, value = 0.5, step = 0.01),
      sliderInput('nMP', "n parameter", min = -10.0, max = 10.0, value = 0.5, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Modified Page II'",
      sliderInput('kMPII', "k parameter", min = -10, max = 10, value = 0.5, step = 0.01),
      sliderInput('LMPII', "L parameter", min = -10, max = 10, value = 0.1, step = 0.01),
      sliderInput('nMPII', "n parameter", min = -10, max = 10, value = 0.5, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Page'",
      sliderInput('kPa', "k parameter", min = -10, max = 10, value = -0.5, step = 0.01),
      sliderInput('nPa', "n parameter", min = -10, max = 10, value = 0.6, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Simplified Fick Diffusion'",
      sliderInput('aSF', "a parameter", min = -10.0, max = 10.0, value = 0.8, step = 0.01),
      sliderInput('cSF', "c parameter", min = -10.0, max = 10.0, value = 0.5, step = 0.01),
      sliderInput('LSF', "L parameter", min = -10.0, max = 10.0, value = 1.0, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Two-term'",
      sliderInput('aTt', "a parameter", min = -10.0, max = 10.0, value = 0.3, step = 0.01),
      sliderInput('k1Tt', "k1 parameter", min = -10.0, max = 10.0, value = 0.5, step = 0.01),
      sliderInput('bTt', "b parameter", min = -10.0, max = 10.0, value = 0.5, step = 0.01),
      sliderInput('k2Tt', "k2 parameter", min = -10.0, max = 10.0, value = 0.1, step = 0.01)
    ),
    
    conditionalPanel(
      condition = "input.modelSelect == 'Wang-Singh'",
      sliderInput('aWS', "a parameter", min = -10, max = 10, value = -0.5, step = 0.01),
      sliderInput('bWS', "b parameter", min = -3.0, max = 3.0, value = 0.05, step = 0.01)
    ),
    
    tags$hr(),
    
    tags$div(style="display:inline-block", title="CTRL+click to save plot", downloadButton("savePlot", "Save PNG")),
    tags$div(style="display:inline-block", title="CTRL+click to save data", downloadButton("saveData", "Save data")),
    tags$button(
      style ="display:inline-block",
      title="Browser tab will remain open",
      id = 'close',
      type = "button",
      class = "btn action-button",
      onclick = "setTimeout(function(){window.close();},500);",  # close browser
      "Stop App"
    ),
    actionButton("saveModel", "Save Model Results")
  ),
  fluidRow(
    column(width = 7,
           plotOutput("plot1", width = "auto", height = 600,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                        # ,
                        # resetOnNew = TRUE
                      )
           )
    )
    
  ),
  #   column(width = 5,
  #        plotOutput("plot2", width = "auto", height = 500)
  #        )
  # ),
  fluidRow(
    column(width = 4,
           h4("Manual Correlation"),
           verbatimTextOutput('corrMan')
    ),
    column(width = 4,
           h4("nls() Correlation"),
           verbatimTextOutput('corrNLS')
    )
    # 
    # column(width = 6,
    #        h4("Points near click"),
    #        verbatimTextOutput("click_info")
    # ),
    # column(width = 6,
    #        h4("Brushed points"),
    #        verbatimTextOutput("brush_info")
    # )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Model", verbatimTextOutput('viewModelResults')),
      tabPanel("Plot Data", tableOutput('viewPlotData')),
      tabPanel("Info", verbatimTextOutput('viewInfo'))
    )
  )
)