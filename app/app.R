

# Calculate growing-season heat accumulation relative to cotton development

# Libraries
library(azmetr)
library(dplyr)
library(ggplot2)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)

# Functions
#source("./R/fxnABC.R", local = TRUE)

# Scripts
#source("./R/scr##DEF.R", local = TRUE)


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  "azmet-shiny-template.html",
  
  sidebarLayout = sidebarLayout(
    position = "left",
    
    sidebarPanel(
      id = "sidebarPanel",
      width = 4,
      
      verticalLayout(
        helpText(em(
          "Select an AZMet station, specify the heat variable, and set dates for the start and end of the period of interest. Then, click or tap 'CALCULATE CUMULATIVE VALUES'."
        )),
        
        br(),
        selectInput(
          inputId = "azmetStation", 
          label = "AZMet Station",
          choices = stationNames[order(stationNames$stationName), ]$stationName,
          selected = "Aguila"
        ),
        
        selectInput(
          inputId = "heatVariable",
          label = "Heat Variable",
          choices = heatVariables,
          selected = "Heat Units 86-45 °F"
        ),
        
        dateInput(
          inputId = "startDate",
          label = "Start Date",
          value = initialStartDate,
          min = Sys.Date() - lubridate::years(1),
          max = Sys.Date() - 1,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        dateInput(
          inputId = "endDate",
          label = "End Date",
          value = initialEndDate,
          min = Sys.Date() + 1 - lubridate::years(1),
          max = initialEndDate,
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        br(),
        actionButton(
          inputId = "calculateCumulativeValues", 
          label = "CALCULATE CUMULATIVE VALUES",
          class = "btn btn-block btn-blue"
        )
      )
    ), # sidebarPanel()
    
    mainPanel(
      id = "mainPanel",
      width = 8,
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureTitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureSubtitle"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput(outputId = "figure"))
      ), 
      
      br(), br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFooterHelpText"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFooter"))
      ),
      br()
    ) # mainPanel()
  ) # sidebarLayout()
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Reactive events -----
  
  # AZMet heat accumulation data
  dataAZMetDataMerge <- eventReactive(input$calculateCumulativeValues, {
    validate(
      need(expr = input$startDate <= input$endDate, message = FALSE)
    )
    
    idCalculatingCumulativeValues <- showNotification(
      ui = "Calculating cumulative values . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idCalculatingCumulativeValues",
      type = "message"
    )
    
    on.exit(removeNotification(id = idCalculatingCumulativeValues), add = TRUE)
    
    # Calls 'fxnAZMetDataELT()' and 'fxnAZMetDataSumHeat()'
    fxnAZMetDataMerge(
      azmetStation = input$azmetStation, 
      heatVariable = input$heatVariable,
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  # Build figure
  figure <- eventReactive(dataAZMetDataMerge(), {
    fxnFigure(
      inData = dataAZMetDataMerge(), 
      azmetStation = input$azmetStation,
      heatVariable = input$heatVariable,
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  # Build figure footer
  figureFooter <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureFooter(
      azmetStation = input$azmetStation,
      heatVariable = input$heatVariable,
      startDate = input$startDate, 
      endDate = input$endDate,
      timeStep = "Daily"
    )
  })
  
  # Build figure footer help text
  figureFooterHelpText <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureFooterHelpText()
  })
  
  # Build figure subtitle
  figureSubtitle <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureSubtitle(
      azmetStation = input$azmetStation, 
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  # Build figure title
  figureTitle <- eventReactive(input$calculateCumulativeValues, {
    validate(
      need(
        expr = input$startDate <= input$endDate,
        message = "Please select a 'Start Date' that is earlier than or the same as the 'End Date'."
      ),
      errorClass = "datepicker"
    )
    
    fxnFigureTitle(
      inData = dataAZMetDataMerge(), 
      heatVariable = input$heatVariable,
      endDate = input$endDate
    )
  })
  
  # Outputs -----
  
  output$figure <- renderPlot({
    figure()
  }, res = 96)
  
  output$figureFooter <- renderUI({
    figureFooter()
  })
  
  output$figureFooterHelpText <- renderUI({
    figureFooterHelpText()
  })
  
  output$figureSubtitle <- renderUI({
    figureSubtitle()
  })
  
  output$figureTitle <- renderUI({
    figureTitle()
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)
