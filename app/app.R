# Calculate cumulative values of heat variables by station and date range


# UI --------------------


ui <- 
  htmltools::htmlTemplate(
    
    filename = "azmet-shiny-template.html",
    
    pageHeatAccumulationCalculator = 
      bslib::page(
        title = NULL,
        theme = theme, # `scr##_theme.R`
        
        bslib::layout_sidebar(
          sidebar = sidebar, # `scr##_sidebar.R`
          
          shiny::htmlOutput(outputId = "navsetCardTabTitle"),
          shiny::htmlOutput(outputId = "navsetCardTabSummary"),
          shiny::uiOutput(outputId = "navsetCardTab")
        ) |>
          htmltools::tagAppendAttributes(
            #https://getbootstrap.com/docs/5.0/utilities/api/
            class = "border-0 rounded-0 shadow-none"
          ),
        
        shiny::htmlOutput(outputId = "downloadButtonsDiv"), # Common, regardless of card tab
        shiny::htmlOutput(outputId = "pageBottomText")
      )
  )


# Server --------------------


server <- function(input, output, session) {
  
  shinyjs::useShinyjs(html = TRUE)
  shinyjs::hideElement(id = "downloadButtonsDiv")
  shinyjs::hideElement(id = "navsetCardTab")
  
  
  # Observables -----
  
  shiny::observeEvent(heatAccumulation(), {
    shinyjs::showElement(id = "downloadButtonsDiv")
    shinyjs::showElement(id = "navsetCardTab")
    showNavsetCardTab(TRUE)
    showPageBottomText(TRUE)
  })
  
  # To update available dates based on selected station
  shiny::observeEvent(input$azmetStation, {
    stationStartDate <-
      dplyr::filter(azmetStationMetadata, meta_station_name == input$azmetStation) %>% 
      dplyr::pull(start_date)
    
    stationStartDateMinimum <- stationStartDate
    stationEndDateMinimum <- stationStartDate
    
    if (stationStartDate > input$startDate) {
      stationStartDateSelected <- stationStartDate
    } else {
      stationStartDateSelected <- input$startDate
    }
    
    if (stationStartDate > input$endDate) {
      stationEndDateSelected <- stationStartDate
    } else {
      stationEndDateSelected <- input$endDate
    }
    
    shiny::updateDateInput(
      inputId = "startDate",
      label = "Start Date",
      value = stationStartDateSelected,
      min = stationStartDateMinimum,
      max = Sys.Date() - 1
    )
    
    shiny::updateDateInput(
      inputId = "endDate",
      label = "End Date",
      value = stationEndDateSelected,
      min = stationEndDateMinimum,
      max = Sys.Date() - 1
    )
  })
  
  # Catch input errors before data download, show error modal
  shiny::observeEvent(input$calculateTotal, {
    if (input$startDate > input$endDate) {
      shiny::showModal(datepickerErrorModal) # `scr##_datepickerErrorModal.R`
    }
    
    if (
      input$azmetStation == "Yuma N.Gila" & 
      lubridate::int_overlaps(
        int1 = yugNodataInterval, 
        int2 = lubridate::interval(input$startDate, input$endDate)
      ) == TRUE
    ) {
      shiny::showModal(datepickerYumaNGilaErrorModal) # `scr##_datepickerYumaNGilaErrorModal.R`
    }
  })
  
  # To update icon in navsetCardTab title
  shiny::observeEvent(input$navsetCardTab, {
    if (input$navsetCardTab == "barChart") {
      navsetCardTabTitleIcon("bar-chart-fill")
    } else if (input$navsetCardTab == "table") {
      navsetCardTabTitleIcon("table")
    } else if (input$navsetCardTab == "timeSeries") {
      navsetCardTabTitleIcon("graph-up")
    }
  })
  
  
  heatAccumulation <- 
    shiny::eventReactive(input$calculateTotal, {
      # Catch input errors before data download
      shiny::validate(
        shiny::need(
          expr = input$startDate <= input$endDate,
          message = FALSE # Failing validation test
        ),
        
        shiny::need(
          expr =
            !(input$azmetStation == "Yuma N.Gila" &
                lubridate::int_overlaps(
                  int1 = yugNodataInterval,
                  int2 = lubridate::interval(input$startDate, input$endDate)
                )
            ),
          message = FALSE # Failing validation test
        )
      )
      
      idCalculateTotal <- shiny::showNotification(
        ui = "Calculating heat accumulation . . .",
        action = NULL,
        duration = NULL,
        closeButton = FALSE,
        id = "idCalculateTotal",
        type = "message"
      )
      
      on.exit(
        shiny::removeNotification(id = idCalculateTotal),
        add = TRUE
      )
      
      fxn_heatAccumulation(
        azmetStation = input$azmetStation,
        startDate = input$startDate,
        endDate = input$endDate,
        heatVariable = input$heatVariable
      )
    })
  
  navsetCardBarChart <- 
    shiny::eventReactive(heatAccumulation(), {
      fxn_navsetCardBarChart(
        inData = heatAccumulation()[[2]],
        azmetStation = input$azmetStation,
        heatVariable = input$heatVariable
      )
    })
  
  navsetCardBarChartCaption <- 
    shiny::eventReactive(heatAccumulation(), {
      fxn_navsetCardBarChartCaption(
        azmetStation = input$azmetStation,
        inData = heatAccumulation()[[2]],
        startDate = input$startDate,
        endDate = input$endDate,
        heatVariable = input$heatVariable
      )
    })
  
  navsetCardTable <- 
    shiny::eventReactive(heatAccumulation(), {
      fxn_navsetCardTable(
        inData = heatAccumulation()[[1]],
        startDate = input$startDate,
        endDate = input$endDate,
        heatVariable = input$heatVariable
      )
    })
  
  navsetCardTableCaption <-
    shiny::eventReactive(heatAccumulation(), {
      fxn_navsetCardTableCaption(heatVariable = input$heatVariable)
    })
  
  navsetCardTabSummary <-
    shiny::eventReactive(heatAccumulation(), {
      fxn_navsetCardTabSummary(
        azmetStation = input$azmetStation,
        inData = heatAccumulation()[[2]],
        startDate = input$startDate,
        endDate = input$endDate,
        heatVariable = input$heatVariable
      )
    })
  
  navsetCardTabTitle <-
    shiny::eventReactive(list(navsetCardTabTitleIcon(), heatAccumulation()), {
      fxn_navsetCardTabTitle(
        azmetStation = input$azmetStation,
        navsetCardTabTitleIcon = navsetCardTabTitleIcon()
      )
    })
  
  navsetCardTabTooltipText <- 
    shiny::eventReactive(input$navsetCardTab, {
      fxn_navsetCardTabTooltipText(navsetCardTab = input$navsetCardTab)
    })
  
  navsetCardTimeSeries <- 
    shiny::eventReactive(heatAccumulation(), {
      fxn_navsetCardTimeSeries(
        inData = heatAccumulation()[[1]],
        startDate = input$startDate,
        endDate = input$endDate,
        heatVariable = input$heatVariable
      )
    })
  
  navsetCardTimeSeriesCaption <-
    shiny::eventReactive(heatAccumulation(), {
      fxn_navsetCardTimeSeriesCaption(
        azmetStation = input$azmetStation,
        inData = heatAccumulation()[[1]],
        startDate = input$startDate,
        endDate = input$endDate,
        heatVariable = input$heatVariable
      )
    })
  
  pageBottomText <- 
    shiny::eventReactive(heatAccumulation(), {
      fxn_pageBottomText()
    })
  
  
  # Outputs -----
  
  output$dailyTable <- 
    renderTable({
      heatAccumulation()[[1]]
    })
  
  output$seasonalTable <- 
    renderTable({
      heatAccumulation()[[2]]
    })
  
  output$downloadButtonsDiv <-
    shiny::renderUI({
      fxn_downloadButtonsDiv()
    })
  
  # output$downloadCSV <-
  #   shiny::downloadHandler(
  #     filename = function() {
  #       if (input$heatVariable == "Chill Portions") {
  #         heatVariableText <- "chill-portions"
  #       } else if (input$heatVariable == "Hours below 32 °F") {
  #         heatVariableText <- "hours-below-32F"
  #       } else if (input$heatVariable == "Hours below 45 °F") {
  #         heatVariableText <- "hours-below-45F"
  #       } else if (input$heatVariable == "Hours between 32 and 45 °F") {
  #         heatVariableText <- "hours-between-32-and-45F"
  #       } else if (input$heatVariable == "Hours above 68 °F") {
  #         heatVariableText <- "hours-above-68F"
  #       } else if (input$heatVariable == "Utah Model") {
  #         heatVariableText <- "utah-model-chill-units"
  #       }
  #       
  #       paste0("AZMet-chill-accumulation-calculator-", heatVariableText, ".csv")
  #     },
  #     content = function(file) {
  #       vroom::vroom_write(x = heatAccumulation()[[1]], file = file, delim = ",")
  #     }
  #   )
  
  # output$downloadTSV <-
  #   shiny::downloadHandler(
  #     filename = function() {
  #       if (input$heatVariable == "Chill Portions") {
  #         heatVariableText <- "chill-portions"
  #       } else if (input$heatVariable == "Hours below 32 °F") {
  #         heatVariableText <- "hours-below-32F"
  #       } else if (input$heatVariable == "Hours below 45 °F") {
  #         heatVariableText <- "hours-below-45F"
  #       } else if (input$heatVariable == "Hours between 32 and 45 °F") {
  #         heatVariableText <- "hours-between-32-and-45F"
  #       } else if (input$heatVariable == "Hours above 68 °F") {
  #         heatVariableText <- "hours-above-68F"
  #       } else if (input$heatVariable == "Utah Model") {
  #         heatVariableText <- "utah-model-chill-units"
  #       }
  #       
  #       paste0("AZMet-chill-accumulation-calculator-", heatVariableText, ".tsv")
  #     },
  #     content = function(file) {
  #       vroom::vroom_write(x = heatAccumulation()[[1]], file = file, delim = "\t")
  #     }
  #   )
  
  output$navsetCardBarChart <-
    plotly::renderPlotly({
      navsetCardBarChart()
    })
  
  output$navsetCardBarChartCaption <-
    shiny::renderUI({
      navsetCardBarChartCaption()
    })
  
  output$navsetCardTab <-
    shiny::renderUI({
      shiny::req(showNavsetCardTab())
      navsetCardTab # `scr##_navsetCardTab.R`
    })
  
  output$navsetCardTable <-
    reactable::renderReactable({
      navsetCardTable()
    })
  
  output$navsetCardTableCaption <-
    shiny::renderUI({
      navsetCardTableCaption()
    })
  
  output$navsetCardTabSummary <-
    shiny::renderUI({
      navsetCardTabSummary()
    })
  
  output$navsetCardTabTitle <-
    shiny::renderUI({
      navsetCardTabTitle()
    })
  
  output$navsetCardTabTooltipText <-
    shiny::renderUI({
      navsetCardTabTooltipText()
    })
  
  output$navsetCardTimeSeries <-
    plotly::renderPlotly({
      navsetCardTimeSeries()
    })
  
  output$navsetCardTimeSeriesCaption <-
    shiny::renderUI({
      navsetCardTimeSeriesCaption()
    })
  
  output$pageBottomText <-
    shiny::renderUI({
      shiny::req(showPageBottomText())
      pageBottomText()
    })
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
