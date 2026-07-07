#' `fxn_navsetCardBarChart` Generates bar chart of heat accumulation of current and recent years
#' 
#' @param inData - Data table [[2]] from `fxn_heatAccumulation.R`
#' @param azmetStation - User-specified AZMet station
#' @param heatVariable - Heat variable selected by user
#' @return `navsetCardBarChart` - plotly bar chart

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_navsetCardBarChart <- function(inData, azmetStation, heatVariable) {
  
  averageAccumulation <- 
    round(mean(inData$heat_accumulation_seasonal, na.rm = TRUE), digits = 1)
  
  if (heatVariable == "Growing Degree Hours") {
    axisVarUnits <- "Degree Hours Fahrenheit (DDH)"
    hoverTextVarUnits <- "DDH"
  } else { #if (heatVariable %in% c("Heat Units 94-55 °F", "Heat Units 86-55 °F", "Heat Units 86-50 °F", "Heat Units 86-45 °F"))
    axisVarUnits <- "Degree Days Fahrenheit (DDF)"
    hoverTextVarUnits <- "DDF"
  }
  
  dataCurrentYear <- inData %>% 
    dplyr::filter(end_date_year == max(end_date_year)) %>%
    dplyr::mutate(end_date_year = as.factor(end_date_year))
  
  dataOtherYears <- inData %>% 
    dplyr::filter(end_date_year != max(end_date_year)) %>% 
    dplyr::mutate(end_date_year = as.factor(end_date_year))
  
  layoutFontFamily <- "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  
  ticktext <- inData$date_year_label
  tickvals <- inData$end_date_year
  
  
  # Bar Chart -----
  
  # For stations with only one year of data
  if (nrow(inData) == 1) {
    navsetCardBarChart <- 
      plotly::plot_ly( # Bars for `dataOtherYears`
        data = dataOtherYears,
        x = ~end_date_year,
        y = ~heat_accumulation_seasonal,
        marker = list(color = "#bfbfbf"),
        name = "other years",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = 
          ~paste0(
            "<br><b>AZMet Station:</b> ", azmetStation,
            "<br><b>Year:</b> ", date_year_label,
            "<br><b>Accumulation:</b> ", heat_accumulation_seasonal_label, " ", hoverTextVarUnits
          ),
        type = "bar"
      ) %>% 
      
      plotly::add_trace( # Bar for `dataCurrentYear`
        inherit = FALSE,
        data = dataCurrentYear,
        x = ~end_date_year,
        y = ~heat_accumulation_seasonal,
        marker = list(color = "#191919"),
        name = "current year",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = 
          ~paste0(
            "<br><b>AZMet Station:</b> ", azmetStation,
            "<br><b>Year:</b> ", date_year_label,
            "<br><b>Accumulation:</b> ", heat_accumulation_seasonal_label, " ", hoverTextVarUnits
          ),
        type = "bar"
      ) %>%
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE,
        modeBarButtonsToRemove = 
          c(
            "autoScale2d",
            "hoverClosestCartesian", 
            "hoverCompareCartesian", 
            "lasso2d",
            "select"
          ),
        scrollZoom = FALSE,
        toImageButtonOptions = 
          list(
            format = "png", # Either png, svg, jpeg, or webp
            filename = "AZMet-heat-accumulation-calculator",
            height = 400,
            width = 700,
            scale = 5
          )
      ) %>%
      
      plotly::layout(
        font = list(color = "#191919", family = layoutFontFamily, size = 13),
        hoverlabel = 
          list(
            font = list(family = layoutFontFamily, size = 14)
          ),
        margin = 
          list(
            l = 0,
            r = 0, # For space between plot and modebar
            b = 0,
            t = 0,
            pad = 3 # For space between gridlines and yaxis labels
          ),
        modebar = list(bgcolor = "#FFFFFF", orientation = "v"),
        xaxis = 
          list(
            fixedrange = TRUE,
            linewidth = 0,
            ticktext = ticktext,
            tickvals = tickvals,
            title = 
              list(
                font = list(size = 14), 
                standoff = 25, 
                text = "<b>Year</b>"
              ),
            zeroline = FALSE
          ),
        yaxis = 
          list(
            fixedrange = TRUE,
            gridcolor = "#c9c9c9",
            title = 
              list(
                font = list(size = 14),
                standoff = 25,
                text = paste0("<b>", axisVarUnits, "</b>")
              ),
            zeroline = TRUE,
            zerolinecolor = "#c9c9c9"
          )
        )
  } else {
    navsetCardBarChart <- 
      plotly::plot_ly( # Bars for `dataOtherYears`
        data = dataOtherYears,
        x = ~end_date_year,
        y = ~heat_accumulation_seasonal,
        marker = list(color = "#bfbfbf"),
        name = "other years",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = 
          ~paste0(
            "<br><b>AZMet Station:</b> ", azmetStation,
            "<br><b>Year:</b> ", date_year_label,
            "<br><b>Accumulation:</b> ", heat_accumulation_seasonal_label, " ", hoverTextVarUnits
          ),
        type = "bar"
      ) %>% 
      
      plotly::add_trace( # Bar for `dataCurrentYear`
        inherit = FALSE,
        data = dataCurrentYear,
        x = ~end_date_year,
        y = ~heat_accumulation_seasonal,
        marker = list(color = "#191919"),
        name = "current year",
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = 
          ~paste0(
            "<br><b>AZMet Station:</b> ", azmetStation,
            "<br><b>Year:</b> ", date_year_label,
            "<br><b>Accumulation:</b> ", heat_accumulation_seasonal_label, " ", hoverTextVarUnits
          ),
        type = "bar"
      ) %>%
      
      plotly::config(
        displaylogo = FALSE,
        displayModeBar = FALSE,
        modeBarButtonsToRemove = 
          c(
            "autoScale2d",
            "hoverClosestCartesian", 
            "hoverCompareCartesian", 
            "lasso2d",
            "select"
          ),
        scrollZoom = FALSE,
        toImageButtonOptions = 
          list(
            format = "png", # Either png, svg, jpeg, or webp
            filename = "AZMet-heat-accumulation-calculator",
            height = 400,
            width = 700,
            scale = 5
          )
      ) %>%
      
      plotly::layout(
        annotations = 
          list(
            align = "left",
            font = list(color = "#808080", family = layoutFontFamily, size = 14),
            showarrow = FALSE,
            text = 
              paste(
                "<b>Average: ", format(round(averageAccumulation, digits = 1), nsmall = 1), hoverTextVarUnits, "</b>"
              ),
            x = 0,
            xanchor = "left",
            xref = "paper",
            xshift = 12,
            y = averageAccumulation,
            yanchor = "bottom",
            yref = "y",
            yshift = 0
          ),
        font = list(color = "#191919", family = layoutFontFamily, size = 13),
        hoverlabel = 
          list(
            font = list(family = layoutFontFamily, size = 14)
          ),
        margin = 
          list(
            l = 0,
            r = 0, # For space between plot and modebar
            b = 0,
            t = 0,
            pad = 3 # For space between gridlines and yaxis labels
          ),
        modebar = list(bgcolor = "#FFFFFF", orientation = "v"),
        shapes =
          list(
            type = "line",
            layer = "above",
            line = list(color = "#808080", dash = "solid", width = 1),
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = averageAccumulation,
            y1 = averageAccumulation,
            yref = "y"
          ),
        xaxis = 
          list(
            fixedrange = TRUE,
            linewidth = 0,
            ticktext = ticktext,
            tickvals = tickvals,
            title = 
              list(
                font = list(size = 14),
                standoff = 25,
                text = "<b>Year</b>"
              ),
            zeroline = FALSE
          ),
        yaxis = 
          list(
            fixedrange = TRUE,
            gridcolor = "#c9c9c9",
            title = 
              list(
                font = list(size = 14),
                standoff = 25,
                text = paste0("<b>", axisVarUnits, "</b>")
              ),
            zeroline = TRUE,
            zerolinecolor = "#c9c9c9"
          )
      )
  }
  
  return(navsetCardBarChart)
}
