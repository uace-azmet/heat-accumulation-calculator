#' `fxn_navsetCardTimeSeriesCaption.R` - Build caption for time series graph based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - Data table [[1]] from `fxn_heatAccumulation.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param heatVariable - Heat variable selected by user
#' @return `navsetCardTimeSeriesCaption` Caption for time series graph based on user input


fxn_navsetCardTimeSeriesCaption <- function(azmetStation, inData, startDate, endDate, heatVariable) {
  
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation) %>% 
    dplyr::pull(start_date)
  
  if (heatVariable == "Growing Degree Hours") {
    heatVariableText <- "growing degree hours"
  } else if (heatVariable == "Heat Units 94-55 °F") {
    heatVariableText <- "heat units 94-55 °F"
  } else if (heatVariable == "Heat Units 86-55 °F") {
    heatVariableText <- "heat units 86-55 °F"
  } else if (heatVariable == "Heat Units 86-50 °F") {
    heatVariableText <- "heat units 86-50 °F"
  } else if (heatVariable == "Heat Units 86-45 °F") {
    heatVariableText <- "heat units 86-45 °F"
  }
  
  if (heatVariable == "Growing Degree Hours") {
    variableUnits <- "degree hours Fahrenheit (DHF)"
  } else { #if (heatVariable %in% c("Heat Units 94-55 °F", "Heat Units 86-55 °F", "Heat Units 86-50 °F", "Heat Units 86-45 °F"))
    variableUnits <- "degree days Farenheit (DDF)"
  }
  
  if (length(unique(inData$date_year_label)) == 1) {
    captionText <- 
      paste0(
        "Heat accumulation (black line in graph) is based on the sum of daily totals during the period of interest as represented by ", heatVariableText, ". Line breaks denote no data for that day. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  } else {
    captionText <- 
      paste0(
        "Heat accumulation for the current year (black line in graph) is based on the sum of daily totals during the period of interest as represented by ", heatVariableText, ". Totals for past years (gray lines in graph) are based on the same start and end month and day, but during those respective years. Line breaks denote no data for that day. Data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
      )
  }
  
  # Generate caption text with `heatR` reference
  if (heatVariable == "Growing Degree Hours") {
    captionText <- 
      paste0(
        captionText, " Growing degree hours are based on calculations in the <a href=https://eikeluedeling.r-universe.dev/chillR>chillR</a> R package."
      )
  } else { #if (heatVariable %in% c("Heat Units 94-55 °F", "Heat Units 86-55 °F", "Heat Units 86-50 °F", "Heat Units 86-45 °F"))
    captionText <- 
      paste0(
        captionText,
        " More information on the calculation of heat units is in Extension bulletin ", bulletinURL, "."
      )
  }
  
  variableKeyText <- 
    paste0(
      "Variable key: <strong>Day<sub>period</sub></strong> day number of the period of interest; <strong>Heat<sub>cumulative</sub></strong> accumulation of daily heat values in ", variableUnits, " during the period of interest as represented by ", heatVariableText
    )
  
  # Account for multi-month absence of YUG data in 2021
  nonOperational <- 0
  
  if (azmetStation == "Yuma N.Gila") {
    while (startDate >= azmetStationStartDate) {
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
        nonOperational <- 1
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # Generate figure footer based on presence/absence of non-operational dates
  if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
    navsetCardTimeSeriesCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            captionText,
            "However, we do not show heat accumulation for dates during the period from June 16, 2021 through October 21, 2021, when the ", azmetStation, " station was not in operation.",
            variableKeyText,
            sep = " "
          )
        ),
        
        class = "navset-card-caption"
      )
  } else {
    navsetCardTimeSeriesCaption <- 
      htmltools::p(
        htmltools::HTML(
          paste(captionText, variableKeyText, sep = " ")
        ), 
        class = "navset-card-caption"
      )
  }
  
  return(navsetCardTimeSeriesCaption)
}
