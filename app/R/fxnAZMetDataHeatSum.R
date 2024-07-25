#' fxnAZMetDataHeatSum: calculates heat accumulation based on user input
#' 
#' @param inData - dataAZMetdataELT
#' @param azmetStation - AZMet station selection by user
#' @param heatVariable - Heat variable selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `dataAZMetDataHeatSum` - Data table with cumulative heat units by year


fxnAZMetDataHeatSum <- function(inData, azmetStation, heatVariable, startDate, endDate) {
  # For x-axis labels and related text of comparison to previous years
  if (lubridate::year(startDate) == lubridate::year(endDate)) { # For data request spanning a single calendar year
    dateYear <- as.character(lubridate::year(startDate))
  } else { # For data request spanning two calendar years
    dateYear <- 
      paste(
        lubridate::year(startDate), 
        lubridate::year(endDate), 
        sep = "-"
      )
  }
  
  if (nrow(inData) == 0) { # For case of empty data return
    dataAZMetDataHeatSum <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = length(c("meta_station_name", "etTotal", "etTotalLabel", "endDateYear", "dateYearLabel"))
    ))
    
    colnames(dataAZMetDataHeatSum) <- 
      c("meta_station_name", "heatSum", "heatSumLabel", "endDateYear", "dateYearLabel")
    
    dataAZMetDataHeatSum <- dataAZMetDataHeatSum %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(heatSum = 0.00) %>%
      dplyr::mutate(heatSumLabel = "NA") %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  } else {
    if (heatVariable == "Heat Units 86-45 °F") {
      dataAZMetDataHeatSum <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(heat_units_45F_cumulative = sum(heat_units_45F, na.rm = TRUE)) %>%
        dplyr::rename(heatSum = heat_units_45F_cumulative) %>%
        dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYear)
    } else if (heatVariable == "Heat Units 86-50 °F") {
      dataAZMetDataHeatSum <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(heat_units_50F_cumulative = sum(heat_units_50F, na.rm = TRUE)) %>%
        dplyr::rename(heatSum = heat_units_50F_cumulative) %>%
        dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYear)
    } else if (heatVariable == "Heat Units 86-55 °F") {
      dataAZMetDataHeatSum <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(heat_units_55F_cumulative = sum(heat_units_55F, na.rm = TRUE)) %>%
        dplyr::rename(heatSum = heat_units_55F_cumulative) %>%
        dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYear)
    } else if (heatVariable == "Heat Units 94-55 °F") {
      dataAZMetDataHeatSum <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(heat_units_9455F_cumulative = sum(heat_units_9455F, na.rm = TRUE)) %>%
        dplyr::rename(heatSum = heat_units_9455F_cumulative) %>%
        dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 0), nsmall = 0)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYear)
    }
  }
  
  return(dataAZMetDataHeatSum)
}
