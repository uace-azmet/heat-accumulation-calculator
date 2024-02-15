#' fxnAZMetDataSumHeat: calculates heat accumulation based on user input
#' 
#' @param inData - dataAZMetdataELT
#' @param azmetStation - AZMet station selection by user
#' @param heatVariable - Heat variable selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `dataAZMetDataSumHeat` - Data table with cumulative heat units by year


fxnAZMetDataSumHeat <- function(inData, azmetStation, heatVariable, startDate, endDate) {
  if (length(unique(inData$date_year)) == 1) { # For single calendar year in data
    dateYear <- as.character(unique(inData$date_year))
  } else { # For two calendars year in data
    dateYear <- paste(min(unique(inData$date_year)), max(unique(inData$date_year)), sep = "-")
  }
  
  if (heatVariable == "Heat Units 86-45 °F") {
    dataAZMetDataSumHeat <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(heat_units_45F_cumulative = sum(heat_units_45F, na.rm = TRUE)) %>%
      dplyr::rename(heatSum = heat_units_45F_cumulative) %>%
      dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 0), nsmall = 0)) %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  } else if (heatVariable == "Heat Units 86-50 °F") {
    dataAZMetDataSumHeat <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(heat_units_50F_cumulative = sum(heat_units_50F, na.rm = TRUE)) %>%
      dplyr::rename(heatSum = heat_units_50F_cumulative) %>%
      dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 0), nsmall = 0)) %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  } else if (heatVariable == "Heat Units 86-55 °F") {
    dataAZMetDataSumHeat <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(heat_units_55F_cumulative = sum(heat_units_55F, na.rm = TRUE)) %>%
      dplyr::rename(heatSum = heat_units_55F_cumulative) %>%
      dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 0), nsmall = 0)) %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  } else if (heatVariable == "Heat Units 94-55 °F") {
    dataAZMetDataSumHeat <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(heat_units_9455F_cumulative = sum(heat_units_9455F, na.rm = TRUE)) %>%
      dplyr::rename(heatSum = heat_units_9455F_cumulative) %>%
      dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 0), nsmall = 0)) %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYear)
  }
  
  return(dataAZMetDataSumHeat)
}
