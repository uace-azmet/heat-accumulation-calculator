#' `fxn_heatTotal` - Calculates heat accumulation for an individual season
#' 
#' @param inData - Returned output from `fxn_dailyData.R`
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param heatVariable - Heat variable selected by user
#' @return `heatTotal` - Data table with heat accumulation for an individual season


fxn_heatTotal <- function(inData, azmetStation, startDate, endDate, heatVariable) {
  
  # For x-axis labels and related text of comparison to previous years
  if (lubridate::year(startDate) == lubridate::year(endDate)) { # For data request spanning a single calendar year
    dateYearLabel <- as.character(lubridate::year(startDate))
  } else { # For data request spanning two calendar years
    dateYearLabel <- 
      paste(
        lubridate::year(startDate), 
        lubridate::year(endDate), 
        sep = "-"
      )
  }
  
  if (nrow(inData) == 0) { # For case of empty data return
    heatTotal <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = length(c("meta_station_name", "heatTotal", "heatTotalLabel", "endDateYear", "dateYearLabel"))
    ))
    
    colnames(heatTotal) <- 
      c("meta_station_name", "heatTotal", "heatTotalLabel", "endDateYear", "dateYearLabel")
    
    heatTotal <- heatTotal %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(heatTotal = 0.0) %>%
      dplyr::mutate(heatTotalLabel = "NA") %>%
      dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
      dplyr::mutate(dateYearLabel = dateYearLabel)
  } else {
    if (heatVariable == "Heat Units 94-55 °F") {
      heatTotal <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(heat_units_9455F_total = sum(heat_units_9455F, na.rm = TRUE)) %>%
        dplyr::rename(heatTotal = heat_units_9455F_total) %>%
        dplyr::mutate(heatTotalLabel = format(round(heatTotal, digits = 1), nsmall = 1)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (heatVariable == "Heat Units 86-55 °F") {
      heatTotal <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(cheat_units_55F_total = sum(heat_units_55F, na.rm = TRUE)) %>%
        dplyr::rename(heatTotal = heat_units_55F_total) %>%
        dplyr::mutate(heatTotalLabel = format(round(heatTotal, digits = 1), nsmall = 1)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (heatVariable == "Heat Units 86-50 °F") {
      heatTotal <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(heat_units_50F_total = sum(heat_units_50F, na.rm = TRUE)) %>%
        dplyr::rename(heatTotal = heat_units_50F_total) %>%
        dplyr::mutate(heatTotalLabel = format(round(heatTotal, digits = 1), nsmall = 1)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    } else if (heatVariable == "Heat Units 86-45 °F") {
      heatTotal <- inData %>%
        dplyr::group_by(meta_station_name) %>%
        dplyr::summarize(heat_units_45F_total = sum(heat_units_45F, na.rm = TRUE)) %>%
        dplyr::rename(heatTotal = heat_units_45F_total) %>%
        dplyr::mutate(heatTotalLabel = format(round(heatTotal, digits = 1), nsmall = 1)) %>%
        dplyr::mutate(endDateYear = lubridate::year(endDate)) %>%
        dplyr::mutate(dateYearLabel = dateYearLabel)
    }
  }
  
  return(heatTotal)
}
