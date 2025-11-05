#' `fxn_seasonalTotals` - Combines seasonal heat accumulation from individual years
#' 
#' @param azmetStation - AZMet station selected by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param heatVariable - Heat variable selected by user
#' @return `seasonalTotals` - Data table of seasonal heat accumulation from individual years


fxn_seasonalTotals <- function(azmetStation, startDate, endDate, heatVariable) {
  
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation)$start_date
    
  azDaily <- 
    fxn_azDaily(
      azmetStation = azmetStation,
      startDate = azmetStationStartDate, # To call API only once
      endDate = endDate
    )
  
  while (startDate >= azmetStationStartDate) {
    seasonalData <- 
      dplyr::filter(
        azDaily,
        datetime >= startDate & datetime <= endDate
      )
    
    # Calculate seasonal total from individual year and prepare data for graph
    heatTotal <- 
      fxn_heatTotal(
        inData = seasonalData,
        azmetStation = azmetStation, 
        startDate = startDate, 
        endDate = endDate,
        heatVariable = heatVariable
      )
    
    # Account for multi-month absence of YUG data in 2021
    if (azmetStation == "Yuma N.Gila") {
      nodataDateRange <-
        lubridate::interval(
          start = lubridate::date("2021-06-16"),
          end = lubridate::date("2021-10-21")
        )

      userDateRange <- lubridate::interval(start = startDate, end = endDate)

      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        heatTotal$heatTotal <- 0
        heatTotal$heatTotalLabel <- "NA"
      }
    }

    if (exists("seasonalTotals") == FALSE) {
      seasonalTotals <- heatTotal
    } else {
      seasonalTotals <- rbind(seasonalTotals, heatTotal)
    }
    
    startDate <- min(seq(lubridate::date(startDate), length = 2, by = "-1 year"))
    endDate <- min(seq(lubridate::date(endDate), length = 2, by = "-1 year"))
  }
  
  # Account for multi-month absence of YUG data in 2021
  if (azmetStation == "Yuma N.Gila") {
    seasonalTotals <- seasonalTotals %>% 
      dplyr::filter(heatTotalLabel != "NA")
  }
  
  return(seasonalTotals)
}
