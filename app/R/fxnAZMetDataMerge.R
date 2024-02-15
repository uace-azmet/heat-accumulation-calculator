#' fxnAZMetDataMerge: downloads and merges individual-year data since API database start and based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param heatVariable - Heat variable selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `dataAZMetDataMerge` - merged data tables from individual years


fxnAZMetDataMerge <- function(azmetStation, heatVariable, startDate, endDate) {
  azmetStationStartDate <- lubridate::as_date("2021-01-01") # Placeholder for station start date
  
  while (startDate >= azmetStationStartDate) {
    
    dataAZMetDataELT <- fxnAZMetDataELT(
      azmetStation = azmetStation, timeStep = "Daily", startDate = startDate, endDate = endDate
    )
    
    # For case of empty data return
    if (nrow(dataAZMetDataELT) == 0) {
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    } else {
      # Heat accumulation calculation
      dataAZMetDataSumHeat <- fxnAZMetDataSumHeat(
        inData = dataAZMetDataELT,
        azmetStation = azmetStation, 
        heatVariable = heatVariable,
        startDate = startDate, 
        endDate = endDate
      )
      
      if (exists("dataAZMetDataMerge") == FALSE) {
        dataAZMetDataMerge <- dataAZMetDataSumHeat
      } else {
        dataAZMetDataMerge <- rbind(dataAZMetDataMerge, dataAZMetDataSumHeat)
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # For case of missing data from Yuma North Gila
  if (azmetStation == "Yuma North Gila" && endDate >= lubridate::as_date(paste0(lubridate::year(endDate), "-06-16"))) {
    dataAZMetDataMerge <- dataAZMetDataMerge %>%
      dplyr::filter(endDateYear != 2021)
  }
  
  # For case of data record length from Wellton ETo and Yuma Valley ETo
  if (azmetStation == "Wellton ETo" && endDate > lubridate::as_date(paste0(lubridate::year(endDate), "-05-02"))) {
    dataAZMetDataMerge <- dataAZMetDataMerge %>%
      dplyr::filter(endDateYear >= 2024)
  }
  if (azmetStation == "Yuma Valley ETo" && endDate > lubridate::as_date(paste0(lubridate::year(endDate), "-05-02"))) {
    dataAZMetDataMerge <- dataAZMetDataMerge %>%
      dplyr::filter(endDateYear >= 2024)
  }
  
  return(dataAZMetDataMerge)
}
