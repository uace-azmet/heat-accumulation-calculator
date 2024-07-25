#' fxnAZMetDataMerge: downloads and merges individual-year data since API database start and based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param heatVariable - Heat variable selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `dataAZMetDataMerge` - merged data tables from individual years


fxnAZMetDataMerge <- function(azmetStation, heatVariable, startDate, endDate) {
  azmetStationStartDate <- apiStartDate # Placeholder for station start date
  
  while (startDate >= azmetStationStartDate) {
    
    dataAZMetDataELT <- fxnAZMetDataELT(
      azmetStation = azmetStation, 
      timeStep = "Daily", 
      startDate = startDate, 
      endDate = endDate
    )
    
    dataAZMetDataHeatSum <- fxnAZMetDataHeatSum(
      inData = dataAZMetDataELT,
      azmetStation = azmetStation, 
      heatVariable = heatVariable,
      startDate = startDate, 
      endDate = endDate
    )
    
    if (azmetStation == "Yuma North Gila") {
      nodataDateRange <- 
        lubridate::interval(
          start = lubridate::date("2021-06-16"), 
          end = lubridate::date("2021-10-21")
        )
      
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        dataAZMetDataHeatSum$heatSum <- 0.00
        dataAZMetDataHeatSum$heatSumLabel <- "NA" 
      }
    }
    
    if (exists("dataAZMetDataMerge") == FALSE) {
      dataAZMetDataMerge <- dataAZMetDataHeatSum
    } else {
      dataAZMetDataMerge <- rbind(dataAZMetDataMerge, dataAZMetDataHeatSum)
    }
    
    startDate <- min(seq(startDate, length = 2, by = "-1 year"))
    endDate <- min(seq(endDate, length = 2, by = "-1 year"))
  }
  
  return(dataAZMetDataMerge)
}
