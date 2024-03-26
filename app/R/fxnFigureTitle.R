#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @param inData - data table of seasonal chill accumulation values by year
#' @param heatVariable - Heat variable selection by user
#' @param endDate - End date of period of interest
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function(inData, heatVariable, endDate) {
  currentYear <- lubridate::year(endDate)
  currentYearHeat <- 
    inData$heatSum[which(inData$endDateYear == currentYear)]
  currentYearHeatText <- inData$dateYearLabel[which(inData$endDateYear == currentYear)]
  
  previousYear <- currentYear - 1
  previousYearHeat <- 
    inData$heatSum[which(inData$endDateYear == previousYear)]
  previousYearHeatText <- inData$dateYearLabel[which(inData$endDateYear == previousYear)]
  
  if (nrow(inData) < 2) {
    figureTitle <- 
      htmltools::h4(
        htmltools::HTML(
          paste(
            "Total Number of", heatVariable, "in", currentYear,
            sep = " "
          ),
        ),
        
        class = "figure-title"
      )
  } else {
    if (currentYearHeat > (previousYearHeat + (0.1 * previousYearHeat))) {
      comparisonText <- "Greater than"
    } else if (currentYearHeat < (previousYearHeat - (0.1 * previousYearHeat))) {
      comparisonText <- "Less than"
    } else {
      comparisonText <- "Similar to"
    }
    
    figureTitle <- 
      htmltools::h4(
        htmltools::HTML(
          paste(
            "Total Number of", heatVariable, "in", currentYearHeatText, comparisonText, "That in", previousYearHeatText,
            sep = " "
          ),
        ),
        
        class = "figure-title"
      )
    
  }
  
  return(figureTitle)
}
