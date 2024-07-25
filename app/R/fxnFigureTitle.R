#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @param inData - data table of seasonal chill accumulation values by year
#' @param endDate - End date of period of interest
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function(inData, endDate) {
  heatSum <- dplyr::filter(inData, endDateYear == lubridate::year(endDate))$heatSum
  heatSum <- format(round(heatSum, digits = 1), nsmall = 1)
  
  figureTitle <- 
    htmltools::h4(
      htmltools::HTML(
        paste0(
          "<b>", heatSum, " degree days", "</b>"
        ),
      ),
      
      class = "figure-title"
    )
  
  return(figureTitle)
}
