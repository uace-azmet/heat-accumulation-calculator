#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table of seasonal heat accumulation values by year
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param heatVariable - Heat variable selection by user
#' @return `figureSubtitle` - Subtitle for figure based on selected AZMet station


fxnFigureSubtitle <- function(azmetStation, inData, startDate, endDate, heatVariable) {
  currentYear <- lubridate::year(endDate)
  previousYear <- currentYear - 1
  
  currentYearHeatSum <- dplyr::filter(inData, endDateYear == currentYear)$heatSum
  previousYearHeatSum <- dplyr::filter(inData, endDateYear == previousYear)$heatSum
  totalComparePreviousSum <- currentYearHeatSum - previousYearHeatSum
  
  previousYearText <- dplyr::filter(inData, endDateYear == previousYear)$dateYearLabel
  
  if (totalComparePreviousSum == 0) {
    compareTextPrevious <- "the same as"
  } else if (totalComparePreviousSum > 0 & totalComparePreviousSum < 1) {
    compareTextPrevious <- 
      paste0(
        format(abs(round(totalComparePreviousSum, digits = 1)), nsmall = 1), " degree days Fahrenheit greater than"
      )
  } else if (totalComparePreviousSum == 1) {
    compareTextPrevious <- 
      paste0(
        format(abs(round(totalComparePreviousSum, digits = 1)), nsmall = 1), " degree day Fahrenheit greater than"
      )
  } else if (totalComparePreviousSum > 1) {
    compareTextPrevious <- 
      paste0(
        format(abs(round(totalComparePreviousSum, digits = 1)), nsmall = 1), " degree days Fahrenheit greater than"
      )
  } else if (totalComparePreviousSum < 0 & totalComparePreviousSum > -1) {
    compareTextPrevious <- 
      paste0(
        format(abs(round(totalComparePreviousSum, digits = 1)), nsmall = 1), " degree days Fahrenheit less than"
      )
  } else if (totalComparePreviousSum == -1) {
    compareTextPrevious <- 
      paste0(
        format(abs(round(totalComparePreviousSum, digits = 1)), nsmall = 1), " degree day Fahrenheit less than"
      )
  } else { # if (totalComparePreviousSum < -1)
    compareTextPrevious <- 
      paste0(
        format(abs(round(totalComparePreviousSum, digits = 1)), nsmall = 1), " degree days Fahrenheit less than"
      )
  }
  
  # TODO: Add average information
  # TODO: if() for != MOH, WEL, YUE
  if (nrow(inData) == 1) {
    figureSubtitle <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Heat accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearHeatSum, digits = 1), nsmall = 1), " degree days Fahrenheit</b>."
          ),
        ),
        
        class = "figure-subtitle"
      )
  } else {
    figureSubtitle <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Heat accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is ", "<b>", format(round(currentYearHeatSum, digits = 1), nsmall = 1), " degree days Fahrenheit</b>. This is ", compareTextPrevious, " the total during this same month-day period in ", previousYearText, "."
          ),
        ),
        
        class = "figure-subtitle"
      )
  }
  
  return(figureSubtitle)
}
