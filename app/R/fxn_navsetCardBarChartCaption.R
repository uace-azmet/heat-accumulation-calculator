#' `fxn_navsetCardBarChartCaption.R` - Build caption for bar chart based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - Data table [[2]] from `fxn_heatAccumulation.R`
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param heatVariable - Heat variable selected by user
#' @return `navsetCardBarChartCaption` Caption for bar chart based on selected station


fxn_navsetCardBarChartCaption <- 
  function(azmetStation, inData, startDate, endDate, heatVariable) {
    
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
    
    if (nrow(inData) == 1) {
      captionText <- 
        paste0(
          "Heat accumulation for the current year (black bar in graph) is based on the sum of daily values of ", heatVariableText, " from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), ". Temperature data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
        )
    } else {
      captionText <- 
        paste0(
          "Heat accumulation for the current year (black bar in graph) is based on the sum of daily values of ", heatVariableText, " from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), ". Accumulations for past years (gray bars in graph) are based on the same start and end month and day, but during those respective years. Average heat accumulation is calculated from values of all individual years shown above. Temperature data for the ", azmetStation, " station in the new AZMet database currently go back to ", gsub(" 0", " ", format(azmetStationStartDate, "%B %d, %Y")), "."
        )
    }
    
    # Account for multi-month absence of YUG data in 2021
    nonOperational <- 0
    
    if (azmetStation == "Yuma N.Gila") {
      nodataDateRange <-
        lubridate::interval(
          start = lubridate::date("2021-06-16"),
          end = lubridate::date("2021-10-21")
        )
      
      while (startDate >= azmetStationStartDate) {
        userDateRange <- lubridate::interval(start = startDate, end = endDate)
        
        if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
          nonOperational <- 1
        }
        
        startDate <- min(seq(startDate, length = 2, by = "-1 year"))
        endDate <- min(seq(endDate, length = 2, by = "-1 year"))
      }
    }
    
    # Generate caption text based on presence/absence of non-operational dates
    if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
      captionText <- 
        paste(
          captionText,
          "However, we do not show heat accumulation for the year with a month-day period that overlaps the period from June 16, 2021 through October 21, 2021, when the ", azmetStation, " station was not in operation.",
          sep = " "
        )
    } else {
      captionText <- captionText
    }
    
    # Generate caption text with `chillR` reference
    if (heatVariable == "Growing Degree Hours") {
      captionText <- 
        paste0(
          captionText,
          " Growing degree hours are based on calculations in the <a href=https://eikeluedeling.r-universe.dev/heatR>heatR</a> R package."
        )
    } else {
      captionText <- captionText
    }
    
    # Format caption text as HTML
    navsetCardBarChartCaption <- 
      htmltools::p(
        htmltools::HTML(captionText), 
        class = "navset-card-caption"
      )
    
    return(navsetCardBarChartCaption)
  }
