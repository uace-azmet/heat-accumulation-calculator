#' `fxn_heatAccumulationSeasonal` - Calculates heat accumulation for an individual season
#' 
#' @param inData - Transformed data table of daily values from `fxn_heatAccumulation.R`
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param heatVariable - Heat variable selected by user
#' @param userDateRange - Date interval based on `startDate` and `endDate`
#' @return `heatAccumulationSeasonal` - Data table with heat accumulation for an individual season


fxn_heatAccumulationSeasonal <- 
  function(azmetStation, inData, startDate, endDate, heatVariable, userDateRange) {
    
    # For x-axis labels and related text of comparison to previous years
    if (lubridate::year(startDate) == lubridate::year(endDate)) { # For data request spanning a single calendar year
      dateYearLabel <- as.character(lubridate::year(startDate))
    } else { # For data request spanning two calendar years
      dateYearLabel <- 
        paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
    }
    
    if (nrow(inData) == 0) { # For case of empty data return
      
      heatAccumulationSeasonal <- 
        data.frame(
          matrix(
            data = NA,
            nrow = 1, 
            ncol = 
              length(
                c(
                  "meta_station_name", 
                  "heat_accumulation_seasonal", 
                  "heat_accumulation_seasonal_label", 
                  "end_date_year", 
                  "date_year_label"
                )
              )
          )
        )
      
      colnames(heatAccumulationSeasonal) <- 
        c(
          "meta_station_name", 
          "heat_accumulation_seasonal", 
          "heat_accumulation_seasonal_label", 
          "end_date_year", 
          "date_year_label"
        )
      
      heatAccumulationSeasonal <- heatAccumulationSeasonal %>%
        dplyr::mutate(meta_station_name = azmetStation) %>%
        dplyr::mutate(heat_accumulation_seasonal = 0.00) %>%
        dplyr::mutate(heat_accumulation_seasonal_label = "NA") %>%
        dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
        dplyr::mutate(date_year_label = dateYearLabel)
      
    } else {
      heatAccumulationSeasonal <- inData %>%
        dplyr::summarize(heat_accumulation_seasonal = sum(heat, na.rm = TRUE)) %>%
        dplyr::mutate(
          heat_accumulation_seasonal_label = 
            format(round(heat_accumulation_seasonal, digits = 1), nsmall = 1)
        ) %>%
        dplyr::mutate(end_date_year = lubridate::year(endDate)) %>%
        dplyr::mutate(date_year_label = dateYearLabel)
    }
    
    if (azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
      heatAccumulationSeasonal$heat_accumulation_seasonal <- NA_real_
      heatAccumulationSeasonal$heat_accumulation_seasonal_label <- "NA"
    }
    
    return(heatAccumulationSeasonal)
  }
