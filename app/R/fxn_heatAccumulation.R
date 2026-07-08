#' `fxn_heatAccumulation` - Calculates heat accumulation by day and season for period of interest and individual years
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @param heatVariable - Heat variable selected by user
#' @return `heatAccumulation` - List of daily [[1]] and seasonal [[2]] data tables of values for individual years


fxn_heatAccumulation <- function(azmetStation, startDate, endDate, heatVariable) {
  
  azmetStationStartDate <- 
    dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation) %>% 
    dplyr::pull(start_date)
    
  
  # Data download, hourly to daily transform -----
  
  startDateDownload <- startDate
  endDateDownload <- endDate
  
  while (startDateDownload >= azmetStationStartDate) {
    if (heatVariable == "Growing Degree Hours") {
      azHourly <-  
        fxn_azHourly(
          azmetStation = azmetStation,
          startDate = startDateDownload, # To call API by individual season
          endDate = endDateDownload
        )
      
      azDaily <- azHourly %>% 
        fxn_hourlyHeatVarsToDaily(inData = ., azmetStation = azmetStation)
    } else { # heatVariable %in% c("Heat Units 94-55 °F", "Heat Units 86-55 °F", "Heat Units 86-50 °F", "Heat Units 86-45 °F")
      azDaily <- 
        fxn_azDaily(
          azmetStation = azmetStation,
          startDate = startDateDownload, # To call API by individual season
          endDate = endDateDownload
        )
    }
    
    if (exists("azDailySeasons") == FALSE) {
      azDailySeasons <- azDaily
    } else {
      azDailySeasons <- rbind(azDailySeasons, azDaily)
    }
    
    startDateDownload <- 
      min(seq(lubridate::date(startDateDownload), length = 2, by = "-1 year"))
    
    endDateDownload <- 
      min(seq(lubridate::date(endDateDownload), length = 2, by = "-1 year"))
  }
  
  
  # Data variable transform -----
  
  if (heatVariable == "Growing Degree Hours") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(heat = growing_degree_hours)
    } else if (heatVariable == "Heat Units 94-55 °F") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(heat = heat_units_9455F)
  } else if (heatVariable == "Heat Units 86-55 °F") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(heat = heat_units_55F)
  } else if (heatVariable == "Heat Units 86-50 °F") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(heat = heat_units_50F)
  } else if (heatVariable == "Heat Units 86-45 °F") {
    azDailySeasons <- azDailySeasons %>% 
      dplyr::rename(heat = heat_units_45F)
  }
  
  azDailySeasons <- azDailySeasons %>% 
    dplyr::select(dplyr::all_of(c("datetime", "meta_station_name", "heat")))
  
  # Calculate accumulation by individual year
  while (startDate >= azmetStationStartDate) {
    
    userDateRange <- lubridate::interval(start = startDate, end = endDate)
    
    if (azmetStation == "Yuma N.Gila" & startDate %within% yugNodataInterval & endDate %within% yugNodataInterval) {
      # Handle empty daily data table at YUG
      singleYearDaily <-
        tibble::tibble( 
          datetime = seq(lubridate::ymd(startDate), lubridate::ymd(endDate), by = "days"),
          meta_station_name = azmetStation,
          heat = NA_real_,
          heat_acc = NA_real_
        )
    } else {
      singleYearDaily <- 
        dplyr::filter(azDailySeasons, datetime >= startDate & datetime <= endDate)
    }
    
    singleYearDaily <- singleYearDaily %>% 
      dplyr::mutate(
        heat_acc = 
          dplyr::if_else(
            condition = is.na(heat),
            true = NA_real_,
            false = 
              round((cumsum(tidyr::replace_na(heat, 0))), digits = 1)
          )
      )
    
    singleYearDaily <- singleYearDaily %>%
      dplyr::mutate(
        date_year_label =
          dplyr::if_else(
            condition = lubridate::year(startDate) == lubridate::year(endDate),
            true = as.character(lubridate::year(startDate)),
            false = paste(lubridate::year(startDate), lubridate::year(endDate), sep = "-")
          ),
        day_of_period = dplyr::row_number()
      )
    
    if (azmetStation == "Yuma N.Gila" & lubridate::int_overlaps(int1 = yugNodataInterval, int2 = userDateRange) == TRUE) {
      # Handle partially empty or empty daily data table at YUG
      singleYearDaily <- singleYearDaily %>%
        dplyr::mutate(
          heat_acc =
            dplyr::if_else(
              condition = datetime < yugNodataStartDate,
              true = heat_acc,
              false = NA_real_
            )
        )
    }
    
    # With `singleYearDaily` transformed, calculate seasonal totals
    singleYearSeasonal <-
      fxn_heatAccumulationSeasonal(
        azmetStation = azmetStation,
        inData = singleYearDaily,
        startDate = startDate,
        endDate = endDate,
        heatVariable = heatVariable,
        userDateRange = userDateRange
      )
    
    # Build data tables for return
    if (exists("dailyAccumulations") == FALSE) {
      dailyAccumulations <- singleYearDaily
    } else {
      dailyAccumulations <- rbind(dailyAccumulations, singleYearDaily)
    }
    
    if (exists("seasonalAccumulations") == FALSE) {
      seasonalAccumulations <- singleYearSeasonal
    } else {
      seasonalAccumulations <- rbind(seasonalAccumulations, singleYearSeasonal)
    }
    
    # Setup for analysis of data from previous year
    startDate <- min(seq(lubridate::date(startDate), length = 2, by = "-1 year"))
    endDate <- min(seq(lubridate::date(endDate), length = 2, by = "-1 year"))
  }
  
  return(list(dailyAccumulations, seasonalAccumulations))
}
