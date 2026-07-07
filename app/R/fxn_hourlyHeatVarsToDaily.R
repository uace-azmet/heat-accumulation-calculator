#' `fxn_hourlyHeatVarsToDaily.R` Compute heat values for variables dependent on hourly data
#' 
#' @param inData - returned output from `fxn_hourlyData.R`
#' @param azmetStation - user-specified AZMet station
#' @return `hourlyHeatVarsToDaily` - Tibble of daily values for heat variables dependent on hourly data


fxn_hourlyHeatVarsToDaily <- function(inData, azmetStation) {
  
  hourlyHeatVarsToDaily <- 
    as.data.frame(inData) %>%
    
    # Patch for `date_doy` and `date_year` error in database, since corrected
    # dplyr::mutate(date_doy = lubridate::yday(date_datetime)) %>%
    # dplyr::mutate(date_year = lubridate::year(date_datetime)) %>%
    
    dplyr::rename(
      Year = date_year,
      JDay = date_doy,
      Temp = temp_airC
    ) %>% 
    dplyr::mutate(
      Year = as.integer(Year),
      JDay = as.integer(JDay),
      # Hour = lubridate::hour(date_datetime),
      Hour = 
        lubridate::hour(
          strptime(
            paste(lubridate::date(date_datetime), date_hour, sep = " "),
            format = "%Y-%m-%d %H%M",
            tz = "America/Phoenix"
          )
        ),
      Month = lubridate::month(date_datetime, label = FALSE),
      Day = as.numeric(lubridate::day(date_datetime))
    ) %>%
    dplyr::mutate( # To match hour values in `heatR`, from 0 to 23 on a given date
      Hour = Hour - 1,
      Hour = 
        dplyr::if_else(
          condition = Hour == -1,
          true = 23,
          false = Hour
        )
    ) %>% 
    dplyr::select(Year, JDay, Hour, Temp, Month, Day) %>% 
    na.omit(object = .) %>%  # `heatR` does not handle NAs within next function
    chillR::daily_chill(
      hourtemps = .,
      running_mean = 1, # default value for no smoothing
      models = list(Chilling_Hours = Chilling_Hours, GDH = GDH),
      THourly = NULL
    ) %>%
    magrittr::extract2("daily_chill") %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      date_year = Year,
      growing_degree_hours = GDH
    ) %>%
    dplyr::mutate(
      datetime = as.Date(as.character(YYMMDD), "%Y%m%d"),
      date_year = lapply(date_year, as.character),
      date_doy = lubridate::yday(datetime),
      meta_station_name = azmetStation
    )
  
  return(hourlyHeatVarsToDaily)
}
