# Load auxiliary files
stationNames <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# Set auxiliary variables
heatVariables <- c("Heat Units 86-45 °F", "Heat Units 86-50 °F", "Heat Units 86-55 °F", "Heat Units 94-55 °F")

initialStartDate <- Sys.Date() - lubridate::dmonths(1)

initialEndDate <- Sys.Date() - 1
