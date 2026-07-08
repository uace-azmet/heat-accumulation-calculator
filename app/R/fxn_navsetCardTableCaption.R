#' `fxn_navsetCardTableCaption.R` - Build caption for table summary based on user input
#' 
#' @param heatVariable - Chill variable selected by user
#' @return `navsetCardTableCaption` Caption for table summary based on user input


fxn_navsetCardTableCaption <- function(heatVariable) {
  
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
  
  if (heatVariable == "Growing Degree Hours") {
    variableUnits <- "degree hours Fahrenheit (DHF)"
  } else { #if (heatVariable %in% c("Heat Units 94-55 °F", "Heat Units 86-55 °F", "Heat Units 86-50 °F", "Heat Units 86-45 °F"))
    variableUnits <- "degree days Farenheit (DDF)"
  }
  
  captionText <- "Values of 'NA' denote no data."
  
  # Generate caption text with `heatR` reference
  if (heatVariable == "Growing Degree Hours") {
    captionText <- 
      paste0(
        captionText, " Growing degree hours are based on calculations in the <a href=https://eikeluedeling.r-universe.dev/chillR>chillR</a> R package."
      )
  } else { #if (heatVariable %in% c("Heat Units 94-55 °F", "Heat Units 86-55 °F", "Heat Units 86-50 °F", "Heat Units 86-45 °F"))
    captionText <- 
      paste0(
        captionText,
        " More information on the calculation of heat units is in Extension bulletin ", bulletinURL, "."
      )
  }
  
  variableKeyText <- 
    paste0(
      "Variable key: <strong>Day<sub>period</sub></strong> day number of the period of interest; <strong>Heat</strong> daily heat values in ", variableUnits, " as represented by ", heatVariableText, "; <strong>Heat<sub>cumulative</sub></strong> accumulation of daily heat values in ", variableUnits, " during the period of interest as represented by ", heatVariableText
    )
  
  # Format caption text as HTML
  navsetCardTableCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste(captionText, variableKeyText, sep = " ")
      ), 
      class = "navset-card-caption"
    )
  
  return(navsetCardTableCaption)
}
