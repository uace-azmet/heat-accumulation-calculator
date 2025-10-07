# Initial code to calculate heat units with user input on thresholds, request from Randy Norton
# max threshold = 27.2 C, base = 4.44 C, WIB data 2019-2024, for wheat


# Setup ----------

library(dplyr)

source("./app/norton-request/azmet.daily.data.download.R", local = TRUE)

stationList <- read.csv("./app/norton-request/azmet-station-list.csv", sep = ",")
stationList <- stationList %>% 
  dplyr::mutate(start_yr = 2019) %>% 
  dplyr::mutate(end_yr = 2024)

stationName <- "Willcox Bench"

stationData <- azmet.daily.data.download(stn_list = stationList, stn_name = stationName)


# Define functions ----------


# From CALS CCT / AZMET / AZMet Core / observation.py -----


# def calculateHeatUnits(self, strTempAirMax = "", strTempAirMin = "", strTempAirUpper = "", strTempAirLower = ""):
fxn_calculateHeatUnits <- function(tempAirMax, tempAirMin, upperTheshold, lowerThreshold) {

    # aryFltSine = [	1.000,0.981,0.962,0.944,0.927,0.910,0.893,0.876,0.859,0.843,
  #                0.827,0.811,0.796,0.780,0.765,0.750,0.735,0.721,0.706,0.692,
  #                0.678,0.664,0.650,0.636,0.622,0.609,0.596,0.583,0.570,0.557,
  #                0.544,0.532,0.519,0.507,0.495,0.483,0.471,0.459,0.448,0.436,
  #                0.425,0.413,0.402,0.391,0.381,0.370,0.359,0.349,0.339,0.328,
  #                0.318,0.308,0.299,0.289,0.279,0.270,0.261,0.251,0.242,0.233,
  #                0.225,0.216,0.208,0.199,0.191,0.183,0.175,0.167,0.159,0.152,
  #                0.144,0.137,0.130,0.123,0.116,0.109,0.102,0.096,0.090,0.084,
  #                0.078,0.072,0.066,0.061,0.055,0.050,0.045,0.040,0.036,0.031,
  #                0.027,0.023,0.019,0.016,0.013,0.010,0.007,0.004,0.002,0.001,
  #                0.000
  # ]
  aryFltSine = c(1.000,0.981,0.962,0.944,0.927,0.910,0.893,0.876,0.859,0.843,0.827,0.811,0.796,0.780,0.765,0.750,0.735,0.721,0.706,0.692,0.678,0.664,0.650,0.636,0.622,0.609,0.596,0.583,0.570,0.557,0.544,0.532,0.519,0.507,0.495,0.483,0.471,0.459,0.448,0.436,0.425,0.413,0.402,0.391,0.381,0.370,0.359,0.349,0.339,0.328,0.318,0.308,0.299,0.289,0.279,0.270,0.261,0.251,0.242,0.233,0.225,0.216,0.208,0.199,0.191,0.183,0.175,0.167,0.159,0.152,0.144,0.137,0.130,0.123,0.116,0.109,0.102,0.096,0.090,0.084,0.078,0.072,0.066,0.061,0.055,0.050,0.045,0.040,0.036,0.031,0.027,0.023,0.019,0.016,0.013,0.010,0.007,0.004,0.002,0.001,0.000)
  
  # if (((strTempAirMax != "-7999") and (strTempAirMax != "-6999") and (strTempAirMax != "-9999.0"))
  #     and ((strTempAirMin != "-7999") and (strTempAirMin != "-6999") and (strTempAirMin != "-9999.0"))
  #     and ((strTempAirUpper != "-7999") and (strTempAirUpper != "-6999") and (strTempAirUpper != "-9999.0"))
  #     and ((strTempAirLower != "-7999") and (strTempAirLower != "-6999") and (strTempAirLower != "-9999.0"))):
  if (tempAirMax != -7999 | tempAirMax != -6999 | tempAirMax != -9999.0) {
    if (tempAirMin != -7999 | tempAirMin != -6999 | tempAirMin != -9999.0) {
      # fltTempAirMax = float(decimal.Decimal(strTempAirMax))
      # fltTempAirMin = float(decimal.Decimal(strTempAirMin))
      # fltTempAirUpper = float(decimal.Decimal(strTempAirUpper))
      # fltTempAirLower = float(decimal.Decimal(strTempAirLower))
      # fltHeatUnits = 0.0
      # intSineWavePoint1 = 0
      # intSineWavePoint2 = 0
      # fltTempAirAlpha = 0.0
      fltTempAirMax <- tempAirMax
      fltTempAirMin <- tempAirMin
      fltTempAirUpper <- upperTheshold
      fltTempAirLower <- lowerThreshold
      fltHeatUnits <- 0.0
      intSineWavePoint1 <- 0
      intSineWavePoint2 <- 0
      fltTempAirAlpha <- 0.0
      
      # # 15020 TM = (X + N) / 2
      # fltTempAirMean = ((fltTempAirMax + fltTempAirMin) / 2.0)
      #			fltTempAirSum = (fltTempAirMax + fltTempAirMin)
      fltTempAirMean <- ((fltTempAirMax + fltTempAirMin) / 2.0)
      
      # 15040 A = (X - N) / 2
      fltTempAirAlpha <- ((fltTempAirMax - fltTempAirMin) / 2.0)
      
      #			print("calculateHeatUnits fltTempAirMax: ", fltTempAirMax, " fltTempAirUpper: " , fltTempAirUpper)
      #			print("calculateHeatUnits fltTempAirMin: ", fltTempAirMin, " fltTempAirLower: ", fltTempAirLower)
      
      #OK 15060 IF X > TU GOTO 15200
      #OK 15080 IF N > TL THEN H = TM - TL: RETURN
      #OK 15100 R = CINT(((TL - N) / (X - N)) * 100)
      #OK 15110 IF R > 100 THEN R = 100
      #OK 15120 H = A * SI(R): RETURN
      #OK 15200 IF N < TL GOTO 15300
      #OK 15220 R = CINT(((TU - N) / (X - N)) * 100)
      #OK 15230 IF R < 0 THEN H = TU - TL: RETURN
      #OK 15240 H = (TM - TL) - SI(R) * A: RETURN
      #OK 15300 R1 = CINT(((TL - N) / (X - N)) * 100)
      #OK 15320 R2 = CINT(((TU - N) / (X - N)) * 100)
      #OK 15340 H = A * (SI(R1) - SI(R2)): RETURN
      #OK 15500
      # RETURN
      
      # 15060 IF X > TU GOTO 15200
      # if (fltTempAirMax > fltTempAirUpper):
      if (fltTempAirMax > fltTempAirUpper) {
        # 15200 IF N < TL GOTO 15300
        # if (fltTempAirMin < fltTempAirLower):
        if (fltTempAirMin < fltTempAirLower) {
          # 15300 R1 = CINT(((TL - N) / (X - N)) * 100)
          # fltSineWavePoint1 = ((fltTempAirLower - fltTempAirMin) / (fltTempAirMax - fltTempAirMin) * 100)
          fltSineWavePoint1 <- ((fltTempAirLower - fltTempAirMin) / (fltTempAirMax - fltTempAirMin) * 100)
          
          # intSineWavePoint1 = self.roundValue(fltSineWavePoint1, "1", "int") # see line 44
          intSineWavePoint1 <- round(fltSineWavePoint1, digits = 0)
          
          # 15320 R2 = CINT(((TU - N) / (X - N)) * 100)
          # fltSineWavePoint2 = ((fltTempAirUpper - fltTempAirMin) / (fltTempAirMax -fltTempAirMin) * 100)
          fltSineWavePoint2 <- ((fltTempAirUpper - fltTempAirMin) / (fltTempAirMax -fltTempAirMin) * 100)
          
          # intSineWavePoint2 = self.roundValue(fltSineWavePoint2, "1", "int") # see line 44
          intSineWavePoint2 <- round(fltSineWavePoint2, digits = 0)
          
          # 15340 H = A * (SI(R1) - SI(R2)): RETURN
          #					print("calculateHeatUnits intSineWavePoint1: ", intSineWavePoint1, "intSineWavePoint2: " , intSineWavePoint2)
          # fltHeatUnits =  fltTempAirAlpha * (aryFltSine[intSineWavePoint1] - aryFltSine[intSineWavePoint2])
          fltHeatUnits <- fltTempAirAlpha * (aryFltSine[intSineWavePoint1] - aryFltSine[intSineWavePoint2])
        } else {
          # else:
          # 15220 R = CINT(((TU - N) / (X - N)) * 100)
          fltSineWavePoint1 = ((fltTempAirUpper - fltTempAirMin) / (fltTempAirMax -fltTempAirMin) * 100)
          
          # !!! START HERE !!!
        }
        # 15230 IF R < 0 THEN H = TU - TL: RETURN
        if (fltSineWavePoint1 < 0.0):
          fltHeatUnits = fltTempAirUpper - fltTempAirLower
          else:
            # 15240 H = (TM - TL) - SI(R) * A: RETURN
            intSineWavePoint1 = self.roundValue(fltSineWavePoint1, "1", "int")
            if (intSineWavePoint1 > 100):
              intSineWavePoint1 = 100
              fltHeatUnits = (fltTempAirMean - fltTempAirLower) - aryFltSine[intSineWavePoint1] * fltTempAirAlpha
      } else {
        else:
          # 15080 IF N > TL THEN H = TM - TL: RETURN
          if (fltTempAirMin > fltTempAirLower):
          fltHeatUnits = fltTempAirMean - fltTempAirLower
          else:
            # 15100 R = CINT(((TL - N) / (X - N)) * 100)
            fltSineWavePoint1 = ((fltTempAirLower - fltTempAirMin) / (fltTempAirMax - fltTempAirMin)) * 100
            intSineWavePoint1 = self.roundValue(fltSineWavePoint1, "1", "int")
            # 15110 IF R > 100 THEN R = 100
            if (intSineWavePoint1 > 100):
              intSineWavePoint1 = 100
            
            if (intSineWavePoint1 < 0):
              intSineWavePoint1 = 0
            
            # 15120 H = A * SI(R): RETURN
            #				print("calculateHeatUnits intSineWavePoint1: ", intSineWavePoint1, "intSineWavePoint2: " , intSineWavePoint2)
            fltHeatUnits = fltTempAirAlpha * aryFltSine[intSineWavePoint1]
            
      }
      
      #   dictReturn = {}
      dictReturn = { "fltHeatUnits": fltHeatUnits, "decHeatUnits": ( self.roundValue(fltHeatUnits, "1.00", "dec") ) }
      
    } else {
      # else:
      # dictReturn = { "fltHeatUnits": -9999.0, "decHeatUnits": -9999.0 }
      dictReturn <- 
    }
  }
  
  # return dictReturn
  return(dictReturn)
}










  
  



# convert Celsius heat units to Fahrenheit heat units
def convertHeatUnitCelsiusToHeatUnitFahrenheit(self, strHUCelsius = ""):
  decHUCelsius = decimal.Decimal(strHUCelsius)
fltHUF = float(decHUCelsius) * 1.8
return fltHUF




# Calculate heat units by user input ----------


# From CALS CCT / AZMET / AZMet Core / observationDaily.py
# dictHeatUnits = self.calculateHeatUnits(self.dictObservations["obs_dyly_temp_air_max"], self.dictObservations["obs_dyly_temp_air_min"], "30.0", "10.0")
# self.dictObservationsDerived["obs_dyly_derived_heat_units_10C"] = dictHeatUnits["fltHeatUnits"]
# self.dictObservationsDerived["obs_dyly_derived_heat_units_50F"] = self.convertHeatUnitCelsiusToHeatUnitFahrenheit(dictHeatUnits["fltHeatUnits"])


wib <- stationData %>%
  dplyr::select(c(Year, JDay, Date, Month, Day, stn_no, Tmax, Tmin, HU8555)) %>% 
  dplyr::mutate(HU_degC = fxn_calculateHeatUnits(upperThreshold = , lowerThreshold = ))
