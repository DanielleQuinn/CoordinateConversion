require(sp)
require(tidyverse)
require(glue)
require(stringr)

source("./R/DD_to_DDM.R")
source("./R/DD_to_DMS.R")
source("./R/DD_to_UTM.R")
source("./R/DDM_to_DD.R")
source("./R/DDM_to_DMS.R")
source("./R/DDM_to_UTM.R")
source("./R/DMS_to_DD.R")
source("./R/DMS_to_DDM.R")
source("./R/DMS_to_UTM.R")
source("./R/UTM_to_DD.R")
source("./R/UTM_to_DMS.R")
source("./R/UTM_to_DDM.R")
source("./R/sp_convert.R")

# Testing ----
# From DD
testdata_DD <- data.frame(lat = c( -63, -89.2345, 4.5, 4.5),
                          lon = c(144, 23.34, 57.3246, 57.3246),
                          test = c("A", "B", "C", "D"))

# Convert 144 longitude to DDM
DD_to_DDM(DD_input = 144, axis = "horizontal")

sp_convert(testdata_DD) # Works
sp_convert(testdata_DD, to = "DMS") # Works
sp_convert(testdata_DD, to = "DDM") # Works
sp_convert(testdata_DD, to = "UTM") # Works
sp_convert(testdata_DD, to = c("DDM", "DMS")) # Works
sp_convert(testdata_DD, to = c("DDM", "DMS", "DD")) # Works

# From DDM
testdata_DDM <- sp_convert(testdata_DD, to = "DDM") %>%
  select(lat = lat_DDM, lon = lon_DDM)

sp_convert(testdata_DDM, from = "DDM") # Works
sp_convert(testdata_DDM, from = "DDM", to = "DD") # Works
sp_convert(testdata_DDM, from = "DDM", to = "DMS") # Works
sp_convert(testdata_DDM, from = "DDM", to = "UTM") # Works
sp_convert(testdata_DDM, from = "DDM", to = c("DMS", "DD")) # Works
sp_convert(testdata_DDM, from = "DDM", to = c("DMS", "DD", "DDM")) # Works

# From DMS
testdata_DMS <- sp_convert(testdata_DD, to = "DMS") %>%
  select(lat = lat_DMS, lon = lon_DMS)

sp_convert(testdata_DMS, from = "DMS") # Works
sp_convert(testdata_DMS, from = "DMS", to = "DD") # Works
sp_convert(testdata_DMS, from = "DMS", to = "DDM") # Works
sp_convert(testdata_DMS, from = "DMS", to = "UTM") # Works

# From UTM
testdata_UTM <- sp_convert(testdata_DD, to = "UTM") %>%
  select(easting, northing, zone)

sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM") # Works
sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM", to = "DD") # Works
sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM", to = "DDM") # Works
sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM", to = "DMS") # Works
sp_convert(testdata_UTM, x = "easting", y = "northing", from = "UTM", to = c("DMS", "DD", "UTM")) # Works


# Parameters
# data = dataframe with spatial data
# x - "longitude column name"
# y - "latitude column name"
# from - what format is your spatial data in currently 
# DD -> Decimal Degrees
# DMS -> Degrees, minutes, seconds
# DDM <- Degrees, decimal minutes
# UTM -> UTM 
#If from = UTM, then must include UTM = "zone column name"
#to - default is "all", meaning that function will convert to all fromats automatically
#however, DD, DCM, DDM, or UTM can be entered to convert it to one of those 
#can specify which ellipsoid to use, default is WGS78