

##############################
# INTRODUCTION
##############################

# DATA PREPARATION FOR THE SHINY APP
# PART 3: LINK INDIVIDUAL DATA / GEOSPATIAL DATA

# We have different geospatial data: region large / medium + normal / cartogram

# The phenotype data can be:
# - a data frame build using Abdel data (script 1_Compute_Individual_Data_Matrix.R), 
# - OR a data frame provided by the user of the application.

# Thus this script has one argument only: an input data frame which has several columns: longitude, latitude, var1, var2...





##############################
# TO USE THIS FUNCTION
##############################

# 1 - I run this script on Abdel data using these command lines. The result is several spatial polygon data frames I use as input for the application.
# Load the individual matrix saved in part1
#load("/Users/y.holtz/Desktop/ABDEL_DATA/Individual_Matrix_UKB.Rdata")
#ls()
#load("~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/DATA/Map_data.Rdata")
#input=UKB
#output="/Users/y.holtz/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/DATA/Abdel_data.Rdata"


# 2 - The user of the app can load his data and it will run the script as well:
#data=read.table("/Users/y.holtz/Desktop/example_file.csv.gz", header=T)
#ls()
#source("~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/3_Link_With_Shapes.R")
#load("~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/DATA/Map_data.Rdata")
#a=compute_shape_sumstat(data, "yo")







compute_shape_sumstat = function(input, output){
  
  
  
  ##############################
  # ENVIRONMENT
  ##############################
  
  # libraries
  library(sp)
  library(devtools)
  library(dplyr)
  library(stringr)
  library(maps)
  library(mapdata)
  library(rgdal)
  library(lattice)
  library(spdep)
  library(rgeos)
  library(cartogram)
  

  incProgress(0.1, detail = "Start linking with regions")

  
  
  
  
  
  ################################################################
  # PART X - LINK SPATIAL / INDIVIDUAL
  ################################################################
  
  #turn into SpatialObject
  coordinates(input) <- c("longitude","latitude")
  
  # assign projection to UKB
  bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
  proj4string(input) <- bng
  
  
 
  
  # ----------- REGION LARGE ---------------------------
  
  # aggregate polygenic scores, PCs, and residuals by each UK region of the initial shape file. 
  GBR_dist.UKB.over <- over(GBR_dist.bng, input, fn = mean)  
  
  # I need to add the number of person per region!
  GBR_dist.UKB.over$nb_people <- over(GBR_dist.bng, input, fn = length)[,1]
  
  # clean format + add the geolabel column + use the good projection
  GBR_region = SpatialPolygonsDataFrame(GBR_dist.bng, data=GBR_dist.UKB.over)
  GBR_region@data$geo_label = GBR_dist.bng@data$geo_label
  GBR_region= spTransform(GBR_region, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  
  
  incProgress(0.1, detail = "Aggregation to region OK")
 
  
  # ----------- HEXAGONE---------------------------
  
  # aggregate polygenic scores, PCs, and residuals by each UK region of the initial shape file. 
  UKB_hexa.bng.over <- over(UKB_hexa.bng, input, fn = mean)  
  
  # I need to add the number of person per region!
  UKB_hexa.bng.over$nb_people <- over(UKB_hexa.bng, input, fn = length)[,1]
  
  # clean format + add the geolabel column + use the good projection
  GBR_hexa = SpatialPolygonsDataFrame(UKB_hexa.bng, data=UKB_hexa.bng.over)
  GBR_hexa@data$geo_label = UKB_hexa.bng@data$LAD12NM
  GBR_hexa = spTransform(GBR_hexa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  
   incProgress(0.1, detail = "Aggregation to hexagon OK")
 
  
  
  
  
  
  ################################################################
  # PART X - CARTOGRAM CREATION
  ################################################################
  
  
  # This part is going to create the same object, but transforming the shapes proportionnaly of the number of people into them
  
  
  # ----------- REGION LARGE ---------------------------
  GBR_region_cartogram = cartogram(GBR_region, "nb_people", itermax=5)
  incProgress(0.1, detail = "Cartogram region OK")
 
  # ----------- HEXAGONE ---------------------------
  GBR_hexa_cartogram = cartogram(GBR_hexa, "nb_people", itermax=5)
  
   incProgress(0.1, detail = "Cartogram hexagone OK")


  
  
  
  ################################################################
  # PART X - SAVE OBJECTS
  ################################################################
  
  
  # Save objects when I do it on abdel data
  #save(GBR_region, GBR_region_cartogram, GBR_hexa, GBR_hexa_cartogram, file=output)
  
  # Just return the objects if this is done on user data
  return( list(GBR_region, GBR_region_cartogram, GBR_hexa, GBR_hexa_cartogram) )

}
  

























