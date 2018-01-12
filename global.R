

# -----------------------------------
# 1- LOAD LIBRARIES

# libraries
library(shiny)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library("shinyWidgets")
library(RColorBrewer)		# awesome color palettes
library(d3heatmap)			# for heatmaps
library(plotly)
library(DT)
library(networkD3)			# Classification tree of disease
library(packcircles)		# for calculating packcircles
library(gridExtra)
library(viridis)
library(grid)
library(ggiraph)			# To animate the ggplot2 bubble chart

library(shinycssloaders)	# Spinner if plot takes time to load

library(sp)
library(rgdal)
library(rgeos)

library(leaflet)

library(GGally)				# For the scatterplot matrix

library(d3heatmap)





# -----------------------------------
# 1- LOAD DATA

# Geospatial object (=map boundaries + value of each region):
load("DATA/Abdel_data.Rdata")

# Spatial Autocorrelation values:
load("DATA/Spatial_Autocor.Rdata")





# -----------------------------------
# 2- LIST OF VARIABLES
all=colnames(GBR_region@data)
list_PC_UKB = all[ grepl("^PC", all ) & !grepl("1kG", all ) ]
list_PC_1KG = all[ grepl("^PC", all ) & grepl("1kG", all ) ]
list_PRS_reg_UKB = all[ grepl("\\.residual", all )  ]
list_PRS_reg_1KG = all[ grepl("_residual", all )  ]
list_PRS=all[ !all %in% c(list_PC_UKB, list_PC_1KG, list_PRS_reg_UKB, list_PRS_reg_1KG)]





# -----------------------------------
# 3- SOURCE FUNCTION THAT ANALYSE USER DATA

# A function that read user data frame and calculates aggregated data per region
source("FUNCTIONS/3_Link_With_Shapes.R")
# This function needs the geographical boundaries
load("DATA/Map_data.Rdata")

# A function that calculates spatial autocorrelation of a geospatial data frame.
source("FUNCTIONS/4_Spatial_Autocorrelation.R")






# -----------------------------------
# 4- A FUNCTION THAT RETURN THE APPROPRIATE DATASET TO FIT USER NEEDS
user_data=NULL

return_appropriate_dataset = function( map_variable, map_geo_unit, map_geo_transfo, user_data){

	# Case number one: we show our data:
	if( map_variable %in% all ){
		if(map_geo_unit==1 & map_geo_transfo==1){ return(GBR_region) }
		if(map_geo_unit==1 & map_geo_transfo==2){ return(GBR_region_cartogram) }
		if(map_geo_unit==3 & map_geo_transfo==1){ return(GBR_hexa) }
		if(map_geo_unit==3 & map_geo_transfo==2){ return(GBR_hexa_cartogram) }
	# Second case: we show user uploaded data:
	} else {
		if(map_geo_unit==1 & map_geo_transfo==1){ return(user_data[[1]]) }
		if(map_geo_unit==1 & map_geo_transfo==2){ return(user_data[[2]]) }
		if(map_geo_unit==3 & map_geo_transfo==1){ return(user_data[[3]]) }
		if(map_geo_unit==3 & map_geo_transfo==2){ return(user_data[[4]]) }
	}

}




# -----------------------------------
# 5- GENERAL PARAMETERS
multimap_height="700px"





















