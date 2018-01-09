

# -----------------------------------
# 1- LOAD LIBRARIES

# libraries
library(shiny)
library(dplyr)
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

load("DATA/Abdel_data.Rdata")







# -----------------------------------
# 2- LIST OF VARIABLES
all=colnames(GBR_region@data)
list_PC_UKB = all[ grepl("^PC", all ) & !grepl("1kG", all ) ]
list_PC_1KG = all[ grepl("^PC", all ) & grepl("1kG", all ) ]
list_PRS_reg_UKB = all[ grepl("\\.residual", all )  ]
list_PRS_reg_1KG = all[ grepl("_residual", all )  ]
list_PRS=all[ !all %in% c(list_PC_UKB, list_PC_1KG, list_PRS_reg_UKB, list_PRS_reg_1KG)]
#list_user_variables=NULL





# -----------------------------------
# 3- SOURCE FUNCTION THAT ANALYSE USER DATA
source("3_Link_With_Shapes.R")
load("~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/DATA/Map_data.Rdata")







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




