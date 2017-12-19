

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
  






