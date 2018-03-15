
##############################
# INTRODUCTION
##############################

# DATA PREPARATION FOR THE SHINY APP
# PART 4: COMPUTE SPATIAL AUTOCORRELATION

# This script take as input a spatial polygon data frame with several variables in his @data slot.
# It returns the spatial autocorrelation for all its variable.




##############################
# TO USE THIS FUNCTION
##############################

# 1 - I run this script on Abdel data using these command lines. 
#load("~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/DATA/Abdel_data.Rdata")
#moran_data=compute_autocor(GBR_region)
#save(moran_data, file="~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/DATA/Spatial_Autocor.Rdata")

# 2 - This function is then called in the shiny app in case a user loads his data




# A few libraries are necessary
library(sp)
library(spdep)
library(dplyr)


# A function that return spatial autocorelation for all the variable of a spatial polygon data frame:
compute_autocor = function(input){
  
incProgress(0.1, detail = "Start Moran coeeficient computation")

  # Keep only numerical data in input
  input@data = input@data[ , which(sapply(input@data, is.numeric)) ]

  
  # make neighbour list object
  nb <- poly2nb(input)

  incProgress(0.1, detail = "Neigbhour list object")

  # make spatial weights object. This object cannot b printed
  sp_weights <- nb2listw(nb, style="B", zero.policy=TRUE)



  # moran test
  tmp <- lapply(input@data, moran.mc, sp_weights, nsim=100000, zero.policy = TRUE, na.action=na.omit)
  tmp <- t(simplify2array(tmp))
  tmp <- as.data.frame(tmp[,1:3])
  incProgress(0.1, detail = "Neigbhour list object")


  moran_test <- t(do.call(rbind, lapply(tmp, unlist)))
  

  rownames(moran_test) = gsub(".statistic", "", rownames(moran_test))
  return(moran_test)
}





