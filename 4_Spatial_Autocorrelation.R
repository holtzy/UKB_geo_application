
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
#ls()
#input=GBR_region
#output="~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/DATA/Spatial_Autocor.Rdata"


# 2 - The user of the app can load his data and it will run the script as well:
#data=read.table("/Users/y.holtz/Desktop/example_file.csv.gz", header=T)
#ls()
#source("~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/3_Link_With_Shapes.R")
#load("~/Dropbox/QBI/15_ABDEL_UKB_MAP/UKB_geo_application/DATA/Map_data.Rdata")
#a=compute_shape_sumstat(data, "yo")







compute_autocor = function(input, output){
  
  # Keep only numerical data in input
  input@data = input@data[ , which(sapply(input@data, is.numeric)) ]
  
  # make neighbour list object
  nb.GBR_dist.UKB.PCs <- poly2nb(input)

  # make spatial weights object
  sp_weights.GBR_dist.UKB.PCs <- nb2listw(nb.GBR_dist.UKB.PCs, style="B", zero.policy=TRUE)
  B.GBR_dist.UKB.PCs <- as(sp_weights.GBR_dist.UKB.PCs, "symmetricMatrix")

  # make spatial weights matrix
  wm.GBR_dist.UKB.PCs <- nb2mat(nb.GBR_dist.UKB.PCs, style='B', zero.policy = TRUE)
  
  # moran test
  moran_test.all <- lapply(input@data, moran.mc, sp_weights.GBR_dist.UKB.PCs, nsim=10, zero.policy = TRUE, na.action=na.omit)
  MoransResults.full <- t(simplify2array(moran_test.all))
  MoransResults.stats <- as.data.frame(MoransResults.full[,1:3])
  MoransResults.file <- t(do.call(rbind, lapply(MoransResults.stats, unlist)))
  

  
  
    # make neighbour list object: for each region, I get a list of its neighbours.
  nb <- poly2nb(input)
  
  # make spatial weights object: I give a weight for every neighbour region.
  lw <- nb2listw( nb, zero.policy=TRUE, style="B")
  
  # I transform the neighbour list object in a spatial weights matrix
  wm <- nb2mat(nb, style='B', zero.policy = TRUE)
  
  # Moran stat for one value only
  tmp=moran.test(GBR_region@data$PC1, lw, zero.policy=TRUE, na.action=na.omit)
  tmp
  
  # Moran test for all traits
  moran_test.all <- lapply(GBR_dist.UKB.over, moran.mc, sp_weights.GBR_dist.UKB.PCs, nsim=10000, zero.policy = TRUE, na.action=na.omit)





B.GBR_dist.UKB.PCs <- as(sp_weights.GBR_dist.UKB.PCs, "symmetricMatrix")
all(B.GBR_dist.UKB.PCs == t(B.GBR_dist.UKB.PCs))

# make spatial weights matrix
wm.GBR_dist.UKB.PCs <- nb2mat(nb.GBR_dist.UKB.PCs, style='B', zero.policy = TRUE)

# moran test
moran_test.all <- lapply(GBR_dist.UKB.over, moran.mc, sp_weights.GBR_dist.UKB.PCs, nsim=10000, zero.policy = TRUE)
#MoransResults.full <- t(simplify2array(moran_test.all))
#MoransResults.stats <- as.data.frame(MoransResults.full[,1:3])
#MoransResults.file <- t(do.call(rbind, lapply(MoransResults.stats, unlist)))










