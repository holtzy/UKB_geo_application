

# -----------------------------------
# 1- LOAD LIBRARIES

# A function that install packages if not available/installed.
install_missing_packages = function(pkg, version = NULL, verbose = TRUE){
  availpacks = .packages(all.available = TRUE)
  missingPackage = FALSE
  if(!any(pkg %in% availpacks)){
    if(verbose){
      message("The following package is missing.\n",
              pkg, "\n",
              "Installation will be attempted...")
    }
    missingPackage <- TRUE
  }
  if(!is.null(version) & !missingPackage){
    # version provided and package not missing, so compare.
    if( compareVersion(a = as.character(packageVersion(pkg)),
                       b = version) < 0 ){
      if(verbose){
        message("Current version of package\n", 
                pkg, "\t", 
                packageVersion(pkg), "\n",
                "is less than required.
                Update will be attempted.")
      }
      missingPackage <- TRUE
    }
  }
  if(missingPackage){
    print(pkg)
    print(paste("---- installing a more recent version of",pkg,sep=""))
	install.packages(pkg, repos = "http://cran.r-project.org")  }
}


# Define list of package names and required versions.
deppkgs = c(shiny="0.14.2", dplyr="0.7.4", tibble="1.4.2", ggplot2 = "2.2.1.9", tidyr="0.7.2", shinyWidgets="0.4.1", 
			RColorBrewer="1.1.2", plotly = "4.5.6", DT="0.2", viridis="0.4.0", shinycssloaders="0.2.0", sp="1.2",
			rgdal="1.2", rgeos="0.3", leaflet="1.1.0", stringr="1.3.0", maps="3.2.0", mapdata="2.2", lattice="0.20",
			spdep="0.7", cartogram="0.0.2")

# Loop on package: check, install, update
pkg1 = mapply(install_missing_packages,
              pkg = names(deppkgs), 
              version = deppkgs,
              MoreArgs = list(verbose = TRUE), 
              SIMPLIFY = FALSE,
              USE.NAMES = TRUE)
          





# -----------------------------------
# 1- LOAD DATA

# Geospatial object (=map boundaries + value of each region):
load("DATA/Abdel_data.Rdata")

# Spatial Autocorrelation values:
load("DATA/Spatial_Autocor.Rdata")
# I transform it in a reactive value
react_values <- reactiveValues(moran_df = moran_data)





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

# A function that return the good dataset
return_appropriate_dataset = function( isNew, map_geo_unit, map_geo_transfo, user_data){

	# Case number one: we show our data:
	if( isNew=="original" ){
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


# A function that return the good color palette:
return_color_palette=function(type, vector, palette, nb_quantile){
		
	if(type=="Bin"){
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/nb_quantile) %>% round(40)
		mybins=c(-Inf, mybins, Inf)
		mypalette = colorBin( palette=palette, domain=vector, na.color="transparent", bins=mybins)
	}
	if(type=="Quantile"){
		mypalette = colorQuantile( palette=palette, domain=vector, na.color="transparent", n=7)
	}
	if(type=="Numerical"){
		mypalette = colorNumeric( palette=palette, domain=vector, na.color="transparent")
	}
	return(mypalette)
}



# -----------------------------------
# 5- GENERAL PARAMETERS
multimap_height="700px"






# -----------------------------------
# 6- Enable Bookmarking
enableBookmarking(store = "url")














