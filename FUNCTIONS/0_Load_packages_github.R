load_pkg <- function(){

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
      	install.packages(pkg)  }
      }


      # Define list of package names and required versions.
      deppkgs = c(shiny="0.14.2", dplyr="0.7.4", tibble="1.4.2", ggplot2 = "2.2.1.9", tidyr="0.7.2", shinyWidgets="0.4.1", 
      			RColorBrewer="1.1.2", plotly = "4.5.6", DT="0.2", viridis="0.4.0", shinycssloaders="0.2.0", sp="1.2",
      			rgdal="1.2", rgeos="0.3", leaflet="1.1.0", stringr="1.3.0", maps="3.2.0", mapdata="2.2", lattice="0.20",
      			spdep="0.7", cartogram="0.0.2", forcats="0.3.0")

      # Loop on package: check, install, update
      pkg1 = mapply(install_missing_packages,
                    pkg = names(deppkgs), 
                    version = deppkgs,
                    MoreArgs = list(verbose = TRUE), 
                    SIMPLIFY = FALSE,
                    USE.NAMES = TRUE)

      # Load the packages
      for(i in names(deppkgs)){
        library(i, character.only = TRUE)
        message(i, " package version:\n", packageVersion(i))
      }

}