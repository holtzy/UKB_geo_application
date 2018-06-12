

# -----------------------------------
# 1- LOAD LIBRARIES

print("--------------------- Loading libraries -----------------------------")


# For github
source("FUNCTIONS/0_Load_packages_github.R")
# For the web
#source("FUNCTIONS/0_Load_packages_online.R")
#
load_pkg()


print("--------------------- Packages Loaded -----------------------------")




# -----------------------------------
# 1- LOAD DATA

# Geospatial object (=map boundaries + value of each region):
load("DATA/Abdel_data.Rdata")


# Spatial Autocorrelation values:
load("DATA/Spatial_Autocor.Rdata")
# I transform it in a reactive value
react_values <- reactiveValues(moran_df = moran_data)





# -----------------------------------
# 1- A KEY TO TRANSLATE VARIABLE NAME
list_PRS=c("Educational attainment"="EA2_noUKB", "Openness"="openness.GPC.23andme", "Conscientiousness"="conscientiousness.GPC.23andme", "Extraversion"="extraversion.GPC.23andme", "Agreeableness"="agreeableness.GPC.23andme","Neuroticism"="neuroticism.GPC.23andme","Schizophrenia"="SCZ2","Bipolar disorder"="bipolar","Major Depressive Disorder (MDD)"=	"MDD_PGC2_noUKB","ADHD"="ADHD_2017","Eating disorder"="eating_disorder","Alzheimer"="alzheimer","Autism"="autism_2017.ipsych.pgc","Alcohol consumption"="alcohol_schumann","Smoking (ever vs never)"="smoking_ever_vs_never","Smoking (cigarets per day)"="smoking_cigs_per_day","Smoking (current vs former)"="smoking_current_vs_former","Smoking (age of onset)"="smoking_age_onset","Cannabis consumption"="cannabis","Caffeine consumption"="caffeine","Coronary artery disease"="coronary_artery_disease","Type II diabetes"="diagram_T2D","Height"="height_combined","BMI"="bmi_combined","Body fat"="body_fat")

list_PRS_reg=c("Educational attainment" = "EA2_noUKB.residual",
"Openness" = "openness.GPC.23andme.residual",
"Conscientiousness" = "conscientiousness.GPC.23andme.residual",
"Extraversion" = "extraversion.GPC.23andme.residual",
"Agreeableness" = "agreeableness.GPC.23andme.residual",
"Neuroticism" = "neuroticism.GPC.23andme.residual",
"Schizophrenia" = "SCZ2.residual",
"Bipolar disorder" = "bipolar.residual",
"Major Depressive Disorder (MDD)" = 	"MDD_PGC2_noUKB.residual",
"ADHD" = "ADHD_2017.residual",
"Eating disorder" = "eating_disorder.residual",
"Alzheimer" = "alzheimer.residual",
"Autism" = "autism_2017.ipsych.pgc.residual",
"Alcohol consumption" = "alcohol_schumann.residual",
"Smoking (ever vs never)" = "smoking_ever_vs_never.residual",
"Smoking (cigarets per day)" = "smoking_cigs_per_day.residual",
"Smoking (current vs former)" = "smoking_current_vs_former.residual",
"Smoking (age of onset)" = "smoking_age_onset.residual",
"Cannabis consumption" = "cannabis.residual",
"Caffeine consumption" = "caffeine.residual",
"Coronaty artery disease" = "coronary_artery_disease.residual",
"Type II diabetes" = "diagram_T2D.residual",
"Height" = "height_combined.residual",
"BMI" = "bmi_combined.residual",
"Body fat" = "body_fat.residual"
)




# -----------------------------------
# 2- LIST OF VARIABLES
all=colnames(GBR_region@data)
list_PC = all[ grepl("^PC", all ) ]




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














