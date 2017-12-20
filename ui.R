	# ------------------------------------
	#
	#	UKB geographical analysis
	#
	# ------------------------------------




shinyUI(fluidPage(


	
	# This is to explain you have a CSS file that custom the appearance of the app
	includeCSS("www/style.css") ,





	# -------------------------------------------------------------------------------------
	# === ROW : title + Main tabs
	fluidRow( align="center",
		br(),
		helpText(
			strong("Genes & Geography in great britain", style="color:black; font-size:30pt")
		)
	),
	fluidRow( align="center",
		br(),
		column(2, offset=5, align="center", radioGroupButtons( "section",label = NULL, choices=c("Methods"=2, "Explore"=1, "Compare"=3), selected=1 )),
	
		# Customization button
		column(2, offset=0, align="left", 
			#dropdownButton(
				#circle = TRUE, icon = icon("plus"), width = "600px", tooltip = tooltipOptions(title = "See more"),
			
				h2("Geographical Unit"),
				hr(),
				h5("We propose to work at 3 different geographical scales. This will divide UK in 414, 1k and 10k units respectively. You can also choose your resolution. Better resolution allows to zoom on the map, but a waiting time will be necessary. Click the + button for this option."),
				selectInput(inputId = "map_geo_unit", label = "", choices = c("Region (large)"=1, "Region (Small)"=2, "Hexagones"=3) ),
				br(), br(),
				
				h2("Transformation"),
				hr(),
				h5("We propose to work at 3 different geographical scales. This will divide UK in 414, 1k and 10k units respectively. You can also choose your resolution. Better resolution allows to zoom on the map, but a waiting time will be necessary. Click the + button for this option."),
				selectInput(inputId = "map_geo_transfo", label = "", choices = c("No transformation"=1, "Cartogram"=2) ),
				br(), br(),

				h2("Color Scale"),
				hr(),
				h5("We propose to work at 3 different geographical scales. This will divide UK in 414, 1k and 10k units respectively. You can also choose your resolution. Better resolution allows to zoom on the map, but a waiting time will be necessary. Click the + button for this option."),
				strong("type of color scale:"),
				selectInput(inputId = "type_scale", label = "", choices = c("Bin", "Quantile", "Numerical"), selected="Bin"),


				conditionalPanel("input.type_scale == Bin",
					strong("Number of slice:"),
					sliderInput("slider_quantile", "", min=3, max=20, value=6, ticks=F)
				),

				strong("Color Palette:"),
				selectInput(inputId = "choice_palette", label = "", choices = c("Blues", "Reds", "viridis", "magma", "BuPu"), selected="Blues")
		 	#)
		)
	),

	column(8, offset=2, hr()),	


 	# -------------------------------------------------------------------------------------







	# #########
	#	TAB 1
	# #########
	conditionalPanel("input.section == 1",
		column(2, offset=1, 
			br(), br(), br(), br(),br(), br(), br(), br(),br(), br(), br(), br(),
			h2("Welcome"),
			hr(),
			h5("This application describes the geographical distribution of several variable of the ", strong(a("UK Biobank dataset", style="color:lightblue", href="http://www.ukbiobank.ac.uk")), "(n=502630)."),
			br(),
			h5("More than 100 variables are available for visualization. You can observe them using different geographical units. It is possible to custom and export this map following the surrounding buttons. Use the compare tab above if you want to study the relationship between several variables."),
			br(), br(), br(),br(), br(), br(), br()
		),



		


		# MAP
		column(6, align="center",
			leafletOutput("main_map", height="1100px", width="100%") %>% withSpinner( color= "#2ecc71"),
			br(), br(),
			h3(tags$u(tags$b("Figure 1")),": Geographical distribution in the UK.")
		),
		



		# RIGHT SIDE
		column(2, align="right",
			br(), br(), br(), br(),br(), br(), br(), br(),br(), br(), br(), br(),
			h2("Variable"),
			hr(),
			h5("We propose to represent the geographical distribution of 126 variables. These veriable are split in several groups: Principal Components (PCs), Polygenic Risk Scores (PRS). To understand how these variable have been computed, visit the method section."),
			pickerInput(inputId = "map_variable", label = "", choices = list(Polygenic_Risk_Score = list_PRS, PC_from_UKB = list_PC_UKB, PRS_corrected_UKB=list_PRS_reg_UKB, PC_from_1000genome = list_PC_1KG, PRS_corrected_1000genome=list_PRS_reg_1KG  ), selected='PC1')
		)
	),
	br(), br(),












	# #########
	#	TAB  (COMPARE)
	# #########
	conditionalPanel("input.section == 3",

		fluidRow(column(4, offset=4, align="center", h5("This section aims to compare the geographical distribution of two or more variables. Select as many variable as you like, one map will appear for each. Moreover, scroll to the bottom of this page to check the scatterplot matrix and correlation estimates of each pair."))),
		br(),
		fluidRow( align="center", pickerInput(inputId = "multimap_variable", label = "", choices = list(Polygenic_Risk_Score = list_PRS, PC_from_UKB = list_PC_UKB, PRS_corrected_UKB=list_PRS_reg_UKB, PC_from_1000genome = list_PC_1KG, PRS_corrected_1000genome=list_PRS_reg_1KG  ), multiple=TRUE, selected=c("PC1", "PC2"))),
		br(),

		# If less than 2 maps
		conditionalPanel( "input.multimap_variable.length<2" ,
			fluidRow(align="center", 
				br(),br(),br(),br(),
				h6("Please select at least 2 variables")
			)
		),

		# If 2 maps
		conditionalPanel( "input.multimap_variable.length==2" ,
			fluidRow(
				column(4, offset=2, leafletOutput("compar_map1a", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(4, leafletOutput("compar_map2a", height="900px") %>% withSpinner( color= "#2ecc71"))
			)
		),


		# If 3 maps
		conditionalPanel( "input.multimap_variable.length==3" ,
			fluidRow(column(10, offset=1,
				column(4, leafletOutput("compar_map1b", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(4, leafletOutput("compar_map2b", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(4, leafletOutput("compar_map3b", height="900px") %>% withSpinner( color= "#2ecc71"))
			))
		),

		# If 4 maps
		conditionalPanel( "input.multimap_variable.length==4" ,
			fluidRow(column(10, offset=1,
				column(3, leafletOutput("compar_map1c", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(3, leafletOutput("compar_map2c", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(3, leafletOutput("compar_map3c", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(3, leafletOutput("compar_map4c", height="900px") %>% withSpinner( color= "#2ecc71"))
			))
		),

		# If 5 maps
		conditionalPanel( "input.multimap_variable.length==5" ,
			fluidRow(column(12, offset=1,
				column(2, leafletOutput("compar_map1d", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(2, leafletOutput("compar_map2d", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(2, leafletOutput("compar_map3d", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(2, leafletOutput("compar_map4d", height="900px") %>% withSpinner( color= "#2ecc71")),
				column(2, leafletOutput("compar_map5d", height="900px") %>% withSpinner( color= "#2ecc71"))
			))
		),


		# If more than 5 maps
		conditionalPanel( "input.multimap_variable.length>5" ,
			fluidRow(align="center", 
				br(),br(),br(),br(),
				h6("Please select 5 variables maximum")
			)
		),

		# Legend
		fluidRow(align="center", h3(tags$u(tags$b("Figure 2")),": Geographical distribution of xx, xx and xx in the UK.")),

		# Scatterplot of pairwise comparison
		br(),
		fluidRow( align="center", 
			column(4, offset=4, 
				hr(), 
				h5("To complete this geographical comparison, here are a couple of scatterplots showing the relationship between each pair of variables you have selected:")
			)
		),
		br(),
		fluidRow(align="center", plotlyOutput("scatter", height="700px", width="80%") %>% withSpinner( color= "#2ecc71"), 
		fluidRow(align="center", h3(tags$u(tags$b("Figure 3")),": Pairwise relationship of your selected variables"))

		)





	),















	# #########
	#	TAB (METHOD)
	# #########
	conditionalPanel("input.section == 2",

		fluidRow(column(7, offset=2, align="left",
			br(), br(), br(), br(),br(), 
			h2("Material"),
			hr()
		)),
		fluidRow(column(6, offset=3,
			h5("Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here.")
		)),
		br(),br(),



		fluidRow(column(7, offset=2, align="left",
			br(), br(), br(), br(),br(), 
			h2("PCs and PRS computation"),
			hr()
		)),
		fluidRow(column(6, offset=3,
			h5("Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here.")
		)),
		br(),br(),



		fluidRow(column(7, offset=2, align="left",
			br(), br(), br(), br(),br(), 
			h2("Visualization"),
			hr()
		)),
		fluidRow(column(6, offset=3,
			h5("Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here. Lorem Ipsum blablba Abdel you need to send me text to explain your work here.")
		)),
		br(),br(),



		fluidRow(column(7, offset=2, align="left",
			br(), br(), br(), br(),br(),
			h2("Reproducibility"),
			hr()
		)),
		fluidRow(column(6, offset=3,
			h5("All the result of this study have been obtained using the R programming language. Map are done using the leaflet library developped by Rstudio. All the code are available on Github, and further explanation are available in our publication. Raw data are provided using the button below. Feel free to contact us for further information.")
		)),
		br(),br(),
		fluidRow(align="center", 
			downloadButton("load_ex_format1", label = "Download"),
			actionButton(inputId='ab1', label="Github", icon = icon("github"), onclick ="location.href='https://github.com/holtzy/the-NB-COMO-Project';"), 
			actionButton(inputId='ab1', label="Paper", icon = icon("file-o"), onclick ="location.href='https://www.ncbi.nlm.nih.gov/pubmed/';")
		)
			
	),






	# #########
	#	FOOTER
	# #########


	# -------------------------------------------------------------------------------------
	# === 9/ Footer
	fluidRow( align="center" ,
		br(), br(),
		column(4, offset=4,
			hr(),
			br(), br(),
			"Created by", strong(a("Yan Holtz", style="color:lightblue", href="https://holtzyan.wordpress.com")), ".",
			br(),
			"Source code available on", strong(a("Github", style="color:lightblue", href="https://github.com/holtzy/the-NB-COMO-Project")), ".",
			br(),
			"Copyright © 2017 The COMO Project",
			br(), br(),br()
			
		),
		br(),br()
	)

	
	# -------------------------------------------------------------------------------------





# Close the ui
))
