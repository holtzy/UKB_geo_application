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
		column(4, offset=4, align="center", radioGroupButtons( "section",label = NULL, choices=c("Methods"=2, "Explore"=1, "Compare"=3, "Load your data"=4), selected=1 )),
	
		# Customization button
		column(2, offset=0, align="left", 
			dropdownButton(
				circle = TRUE, icon = icon("wrench"), width = "600px", tooltip = tooltipOptions(title = "Custom your maps!"),
			
				h2("Geographical Unit"),
				hr(),
				h5("We propose to work at 3 different geographical scales. This will divide UK in 414, 1k and 10k units respectively. You can also choose your resolution. Better resolution allows to zoom on the map, but a waiting time will be necessary. Click the + button for this option."),
				selectInput(inputId = "map_geo_unit", label = "", choices = c("Region"=1, "Hexagones"=3) ),
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

				strong("Color Palette:"),
				selectInput(inputId = "choice_palette", label = "", choices = c("Blues", "Reds", "viridis", "magma", "BuPu"), selected="Reds"),

				strong("Number of slice:"),
				sliderInput("slider_quantile", "", min=3, max=20, value=6, ticks=F)
		
		 	)
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
			leafletOutput("main_map", height="800px", width="100%") %>% withSpinner( color= "#2ecc71"),
			br(), br(),
			uiOutput("title_map1"),
			uiOutput("moran_map1")
		),
		



		# RIGHT SIDE
		column(2, align="right",
			br(), br(), br(), br(),br(), br(), br(), br(),br(), br(), br(), br(),
			h2("Variable"),
			hr(),
			h5("We propose to represent the geographical distribution of 126 variables. These veriable are split in several groups: Principal Components (PCs), Polygenic Risk Scores (PRS). To understand how these variable have been computed, visit the method section."),
			uiOutput("map_variable_button"),
			br(),
			h5("Clusterised variable?"),
			dropdownButton( circle = TRUE, size= "xs", width="600px", right=TRUE, up=TRUE, plotOutput("barplot", height="780px"))
		)
	),
	br(), br(),












	# #########
	#	TAB  (COMPARE)
	# #########
	conditionalPanel("input.section == 3",

		fluidRow(column(4, offset=4, align="center", h5("This section aims to compare the geographical distribution of two or more variables. Select as many variable as you like, one map will appear for each. Moreover, scroll to the bottom of this page to check the scatterplot matrix and correlation estimates of each pair."))),
		br(),
		fluidRow( align="center", uiOutput("multimap_variable_button")),
		br(),

		# If less than 2 maps
		conditionalPanel( "input.multimap_variable.length==1" ,
			fluidRow(align="center", 
				br(),br(),br(),br(),
				h6("Please select at least 2 variables")
			)
		),

		# If 2 maps
		conditionalPanel( "input.multimap_variable.length==2" ,
			fluidRow(align="center", 
				column(4, offset=2, leafletOutput("compar_map1a", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap1a") ),
				column(4, leafletOutput("compar_map2a", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap2a") )
			)
		),


		# If 3 maps
		conditionalPanel( "input.multimap_variable.length==3" ,
			fluidRow(align="center", column(10, offset=1,
				column(4, leafletOutput("compar_map1b", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap1b") ),
				column(4, leafletOutput("compar_map2b", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap2b") ),
				column(4, leafletOutput("compar_map3b", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap3b") )
			))
		),

		# If 4 maps
		conditionalPanel( "input.multimap_variable.length==4" ,
			fluidRow(align="center",
				column(3, leafletOutput("compar_map1c", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap1c") ),
				column(3, leafletOutput("compar_map2c", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap2c") ),
				column(3, leafletOutput("compar_map3c", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap3c") ),
				column(3, leafletOutput("compar_map4c", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap4c") )
			)
		),

		# If 5 maps
		conditionalPanel( "input.multimap_variable.length==5" ,
			fluidRow(align="center", column(12, offset=1,
				column(2, leafletOutput("compar_map1d", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap1d") ),
				column(2, leafletOutput("compar_map2d", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap2d") ),
				column(2, leafletOutput("compar_map3d", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap3d") ),
				column(2, leafletOutput("compar_map4d", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap4d") ),
				column(2, leafletOutput("compar_map5d", height=multimap_height) %>% withSpinner( color= "#2ecc71"), uiOutput("title_multimap5d") )
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
		fluidRow(align="center", h3(tags$u(tags$b("Figure 2")),": Geographical distribution of several variables across the UK.")),






		# Scatterplot of pairwise comparison
		br(),
		fluidRow( align="center", 
			column(4, offset=4, 
				hr(), 
				h5("To complete this geographical comparison, here are a couple of scatterplots showing the relationship between each pair of variables you have selected:")
			)
		),
		fluidRow(
			column(6, align="right", uiOutput("choice_X_scatter")),
			column(6, align="left", uiOutput("choice_Y_scatter"))
		),
		br(),
		fluidRow(align="center", column(6, offset=3, plotlyOutput("scatter", height="700px", width="700px") %>% withSpinner( color= "#2ecc71"))), 
		fluidRow(align="center", column(4, offset=4, h3(tags$u(tags$b("Figure 3")),": Scatterplot showing the relationship between 2 variables. Each point represent a region of the map"))),








		# Heatmap
		br(),
		fluidRow( align="center", 
			column(4, offset=4, 
				hr(), 
				h5("In case you would be looking for the highest or lowest relationships, here is an heatmap displaying all of them")
			)
		),
		br(),
		fluidRow(align="center", 
			column(6, offset=3, d3heatmapOutput("heatmap", width="700px", height="700px") %>% withSpinner( color= "#2ecc71")),
			column(1, br(),br(),br(),br(),br(),br(),br(), radioGroupButtons(inputId = "varY_heatmap", label = "", choices = c("PRS (no correction)" = 1, "UKB PCs" = 2, "PRS corrected by UKB"=3, "1000 genome PCs" = 4, "PRS corrected by 1000g"=5  ), selected=2, direction = "vertical") )
		), 
		fluidRow(align="center", column(4, offset=4, h3(tags$u(tags$b("Figure 4")),": Heatmap displaying the Pearson correlation coefficient between variable. Pick up the group of traits of the Y axis using the right buttons (and use bottom buttons for the X axis)."))),
		fluidRow(align="center", radioGroupButtons(inputId = "varX_heatmap", label = "", choices = c("PRS (no correction)" = 1, "UKB PCs" = 2, "PRS corrected by UKB"=3, "1000 genome PCs" = 4, "PRS corrected by 1000g"=5  ), selected=2))


	

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
	#	TAB : ADD YOUR DATA
	# #########
	conditionalPanel("input.section == 4",

		fluidRow(column( 4, offset=4, align="center",
			br(),
			h5("It is possible to load your own data in this application to visualize it on a UK map. First load your file which must be in a specific format. Once this file is correctly uploaded, you can calculate summary statistics per area and visualize it")
		)),

		fluidRow(column(7, offset=2, align="left",
			br(), 
			h2("1 - Load your file"),
			hr(),
			h5("Your file must be composed by at least 3 columns. The two first columns must be longitude and latitude respectively (use OSGB 1936 projection, as provided in the UKBiobank dataset). All other columns are your variables that must be normalized and centered. Each line is an individual. File can be compressed (.gz). Respect header shown in the example. Column must be separated by spaces."),
			br(), br()
		)),
		
		fluidRow( align="center",
			column(4,
				h6("Select your file"),
				br(), br(), br(),
				fileInput("file1", "" , multiple = FALSE)				
			),
			column(4,
				h6("How it should look like:"),
				dataTableOutput('doc_ex1' , width="70%" )
			),
			column(4,
				h6("How it does look like:"),
				uiOutput("error_message"),
				dataTableOutput('doc_real' , width="70%" )
			)
		),

		fluidRow(column(7, offset=2, align="left",
			br(), br(), br(), br(),br(), 
			h2("2 - Calculate summary statistics"),
			hr(),
			h5("Once your file as been read correctly, you can run the spatial analysis. This calculation will calculate summary statics for every region of the maps and build the cartograms. Once the computation is done, you will be automatically redirected to the welcome page and all your variable will be available in the 'variable' button"),
			br(), 
			actionButton("button_computation", "Run analysis"),
			uiOutput("info_message") %>% withSpinner( color= "#2ecc71"),
			useSweetAlert(),
			br()			
		))



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
			"A project by", strong(a("A. Abdellaoui", style="color:lightblue", href="https://www.researchgate.net/profile/Abdel_Abdellaoui")), ",", strong(a("Y. Holtz", style="color:lightblue", href="https://holtzyan.wordpress.com")), "and", strong(a("xxx", style="color:lightblue", href="https://holtzyan.wordpress.com")), ".",
			br(),
			"Source code available on", strong(a("Github", style="color:lightblue", href="https://github.com/holtzy/UKB_geo_application")), ".",
			br(),
			"Copyright © 2017 Genes, Geography in the UKB",
			br(), br(),br()
			
		),
		br(),br()
	)

	
	# -------------------------------------------------------------------------------------





# Close the ui
))
