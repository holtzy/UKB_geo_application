	# ------------------------------------
	#
	#	UKB geographical analysis
	#
	# ------------------------------------



# open server
shinyServer(function(input, output, session) {








  # ------------------------------------------------------------------------------
  # FIRST PLOT
  # ------------------------------------------------------------------------------
		
	output$main_map <- renderLeaflet({

		mydata=return_appropriate_dataset( input$map_variable, input$map_geo_unit, input$map_geo_transfo, user_data() )

		# Set zoom and troke width
		if( input$map_geo_unit!=3 ){
			myzoom=5.7
			mystroke=1
		}else{
			myzoom=6
			mystroke=3
		}

		# Get the variable chosen by user:
		variable=input$map_variable
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		if(input$type_scale=="Bin"){
			mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
			mypalette = colorBin( palette=input$choice_palette, domain=vector, na.color="transparent", bins=mybins)
		}
		if(input$type_scale=="Quantile"){
			mypalette = colorQuantile( palette=input$choice_palette, domain=vector, na.color="transparent", n=7)
		}
		if(input$type_scale=="Numerical"){
			mypalette = colorNumeric( palette=input$choice_palette, domain=vector, na.color="transparent")
		}

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)
		  
		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = TRUE, minZoom = myzoom, maxZoom = 8)) %>% 
		  	addPolygons( 
		    	fillColor = ~mypalette(vector), stroke=TRUE, fillOpacity = 1, color=~mypalette(vector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(vector), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) %>%
  			addLegend( pal=mypalette, values=vector, opacity=0.9, title = variable, position = "bottomright" )

	})


	# And return a title for this plot
	output$title_map1<- renderUI({ 
		mytext=paste( "Geographical distribution of ", input$map_variable, " in the UK", sep="")
		h3( tags$u(tags$b("Figure 1: ")) , mytext )
	})
	output$moran_map1<- renderUI({ 
		req(input$map_variable != "")
		variable=input$map_variable
		my_moran = moran_data[ which(rownames(moran_data)==variable),"statistic"] %>% round(2)
		mytext=paste( "Moran Coefficient: ", my_moran, sep="" )
		h3(mytext)
	})







  # ------------------------------------------------------------------------------
  # BARPLOT WITH THE MORAN VALUES OF VARIABLE
  # ------------------------------------------------------------------------------
	

  output$barplot=renderPlot({ 

  	moran_data %>%
  		data.frame() %>%
  		rownames_to_column(var = "variable") %>%
		filter( ! variable %in%  c( list_PC_1KG, list_PRS_reg_UKB, list_PRS_reg_1KG ) )%>%
		arrange(statistic) %>%
		mutate(variable=factor(variable, variable)) %>%
		ggplot( aes(x=variable, y=statistic, fill=statistic)) +
			geom_bar( stat="identity", width=0.5) +
		    scale_fill_viridis() +
		    coord_flip() +
		    ylab("Moran I value") +
		    xlab("") +
		    theme_minimal() +
		    theme(legend.position="none", axis.text=element_text(size=13))


  	})







  # ------------------------------------------------------------------------------
  # COMPARISON PLOTS (Max=5 plots allowed)
  # ------------------------------------------------------------------------------
		

  	# ----- REACTIVE ZOOM AND STROKE VALUE
  	myzoom=reactive({
  		if( input$map_geo_unit!=3 ){ ifelse(length(input$multimap_variable)==5, return(5.2), return(5.7)) }else{ifelse(length(input$multimap_variable)==5, return(5.6), return(6.2))}
  	})
    mystroke=reactive({
  		if( input$map_geo_unit!=3 ){ return(1) }else{return(3)}
  	})	





	# ------ PLOT1
	output$compar_map1a <- output$compar_map1b <- output$compar_map1c <- output$compar_map1d <- renderLeaflet({

		# I need to have enough chosen value
		req( length(input$multimap_variable)>=1 )

		mydata=return_appropriate_dataset( input$multimap_variable[1], input$map_geo_unit, input$map_geo_transfo,  user_data())
		myzoom=myzoom()
		mystroke=mystroke()

		# Get the variable choose by user:
		variable=input$multimap_variable[1]
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
		mypalette = colorBin( palette=input$choice_palette, domain=vector, na.color="transparent", bins=mybins)

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)

  		print("map1 done") 
		  
		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = FALSE, minZoom = myzoom, maxZoom = 8)) %>% 
		  	addPolygons( 
		    	fillColor = ~mypalette(vector), stroke=TRUE, fillOpacity = 1, color=~mypalette(vector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(vector), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			)


	})






	# ------ PLOT2
	output$compar_map2a <- output$compar_map2b <- output$compar_map2c <- output$compar_map2d <- renderLeaflet({

		# I need to have enough chosen value
		req( length(input$multimap_variable)>=2 )

		mydata=return_appropriate_dataset( input$multimap_variable[2], input$map_geo_unit, input$map_geo_transfo, user_data())
		myzoom=myzoom()
		mystroke=mystroke()

		# Get the variable choose by user:
		variable=input$multimap_variable[2]
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
		mypalette = colorBin( palette=input$choice_palette, domain=vector, na.color="transparent", bins=mybins)

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)
		  
  		print("map2 done") 

		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = FALSE, minZoom = myzoom, maxZoom = 8)) %>% 
		  	addPolygons( 
		    	fillColor = ~mypalette(vector), stroke=TRUE, fillOpacity = 1, color=~mypalette(vector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(vector), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) 


	})


	# ------ PLOT3
	output$compar_map3b <- output$compar_map3c <- output$compar_map3d <- renderLeaflet({

		# I need to have enough chosen value
		req( length(input$multimap_variable)>=3 )

		mydata=return_appropriate_dataset( input$multimap_variable[3], input$map_geo_unit, input$map_geo_transfo,  user_data())
		myzoom=myzoom()
		mystroke=mystroke()

		# Get the variable choose by user:
		variable=input$multimap_variable[3]
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
		mypalette = colorBin( palette=input$choice_palette, domain=vector, na.color="transparent", bins=mybins)

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)
		  
  		print("map3 done") 

		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = FALSE, minZoom = myzoom, maxZoom = 8)) %>% 
		  	addPolygons( 
		    	fillColor = ~mypalette(vector), stroke=TRUE, fillOpacity = 1, color=~mypalette(vector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(vector), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) 


	})


	# ------ PLOT4
	output$compar_map4c <- output$compar_map4d <- renderLeaflet({

		# I need to have enough chosen value
		req( length(input$multimap_variable)>=4 )

		mydata=return_appropriate_dataset( input$multimap_variable[4], input$map_geo_unit, input$map_geo_transfo,  user_data())
		myzoom=myzoom()
		mystroke=mystroke()

		# Get the variable choose by user:
		variable=input$multimap_variable[4]
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
		mypalette = colorBin( palette=input$choice_palette, domain=vector, na.color="transparent", bins=mybins)

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)
		  
  		print("map4 done") 

		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = FALSE, minZoom = myzoom, maxZoom = 8)) %>% 
		  	addPolygons( 
		    	fillColor = ~mypalette(vector), stroke=TRUE, fillOpacity = 1, color=~mypalette(vector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(vector), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) 

	})



	# ------ PLOT5
	output$compar_map5d <- renderLeaflet({

		# I need to have enough chosen value
		req( length(input$multimap_variable)>=5 )

		mydata=return_appropriate_dataset( input$multimap_variable[5], input$map_geo_unit, input$map_geo_transfo, user_data())
		myzoom=myzoom()
		mystroke=mystroke()

		# Get the variable choose by user:
		variable=input$multimap_variable[5]
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
		mypalette = colorBin( palette=input$choice_palette, domain=vector, na.color="transparent", bins=mybins)

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)
		  
  		print("map5 done") 

		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = FALSE, minZoom = myzoom, maxZoom = 8)) %>% 
		  	addPolygons( 
		    	fillColor = ~mypalette(vector), stroke=TRUE, fillOpacity = 1, color=~mypalette(vector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(vector), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) 


	})




	# Titles
	output$title_multimap1a <- output$title_multimap1b <- output$title_multimap1c <- output$title_multimap1d <- renderUI({ 
		req( length(input$multimap_variable)>=1 )
		my_moran = moran_data[ which(rownames(moran_data)==input$multimap_variable[1]),"statistic"] %>% round(2)
		helpText(a(input$multimap_variable[1], style="color:#2ecc71; font-size:22px;") , " (", my_moran, ")", sep="")
	})
	output$title_multimap2a <- output$title_multimap2b <- output$title_multimap2c <- output$title_multimap2d<- renderUI({ 
		req( length(input$multimap_variable)>=2 )
		my_moran = moran_data[ which(rownames(moran_data)==input$multimap_variable[2]),"statistic"] %>% round(2)
		helpText(a(input$multimap_variable[2], style="color:#2ecc71; font-size:22px;") , " (", my_moran, ")", sep="")
	})
	output$title_multimap3b <- output$title_multimap3c <- output$title_multimap3d <-  renderUI({ 
		req( length(input$multimap_variable)>=3 )
		my_moran = moran_data[ which(rownames(moran_data)==input$multimap_variable[3]),"statistic"] %>% round(2)
		helpText(a(input$multimap_variable[3], style="color:#2ecc71; font-size:22px;") , " (", my_moran, ")", sep="")
	})
	output$title_multimap4c <- output$title_multimap4d <- renderUI({ 
		req( length(input$multimap_variable)>=4 )
		my_moran = moran_data[ which(rownames(moran_data)==input$multimap_variable[4]),"statistic"] %>% round(2)
		helpText(a(input$multimap_variable[4], style="color:#2ecc71; font-size:22px;") , " (", my_moran, ")", sep="")
	})
	output$title_multimap5d<- renderUI({ 
		req( length(input$multimap_variable)>=5 )
		my_moran = moran_data[ which(rownames(moran_data)==input$multimap_variable[5]),"statistic"] %>% round(2)
		helpText(a(input$multimap_variable[5], style="color:#2ecc71; font-size:22px;") , " (", my_moran, ")", sep="")
	})








  # ------------------------------------------------------------------------------
  # SCATTERPLOT COMPARISON
  # ------------------------------------------------------------------------------
	

	# 2 input selector for X and Y axis
	output$choice_X_scatter<- renderUI({ 		pickerInput(inputId = "Xaxis_scatter", label = "X axis", choices = input$multimap_variable, multiple=FALSE, selected=input$multimap_variable[1], width="300px") })
	output$choice_Y_scatter<- renderUI({ 		pickerInput(inputId = "Yaxis_scatter", label = "Y axis", choices = input$multimap_variable, multiple=FALSE, selected=input$multimap_variable[2], width="300px") })



  	output$scatter=renderPlotly({ 

  		# Do it only if the user is in the 'compare' tab
  		req(input$section==3)
		
		# Get appropriate datasets (may be the user wants to compare its data with abdel data)
		mydata1=return_appropriate_dataset( input$Xaxis_scatter, input$map_geo_unit, input$map_geo_transfo,  user_data()) 
		mydata2=return_appropriate_dataset( input$Yaxis_scatter, input$map_geo_unit, input$map_geo_transfo,  user_data()) 

		# if not the same, I merge
		if(!identical(mydata1,mydata2)){
			print("pas pareil")
			tmp=cbind(mydata1@data, mydata2@data)
		}else{
			tmp=mydata1@data
		}

  		# Recover the 2 chosen variables
  		tmp$varx=tmp[ , input$Xaxis_scatter]
  		tmp$vary=tmp[ , input$Yaxis_scatter]

  		# Prepare text
		tmp$text=paste( tmp$geo_label, "\n", input$Xaxis_scatter, ": ", round(tmp$varx, 2), "\n", input$Yaxis_scatter, ": ", round(tmp$vary, 2), "\n", "Number of individual:", tmp$nb_people, sep="")

  		# Make the plot
		p=ggplot(data=tmp, aes(x=varx, y=vary, color=nb_people, text=text, size=nb_people)) + 
  			geom_point() +
  			coord_equal() +
  			scale_color_viridis() +
  			theme_bw() +
  			theme(legend.position = "none") +
  			xlab(input$Xaxis_scatter) + 
  			ylab(input$Yaxis_scatter) +
  			ggtitle(paste("Correlation: ", round(cor(tmp$varx, tmp$varx, use="complete.obs"),2) , sep=""))


  		ggplotly(p, tooltip="text", )


	})









  # ------------------------------------------------------------------------------
  # HEATMAP
  # ------------------------------------------------------------------------------
	

	output$heatmap=renderD3heatmap({

  		# Do it only if the user is in the 'compare' tab
  		req(input$section==3)

  		# recover selected data
		mydata=return_appropriate_dataset( input$multimap_variable[1], input$map_geo_unit, input$map_geo_transfo, user_data())

		# calculate complete correlation matrix
		mycor = GBR_region@data %>% select( -geo_label) %>% cor( . , use="complete.obs")
		diag(mycor)=NA

		# Keep only fields that interest user
		mylist=list(list_PRS, list_PC_UKB , list_PRS_reg_UKB , list_PC_1KG ,  list_PRS_reg_1KG)
		row_to_keep = which( rownames(mycor) %in% mylist[[as.numeric(input$varX_heatmap)]] )
		col_to_keep = which( rownames(mycor) %in% mylist[[as.numeric(input$varY_heatmap)]] )
		mycor=mycor[ row_to_keep, col_to_keep ]

		# graphic
		d3heatmap(mycor, color = "Blues")

	
	})








  # ------------------------------------------------------------------------------
  # LOAD USER DATA
  # ------------------------------------------------------------------------------


	# --- First step = load the User data frame
	inFile=reactive({
				
		# If nothing is choosen I just ask to user to choose something
		if ( is.null(input$file1)) {

  			output$error_message<- renderUI({ helpText("Please select your file") })
  			return(NULL)
						
				
		# If the user proposes a dataset:
		}else{

			# Try to read the data
			userdata=try( read.table( input$file1$datapath, header=TRUE )) 

			# if the file is NOT readable by R
			if(class(userdata)=="try-error"){
	  			output$error_message<- renderUI({ helpText("File input is not readable by R. Please check your format" , style="color:red ; font-family: 'times'; font-size:13pt") })
				return(NULL)
			

			# If the file is read correctly
			} else {
				output$error_message<- renderUI({ helpText(NULL) })
				return(userdata)
			}
		}

	})


	# --- Second step: show the loaded data
	observe({
		output$doc_real <- DT::renderDataTable(
			DT::datatable( inFile()[c(1:5) , ] , rownames = FALSE , options = list(dom = 't' ))
		)
	})



	# --- Third step: run a function that make link between shapes and user data frame. It returns several objects
	user_data <- eventReactive(input$button_computation, {	
			
			# run the function
			print("compute data")
			tmp=compute_shape_sumstat( inFile(), "")
			print("ok - data ready")
			return(tmp)		
	})

	# And show a message that tells if computation is over.
	output$info_message<- renderUI({ 
		if(is.null( user_data() )){
			helpText("pas de user data") 
		}else{
			helpText("Computation done. You're ready to explore your data.") 
			sendSweetAlert( session = session, title = "Done !!", text = "Your variable are ready to be visualized!", type = "success" )	
		}
	})



	# --- Last step: the UI buttons must be re-generated when new data are loaded:
	output$map_variable_button <- renderUI({
		pickerInput(inputId = "map_variable", label = "", choices = list(User_variables=colnames(inFile())[-c(1)], Polygenic_Risk_Score = list_PRS, PC_from_UKB = list_PC_UKB, PRS_corrected_UKB=list_PRS_reg_UKB, PC_from_1000genome = list_PC_1KG, PRS_corrected_1000genome=list_PRS_reg_1KG  ), selected='PC1')
	})

	output$multimap_variable_button <- renderUI({
		pickerInput(inputId = "multimap_variable", label = "", choices = list(User_variables=colnames(inFile())[-c(1)], Polygenic_Risk_Score = list_PRS, PC_from_UKB = list_PC_UKB, PRS_corrected_UKB=list_PRS_reg_UKB, PC_from_1000genome = list_PC_1KG, PRS_corrected_1000genome=list_PRS_reg_1KG  ), multiple=TRUE, selected=c("PC1", "PC2"), width="300px")
	})
		
		










  # ------------------------------------------------------------------------------
  # Display an example data set
  # ------------------------------------------------------------------------------

	ex1=data.frame(
			longitude=c( 297500, 391500, 436500, 411500, 533500, 264500 ),
			latitude=c(  669500, 298500, 566500, 531500, 190500, 194500 ),
			new_var_1=c( 0.123, -0.02, 0.011, 0.33, -0.211, -0.012),
			new_var_2=c( 0.193, 0.031, 0.99, 0.390, -0.81, 0.912)
		)


	output$doc_ex1 <- DT::renderDataTable(
		DT::datatable(ex1 , rownames = FALSE , options = list(dom = 't' ))
	)




# Close the ShinyServer  
})
  	  	












