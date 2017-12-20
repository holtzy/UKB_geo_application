	# ------------------------------------
	#
	#	UKB geographical analysis
	#
	# ------------------------------------



# open server
shinyServer(function(input, output) {










  # ------------------------------------------------------------------------------
  # FIRST PLOT
  # ------------------------------------------------------------------------------
		
	output$main_map <- renderLeaflet({

		# Select the data: region large / region medium / hexagone?
		if(input$map_geo_unit==1 & input$map_geo_transfo==1){ mydata=GBR_region }
		if(input$map_geo_unit==1 & input$map_geo_transfo==2){ mydata=GBR_region_cartogram }
		if(input$map_geo_unit==3 & input$map_geo_transfo==1){ mydata=GBR_hexa }
		if(input$map_geo_unit==3 & input$map_geo_transfo==2){ mydata=GBR_hexa_cartogram }

		# Set zoom and troke width
		if( input$map_geo_unit!=3 ){
			myzoom=6.3
			mystroke=1
		}else{
			myzoom=7
			mystroke=3
		}

		# Get the variable choose by user:
		variable=input$map_variable
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
		mypalette = colorBin( palette=input$choice_palette, domain=vector, na.color="transparent", bins=mybins)

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)
		  
		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = FALSE, minZoom = myzoom, maxZoom = 8)) %>% 
		  	addPolygons( 
		    	fillColor = ~mypalette(vector), stroke=TRUE, fillOpacity = 1, color=~mypalette(vector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(vector), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) %>%
  		addLegend( pal=mypalette, values=~variable, opacity=0.9, title = variable, position = "bottomright" )

	})











  # ------------------------------------------------------------------------------
  # COMPARISON PLOTS (Max=5 plots allowed)
  # ------------------------------------------------------------------------------
		

	# ------ PLOT1
	output$compar_map1a <- output$compar_map1b <- output$compar_map1c <- output$compar_map1d <- renderLeaflet({

		# Select the data: region large / region medium / hexagone?
		if(input$map_geo_unit==1 & input$map_geo_transfo==1){ mydata=GBR_region }
		if(input$map_geo_unit==1 & input$map_geo_transfo==2){ mydata=GBR_region_cartogram }
		if(input$map_geo_unit==3 & input$map_geo_transfo==1){ mydata=GBR_hexa }
		if(input$map_geo_unit==3 & input$map_geo_transfo==2){ mydata=GBR_hexa_cartogram }

		# Set zoom and troke width
		if( input$map_geo_unit!=3 ){
			myzoom=6.3
			mystroke=1
		}else{
			myzoom=7
			mystroke=3
		}


		# Get the variable choose by user:
		variable=input$multimap_variable[1]
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
		mypalette = colorBin( palette="viridis", domain=vector, na.color="transparent", bins=mybins)

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)
		  
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

		# Select the data: region large / region medium / hexagone?
		if(input$map_geo_unit==1 & input$map_geo_transfo==1){ mydata=GBR_region }
		if(input$map_geo_unit==1 & input$map_geo_transfo==2){ mydata=GBR_region_cartogram }
		if(input$map_geo_unit==3 & input$map_geo_transfo==1){ mydata=GBR_hexa }
		if(input$map_geo_unit==3 & input$map_geo_transfo==2){ mydata=GBR_hexa_cartogram }

		# Set zoom and troke width
		if( input$map_geo_unit!=3 ){
			myzoom=6.3
			mystroke=1
		}else{
			myzoom=7
			mystroke=3
		}


		# Get the variable choose by user:
		variable=input$multimap_variable[2]
		vector=as.numeric(as.character(mydata@data[ , variable]))

		# Create a color palette with handmade bins.
		mybins=seq( min(vector, na.rm=TRUE), max(vector, na.rm=TRUE), (max(vector, na.rm=TRUE)-min(vector, na.rm=TRUE))/input$slider_quantile) %>% round(2)
		mypalette = colorBin( palette="viridis", domain=vector, na.color="transparent", bins=mybins)

		# text
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>%
		  lapply(htmltools::HTML)
		  
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
	#output$compar_map3b <- output$compar_map3c <- output$compar_map3d <- renderLeaflet({


	# ------ PLOT4
	#output$compar_map4c <- output$compar_map4d <- renderLeaflet({



	# ------ PLOT5
	#output$compar_map5d <- renderLeaflet({













  # ------------------------------------------------------------------------------
  # SCATTERPLOT COMPARISON
  # ------------------------------------------------------------------------------
		

  output$scatter=renderPlotly({ 

	# Select the data: region large / region medium / hexagone?
		if(input$map_geo_unit==1 & input$map_geo_transfo==1){ mydata=GBR_region }
		if(input$map_geo_unit==1 & input$map_geo_transfo==2){ mydata=GBR_region_cartogram }
		if(input$map_geo_unit==3 & input$map_geo_transfo==1){ mydata=GBR_hexa }
		if(input$map_geo_unit==3 & input$map_geo_transfo==2){ mydata=GBR_hexa_cartogram }

  		# Prepare text
		mydata@data$text="yoyo"

  		# Make the plot
		p=ggplot(data=mydata@data, aes(x=PC1, y=PC2, color=PC1, text=text)) + 
  			geom_point() +
  			coord_equal() +
  			scale_color_viridis() +
  			theme_bw() +
  			theme(legend.position = "none") #+
  			#ggtitle(paste("Correlation: ", round(cor(tmp@data$PC1, tmp@data$PC2),2) , sep=""))


  		ggplotly(p, tooltip="text", )


	})




  #output$scatter_matrix=renderPlotly({ 
		
	#p=ggpairs(tmp@data[,c(1:2)]) +
  		#t#heme_void()

 		

 	#ggplotly(p, tooltip="text", )


	#})




# Close the ShinyServer  
})
  	  	












