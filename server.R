	# ------------------------------------
	#
	#	UKB geographical analysis
	#
	# ------------------------------------



# open server
shinyServer(function(input, output, session) {



  # ------------------------------------------------------------------------------
  # RECOVER USER CHOICES - USEFUL ALL ALONG THE APP
  # ------------------------------------------------------------------------------

  	# Which kind of variable do I have? Original, or newly loaded by the user?
  	isNew=reactive({
		req(input$map_variable != "")
		return( ifelse(input$map_variable %in% all, "original", "new") )
  	})

  	# Which type of map am I displaying
  	mytype=reactive({
 		if(input$map_geo_unit==1 & input$map_geo_transfo==1){ return(GBR_region) }
		if(input$map_geo_unit==1 & input$map_geo_transfo==2){ return(GBR_region_cartogram) }
		if(input$map_geo_unit==3 & input$map_geo_transfo==1){ return(GBR_hexa) }
		if(input$map_geo_unit==3 & input$map_geo_transfo==2){ return(GBR_hexa_cartogram) }
  	})

  	# On which dataset are we gonna work?
  	mydata=reactive({
 		req(isNew() != "")
 		return_appropriate_dataset( isNew(), input$map_geo_unit, input$map_geo_transfo, user_data() )
  	})

	# What is the subsequent zoom? and stroke? and center part of the map?
  	myzoom=reactive({  ifelse(input$map_geo_unit!=3, return(5.7), return(6) ) 	})
  	mystroke=reactive({  ifelse(input$map_geo_unit!=3, return(1), return(3) ) 	})

	# What is the vector of value we are going to plot
	myvector=reactive({ 
 		req(input$map_variable != "")
		mydata=mydata()
		return( as.numeric(as.character( mydata@data[ , input$map_variable]) ) )
	})

	# What is the color palette we want to use?
	mypalette=reactive({
		req(input$map_variable != "")
		mypalette=return_color_palette( input$type_scale, myvector(), input$choice_palette, input$slider_quantile  )
		return(mypalette)
	})

	# text I will return on the map
	mytext=reactive({
		req(input$map_variable != "")
		mydata=mydata()
		vector=myvector()
		paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(vector,2), sep="") %>% lapply(htmltools::HTML)
	})

	# Now I have a set of reactive value that I can use on my map.
	# If the user change only the number of bins in the palette for example, only the implicated reactives will be recalculated!













  # ------------------------------------------------------------------------------
  # FIRST PLOT
  # ------------------------------------------------------------------------------
		
	# First I do a default background map
	output$main_map <- renderLeaflet({

		# I will remake the map from the beginning if the user change involves change in shape 
		mydata=mytype()
		myzoom=myzoom()
	  
		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = TRUE, minZoom = myzoom, maxZoom = 8)) %>% 
		  	addPolygons( data=mydata , stroke=FALSE, fillColor="transparent" ) 
	})



	# Second I do a observer that will change the map using leaflet proxy.
	observeEvent( { input$map_variable ; input$map_geo_transfo ; input$map_geo_unit ; input$type_scale ; input$choice_palette ; input$slider_quantile},  {
		
		# This chunck must be run only if the map_variable has been computed by shiny. Indeed, the object map_geo_transfo and map_geo_unit are created before, so I need to make this check
		req(input$map_variable != "")
		req(input$section==1)
		print("modify main plot")
		
		# I get back all the user choice:
		mydata=mydata()
		mystroke=mystroke()
		variable=input$map_variable
		mytext=mytext()
		myvector=myvector()
		mypalette=mypalette()

		## plot the subsetted data
    	leafletProxy("main_map") %>%
      		clearShapes() %>%
      		clearControls() %>%
		  	addPolygons( 
		  		data=mydata, 
		    	fillColor = ~mypalette(myvector), fillOpacity = 1,  
		    	stroke=TRUE, color=ifelse(input$map_geo_unit==1, "black", mypalette(myvector) ), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(myvector), fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) %>%
  			addLegend( pal = mypalette, values = myvector, opacity=1, title = variable, position = "bottomleft" )
  	})



	# And return a title for this plot
	output$title_map1<- renderUI({

		# Name = if PC, PC, else I have to find the real name
		if(input$map_variable %in% list_PC)	{ 
			mytext=paste( "Geographical distribution of ", input$map_variable, " in the UK", sep="")
		}else{
			mytext=paste( "Geographical distribution of ", names(list_PRS)[ match( gsub(".residual", "", input$map_variable), list_PRS) ], " in the UK", sep="")
		}
		h3( tags$u(tags$b("Figure 1: ")) , mytext )
	})


	output$moran_map1<- renderUI({ 
		req(input$map_variable != "")
		variable=input$map_variable
		temp <- react_values$moran_df[ which(rownames(react_values$moran_df)==variable), ]
		my_moran =  temp["statistic"] %>% round(2)
		my_pval = temp['p.value'] %>%  format.pval(digits=2)
		mytext=paste( "Moran Coefficient: ", my_moran, " (p=", my_pval, ")", sep="" )
		h3(mytext)
	})






  # ------------------------------------------------------------------------------
  # BARPLOT WITH THE MORAN VALUES OF VARIABLE
  # ------------------------------------------------------------------------------
	

  output$barplot=renderPlot({ 

  	moran_data %>%
  		data.frame() %>%
  		rownames_to_column(var = "variable") %>%
		filter( variable %in%  list_PC ) %>%
		arrange(statistic) %>%
		tail(40) %>%
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


#output$barplot2=renderPlot({
#	moran_data %>%
#	  		data.frame() %>%
#	  		rownames_to_column(var = "variable") %>%
#			filter( variable %in% c(list_PRS, list_PRS_reg) ) %>%
#	  		mutate(type = ifelse(variable %in% list_PRS, 1, 2)) %>%
#	  		mutate( variable_clean = names(list_PRS)[ match( gsub(".residual", "", variable), list_PRS) ] ) %>%
#	  		arrange( desc(type), statistic) %>%
#	  		mutate(variable_clean = factor(variable_clean, unique(variable_clean))) %>%
#	  		ggplot( aes(x=variable_clean, y=statistic, color=as.factor(type))) +
#	  			geom_point( size=4) +
#	 		    coord_flip() +
#			    ylab("Moran I value") +
#			    xlab("") +
#			    theme_minimal() +
#			    theme(legend.position="none", axis.text=element_text(size=13))
 # 	})
	 			

output$barplot3=renderPlot({

	temp <- moran_data %>%
	  	data.frame() %>%
	  	rownames_to_column(var = "variable") %>%
	  	filter( variable %in% c(list_PRS, list_PRS_reg) ) %>%
		mutate( type = ifelse(variable %in% list_PRS, "no correction", "100 PCs correction")) %>%
	  	mutate( variable_clean = names(list_PRS)[ match( gsub(".residual", "", variable), list_PRS) ] ) %>%
	  	arrange( type, statistic) %>%
	  	mutate(variable_clean = factor(variable_clean, unique(variable_clean)))

	extreme <- temp %>%
	  group_by( variable_clean) %>%
	  summarize( mymax=max(statistic), mymin=min(statistic)) 

	ggplot() +
	    geom_segment( data=extreme, aes(x=variable_clean, xend=variable_clean, y=mymin, yend=mymax), color="grey", size=1, alpha=0.2 )  +
	    geom_point( data=temp, aes(x=variable_clean, y=statistic, color=type, alpha=type), size=5) +
	    scale_alpha_manual(values=c(1,0.2), guide = FALSE) +
	    coord_flip() +
	    ylab("Moran I value") +
	    xlab("") +
	    theme_minimal() +
	    theme( axis.text=element_text(size=13)) +
	    labs(color="Type of correction")

  	})








  # ------------------------------------------------------------------------------
  # COMPARISON PLOTS (Max=5 plots allowed)
  # ------------------------------------------------------------------------------
		
	# ------ BACKGROUND MAP -------- #
	output$compar_map1 <- output$compar_map2 <- output$compar_map3 <- output$compar_map4 <- renderLeaflet({
		
		#req(input$section==3)
		
		# I will remake the map from the beginning if the user change involves change in shape 
		mydata=mytype()
		myzoom=myzoom()
				  
		# Final Map
		leaflet(mydata, options = leafletOptions(zoomControl = FALSE, minZoom = myzoom, maxZoom = myzoom)) %>% 
		  	addPolygons( data=mydata , stroke=FALSE, fillColor="transparent", color="transparent" )
	})




	# ------ MAP 1 -------- #
		
	# And modify this map with leaflet proxy
	observeEvent( { input$section ; input$multimap_variable1 ; input$map_geo_transfo ; input$map_geo_unit ; input$type_scale ; input$choice_palette ; input$slider_quantile},  {
		
		# This chunck must be run only if the map_variable has been computed by shiny. Indeed, the object map_geo_transfo and map_geo_unit are created before, so I need to make this check
		req(input$multimap_variable1 != "")
		req(input$section==3)
		print("modify multiplot1")

		# I get back all the users choice:
		isNew=ifelse(input$multimap_variable1 %in% all, "original", "new")
		mydata=return_appropriate_dataset( isNew, input$map_geo_unit, input$map_geo_transfo, user_data() )
		variable=input$multimap_variable1
		myvector=as.numeric(as.character( mydata@data[ , variable]) ) 
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(myvector,2), sep="") %>% lapply(htmltools::HTML)

		mypalette=return_color_palette( input$type_scale, myvector, input$choice_palette, input$slider_quantile  )

		mystroke=mystroke()

    	## plot the subsetted data
    	leafletProxy("compar_map1") %>%
      		clearShapes() %>%
		  	addPolygons( 
		  		data=mydata, 
		    	fillColor = ~mypalette(myvector), stroke=TRUE, fillOpacity = 1, color=~mypalette(myvector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(myvector), fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) 
  	})	





	# ------ MAP 2 -------- #
		
	# And modify this map with leaflet proxy
	observeEvent( { input$section ; input$multimap_variable2 ; input$map_geo_transfo ; input$map_geo_unit ; input$type_scale ; input$choice_palette ; input$slider_quantile},  {
		
		# This chunck must be run only if the map_variable has been computed by shiny. Indeed, the object map_geo_transfo and map_geo_unit are created before, so I need to make this check
		req(input$multimap_variable2 != "")
		req(input$section==3)
		print("modify multiplot2")

		# I get back all the users choice:
		isNew=ifelse(input$multimap_variable2 %in% all, "original", "new")
		mydata=return_appropriate_dataset( isNew, input$map_geo_unit, input$map_geo_transfo, user_data() )
		variable=input$multimap_variable2
		myvector=as.numeric(as.character( mydata@data[ , variable]) ) 
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(myvector,2), sep="") %>% lapply(htmltools::HTML)

		mypalette=return_color_palette( input$type_scale, myvector, input$choice_palette, input$slider_quantile  )
		mystroke=mystroke()

    	## plot the subsetted data
    	leafletProxy("compar_map2") %>%
      		clearShapes() %>%
		  	addPolygons( 
		  		data=mydata, 
		    	fillColor = ~mypalette(myvector), stroke=TRUE, fillOpacity = 1, color=~mypalette(myvector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(myvector), fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) 
  	})	



	# ------ MAP 3 -------- #
		
	# And modify this map with leaflet proxy
	observeEvent( { input$section ; input$multimap_variable3 ; input$map_geo_transfo ; input$map_geo_unit ; input$type_scale ; input$choice_palette ; input$slider_quantile},  {
		
		# This chunck must be run only if the map_variable has been computed by shiny. Indeed, the object map_geo_transfo and map_geo_unit are created before, so I need to make this check
		req(input$multimap_variable3 != "")
		req(input$section==3)
		print("modify multiplot3")

		# I get back all the users choice:
		isNew=ifelse(input$multimap_variable3 %in% all, "original", "new")
		mydata=return_appropriate_dataset( isNew, input$map_geo_unit, input$map_geo_transfo, user_data() )
		variable=input$multimap_variable3
		myvector=as.numeric(as.character( mydata@data[ , variable]) ) 
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(myvector,2), sep="") %>% lapply(htmltools::HTML)

		mypalette=return_color_palette( input$type_scale, myvector, input$choice_palette, input$slider_quantile  )
		mystroke=mystroke()

    	## plot the subsetted data
    	leafletProxy("compar_map3") %>%
      		clearShapes() %>%
		  	addPolygons( 
		  		data=mydata, 
		    	fillColor = ~mypalette(myvector), stroke=TRUE, fillOpacity = 1, color=~mypalette(myvector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(myvector), fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) 
  	})	


	# ------ MAP 4 -------- #
		
	# And modify this map with leaflet proxy
	observeEvent( { input$section ; input$multimap_variable4 ; input$map_geo_transfo ; input$map_geo_unit ; input$type_scale ; input$choice_palette ; input$slider_quantile},  {
		
		# This chunck must be run only if the map_variable has been computed by shiny. Indeed, the object map_geo_transfo and map_geo_unit are created before, so I need to make this check
		req(input$multimap_variable4 != "")
		req(input$section==3)
		print("modify multiplot4")

		# I get back all the users choice:
		isNew=ifelse(input$multimap_variable4 %in% all, "original", "new")
		mydata=return_appropriate_dataset( isNew, input$map_geo_unit, input$map_geo_transfo, user_data() )
		variable=input$multimap_variable4
		myvector=as.numeric(as.character( mydata@data[ , variable]) ) 
		mytext=paste("Region: ", mydata@data$geo_label,"<br/>", "Number of people: ", mydata@data$nb_people, "<br/>", "Value: ", round(myvector,2), sep="") %>% lapply(htmltools::HTML)

		mypalette=return_color_palette( input$type_scale, myvector, input$choice_palette, input$slider_quantile  )
		mystroke=mystroke()

    	## plot the subsetted data
    	leafletProxy("compar_map4") %>%
      		clearShapes() %>%
		  	addPolygons( 
		  		data=mydata, 
		    	fillColor = ~mypalette(myvector), stroke=TRUE, fillOpacity = 1, color=~mypalette(myvector), weight=mystroke,
   				highlight = highlightOptions( weight = 5, color = ~mypalette(myvector), fillOpacity = 0.3, bringToFront = TRUE),
    			label = mytext,
    			labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  			) 
  	})	

	


	# ------ TITLES -------- #

	output$title_multimap1  <- renderUI({ 
		temp <- react_values$moran_df[ which(rownames(react_values$moran_df)==input$multimap_variable1), ]
		my_moran =  temp["statistic"] %>% round(2)
		my_pval = temp['p.value'] %>%  format.pval(digits=2)
		mytext=paste( "Moran: ", my_moran, " (p=", my_pval, ")", sep="" )
		mytitle <- ifelse(input$multimap_variable1 %in% list_PC, input$multimap_variable1, names(list_PRS)[ match( gsub(".residual", "", input$multimap_variable1), list_PRS) ] )
		helpText( a(mytitle, style="color:#2ecc71; font-size:22px;") , mytext)
	})

	output$title_multimap2  <- renderUI({ 
		temp <- react_values$moran_df[ which(rownames(react_values$moran_df)==input$multimap_variable2), ]
		my_moran =  temp["statistic"] %>% round(2)
		my_pval = temp['p.value'] %>%  format.pval(digits=2)
		mytext=paste( "Moran: ", my_moran, " (p=", my_pval, ")", sep="" )
		mytitle <- ifelse(input$multimap_variable2 %in% list_PC, input$multimap_variable2, names(list_PRS)[ match( gsub(".residual", "", input$multimap_variable2), list_PRS) ] )
		helpText( a(mytitle, style="color:#2ecc71; font-size:22px;") , mytext)
	})

	output$title_multimap3  <- renderUI({ 
		temp <- react_values$moran_df[ which(rownames(react_values$moran_df)==input$multimap_variable3), ]
		my_moran =  temp["statistic"] %>% round(2)
		my_pval = temp['p.value'] %>%  format.pval(digits=2)
		mytext=paste( "Moran: ", my_moran, " (p=", my_pval, ")", sep="" )
		mytitle <- ifelse(input$multimap_variable3 %in% list_PC, input$multimap_variable3, names(list_PRS)[ match( gsub(".residual", "", input$multimap_variable3), list_PRS) ] )
		helpText( a(mytitle, style="color:#2ecc71; font-size:22px;") , mytext)
	})

	output$title_multimap4  <- renderUI({ 
		temp <- react_values$moran_df[ which(rownames(react_values$moran_df)==input$multimap_variable4), ]
		my_moran =  temp["statistic"] %>% round(2)
		my_pval = temp['p.value'] %>%  format.pval(digits=2)
		mytext=paste( "Moran: ", my_moran, " (p=", my_pval, ")", sep="" )
		mytitle <- ifelse(input$multimap_variable4 %in% list_PC, input$multimap_variable4, names(list_PRS)[ match( gsub(".residual", "", input$multimap_variable4), list_PRS) ] )
		helpText( a(mytitle, style="color:#2ecc71; font-size:22px;") , mytext)
	})










  # ------------------------------------------------------------------------------
  # SCATTERPLOT COMPARISON
  # ------------------------------------------------------------------------------
	

	# 2 input selector for X and Y axis
	output$choice_X_scatter<- renderUI({ 		pickerInput(inputId = "Xaxis_scatter", label = "X axis", choices = list(User_variables=colnames(inFile())[-c(1)], Polygenic_Risk_Score = list_PRS, PC_from_UKB = list_PC_UKB, PRS_corrected_UKB=list_PRS_reg_UKB, PC_from_1000genome = list_PC_1KG, PRS_corrected_1000genome=list_PRS_reg_1KG  ), multiple=FALSE, selected="PC1", width="300px") })
	output$choice_Y_scatter<- renderUI({ 		pickerInput(inputId = "Yaxis_scatter", label = "Y axis", choices = list(User_variables=colnames(inFile())[-c(1)], Polygenic_Risk_Score = list_PRS, PC_from_UKB = list_PC_UKB, PRS_corrected_UKB=list_PRS_reg_UKB, PC_from_1000genome = list_PC_1KG, PRS_corrected_1000genome=list_PRS_reg_1KG  ), multiple=FALSE, selected="PC2", width="300px") })


  	output$scatter=renderPlotly({ 

  		# Do it only if the user is in the 'compare' tab
  		req(input$section==3)
		
		# Get appropriate datasets (may be the user wants to compare its data with abdel data)
		mydata1=return_appropriate_dataset( ifelse(input$Xaxis_scatter %in% all, "original", "new"), input$map_geo_unit, input$map_geo_transfo,  user_data()) 
		mydata2=return_appropriate_dataset( ifelse(input$Yaxis_scatter %in% all, "original", "new"), input$map_geo_unit, input$map_geo_transfo,  user_data()) 

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

		# Compute correlation
		mycor <- cor.test(tmp$varx, tmp$vary, use="complete.obs")
		clean_pval <- mycor$p.value %>% format.pval()
		clean_cor <- mycor$estimate %>% round(2)

  		# Make the plot
		p=ggplot(data=tmp, aes(x=varx, y=vary, color=nb_people, text=text, size=nb_people)) + 
  			geom_point(alpha=0.8) +
  			coord_equal() +
  			scale_color_viridis() +
  			theme_bw() +
  			theme(legend.position = "none") +
  			xlab(input$Xaxis_scatter) + 
  			ylab(input$Yaxis_scatter) +
  			ggtitle(paste("Correlation: ", clean_cor, " (p: ", clean_pval, ")", sep=""))


  		ggplotly(p, tooltip="text", )


	})









  # ------------------------------------------------------------------------------
  # HEATMAP
  # ------------------------------------------------------------------------------
	

	# Heatmap is incompatible with Leaflet legend... CSS incompatibility because the same .info class is used..
	# Thus I will do a heatmap using ggplot2 and plotly instead.
	#output$heatmap=renderD3heatmap({

  		# Do it only if the user is in the 'compare' tab
  	#	req(input$section==3)

  		# recover selected data
	#	mydata=return_appropriate_dataset( "original", input$map_geo_unit, input$map_geo_transfo, user_data())

		# calculate complete correlation matrix
	#	mycor = GBR_region@data %>% select( -geo_label) %>% cor( . , use="complete.obs")
	#	diag(mycor)=NA

		# Keep only fields that interest user
	#	mylist=list(list_PRS, list_PC_UKB , list_PRS_reg_UKB , list_PC_1KG ,  list_PRS_reg_1KG)
	#	row_to_keep = which( rownames(mycor) %in% mylist[[as.numeric(input$varX_heatmap)]] )
	#	col_to_keep = which( rownames(mycor) %in% mylist[[as.numeric(input$varY_heatmap)]] )
	#	mycor=mycor[ row_to_keep, col_to_keep ]

		# graphic
	#	d3heatmap(mycor, color = "Blues")

	
	#})



	output$heatmap=renderPlotly({
  		
  		# Do it only if the user is in the 'compare' tab
  		req(input$section==3)

  		# recover selected data
		mydata=return_appropriate_dataset( "original", input$map_geo_unit, input$map_geo_transfo, user_data())

		# calculate complete correlation matrix
		mycor = GBR_region@data %>% select( -geo_label) %>% cor( . , use="complete.obs")
		diag(mycor)=NA

		# Keep only fields that interest user
		mylist=list(list_PRS, list_PRS_reg, list_PC)
		row_to_keep = which( rownames(mycor) %in% mylist[[as.numeric(input$varX_heatmap)]] )
		col_to_keep = which( rownames(mycor) %in% mylist[[as.numeric(input$varY_heatmap)]] )
		mycor=mycor[ row_to_keep, col_to_keep ]

		# Make a long format for ggplot2
		don <- mycor %>% 
			as.data.frame() %>% 
			rownames_to_column("var1") %>% 
			gather(var2, value, -1) %>%
			mutate( var1 = ifelse(var1 %in% list_PC, var1, names(list_PRS)[ match( gsub(".residual", "", var1), list_PRS) ] ) ) %>%				#Use the key to retrieve the proper name
			mutate( var2 = ifelse(var2 %in% list_PC, var2, names(list_PRS)[ match( gsub(".residual", "", var2), list_PRS) ] )  ) 

		# Make the plot
		p <- don %>%  
		  ggplot(aes( x=var1, y=var2)) + 
			geom_tile(aes(fill = value), colour = "white", size=4) + 
			#scale_fill_gradient(low = "white", high = "steelblue", breaks=c(0, .2, .4, .6, .8, 1), labels=c(0, .2, .4, .6, .8, 1) ) +
			scale_fill_distiller(palette = "PRGn") +
			theme_grey(base_size = 9) + 
			labs(x = "", y = "") + 
			scale_x_discrete(expand = c(0, 0)) +
			scale_y_discrete(expand = c(0, 0)) + 
			theme(
			  #legend.position = "none", 
			  axis.ticks = element_blank(), 
			  axis.text.x = element_text(size = 10, angle = 45, hjust = 0, colour = "grey50"),
			  axis.title = element_text(size = 14, angle = 0, hjust = 1, colour = "#2ecc71"),
			  axis.text.y = element_text(size = 10, angle = 0, hjust = 0, colour = "grey50"),
			  plot.margin = unit(c(1.8,1.8,1.8,1.8), "cm")
			)

		# Plotly call
		ggplotly(p)


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
			withProgress(message = 'Making plot', value = 0, {
				tmp=compute_shape_sumstat( inFile(), "")
			})
			print("ok - data ready")
			return(tmp)		
	})


	# --- Fourth step: compute the moran's value for all the new variables.
	observeEvent( user_data(), {
		print("compute moran's statistic")
		withProgress(message = 'Morans stat', value = 0, {
			tmp <- compute_autocor( user_data()[[1]] )
			react_values$moran_df <- rbind( react_values$moran_df, tmp )
		})
		print("added OK")
	})	


	# And show a message that tells if computation is over.
	output$info_message<- renderUI({ 
		if(is.null( user_data() )){
			return( helpText("pas de user data") )
		}else{
			sendSweetAlert( session = session, title = "Done !!", text = "Your variable are ready to be visualized! Please go to the 'explore' tab now", type = "success" )	
			return( helpText("Computation done. Please go to the 'explore' tab.") )
		}
	})



	# --- Last step: the UI buttons must be re-generated when new data are loaded:
	output$map_variable_button <- renderUI({
		pickerInput(inputId = "map_variable", label = "", choices = list(`User variables`=colnames(inFile())[-c(1)], `Polygenic Risk Score` = list_PRS, `PRS (corrected with PCs)`=list_PRS_reg, PCs = list_PC), selected='PC1')
	})
	output$multimap_variable_button1 <- renderUI({
		pickerInput(inputId = "multimap_variable1", label = "", choices = list(`User variables`=colnames(inFile())[-c(1)], `Polygenic Risk Score` = list_PRS,  `PRS (corrected with PCs)`=list_PRS_reg, PCs = list_PC), multiple=FALSE, selected="PC1", width="300px")
	})
	output$multimap_variable_button2 <- renderUI({
		pickerInput(inputId = "multimap_variable2", label = "", choices = list(`User variables`=colnames(inFile())[-c(1)], `Polygenic Risk Score` = list_PRS,  `PRS (corrected with PCs)`=list_PRS_reg, PCs = list_PC), multiple=FALSE, selected="PC2", width="300px")
	})
	output$multimap_variable_button3 <- renderUI({
		pickerInput(inputId = "multimap_variable3", label = "", choices = list(`User variables`=colnames(inFile())[-c(1)], `Polygenic Risk Score` = list_PRS,  `PRS (corrected with PCs)`=list_PRS_reg, PCs = list_PC), multiple=FALSE, selected="PC3", width="300px")
	})
	output$multimap_variable_button4 <- renderUI({
		pickerInput(inputId = "multimap_variable4", label = "", choices = list(`User variables`=colnames(inFile())[-c(1)], `Polygenic Risk Score` = list_PRS,  `PRS (corrected with PCs)`=list_PRS_reg, PCs = list_PC), multiple=FALSE, selected="PC4", width="300px")
	})		



	# --- Offer to the user to download his aggregated data
	output$downloadData <- downloadHandler(
    	filename = "UKB_geo.csv",
    	content = function(file) {
    		data=user_data()[[1]]@data
    		write.csv(data, file, row.names=FALSE)
    	}
  	)

	# --- A flag that tells if user data is available or not
	output$flagOK <- reactive( !is.null(user_data()) )
	outputOptions(output, 'flagOK', suspendWhenHidden=FALSE)









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
  	  	












