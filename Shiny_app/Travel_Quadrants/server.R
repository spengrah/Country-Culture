#
# Server logic for "Where Should I Travel Next?" shiny application
#


library(shiny); require(dplyr); require(leaflet); require(jsonlite)
require(geojsonio); require(scales);
# require(tidyr)
# require(fmsb)
# require(ggplot2)

## this code only runs once and is available across sessions ---------------
map_data <- geojson_read("data/countries.geojson", what = "sp")
culture_dims <- c("IDV", "IND", "LTO", "MAS", "PDI", "UAI")

# function that, given a dataset and a home country, calculates the Euclidean
# distance between home and all other countries using all 6 cultural dimensions.
# returns a new dataset with normalized edist appended
edist <- function(X, home_country) {
	ed_df <- select(rawData, IDV, IND, LTO, MAS, PDI, UAI)
	
	distE <- as.matrix(dist(ed_df, method = "euclidean"))[,home_country]
	home_df <- ed_df[row.names(ed_df) == home_country,]
	anti_home <- ifelse(home_df < 50, 100, 0)
	anti_matrix <- matrix(c(home_df, anti_home), nrow = 2, ncol = 6, byrow = T)
	anti_dist <- dist(anti_matrix)[1]
	CD_norm <- 100*(distE/anti_dist)
	
	df <- cbind(X, CD_norm)
	df
}

# function that returns a dataset with GDPPC as the Y variables
GDP <- function(X) {
	GDP_df <- X %>% select(-CEX, -CEXnorm) %>%
		rename(y_var_raw = GDP, y_var_norm = GDPnorm)
}

# function that returns a dataset with CEXPC as the Y variables
CEX <- function(X) {
	CEX_df <- X %>% select(-GDP, -GDPnorm) %>%
		rename(y_var_raw = CEX, y_var_norm = CEXnorm)
}

# ***Recommendation Engine***
# Function to generate recommendations based on min euclidean distance between
# new countries and each visited country, within the N-dimensional space created
# by the user-selected dimensions. Returns back the input dataset with a
# recommendation score appended.
inputsCheck <- function(X, visited, home, dims){
	char_dims <- unlist(dims) # coerce dims to vector
	if (length(char_dims) == 1) {
		rdata <- as.data.frame(X[ ,char_dims])
		row.names(rdata) <- X$Country
		names(rdata) <- eval(char_dims)
		incompletes <- X[!complete.cases(X[ ,char_dims]), "Country"]
	}
	else {
		rdata <- X[ ,char_dims] #subset X to only include user-selected dimensions
		incompletes <- row.names(rdata[!complete.cases(rdata), ])
	}
	
	# check that home country is valid
	if (home %in% incompletes) {
		message <- paste(home, "has missing data for one or more of the dimensions",
						 "you selected. Please select different dimensions or try",
						 "a different home country.")
		
		return(list(type = "error", message = message, incompletes = NULL, data = NULL,
					visited = NULL, valid_visited = NULL, invalid_visited = NULL,
					home = NULL, dims = NULL, rdata = NULL))
	}
	
	# check that visited countries are valid. If not, return only visited
	# countries that are valid
	valid_visited <- visited[!visited %in% incompletes]
	invalid_visited <- visited[visited %in% incompletes]
	
	if (length(invalid_visited)>0) {
		# TO DO: list out individual invalid countries
		message <- paste("The following countries that you've visited have",
						 "missing data for one or more of the dimensions",
						 "you selected. You can leave them in, but they will not",
						 "contribute to your travel recommendations.",
						 paste(invalid_visited, collapse = ", "))
		return(list(type = "warning", message = message, incompletes = incompletes, data = X,
					visited = visited, valid_visited = valid_visited, 
					invalid_visited = invalid_visited,
					home = home, dims = char_dims, rdata = rdata))
	}
	
	return(list(type = NULL, message = NULL, incompletes = incompletes, data = X,
				visited = visited, valid_visited = visited, invalid_visited = NULL,
				home = home, dims = char_dims, rdata = rdata))
	
} 

recommend <- function(inputs) {
		home <- inputs$home
		visited <- inputs$visited
		valid_visited <- inputs$valid_visited
		X <- inputs$data
		rdata <- inputs$rdata
		incompletes <- inputs$incompletes
		dims <- inputs$dims
		
		missing_and_not_visited <- incompletes[!(incompletes %in% visited)]
		current <- c(home, valid_visited) # vector for all visited countries
		
		
		# remove Countries (rows) that don't have complete data for
		# user-selected dimensions
		if (length(dims) == 1) {
			rdata2 <- as.data.frame(rdata[complete.cases(rdata),])
			labels <- X$Country
			rows <- labels[!labels %in% incompletes]
			row.names(rdata2) <- labels[!labels %in% incompletes]
			names(rdata2) <- eval(dims)
		}
		else rdata2 <- rdata[complete.cases(rdata),]
	
		# weight the 6 culture dims collectively equally to each other dim
		if (sum(culture_dims %in% dims) == 6) {
			for(i in culture_dims) {
				rdata2[, i] <- rdata2[, i]/sqrt(6)
			}
		}
		
		# 2. generate a distance matrix for all countries
		rdist <- as.data.frame(as.matrix(suppressWarnings(dist(rdata2))))
		
		# 3. subset the distance matrix for only the rows corresponding to visited
		# and home countries...
		r_df <- rdist %>% filter(row.names(rdist) %in% current)
		# ...and columns corresponding to all other countries
		r_df2 <- r_df[, -which(names(r_df) %in% current)]
		
		# 4. summarize by column according to min - POTENTIALLY CHANGE THIS
		r_means <- summarize_each(r_df2, funs(min))
		
		# 5. return the dataframe with recommendation score, type, and display color
		# variables appended
		new <- data.frame(Country = names(r_means), score = unlist(r_means),
						  type = "new", stringsAsFactors = F)
		
		h <- data.frame(Country = home, score = NA, type = "home", stringsAsFactors = F)
		
		if (length(visited) == 0) {v <- data.frame(Country = NA, score = NA,
												   type = NA)
		}
		else {v <- data.frame(Country = visited,
							  score = NA,
							  type = "visited", 
							  stringsAsFactors = F)
		}
		
		if (length(missing_and_not_visited ) == 0) {missing <- data.frame(Country = NA, score = NA,
															 type = "missing")
		
		}
		else {
			missing <- data.frame(Country = missing_and_not_visited,
								  score = NA,
								  type = "missing",
								  stringsAsFactors = F)
		}
		
		vars <- rbind(h, new, missing, v)
		result_df <- left_join(X, vars, by = "Country")
	}

# set up styles for dimensions radar chart
# colors_border <- c(alpha("blue", .9),
# 				   alpha("#ffba38", .9))
# colors_in <- c(alpha("blue", .4),
# 			   alpha("#ffba38", .4))

## code inside this unnamed function runs each session
shinyServer(function(input, output, session) {
	# interactive selection of wealth metric
	Ymethod <- reactive({
		if (input$Ymethod == "GDP") {GDP}
		else CEX
	})
	
	# render the initial map
	output$my_map <- renderLeaflet({
		leaflet() %>%
			addProviderTiles("CartoDB.Positron") %>%
			setView(lng = 60, lat = 37.45, zoom = 2)
	})

	# observer to handle recommendation updates based on inputs
	observe ({
		# assign country types according to user input
		home <- input$home
		visited <- input$visited
		
		# assign dimensions according to user input
		dims <- input$dimensions
		if ("culture" %in% dims) {
			dims <- c(dims[!dims %in% "culture"], culture_dims)
		}
		if ("economy" %in% dims) {
			dims <- c(dims[!dims %in% "economy"],input$economy_choice)
		}
		
		if (!is.null(dims)) {
			# check inputs
			inputs <- inputsCheck(rawData, visited, home, dims)
			message <- inputs$message
			invalid_visited <- inputs$invalid_visited
			message_type <- inputs$type
			
			# bring up a warning if visited countries have missing data
			# for given inputs
			if (is.null(message_type)) {
				removeNotification("visited_warning")
			}
			else if (message_type == "warning"){
				showNotification(
					ui = message,
					duration = 5,
					type = message_type,
					id = "visited_warning"
				)
			}
			
			# throw an error modal if the home country has missing data 
			# for given inputs
			else if (message_type == "error") {
				return(showModal(modalDialog(
					title = "Missing Data",
					message,
					easyClose = T,
					footer = NULL
				)))
			}
	
			# calculate recommendation scores
			countries <- recommend(inputs)
			
			# update country input lists
			home_list <- sort(filter(countries, type != "visited")$Country)
			updateSelectInput(session, "home", choices = home_list, selected = home)
			# TO DO: figure out how to exclude home country from visited list without
			# 		 re-rendering the visitor list after each selection
			# updateSelectInput(session, "visited", choices = visited_list, selected = visited)
	
			# set up country polygon colors
			top3 <- head(arrange(countries, desc(score)), 3)$Country
			rec_score <- countries$score
			score_alpha <- rec_score/max(rec_score, na.rm = T)
			
			colors <- data.frame(Country = countries$Country, 
								 fill = ifelse(countries$type == "missing", "grey",
								 			  ifelse(countries$type == "home", "blue",
							 			   	      ifelse(countries$type == "visited", "green",
							 		 	   	   	     "red"))),
								 line = ifelse(countries$type == "missing", "#eaeaea", 
								 			  ifelse(countries$Country %in% top3, "orange",
								 			  	   "#919191")),
								 weight = ifelse(countries$Country %in% top3, 4, 1),
								 alpha = ifelse(countries$type == "missing", 0,
								 			   ifelse(countries$type == "home", .6,
								 			   	   ifelse(countries$type == "visited", .6,
								 			   	   	   score_alpha)))
								 )
			
			# update polygons according to recommendation scores
			leafletProxy("my_map", data = map_data) %>%
				clearShapes() %>%
				addPolygons(smoothFactor = 0.5,
							# fill the polygon
							fillOpacity = colors$alpha, fillColor = colors$fill,
							# render the lines
							stroke = TRUE, opacity = 1, color = colors$line, weight = colors$weight,
							highlightOptions = highlightOptions(color = "white", weight = 2,
																bringToFront = TRUE))
			
			##TEMPORARY: print the list of countries and recommend scores as a simple table
			output$recs <- renderTable({
				result <- countries %>% select(Country, score) %>% arrange(desc(score))
			})
		}
	})
	
	## return to later --------
	# # tooltip
	# output$hover_info <- renderUI({
	# 	hover <- input$plot_hover
	# 	point <- nearPoints(plotdata(), hover, xvar = "CD_norm",
	# 						yvar = "y_var_raw", maxpoints = 1, addDist = T)
	# 	if (nrow(point) == 0) return(NULL)
	# 		# the following code borrowed from Pawel via, https://gitlab.com/snippets/16220
	# 		# calculate point position INSIDE the image as percent of total dimensions
	# 		# from left (horizontal) and from top (vertical)
	# 		left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
	# 		top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
	# 		
	# 		# calculate distance from left and bottom side of the picture in pixels
	# 		left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
	# 		top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
	# 		
	# 		# create style property for tooltip
	# 		# background color is set so tooltip is a bit transparent
	# 		# z-index is set so we are sure are tooltip will be on top
	# 		style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
	# 						"left:", left_px + 2, "px; top:", 
	# 						top_px + 2, "px; padding: 5px;")
	# 		
	# 		wellPanel(
	# 			style = style,
	# 			HTML(paste0("<b>",as.character(point$Country),"</b>"), "<br/>",
	# 				 paste0("<b>",y_tooltip(),": </b>"), dollar_format()(point$y_var_raw), "<br/>",
	# 				 "<b>Culture Diff: </b>", round(point$CD_norm,0), "<br/>"
	# 				 )
	# 		)
	# })
	# 
	# # the click
	# click <- reactive({input$plot_click})
	# 
	# # data for the clicked country
	# point <- reactive({
	# 	nearPoints(plotdata(), click(), xvar = "CD_norm",
	# 			   yvar = "y_var_raw", maxpoints = 1)
	# })
	# 
	# # render the cultural dimensions radar chart
	# output$click_plot <- renderPlot({
	# 	point_home <- rbind(filter(plotdata(), Country == home()), point())
	# 	
	# 	radar_data <- rbind(rep(100, 6), rep(0, 6), 
	# 						select(point_home, IDV, IND, LTO, MAS, PDI, UAI))
	# 	
	# 	c_names <- c(home(), as.character(point()$Country))
	# 
	# 	if (req(!is.null(click))) {
	# 		par(mar = c(0,0,0,0))
	# 		radarchart(radar_data, axistype = 1, pty = 16, 
	# 				   pcol = colors_border, plwd = 2, plty = 1, pfcol = colors_in,
	# 				   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), 
	# 				   cglwd=0.8, vlcex=1)
	# 		
	# 		legend(x=0.4, y=1.2, legend = c_names, bty = "n", pch=20, 
	# 			   col=colors_border , text.col = "black", cex=1, pt.cex=3)
	# 	}
	# })
	
	# # render the wikipedia URL
	# output$click_url <- renderUI ({
	# 	if (req(!is.null(click()))) {
	# 		country_name <- point()$Country
	# 		if (length(country_name) == 0) {
	# 			HTML(" ")
	# 		}
	# 		else {
	# 		country_url <- gsub(" ", "_", country_name)
	# 		p(align = "right", HTML(paste0("Read more about ", country_name, " on <a href= 'https://en.wikipedia.org/wiki/", 
	# 					country_url, "'> Wikipedia </a>")))
	# 		}
	# 	}
	# 	
	# })
	
})
