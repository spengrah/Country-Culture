#
# Server logic for "Where Should I Travel Next?" shiny application
#


library(shiny); require(dplyr); require(ggplot2); require(scales); 
require(tidyr); require(fmsb); require(ggrepel); require(leaflet); require(maps)
require(geojsonio); require(jsonlite)

## this code only runs once, when app is published ---------------
rawData <- readRDS("data/country_data.rds")
map_data <- geojson_read("data/countries.geojson", what = "sp")

# function that, given a dataset and a home country, calculates CDI 
# and returns a new dataset with CDI appended
# CDI <- function(X, home_country) {
# 	cdi_matrix <- as.matrix(select(X, IDV, IND, LTO, MAS, PDI, UAI))
# 	home_vector <- as.numeric(subset(cdi_matrix, 
# 									 row.names(cdi_matrix) == home_country))
# 	
# 	diffs <- abs(sweep(cdi_matrix, 2, home_vector))
# 	diff_sums <- apply(diffs, 1, sum)
# 	anti_home <- ifelse(home_vector < 50, 100, 0)
# 	anti_diff <- sum(abs(home_vector - anti_home))
# 	CD_norm <- 100*(diff_sums/anti_diff)
# 	
# 	df <- cbind(X, CD_norm)
# 	df
# }

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
	GDP_df <- X %>% select(-CEXPC, -CEXPC_norm) %>%
		rename(y_var_raw = GDPPC, y_var_norm = GDPPC_norm)
}

# function that returns a dataset with CEXPC as the Y variables
CEX <- function(X) {
	GDP_df <- X %>% select(-GDPPC, -GDPPC_norm) %>%
		rename(y_var_raw = CEXPC, y_var_norm = CEXPC_norm)
}


# set up styles for dimensions radar chart
colors_border <- c(alpha("blue", .9),
				   alpha("#ffba38", .9))
colors_in <- c(alpha("blue", .4),
			   alpha("#ffba38", .4))

## code inside this unnamed function runs each session ------------
shinyServer(function(input, output) {
	# define selected countries
	home <- reactive({input$home})
	visited <- reactive({input$visited})

	
	# interactive selection of wealth metric
	Ymethod <- reactive({
		if (input$Ymethod == "GDP") {GDP}
		else CEX
	})
	
	# Function to generate recommendations based on min euclidean distance between
	# new countries and each visited country, within the N-dimensional space created
	# by the user-selected dimensions. Returns back the input dataset with a
	# recommendation score appended.
	recommend <- function(X, visited, home, dims) {
		
		# check that all dims are variables in X, and throw an error if not
		char_dims <- unlist(dims) # coerce dims to vector
		check <- char_dims %in% names(X)
		extra_dims <- cbind(char_dims, check)
		extra_dims <- extra_dims[char_dims == F]
		
		if (!is.null(nrow(extra_dims))) {
			stop(paste("The following dimensions are not present in the dataset:\n",
					   extra_dims[,1]))
		}
		
		current <- c(home, visited) # vector for all visited countries
		
		culture_dims <- c("IDV", "IND", "LTO", "MAS", "PDI", "UAI")
		
		# 1. subset the dataset to include only the user-selected dimensions
		rdata <- X[ ,char_dims]
		
		# remove Countries (rows) that don't have complete data for
		# user-selected dimensions
		rdata2 <- rdata[complete.cases(rdata),]
		
		incompletes <- rdata[!complete.cases(rdata),]$Country
		
		# weight the 6 culture dims collectively equally to each other dim
		if (sum(culture_dims %in% char_dims) == 6) {
			for(i in culture_dims) {
				rdata2[, i] <- rdata2[, i]/sqrt(6)
			}
		}
			
		# 2. generate a distance matrix for all countries
		rdist <- as.data.frame(as.matrix(dist(rdata2)))
		
		# 3. subset the distance matrix for only the rows corresponding to visited
		# and home countries...
		r_df <- rdist %>% filter(row.names(rdist) %in% current)
		# ...and columns corresponding to all other countries
		r_df2 <- r_df[, -which(names(r_df) %in% current)]
		
		# 4. summarize by column according to min - POTENTIALLY CHANGE THIS
		r_means <- summarize_each(r_df2, funs(min))
	
		# 5. return an ordered list of countries and their recommend score
		new <- data.frame(Country = names(r_means), score = unlist(r_means))
		hv <- data.frame(Country = current, score = rep(0, times = length(current)),
						 row.names = current)
		incomplete <- data.frame(Country = incompletes, score = rep(NA,
										times = length(incompletes)))
		rec_score <- rbind(hv, new, incomplete)
		result_df <- left_join(X, rec_score, by = "Country")
	}
	
	# function that, given a dataset and input + recommended country vectors, 
	# returns a vector denoting country types
	my_countries <- function(X, visited, home, recommended) {
		temp <- data.frame("country" = X$Country,
						   "type" = vector(mode = "character", length = nrow(X)))
		ifelse(temp$country %in% home, temp$type <- "home",
			   ifelse(temp$country %in% recommended, temp$type <- "recommended",
			   	   ifelse(temp$country %in% visited, temp$type <- "visited",
			   	   	   temp$type <- "other")))
	}
	
	# render the initial map
	output$my_map <- renderLeaflet({
		leaflet(map("world", fill = T)) %>% addTiles()
	})

	# observer to handle recommendation updates based on inputs
	observe ({
		dims <- list("IDV", "IND", "LTO", "MAS", "PDI", "UAI", "y_var_norm")
		recs <- recommend(CEX(rawData), visited(), home(), dims)
		rec_score <- recs$score

		pal <- colorNumeric(palette = "Blues", domain = rec_score, na.color = "black")

		leafletProxy("my_map", data = map_data) %>%
			clearShapes() %>%
			addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
						opacity = 1.0, fillOpacity = .5,
						fillColor = pal(rec_score),
						stroke = FALSE,
						highlightOptions = highlightOptions(color = "white", weight = 2,
															bringToFront = TRUE))
	})

	
	##TEMPORARY: print the list of countries and recommend scores as a simple table
	# output$recs <- renderTable({
	# 	dims <- list("IDV", "IND", "LTO", "MAS", "PDI", "UAI", "y_var_norm")
	# 	recs <- recommend(CEX(rawData), visited(), home(), dims)
	# 	result <- select(recs, Country, score)
	# })
	
	# tooltip
	output$hover_info <- renderUI({
		hover <- input$plot_hover
		point <- nearPoints(plotdata(), hover, xvar = "CD_norm",
							yvar = "y_var_raw", maxpoints = 1, addDist = T)
		if (nrow(point) == 0) return(NULL)
			# the following code borrowed from Pawel via, https://gitlab.com/snippets/16220
			# calculate point position INSIDE the image as percent of total dimensions
			# from left (horizontal) and from top (vertical)
			left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
			top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
			
			# calculate distance from left and bottom side of the picture in pixels
			left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
			top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
			
			# create style property for tooltip
			# background color is set so tooltip is a bit transparent
			# z-index is set so we are sure are tooltip will be on top
			style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
							"left:", left_px + 2, "px; top:", 
							top_px + 2, "px; padding: 5px;")
			
			wellPanel(
				style = style,
				HTML(paste0("<b>",as.character(point$Country),"</b>"), "<br/>",
					 paste0("<b>",y_tooltip(),": </b>"), dollar_format()(point$y_var_raw), "<br/>",
					 "<b>Culture Diff: </b>", round(point$CD_norm,0), "<br/>"
					 )
			)
	})
	
	# the click
	click <- reactive({input$plot_click})
	
	# data for the clicked country
	point <- reactive({
		nearPoints(plotdata(), click(), xvar = "CD_norm",
				   yvar = "y_var_raw", maxpoints = 1)
	})
	
	# render the cultural dimensions radar chart
	output$click_plot <- renderPlot({
		point_home <- rbind(filter(plotdata(), Country == home()), point())
		
		radar_data <- rbind(rep(100, 6), rep(0, 6), 
							select(point_home, IDV, IND, LTO, MAS, PDI, UAI))
		
		c_names <- c(home(), as.character(point()$Country))

		if (req(!is.null(click))) {
			par(mar = c(0,0,0,0))
			radarchart(radar_data, axistype = 1, pty = 16, 
					   pcol = colors_border, plwd = 2, plty = 1, pfcol = colors_in,
					   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), 
					   cglwd=0.8, vlcex=1)
			
			legend(x=0.4, y=1.2, legend = c_names, bty = "n", pch=20, 
				   col=colors_border , text.col = "black", cex=1, pt.cex=3)
		}
	})
	
	# render the wikipedia URL
	output$click_url <- renderUI ({
		if (req(!is.null(click()))) {
			country_name <- point()$Country
			if (length(country_name) == 0) {
				HTML(" ")
			}
			else {
			country_url <- gsub(" ", "_", country_name)
			p(align = "right", HTML(paste0("Read more about ", country_name, " on <a href= 'https://en.wikipedia.org/wiki/", 
						country_url, "'> Wikipedia </a>")))
			}
		}
		
	})
	
	
})
