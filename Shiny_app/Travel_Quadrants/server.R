#
# Server logic for "Where Should I Travel Next?" shiny application
#


library(shiny); require(dplyr); require(ggplot2); require(scales); require(reshape2)

## this code only runs once, when app is published ---------------
rawData <- readRDS("data/country_data.rds")

# function that, given a dataset and a home country, calculates CDI 
# and returns a new dataset with CDI appended
CDI <- function(X, home_country) {
	cdi_matrix <- as.matrix(select(X, IDV, IND, LTO, MAS, PDI, UAI))
	home_vector <- as.numeric(subset(cdi_matrix, 
									 row.names(cdi_matrix) == home_country))
	
	diffs <- abs(sweep(cdi_matrix, 2, home_vector))
	diff_sums <- apply(diffs, 1, sum)
	anti_home <- ifelse(home_vector < 50, 100, 0)
	anti_diff <- sum(abs(home_vector - anti_home))
	CD_norm <- 100*(diff_sums/anti_diff)
	
	df <- cbind(X, CD_norm)
	df
}

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



# function that, given a dataset and input country vectors, returns a vector 
# denoting country types
my_countries <- function(X, visited, interested, home) {
	temp <- data.frame("country" = X$Country, 
					   "type" = vector(mode = "character", length = nrow(X)))
	ifelse(temp$country %in% visited, temp$type <- "visited",
		   ifelse(temp$country %in% interested, temp$type <- "interested",
		   	   ifelse(temp$country %in% home, temp$type <- "home",
		   	   	   temp$type <- "other")))
}

cult_measures <- c("IDV", "IND", "LTO", "MAS", "PDI", "UAI")

## code inside this unnamed function runs each session ------------
shinyServer(function(input, output) {
	# process selected countries
	home <- reactive({input$home})
	visited <- reactive({input$visited})
	interested <- reactive({input$interested})
	
	# interactive selection of cultural difference method
	CDmethod <- reactive({
		if (input$method == "Cultural Difference Index") {CDI}
		else edist
	})
	
	# interactive calculation of CD and production of new dataset
	dataCD <- reactive({CDmethod()(rawData,home())})
	
	# interactive addition of country type variable to dataset
	plotdata <- reactive({
		mutate(dataCD(), my_countries = 
			   	as.factor(my_countries(dataCD(), visited(), interested(), home())))
	})
	
	## code from here on down runs each time inputs are updated--------
	# eventually renderUI() calls will go here
	# out
	
	# render the scatterplot
	method_label <- reactive({
		if(input$method == "Cultural Difference Index") "Index"
		else "Euclidean Distance"
	})
	
	x_axis <- reactive({
		if(input$method == "Cultural Difference Index") {
			list(breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
			labels = c(home(), "10", "20", "30", "40", "50", "60", "70"))
		}
		else {
			list(breaks = c(0, 20, 40, 60, 80, 100),
			labels = c(home(), "20", "40", "60", "80", "100"))
		}
	})
	
	x_label <- reactive ({
		paste0("Cultural Difference vs. ", home(), " (", method_label(), ")")
	})
	
	output$plot <- renderPlot({
		ggplot(plotdata(), aes(x = CD_norm, y = GDPPC, alpha = my_countries,
							   color = my_countries)) +
			theme_classic() +
			geom_point(size = 5) +
			scale_color_manual(values = c("visited" = "green", 
										  "interested" = "red",
										  "other" = "black",
										  "home" = "blue")) +
			# figure out how to layer the colors over the grey
			scale_alpha_manual(values = c("visited" = 1, "interested" = 1,
										  "other" = .3, "home" = 1),
							   name = "My Countries") +
			scale_x_continuous(x_label(),
							   breaks = c(0, 25, 50, 75),
							   labels = c(home(), "25", "50", "75"),
							   limits = c(0, 75)) +
			scale_y_continuous(name = "GDP Per Capita",
							   breaks = c(0, 25000, 50000, 75000, 100000),
							   labels = c("$0", "$25k", "$50k", "$75k", "$100k"),
							   limits = c(0, 105000)) +
			# expand_limits(x = 0, y = 0) +
			theme(text = element_text(family = "sans", size = 16, color = "#3C3C3C"),
				  plot.title = element_text(size = 16, face = "bold"),
				  plot.caption = element_text(face = "italic", size = 10),
				  axis.title = element_text(face = "bold")) +
			guides(color = "none", alpha = "none")
	})
	
	# render the hover tooltip
	output$hover_info <- renderUI({
		hover <- input$plot_hover
		point <- nearPoints(plotdata(), hover, xvar = "CD_norm",
							yvar = "GDPPC", maxpoints = 1, addDist = T)
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
				HTML(paste("<b>",as.character(point$Country),"</b>"), "<br/>",
					 "<b>GDP: </b>", dollar_format()(point$GDPPC), "<br/>",
					 "<b>Culture Diff: </b>", round(point$CD_norm,0), "<br/>"
					 )
			)
	})
	
	# render the cultural dimensions barplot
	# HOW BIG SHOULD THIS CHART BE?
	output$click_plot <- renderPlot({
		click <- input$plot_click
		point <- nearPoints(plotdata(), click, xvar = "CD_norm",
							yvar = "GDPPC", maxpoints = 1)
		
		point_plus_home <- rbind(point, filter(plotdata(), Country == home()))
		bardata <- melt(point_plus_home, id.vars = "Country", 
						measure.vars = cult_measures, value.name = "score",
						variable.name = "dim")

		if (req(!is.null(click))) {
			ggplot(bardata, aes(x = dim, y = score, fill = Country)) +
				theme_minimal() +
				geom_bar(stat = "identity", position = position_dodge()) +
				scale_fill_manual(values = c("grey", "blue")) +
				ylim(0, 100) +
				xlab("Hoftede's Cultural Dimensions") +
				ylab(NULL)
			# revise labels
				
		}
	})
	
	# render the wikipedia URL
	output$click_url <- renderUI ({
		click <- input$plot_click
		point <- nearPoints(plotdata(), click, xvar = "CD_norm",
							yvar = "GDPPC", maxpoints = 1)
		
		if (req(!is.null(click))) {
			country <- gsub(" ", "_", point$Country)
			HTML(paste0("Read more about ", point$Country, " on <a href= 'https://en.wikipedia.org/wiki/", country, "'> Wikipedia </a>"))
		}
		
	})
	
	
})
