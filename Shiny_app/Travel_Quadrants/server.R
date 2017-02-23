#
# Server logic for "Where Should I Travel Next?" shiny application
#


library(shiny); require(dplyr); require(ggplot2); require(scales); 
require(reshape2); require(fmsb); require(ggrepel)

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
	# process selected countries
	home <- reactive({input$home})
	visited <- reactive({input$visited})

	
	# interactive selection of cultural difference method
	Xmethod <- reactive({
		if (input$Xmethod == "Cultural Difference Index") {CDI}
		else edist
	})
	
	# interactive selection of wealth metric
	Ymethod <- reactive({
		if (input$Ymethod == "GDP") {GDP}
		else CEX
	})
	
	# function that, given a dataset with a cultural diff variable and input
	# country vectors, returns a vector of recommended travel destinations determined
	# by the largest Euclidean distance from home and all visited countries.
	recommend <- function(X, visited, home) {
		
		# 1. subset the dataset to include only the culture dims and the wealth norm metric
		# 
		rdata <- X %>% select(IDV, IND, LTO, MAS, PDI, UAI, y_var_norm) %>% 
					   mutate(y_var_norm = sqrt(6)*y_var_norm) # putting wealth metric on equal footing
		rownames(rdata) <- X$Country
			
		# 2. generate a distance matrix for all countries
		rdist <- as.data.frame(as.matrix(dist(rdata)))

		current <- c(home, visited)
		
		# 3. subset the distance matrix for only the rows corresponding to visited
		# 	 and home countries and columns corresponding to all other countries
		r_df <- rdist %>% filter(row.names(rdist) %in% current)
		r_df2 <- r_df[, -which(names(r_df) %in% current)]
		
		# 4. summarize (mean, min, median, other?) by column.
		r_means <- summarize_each(r_df2, funs(min)) # POTENTIALLY add UI control????
		
		# 5. The 3 columns with the largest resulting values are the recommended countries
		recs <- names(head(sort(unlist(r_means), decreasing = T),3))
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
	
	# interactive calculation of CD and production of new dataset
	dataCD <- reactive({Xmethod()(rawData,home())})
	
	# interactive selection of Y variable
	dataY <- reactive({Ymethod()(dataCD())})
	
	recommended <- reactive({
		recommend(dataY(), visited(), home())
	})
	
	point_styles <- reactive ({
		my_countries(dataY(), visited(), home(), recommended())
	})
	
	# interactive addition of country type variable to dataset
	plotdata <- reactive({
		mutate(dataY(), my_countries = as.factor(point_styles()))
	})
	
	# render the scatterplot
	Xmethod_label <- reactive({
		if(input$Xmethod == "Cultural Difference Index") "Index"
		else "Euclidean Distance"
	})
	
	y_axis <- reactive({
		if(input$Ymethod == "GDP") {
			list(name = y_label(),
				 breaks = c(0, 25000, 50000, 75000, 100000),
				 labels = c("$0", "$25k", "$50k", "$75k", "$100k"),
				 limits = c(0, 105000))
		}
		else {
			list(name = y_label(),
				 breaks = c(0, 10000, 20000, 30000, 40000),
				 labels = c("$0", "$10k", "$20k", "$30k", "$40k"),
				 limits = c(0, 40000))
		}
	})
	
	x_label <- reactive ({
		paste0("Cultural Difference from ", home(), " (", Xmethod_label(), ")")
	})
	
	y_label <- reactive({
		if(input$Ymethod == "GDP") "GDP per capita"
		else "HH Consumption Expenditure per capita"
	})
	
	
	point_labels <- reactive({
			a <- as.character(plotdata()$Country)
			replace(a, !(a %in% recommended()), "")
	})
	
	output$plot <- renderPlot({
		g <- ggplot(plotdata(), aes(x = CD_norm, y = y_var_raw, alpha = my_countries,
							   color = my_countries, size = my_countries)) +
			theme_classic() +
			geom_point() +
			scale_color_manual(values = c("visited" = "#25a31a", 
										  "other" = "black",
										  "home" = "blue",
										  "recommended" = "red")) +
			# figure out how to layer the colors over the grey
			scale_alpha_manual(values = c("visited" = 1,
										  "other" = .3, "home" = 1,
										  "recommended" = 1)) +
			scale_size_manual(values = c("visited" = 5, 
										 "other" = 5,
										 "home" = 5,
										 "recommended" = 6)) +
			scale_x_continuous(x_label(),
							   breaks = c(0, 25, 50, 75),
							   labels = c(home(), "25", "50", "75"),
							   limits = c(0, 75)) +
			do.call(scale_y_continuous, y_axis()) +
			theme(text = element_text(family = "sans", size = 16, color = "#3C3C3C"),
				  plot.title = element_text(size = 16, face = "bold"),
				  plot.caption = element_text(face = "italic", size = 10),
				  axis.title = element_text(face = "bold"),
				  plot.margin = unit(c(0, 1.7, 0, 0), "cm")) +
			guides(color = "none", alpha = "none", size = "none")
		g
		
		if (req(!is.null(point_labels))) {
			g + geom_text_repel(label = point_labels(), size = 4.5, color = "#727272",
								segment.color = NA, point.padding = unit(4, "pt"), nudge_y = .15)
		}
	})
	
	# reactive y_var label for the tooltip
	y_tooltip <- reactive({
		if(input$Ymethod == "GDP") "GDP"
		else "CEX"
	})
	
	# render the hover tooltip
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
	# HOW BIG SHOULD THIS CHART BE?
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
