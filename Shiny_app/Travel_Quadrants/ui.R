#
# User interface definition for "Where Should I Travel Next?" shiny application
#

library(shiny)

dataset <- readRDS("data/country_data.rds")
homeList <- dataset[,1]

# Define UI
shinyUI(fluidPage(
	# Application title
	titlePanel("Where Should I Travel Next?"),
	h5("Maximize the cultural and economic diversity of your travel destinations"),
	br(),
	p("Instructions: select your home country, the countries you've already traveled to, 
          and the countries you're interested in traveling to"),
	br(),
	
	sidebarLayout(
		position = "left",
		# Sidebar with input widgets
		# currently they are static; eventually allow the lists to update dynamically
		# using renderUI() in server.R and uiOutput() in ui.R
		sidebarPanel(
			# 1. single dropdown list to select home country (keep static)
			h5("Select your ", span("home", style = "color:blue"), "country"),
			selectInput("home", label = NULL,
						choices = homeList,
						selected = "United States",
						selectize = T),
			# 2. multiple dropdown list to select countries visited (to make dynamic)
			br(),
			h5("Select the countries you've", span("visited", style = "color:green")),
			selectInput("visited", label = NULL, 
						choices = homeList, 
						multiple = T, selectize = T),
			# 3. multiple drowpdown list to select countries interested (to make dynamic)
			br(),
			h5("Select your countries of", span("interest", style = "color:red")),
			selectInput("interested", label = NULL, 
						choices = homeList, 
						multiple = T, selectize = T)
		),
		
		# Main panel with the scatterplot and the hover
		mainPanel(
			# div to act as the ancestral reference point for the hover code
			div(
				style = "position.relative",
				# scatterplot
				plotOutput(outputId = "plot",
						   click = clickOpts(id = "plot_click"),
						   hover = hoverOpts(id ="plot_hover",
						   				  delay = 100,
						   				  delayType = "debounce")),
				# hover tooltip
				uiOutput("hover_info")
				
			),
			br(),
			
			# clicked country cultural dimensions bar plot
			# WHERE SHOULD THIS BE PLACED?
			plotOutput("click_plot"),
			
			br(),
			
			# clicked country URL
			# WHERE SHOULD THIS BE PLACED?
			htmlOutput("click_url"),
			
			br(),
			
			# WHERE SHOULD THIS BE PLACED?
			em("Based on data from the CIA World Factbook and Geert Hofstede")
			
			# add some more text about cultural dimensions
			# and links to the GDP data and Geert Hofstede's website
			
		))
))
