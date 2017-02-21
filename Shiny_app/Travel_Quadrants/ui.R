#
# User interface definition for "Where Should I Travel Next?" shiny application
#

library(shiny)

dataset <- readRDS("data/country_data.rds")
homeList <- dataset[,1]


# Define UI
shinyUI(fluidPage(
	fluidRow(
		column(12,
		   # Application title
		   titlePanel("Where Should I Travel Next?"),
		   br(),
		   
		   # Instructions
		   p("Select your home country and the countries you've already visited
		     to see your", span("travel recommendations.", style = "color:red")),
		   p("Click on any point to see how that country compares culturally to your home country.")
		)
	),
	
	fluidRow(
		column(4,
			# Sidebar with input widgets
			wellPanel(
				# 1. single dropdown list to select home country (keep static)
				h5("Select your ", span("home", style = "color:blue"), "country"),
				selectInput("home", label = NULL,
							choices = homeList,
							selected = "United States",
							selectize = T),
				# 2. multiple dropdown list to select countries visited
				h5("Select the countries you've", span("visited", style = "color:#25a31a")),
				selectInput("visited", label = NULL, 
							choices = homeList, 
							multiple = T, selectize = T),
				
				# 3. single dropdown list to select cultural distance method
				h5("Select a cultural difference algorithm*"),
				selectInput("method", label = NULL, 
							choices = list("Euclidean Distance", "Cultural Difference Index"),
							selected = "Euclidean Distance",
							multiple = F, selectize = F)
				)
			),
			
			column(8,
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
				   	
				   )
			)
		),
	
	fluidRow(
		column(4,
		   h4("What is this data?"),
		   p("GDP per capita is from the", 
		     HTML(paste0(tags$a(href = "https://www.cia.gov/library/Publications/the-world-factbook/rankorder/2004rank.html",
		   			"CIA World Factbook"), "."))),
		   p("Cultural Dimensions data is from", 
		     tags$a(href = "https://geert-hofstede.com/national-culture.html",
		     	   "Geert Hoftede's research"),
		   "on cross-country cultural differences.",
		   "I encourage you to read more about his research, but here is a quick overview",
		   "of his 6 cultural dimensions:"),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "IDV"),": Individualism vs. Collectivism"))),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "UAI"),": Uncertainty Avoidance Index"))),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "PDI"),": Power Distance Index"))),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "MAS"),": Masculinity vs. Femininity"))),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "LTO"),": Long-term Orientation"))),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "IND"),": Indulgence vs. Restraint"))),
		   br(),
		   
		   h4("*What are these algorithms?"),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "CDI"),
		   			  ": sum of differences between two countries on the 6 dimensions (normalized)"))),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "Euclidean distance"),
		   			  ": the 6-dimensional 'hypotenuse' between two countries (normalized)"))),
		   p(HTML(paste0(tags$span(style = "font-weight:bold", "Recommended"),
		   			  " countries are the 3 countries with the largest minimum Euclidean distance from your current visited and home countries")))
		),
		
		column(8,
			   hr(),
			   br(),
			   # clicked country cultural dimensions bar plot
			   plotOutput("click_plot"),
			   br(),
			   
			   # clicked country URL
			   htmlOutput("click_url")
		)

	)
))
