#
# User interface definition for "Where Should I Travel Next?" shiny application
#

library(shiny)

dataset <- readRDS("data/country_data.rds")
homeList <- dataset[,1]


# Define UI
shinyUI(navbarPage(
	title = "Where Should I Travel Next?",
	position = "fixed-top",
	tabPanel(
		title = HTML("Recommendations</a></li><li><a href=\"https://medium.com/@spengrah/what-country-should-you-travel-to-next-8fe76063ca4\">Blog Post</a></li><li><a href=\"https://github.com/slgraham/Country-Culture\">Github"),
		fluidRow(
			column(12,
			   tags$style(type="text/css", "body {padding-top: 55px;}"),
			   h5("Based on your home and the countries you've already visited, see your", 
			      span("recommended", style = "color:red"), "travel destinations"),
			   br()
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
					selectInput("Xmethod", label = NULL, 
								choices = list("Euclidean Distance", "Cultural Difference Index"),
								selected = "Euclidean Distance",
								multiple = F, selectize = F),
					
					# 4. single dropdown list to select y-axis variable
					h5("Select a country wealth metric (per capita)**"),
					selectInput("Ymethod", label = NULL, 
								choices = list("Household Consumption (CEX)", "GDP"),
								selected = "Household Consumption (CEX)",
								multiple = F, selectize = F),
					
					hr(),
					helpText("Click on any point to see how that country compares culturally to your home country")
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
	)
))
