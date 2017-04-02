#
# User interface definition for "Where Should I Travel Next?" shiny application
#

library(shiny); require(leaflet); library(shinythemes)

country_list <- sort(rawData[,1])

dimensions <- c("Culture" = "culture",
				 "Economy" = "economy",
				 "Urbanization" = "urbanization",
				 "Income Inequality" = "gini",
				 "Population Density" = "densityLogNorm")

econ_dimensions <- c("Consumption Expenditure per capita" = "CEXnorm",
					 "GDP per capita" = "GDPnorm")

# Define UI
shinyUI(navbarPage(
	theme = shinytheme("simplex"),
	## to add to head---------------
	# # facebook opengraph properties
	# HTML("<meta property=\"og:title\" content=\"Where Should I Travel Next?\" />
	# 	<meta property=\"og:description\" content=\"Diversify your travel destinations\" />"),
	# 
	# # facebook share button javascript SDK
	# HTML("<div id=\"fb-root\"></div>
	# 	<script>(function(d, s, id) {
	# 		var js, fjs = d.getElementsByTagName(s)[0];
	# 		if (d.getElementById(id)) return;
	# 		js = d.createElement(s); js.id = id;
	# 		js.src = \"//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.8\";
	# 		fjs.parentNode.insertBefore(js, fjs);
	# 	}(document, 'script', 'facebook-jssdk'));</script>"),
	# 
	# # google analytics script
	# HTML("<script>
	# 	 	(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
	# 	 		(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
	# 	 		m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
	# 	 	})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
	# 	 
	# 	 ga('create', 'UA-92510529-1', 'auto');
	# 	 ga('send', 'pageview');
	# 	 
	# 	 </script>"),
	# 
	# # Facebook share button
	# HTML("<div class=\"fb-share-button\" 
	# 	 data-href=\"https://spengrah.shinyapps.io/travel_destination_diversity/\" 
	# 	 data-layout=\"button_count\" 
	# 	 data-mobile-iframe=\"true\"><a class=\"fb-xfbml-parse-ignore\" 
	# 	 target=\"_blank\" href=\"https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fspengrah.shinyapps.io%2Ftravel_destination_diversity%2F&amp;src=sdkpreparse\">Share</a></div>"),
	# 
	# # twitter anchor element
	# HTML("<a class=\"twitter-share-button\"
	# 	 href=\"https://twitter.com/intent/tweet?text=Where%20Should%20You%20Travel%20Next&url=https://spengrah.shinyapps.io/travel_destination_diversity/&via=spengrah\"
	# 	 data-size=\"large\">
	# 	 Tweet</a>"),
	
	
	title = "Where Should I Travel Next?",
	position = "fixed-top",
	id = "nav",
	tabPanel(
		## CSS--------
		tags$head(
			tags$style(HTML("
			body {
				padding-top: 55px;
			}
			div.outer {
				position: fixed;
				top: 41px;
				left: 0;
				right: 0;
				bottom: 0;
				overflow: hidden;
				padding: 0
			}
			#controls {
				background-color: white;
				padding: 5px 15px 15px 15px;
				cursor: move;
				opacity: 0.70;
				zoom: 0.9;
				transition: opacity 1500ms 400ms;
			}
			#controls:hover {
				opacity: 0.95;
				transition-delay: 0;
			}
			.leaflet-popup-content-wrapper {
				background-color: white;
				opacity: 0.85;
			}
		"))
		),
		
		## continue shiny code-----
		title = "Recommendations",
		fluidRow(
			div(
				class = "outer",
				# leaflet map goes here
				leafletOutput(outputId = "map", width = "100%", height = "100%"),
				# Input controls
				absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
							  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
							  width = 300, height = "90%",

							  # 1. single dropdown list to select home country (keep static)
							  h4("Countries"),
							  h5("Where do you call", span("home", style = "color:blue"), "?"),
							  selectInput("home", label = NULL,
							  			choices = country_list,
							  			selected = "United States of America",
							  			selectize = T),
							  
							  # 2. multiple dropdown list to select countries visited
							  h5("Where have you", span("visited", style = "color:#25a31a"), "?"),
							  selectInput("visited", label = NULL,
							  			choices = country_list, 
							  			multiple = T, selectize = T),
							  br(),
							  
							  h4("Dimensions"),
							  # 3. checkboxes for dimensions
							  h5("How do you want to diversify your travel?"),
							  checkboxGroupInput("dimensions",
							  				   label = NULL,
							  				   choices = dimensions,
							  				   selected = NULL
							  ),
							  
							  # 4. condition panel to choose economy dimension
							  conditionalPanel(
							  	condition = "input.dimensions.includes('economy')",
							  	h5("Pick an economic dimension"),
							  	radioButtons("economy_choice", label = NULL,
							  				choices = econ_dimensions,
							  				selected = econ_dimensions[1])
							  )
				)
			)
		)

	),
	
	tabPanel(
		title = HTML("More Info</a></li><li><a href=\"https://medium.com/@spengrah/what-country-should-you-travel-to-next-8fe76063ca4\">Blog Post</a></li><li><a href=\"https://github.com/slgraham/Country-Culture\">Github"),
		fluidRow(
			column(6,
				   tags$style(type="text/css", "body {padding-top: 55px;}"),
				   h4("What is this?"),
				   p("This is a tool for people who want to pick travel destinations that expose them",
				     "to a wide range of experiences."),
				   p("One way to do that is to make your next trip to the country that is least like any of the countries you've already been to.",
					 "That's what this tool helps you do. Just pick your home country and where you've traveled",
				     "from the dropdowns on the right of the map, Then choose the things that you care about and",
				     "watch the recommendations roll in."),
				   p("The darker the red, the stronger the recommendation.",
				     "The top 3 recommendations will always be highlighted by an orange border.",
				     "Click on any country to see it's underlying data.",
				     "Try changing up your dimension selections to see how recommendations change."),
				   br(),
				   h4("How does it work?"),
				   p("Simply put, recommendations are generated by finding the countries that",
				     "are most \"distant\" from where you've already been."),
				   br(),
				   p("The recommendation engine finds the Euclidean distance between all countries within the dimensional",
				     "space defined by the dimensions you choose. It then looks for the countries farthest \"away\"",
					 "from the nearest country that you've already visited."),
				   br()
					
				
				   # TEMPORARY: recommendation list
				   # ,tableOutput(outputId = "recs")
			),
			column(6,
				   tags$style(type="text/css", "body {padding-top: 55px;}"),
				   h4("Where data does it use?"),
				   p("This tool originated as a way for me to rationalize taking a trip to Japan.",
				     "I had traveled to a number of rich Western countries, and a number of less wealth non-Western",
                     "countries. I had a hypothesis that Japan, a country that falls into neither of those categories,",
				     "would represent a completely new travel experience."), 
				   p("So I gathered some economic and cultural data (more on that below)",
					 "and made a simple 2-dimensional chart. It turned out I was right...sort of. I found that a few",
					 "other countries would fit the bill even better. Intruigued, I decided to add additional dimensions",
					 "and turn the result into an interactive map."),
				   br(),
				   p("Clean data about culture is, as you might expect, pretty rare. \"Culture\" is as blurry as it",
				     "is important; measuring it is not easy. However, I was able to find some interesting work by",
				     a(href = "https://geert-hofstede.com/national-culture.html", "Geert Hofstede"),
				     "who used survey research to develop 6 dimensions of culture across roughly 80 countries:"),
				 h5("Hofstede's Cultural Dimensions"),
				 p(HTML(paste0(tags$span(style = "font-weight:bold", "IDV"),": Individualism vs. Collectivism"))),
				 p(HTML(paste0(tags$span(style = "font-weight:bold", "UAI"),": Uncertainty Avoidance Index"))),
				 p(HTML(paste0(tags$span(style = "font-weight:bold", "PDI"),": Power Distance Index"))),
				 p(HTML(paste0(tags$span(style = "font-weight:bold", "MAS"),": Masculinity vs. Femininity"))),
				 p(HTML(paste0(tags$span(style = "font-weight:bold", "LTO"),": Long-term Orientation"))),
				 p(HTML(paste0(tags$span(style = "font-weight:bold", "IND"),": Indulgence vs. Restraint"))),
				 br(),
					
				   p("Economic and demographic data are from Wikipedia country lists and the CIA World Factbook.")
				 
				   # p("GDP per capita is from the", 
				   #   HTML(paste0(tags$a(href = "https://www.cia.gov/library/Publications/the-world-factbook/rankorder/2004rank.html",
				   #   				   "CIA World Factbook"), "."))),
				   
				   # p("Household Consumption Expenditure (CEX) per capita is from a few sources:"),
				   # tags$ul(tags$li(a(href = "https://en.wikipedia.org/wiki/List_of_countries_by_household_final_consumption_expenditure_per_capita",
				   #     "This eponymous Wikipedia list")),
				   # 		tags$li(a(href = "http://www.tradingeconomics.com/cape-verde/final-consumption-expenditure-etc-percent-of-gdp-wb-data.html",
				   # 			 "TradingEconomics.com")),
				   # 		tags$li(a(href = "http://ivanstat.com/final_consumption_expenditure/iq.html",
				   # 			 "ivanstat.com")),
				   # 		tags$li(a(href = "https://tds.gd/index/q6MMF7HjS4KYUArXSMWZzA/",
				   # 				  "TGS.dg")),
				   # 		tags$li(a(href = "https://eng.stat.gov.tw/ct.asp?xItem=3417&CtNode=1596&mp=5",
				   # 				  "Taiwan National Statistics"))
				   # 		),
				   # br(),
				   
			)
		)
	)
	
))
