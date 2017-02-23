#
# User interface definition for "Where Should I Travel Next?" shiny application
#

library(shiny)

dataset <- readRDS("data/country_data.rds")
homeList <- dataset[,1]


# Define UI
shinyUI(navbarPage(
	# facebook opengraph properties
	HTML("<meta property=\"og:title\" content=\"Where Should I Travel Next?\" />
		<meta property=\"og:description\" content=\"Diversify your travel destinations\" />"),
	
	# facebook share button javascript SDK
	HTML("<div id=\"fb-root\"></div>
		<script>(function(d, s, id) {
			var js, fjs = d.getElementsByTagName(s)[0];
			if (d.getElementById(id)) return;
			js = d.createElement(s); js.id = id;
			js.src = \"//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.8\";
			fjs.parentNode.insertBefore(js, fjs);
		}(document, 'script', 'facebook-jssdk'));</script>"),
	
	# google analytics script
	HTML("<script>
		 	(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
		 		(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
		 		m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
		 	})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
		 
		 ga('create', 'UA-92510529-1', 'auto');
		 ga('send', 'pageview');
		 
		 </script>"),

	title = "Where Should I Travel Next?",
	position = "fixed-top",
	tabPanel(
		title = HTML("Recommendations"),
		fluidRow(
			column(11,
			   tags$style(type="text/css", "body {padding-top: 55px;}"),
			   h5("Based on your home and the countries you've already visited, see your", 
			      span("recommended", style = "color:red"), "travel destinations"),
			   br()
			   
			),
			
			column(1,
				   tags$style(type="text/css", "body {padding-top: 55px;}"),
				   
				   # Facebook share button
				   HTML("<div class=\"fb-share-button\" 
			   	 	data-href=\"https://spengrah.shinyapps.io/travel_destination_diversity/\" 
			   	 	data-layout=\"button_count\" 
			   	 	data-mobile-iframe=\"true\"><a class=\"fb-xfbml-parse-ignore\" 
			   	 	target=\"_blank\" href=\"https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fspengrah.shinyapps.io%2Ftravel_destination_diversity%2F&amp;src=sdkpreparse\">Share</a></div>"),
				   
				   # twitter anchor element
				   HTML("<a class=\"twitter-share-button\"
				   	 href=\"https://twitter.com/intent/tweet?text=Where%20Should%20You%20Travel%20Next&url=https://spengrah.shinyapps.io/travel_destination_diversity/&via=spengrah\"
				   	 data-size=\"large\">
				   	 Tweet</a>")
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
			   h4("Hoftede's Cultural Dimensions"),
			   p(HTML(paste0(tags$span(style = "font-weight:bold", "IDV"),": Individualism vs. Collectivism"))),
			   p(HTML(paste0(tags$span(style = "font-weight:bold", "UAI"),": Uncertainty Avoidance Index"))),
			   p(HTML(paste0(tags$span(style = "font-weight:bold", "PDI"),": Power Distance Index"))),
			   p(HTML(paste0(tags$span(style = "font-weight:bold", "MAS"),": Masculinity vs. Femininity"))),
			   p(HTML(paste0(tags$span(style = "font-weight:bold", "LTO"),": Long-term Orientation"))),
			   p(HTML(paste0(tags$span(style = "font-weight:bold", "IND"),": Indulgence vs. Restraint"))),
			   br(),
			   
			   helpText("See the More Info tab for...more info")

			),
			
			column(8,
				   hr(),
				   helpText("Click on any point above to see how that country compares to your home country's culture"),
				   # clicked country cultural dimensions bar plot
				   plotOutput("click_plot"),
				   br(),
				   
				   # clicked country URL
				   htmlOutput("click_url")
			)
	
		)
	),
	
	tabPanel(
		title = HTML("More Info</a></li><li><a href=\"https://medium.com/@spengrah/what-country-should-you-travel-to-next-8fe76063ca4\">Blog Post</a></li><li><a href=\"https://github.com/slgraham/Country-Culture\">Github"),
		fluidRow(
			column(6,
				   tags$style(type="text/css", "body {padding-top: 55px;}"),
				   h4("How does this work?"),
				   p("Travel recommendations are generated by finding the countries",
					 "that are least similar to your home and the countries you've already visited.",
					 "The recommendation engine first calculates the distance between all the countries",
					 "you've been to and all the countries you haven't yet been to, and then picks",
					 "the 3 countries with the largest minimum distance from one of the countries you've been to."),
				   br(),
					 p("The top chart plots a measure of economic lifestyle (y-axis) against cultural difference from",
					   "your home country (x-axis)."),
					br(),
					p("You can use a simplistic sum of differences index for the cultural",
					   "difference measure, or you can use a slightly more sophisticated Euclidean distance approach.",
					   "The distance method allows for more ways for culture to differ, so countries will be spread",
					   "out more along the x-axis."),
					br(),
					p("You also have two options for the economic lifestyle measure. You can either use GDP per capita,",
					  "which is the most common measure of a country's wealth; or you can use Household Consumption",
					  "Expenditure per capita, which in my assessment better captures what visiting a country would",
					  "be like in economic terms"),
					br(),
					
					h5("Cultural Difference Metrics"),
				   
				   p(HTML(paste0(tags$span(style = "font-weight:bold", "CDI"),
				   			  ": sum of differences between two countries on the 6 dimensions (normalized)"))),
				   p(HTML(paste0(tags$span(style = "font-weight:bold", "Euclidean distance"),
				   			  ": the 6-dimensional 'hypotenuse' between two countries (normalized)")))
			),
			column(6,
				   tags$style(type="text/css", "body {padding-top: 55px;}"),
				   h4("Where is the data from?"),
				   p("GDP per capita is from the", 
				     HTML(paste0(tags$a(href = "https://www.cia.gov/library/Publications/the-world-factbook/rankorder/2004rank.html",
				     				   "CIA World Factbook"), "."))),
				   
				   p("Household Consumption Expenditure (CEX) per capita is from a few sources:"),
				   tags$ul(tags$li(a(href = "https://en.wikipedia.org/wiki/List_of_countries_by_household_final_consumption_expenditure_per_capita",
				       "This eponymous Wikipedia list")),
				   		tags$li(a(href = "http://www.tradingeconomics.com/cape-verde/final-consumption-expenditure-etc-percent-of-gdp-wb-data.html",
				   			 "TradingEconomics.com")),
				   		tags$li(a(href = "http://ivanstat.com/final_consumption_expenditure/iq.html",
				   			 "ivanstat.com")),
				   		tags$li(a(href = "https://tds.gd/index/q6MMF7HjS4KYUArXSMWZzA/",
				   				  "TGS.dg")),
				   		tags$li(a(href = "https://eng.stat.gov.tw/ct.asp?xItem=3417&CtNode=1596&mp=5",
				   				  "Taiwan National Statistics"))
				   		),
				   br(),
				   
				   p("Cultural Dimensions data is from", 
				     a(href = "https://geert-hofstede.com/national-culture.html",
				     	   "Geert Hoftede's research"),
				     "on cross-country cultural differences.",
				     "Based on years of research, he has developed 6 dimensions that describe a country's culture.",
				     "The Recommendations tab has simple definitions for each, and you can read much more about",
				     "the dimensions and Hofstede's research on his website."
				   )
			)
		)
	)
	
))
