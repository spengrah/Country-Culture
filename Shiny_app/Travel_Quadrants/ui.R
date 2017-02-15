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
                      h5("My ", span("home", style = "color:blue"), "country"),
                      selectInput("home", label = NULL,
                                  choices = homeList,
                                  selected = "United States",
                                  selectize = T),
                      # 2. multiple dropdown list to select countries visited (to make dynamic)
                      br(),
                      h5("Countries I've ", span("visited", style = "color:green")),
                      selectInput("visited", label = NULL, 
                                  choices = homeList, 
                                  multiple = T, selectize = T),
                      # 3. multiple drowpdown list to select countries interested (to make dynamic)
                      br(),
                      h5("Countries of ", span("interest", style = "color:red")),
                      selectInput("interested", label = NULL, 
                                  choices = homeList, 
                                  multiple = T, selectize = T)
              ),
              
              # Main panel with the scatterplot and some text
              mainPanel(
                      plotOutput(outputId = "plot",
                                 hover = "plot_hover"),
                      
                      br(),
                      
                      em("Based on data from the CIA World Factbook and Geert Hofstede")
              ))
))
