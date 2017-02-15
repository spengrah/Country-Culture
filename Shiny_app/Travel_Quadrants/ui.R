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
        
        
        sidebarLayout(position = "right",
              # Sidebar with input widgets
              # currently they are static; eventually allow the lists to update dynamically
              # using renderUI() in server.R and uiOutput() in ui.R
              sidebarPanel(
                      # 1. single dropdown list to select home country (keep static)
                      selectInput("home", label = h5("My home country"), 
                                  choices = homeList,
                                  selected = "United States",
                                  selectize = T),
                      # 2. multiple dropdown list to select countries visited (to make dynamic)
                      selectInput("visited", label = h5("Countries I've visited"), 
                                  choices = homeList, 
                                  multiple = T, selectize = T),
                      # 3. multiple drowpdown list to select countries interested (to make dynamic)
                      selectInput("interested", label = h5("Countries of interest"), 
                                  choices = homeList, 
                                  multiple = T, selectize = T)
              ),
              
              # Main panel withe the scatterplot
              mainPanel(
                      plotOutput(outputId = "plot",
                                 hover = NULL), #eventually update this to enable tooltips
                      
                      br(),
                      
                      em("Based on data from the CIA World Factbook and Geert Hofstede")
              ))
))
