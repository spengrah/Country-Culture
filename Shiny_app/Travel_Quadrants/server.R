#
# Server logic for "Where Should I Travel Next?" shiny application
#


library(shiny); require(dplyr); require(ggplot2)

## this code only runs once, when app is published ---------------
rawData <- readRDS("data/country_data.rds")

# function that, given a dataset and a home country, calculates CDI 
# and returns a new dataset with CDI appended, plus the reference country
CDI <- function(X, home_country) {
        cdi_matrix <- as.matrix(select(X, IDV, IND, LTO, MAS, PDI, UAI))
        home_vector <- as.numeric(subset(cdi_matrix, 
                                        row.names(cdi_matrix) == home_country))
        
        diffs <- abs(sweep(cdi_matrix, 2, home_vector))
        diff_sums <- apply(diffs, 1, sum)
        anti_home <- ifelse(home_vector < 50, 100, 0)
        anti_diff <- sum(abs(home_vector - anti_home))
        CDI_norm <- 100*(diff_sums/anti_diff)
        
        df <- cbind(X, CDI_norm)
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

## code inside this unnamed function runs each session ------------
shinyServer(function(input, output) {
        # process selected countries
        home <- reactive({input$home})
        visited <- reactive({input$visited})
        interested <- reactive({input$interested})
        
        # interactive calculation of CDI and production of new dataset
        dataCDI <- reactive({CDI(rawData,home())})
        
        # interactive addition of country type variable to dataset
        plotdata <- reactive({
                mutate(dataCDI(), my_countries = 
                                   as.factor(my_countries(dataCDI(), visited(), interested(), home())))
        })

## code from here on down runs each time inputs are updated--------
        # eventually renderUI() calls will go here
        # out
        
        # render the scatterplot
        output$plot <- renderPlot({
                ggplot(plotdata(), aes(x = CDI_norm, y = GDPPC, alpha = my_countries,
                                     color = my_countries)) +
                theme_classic() +
                geom_point(size = 3) +
                scale_color_manual(values = c("visited" = "green", 
                                              "interested" = "red",
                                              "other" = "black",
                                              "home" = "blue"),
                                   name = "My Countries") +
                scale_alpha_manual(values = c("visited" = 1, "interested" = 1,
                                              "other" = .4, "home" = 1),
                                   name = "My Countries") +
                scale_x_continuous(name = paste("Cultural Difference Index (vs. ", 
                                                home(), ")", sep = ""),
                                   breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
                                   labels = c(home(), "10", "20", "30", "40", "50", "60", "70")) +
                scale_y_continuous(name = "GDP Per Capita",
                                   breaks = c(0, 25000, 50000, 75000, 100000),
                                   labels = c("$0", "$25k", "$50k", "$75k", "$100k"),
                                   limits = c(0, 105000)) +
                expand_limits(x = 0, y = 0) +
                theme(text = element_text(family = "sans", size = 12, color = "#3C3C3C"),
                      plot.title = element_text(size = 16, face = "bold"),
                      plot.caption = element_text(face = "italic", size = 10),
                      axis.title = element_text(face = "bold"),
                      legend.title = element_text(face = "bold"),
                      legend.justification = "bottom",
                      legend.background = element_rect(fill = "#ededed"))
        })
        

})
