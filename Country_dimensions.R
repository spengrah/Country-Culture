require(dplyr); require(ggplot2); require(ggrepel); require(ggthemes)

# read data into R
data_file <- paste(getwd(), "/Country_dimensions.csv", sep = "")
data <- read.csv(file = data_file)

# remove incomplete cases (where some data was NA) and the US
data2 <- data %>% filter(complete.cases(data)) %>%
                   filter(Country != "United States")

# option to plot without Singapore (where I haven't visited)
data3 <- data2 %>% filter(Country !="Singapore")

plotdata <- data3 %>% mutate(visited = ifelse(Country == "Japan", "new", "visited"))

g <- ggplot(plotdata, aes(USdiff_sad_norm, GDPPC)) +
        theme_classic() +
        geom_point(size = 2, aes(color = visited)) +
        scale_color_manual(values = c("new" = "#b20101", "visited" = "#777777")) +
        geom_text_repel(label = plotdata$Country, segment.color = NA,
                        point.padding = unit(1, "pt"), nudge_y = .15) +
        scale_x_continuous(name = "Cultural Difference Index (vs. USA)",
                           breaks = c(0, 10, 20, 30, 40, 50),
                           labels = c("USA", "10", "20", "30", "40", "50")) +
        scale_y_continuous(name = "GDP Per Capita", 
                           breaks = c(0, 20000, 40000, 60000),
                           labels = c("$0", "$20k", "$40k", "$60k"),
                           limits = c(0, 60000)) +
        expand_limits(x = 0, y = 0) +
        labs(title = "Economy and Culture", 
             subtitle = "Among countries I've visited",
             caption = "Based on data from the CIA World Factbook and Geert Hofstede") +
        geom_hline(yintercept = 28000, linetype = "dashed", color = "dark grey",
                   size = .4) +
        geom_vline(xintercept = 27, linetype = "dashed", color = "dark grey",
                   size = .4) +
        theme(text = element_text(family = "sans", size = 12, color = "#3C3C3C"),
              plot.title = element_text(size = 16, face = "bold"),
              plot.caption = element_text(face = "italic", size = 10),
              axis.title = element_text(face = "bold"),
              legend.position = "none")

