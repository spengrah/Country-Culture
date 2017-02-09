require(dplyr); require(ggplot2); require(ggrepel)

# read data into R
data_file <- paste(getwd(), "/Country_dimensions.csv", sep = "")
data <- read.csv(file = data_file)

# remove incomplete cases (where some data was NA) and the US
data2 <- data %>% filter(complete.cases(data)) %>%
                   filter(Country != "United States")

g <- ggplot(data2, aes(USdiff_sad_norm, GDPPC)) +
        theme_minimal() +
        geom_point() +
        geom_text_repel(label = data2$Country, segment.color = NA) +
        scale_x_continuous(name = "Cultural Difference from USA",
                           limits = c(0, 75))