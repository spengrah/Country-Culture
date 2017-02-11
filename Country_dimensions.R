require(dplyr); require(ggplot2); require(ggrepel); require(ggthemes)
require(cluster)

# read data into R
data_file <- paste(getwd(), "/Country_dimensions.csv", sep = "")
data <- read.csv(file = data_file)

# remove incomplete cases (where some data was NA) and the US
data2 <- data %>% filter(complete.cases(data)) %>%
                   filter(Country != "United States")

# option to plot without Singapore (where I haven't visited)
data3 <- data2 %>% filter(Country !="Singapore")

# try out some clustering
# need complete cases, but include the US
# also need to normalize GDPPC - use max GDPPC = $129,700 (Qatar)
maxGDPPC <- 129700
data4 <- data %>% filter(complete.cases(data)) %>%
        filter(Country != "Singapore") %>%
        mutate(GDPPC_norm = GDPPC/maxGDPPC*100)
data5 <- data4 %>% select(2:7, 10, 11)
row.names(data5) <- data4$Country


# clusters with each of the 6 culture dimensions and normalized GDPPC
data8 <- data5 %>% select(1:6, GDPPC_norm)
cl1 <- kmeans(data8, 4)
plot(data8, col = cl1$cluster) # see all the dims
# observations: individualism drives the most amount of the differentiation
plot(data6, col = cl1$cluster) # plot GDP vs. cultural index
# observations: france gets separated from 


# clusters with GDP and culture diff index
data6 <- data5 %>% select(USdiff_sad_norm, GDPPC_norm)
cl2 <- kmeans(data6, 4)
plot(data6, col = cl2$cluster)
points(cl2$centers, col = 1:4, pch = 8, cex = 2)
# silhouette plot
dist6 <- dist(data6)
sil <- silhouette(cl$cluster, dist6)
plot(sil)
# observations: same groups as my quandrants!

# clusters with GDP and individualism
data7 <- data5 %>% select(IDV, GDPPC_norm)
cl3 <- kmeans(data7, 4)
plot(data7, col = cl3$cluster)

# plot the economy and cultural index data
plotdata <- data3 %>% mutate(visited = ifelse(Country == "Japan", "new", "visited"),
                             cluster = factor(cl1$cluster[2:13])) # bring in full dims cluster info
g <- ggplot(plotdata, aes(USdiff_sad_norm, GDPPC)) +
        theme_classic() +
        geom_point(size = 2, aes(color = cluster)) +
        geom_text_repel(label = plotdata$Country, segment.color = NA,
                        point.padding = unit(1, "pt"), nudge_y = .15) +
        scale_color_manual(name = "Cluster",
                           values = c("4" = "black", "2" = "green", "1" = "blue", "3" = "red"),
                           labels = c("France", "Developing", "Japan", "Westerns")) +
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
              legend.title = element_text(face = "bold"),
              legend.justification = "top",
              legend.background = element_rect(fill = "light grey"))
