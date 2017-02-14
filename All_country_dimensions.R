require(dplyr); require(ggplot2); require(ggrepel); require(ggthemes)
require(cluster)

# read data into R
data_dir <- "~/Documents/Blogging/Country-Culture"
data_file <- paste(data_dir, "Mturk Task/Batch_2684442_batch_results.csv", sep = "/")
full_data <- read.csv(file = data_file)
data <- full_data %>% select(Country = Input.Country,
               GDPPC = Answer.GDPPC,
               IDV = Answer.IDV,
               IND = Answer.IND,
               LTO = Answer.LTO,
               MAS = Answer.MAS,
               PDI = Answer.PDI,
               UAI = Answer.UAI) %>%
        mutate(keep = rep(c(TRUE, FALSE), nrow(full_data)/2),
               IND = as.numeric(as.character(IND))) %>%
        filter(keep == TRUE) %>%
        mutate(GDPPC = sub("\\$","",GDPPC)) %>%
        mutate(GDPPC = as.numeric(sub(",","", GDPPC)))

data[data == 0] <- NA #replace zeroes with NA
data <- data %>% filter(complete.cases(data)) %>%
        #normalize GDPPC to within 0-100
        mutate(GDPPC_norm = 100*(GDPPC - min(GDPPC))/(max(GDPPC) - min(GDPPC)))
row.names(data) <- data$Country

# calculate Cultural Difference Index for the USA


        
# first cluster attempt
cldata <- select(data, IDV, IND, LTO, MAS, PDI, UAI, GDPPC_norm)
cl7vars <- kmeans(cldata, 4, nstart = 3)
dist7vars <- dist(cldata)
# check with a silhoette plot
sil7vars <- silhouette(cl7vars$cluster, dist7vars)
plot(sil7vars)
plot_data <- cbind(cldata, cl7vars$cluster)

# just cluster on culture dims
cult_data <- select(data, IDV, IND, LTO, MAS, PDI, UAI)
clcult <- kmeans(cult_data, 6)
distcult <- dist(cult_data)
silcult <- silhouette(clcult$cluster, distcult)
plot(silcult)
View(data.frame(clcult$cluster))

# check robustness of clusters by taking correlations between 2 runs, and then
# averaging that correlation over N iterations
cl_corrs <- function(X, n_iterations, n_clusters){
        set.seed(2017) #reproducability
        corrs <- vector(mode = "numeric") #intialize vector of correlations
        for (n in 1:n_iterations) {
                cl1 <- kmeans(X, n_clusters)
                cl2 <- kmeans(X, n_clusters)
                corr <- cor(cl1$cluster, cl2$cluster)
                corrs <- c(corrs, corr)
        }
        mean_corr <- mean(corrs)
        result <- list("iterations" = n_iterations,
                       "clusters" = n_clusters,
                       "corrs" = corrs,
                       "mean_corr" = mean_corr)
        result
}

# function to compare many 
compare_clusters <- function(X, max_clusters, n_iters) {
        d <- vector(mode = "numeric")
        for(i in 2:max_clusters) {
                corr <- cl_corrs(X = cult_data, n_iterations = n_iters, n_clusters = i)
                print(paste(i, "cluster mean:", round(corr$mean_corr,3), sep = " "))
                d <- c(d, corr$mean_corr)
                
        }
        d
}

compare_clusters(X = cldata, n_iters = 20, max_clusters = 8)
cl_corrs(X = cult_data, n_iterations = 20, n_clusters = 4)


# plot the economy and cultural index data
plotdata <- data %>% mutate(visited = ifelse(Country == "Japan", "new", "visited"))
                             # ,cluster = factor(cl1$cluster[2:13])) # bring in full dims cluster info
g <- ggplot(plotdata, aes(USdiff_sad_norm, GDPPC)) +
        theme_classic() +
        geom_point(size = 2, aes(color = cluster)) +
        geom_text_repel(label = plotdata$Country, segment.color = NA,
                        point.padding = unit(1, "pt"), nudge_y = .15) +
        scale_color_manual(name = "Cluster",
                           values = c("4" = "black", "2" = "green", "1" = "blue", "3" = "red"),
                           labels = c("France", "Developing", "Japan", "Westerns")) +
        # scale_x_continuous(name = "Cultural Difference Index (vs. USA)",
        #                    breaks = c(0, 10, 20, 30, 40, 50),
        #                    labels = c("USA", "10", "20", "30", "40", "50")) +
        # scale_y_continuous(name = "GDP Per Capita", 
        #                    breaks = c(0, 20000, 40000, 60000),
        #                    labels = c("$0", "$20k", "$40k", "$60k"),
        #                    limits = c(0, 60000)) +
        expand_limits(x = 0, y = 0) +
        labs(title = "Country Wealth and Culture", 
             subtitle = "Among countries I've visited, plus Japan",
             caption = "Based on data from the CIA World Factbook and Geert Hofstede") +
        # geom_hline(yintercept = 28000, linetype = "dashed", color = "dark grey",
        #            size = .4) +
        # geom_vline(xintercept = 27, linetype = "dashed", color = "dark grey",
        #            size = .4) +
        theme(text = element_text(family = "sans", size = 12, color = "#3C3C3C"),
              plot.title = element_text(size = 16, face = "bold"),
              plot.caption = element_text(face = "italic", size = 10),
              axis.title = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              legend.justification = "bottom",
              legend.background = element_rect(fill = "#ededed"))
