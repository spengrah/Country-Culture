require(dplyr); require(ggplot2); require(ggrepel); require(ggthemes)
require(cluster); require(plotly)

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
        mutate(GDPPC = as.numeric(sub(",","", GDPPC))) %>%
        select(-keep)


data[data == 0] <- NA #replace zeroes with NA
data <- data %>% filter(complete.cases(data)) %>%
        #normalize GDPPC to within 0-100
        mutate(GDPPC_norm = 100*(GDPPC - min(GDPPC))/(max(GDPPC) - min(GDPPC)))
row.names(data) <- data$Country


## -------------------------------
## calculate Cultural Difference Index for the USA
# create function to calculate CDI
CDI <- function(X, ref_country) {
        cdi_matrix <- as.matrix(select(X, IDV, IND, LTO, MAS, PDI, UAI))
        ref_vector <- as.numeric(subset(cdi_matrix, 
                                        row.names(cdi_matrix) == ref_country))
        
        diffs <- abs(sweep(cdi_matrix, 2, ref_vector))
        diff_sums <- apply(diffs, 1, sum)
        anti_ref <- ifelse(ref_vector < 50, 100, 0)
        anti_diff <- sum(abs(ref_vector - anti_ref))
        CDI_norm <- 100*(diff_sums/anti_diff)
        
        df <- cbind(X, CDI_norm)
        result <- list("df" = df, "Comparison_country" = ref_country)
}

# add the new CDI variable to the dataset
dataCDI <- CDI(data, "United States")$df

##------------------------
## clustering        
# first cluster attempt
cldata <- select(data, IDV, IND, LTO, MAS, PDI, UAI, GDPPC_norm)
cl7vars <- kmeans(cldata, 4, nstart = 3)
dist7vars <- dist(cldata)
# check with a silhoette plot
sil7vars <- silhouette(cl7vars$cluster, dist7vars)
plot(sil7vars)
plot_data <- cbind(cldata, cl7vars$cluster)

## just cluster on culture dims
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

# function to compare robustness between number of clusters
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

## just cluster on GDPPC_norm and CDI
cl2data <- select(dataCDI, GDPPC_norm, CDI_norm)
cl2 <- kmeans(cl2data, 5)
plot(x = cl2data$CDI_norm, y = cl2data$GDPPC, col = cl2$cluster)
distcl2 <- dist(cl2data)
silcl2 <- silhouette(cl2$cluster, distcl2)
plot(silcl2)

##--------------------
## plotting
# add variable denoting which countries I've visited
visited <- c("Mexico", "Canada", "Peru", "Brazil", 
             "United Kingdom", "France", "Tanzania", "South Africa", "Australia",
             "New Zealand", "Thailand")
interested <- c("Japan", "Singapore")
CDI_comp<- CDI(data, "United States")$Comparison_country


my_countries <- function(visited, interested, CDI_comp) {
        temp <- data.frame("country" = dataCDI$Country, 
                           "type" = vector(mode = "character", length = nrow(dataCDI)))
        ifelse(temp$country %in% visited, temp$type <- "visited",
               ifelse(temp$country %in% interested, temp$type <- "interested",
                      ifelse(temp$country %in% CDI_comp, temp$type <- "CDI_comp",
                             temp$type <- "other")))
}

plotdata <- mutate(dataCDI, my_countries = 
                           as.factor(my_countries(visited, interested, CDI_comp)))

# plot the economy and cultural index data


v_countries <- filter(plotdata, my_countries == "visited")
i_countries <- filter(plotdata, my_countries == "interested")
o_countries <- filter(plotdata, my_countries == "other")
labeled_countries <- filter(plotdata, my_countries != "other")

#static plot
static <- ggplot(plotdata, aes(x = CDI_norm, y = GDPPC, alpha = my_countries)) +
        theme_classic() +
        geom_point(size = 2, aes(color = my_countries)) +
        geom_text_repel(data = labeled_countries, alpha = 1, size = 3,
                        label = labeled_countries$Country, segment.color = NA,
                        point.padding = unit(1, "pt"), nudge_y = .15) +
        # scale_color_manual(name = "Cluster",
        #                    values = c("4" = "black", "2" = "green", "1" = "blue", "3" = "red"),
        #                    labels = c("France", "Developing", "Japan", "Westerns")) +
        scale_color_manual(values = c("visited" = "green", 
                                      "interested" = "red",
                                      "other" = "black",
                                      "CDI_comp" = "blue")) +
        scale_alpha_manual(values = c("visited" = 1, "interested" = 1,
                                      "other" = .4, "CDI_comp" = 1)) +
        scale_x_continuous(name = paste("Cultural Difference Index (vs. ", 
                                        CDI_comp, ")", sep = ""),
                           breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
                           labels = c("USA", "10", "20", "30", "40", "50", "60", "70")) +
        scale_y_continuous(name = "GDP Per Capita",
                           breaks = c(0, 25000, 50000, 75000, 100000),
                           labels = c("$0", "$25k", "$50k", "$75k", "$100k"),
                           limits = c(0, 105000)) +
        expand_limits(x = 0, y = 0) +
        labs(title = "My Travel Map", 
             subtitle = "Country Wealth and Culture",
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

#interactive plot
interactive <- ggplot(plotdata, aes(x = CDI_norm, y = GDPPC, alpha = my_countries,
                                    color = my_countries)) +
        theme_classic() +
        geom_point(size = 2) +
        scale_color_manual(values = c("visited" = "green", 
                                      "interested" = "red",
                                      "other" = "black",
                                      "CDI_comp" = "blue")) +
        scale_alpha_manual(values = c("visited" = 1, "interested" = 1,
                                      "other" = .4, "CDI_comp" = 1)) +
        scale_x_continuous(name = paste("Cultural Difference Index (vs. ", 
                                        CDI_comp, ")", sep = ""),
                           breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
                           labels = c("USA", "10", "20", "30", "40", "50", "60", "70")) +
        scale_y_continuous(name = "GDP Per Capita",
                           breaks = c(0, 25000, 50000, 75000, 100000),
                           labels = c("$0", "$25k", "$50k", "$75k", "$100k"),
                           limits = c(0, 105000)) +
        expand_limits(x = 0, y = 0) +
        labs(title = "My Travel Map", 
             subtitle = "Country Wealth and Culture",
             caption = "Based on data from the CIA World Factbook and Geert Hofstede") +
        theme(text = element_text(family = "sans", size = 12, color = "#3C3C3C"),
              plot.title = element_text(size = 16, face = "bold"),
              plot.caption = element_text(face = "italic", size = 10),
              axis.title = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              legend.justification = "bottom",
              legend.background = element_rect(fill = "#ededed"))
