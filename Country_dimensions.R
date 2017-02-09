require(dplyr); require(ggplot2)

# read data into R
data_file <- paste(getwd(), "/Country_dimensions.csv", sep = "")
data <- read.csv(file = data_file)

# remove incomplete cases (where some data was NA)
data2 <- filter(data, complete.cases(data))

