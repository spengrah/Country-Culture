rawData <- readRDS("data/country_data2.rds")

# log transform for population density since Bangladesh skews the distribution so much
density <- ifelse(rawData$pop_density == 0, 1, rawData$pop_density)
densityLog <- log(density)
densityLogNorm <- 100*(densityLog - min(densityLog, na.rm = T))/(max(densityLog, na.rm = T) - min(densityLog, na.rm = T))
rawData$densityLogNorm <- densityLogNorm