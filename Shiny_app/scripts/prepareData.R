require(dplyr); require(maps); require(rgdal); require(leaflet); require(jsonlite)
require(geojsonio)


setwd("~/Documents/Blogging/Country-Culture/Shiny_app/Travel_Quadrants")
map_data <- geojson_read("data/countries.geojson", what = "sp")
rawData <- readRDS("data/country_data2.rds")

#creating the dataset
temp <- read.csv("~/Documents/Blogging/Country-Culture/Shiny_app/raw_data/country_data_all.csv", 
				 header = T, na.strings = "#N/A",stringsAsFactors = F)
temp <- temp %>% mutate(GDPnorm = 100*GDP/max(GDP, na.rm = T),
						CEXnorm = 100*CEX/max(CEX, na.rm = T),
						densityNorm = 100*pop_density/max(pop_density, na.rm = T))
row.names(temp) <- temp$Country
saveRDS(temp, "~/Documents/Blogging/Country-Culture/Shiny_app/Travel_Quadrants/data/country_data2.rds")



# extracting label location (centroid of country). This example is for Angola.
center <- map_data@polygons[2][[1]]@labpt

# extracting area (proportional to square mileage of country). This example is for Angola.
area <- map_data@polygons[2][[1]]@area



countries <- map("world")


# load the country json file
countries <- readOGR("raw_data/countries.geojson", "OGRGeoJSON")
map <- leaflet(countries)
pal <- colorNumeric(
	palette = "Blues",
	domain = 1:177)
map %>% addTiles() %>% addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
								   color = ~pal(1:177))


countries2 <- readOGR("raw_data/countries copy.geojson", "OGRGeoJSON")


countriesJSON <- read_json("raw_data/countries.geojson")



# build off this snippet to add any country data to the geojson
temp <- 1:177
for (i in 1:length(countriesJSON[2]$features)) {
	countriesJSON[2]$features[[i]]$properties$culture <- temp[i]
}

write_json(countriesJSON, "raw_data/countries2.geojson")

coords <- list()
lats <- list()
longs <- list()
for (i in 1:length(map_data[2]$features)) {
	for (j in 1:length(map_data[2]$features[[i]]$geometry$coordinates[[1]])) {
		lats <- c(coords, unlist(map_data[2]$features[[i]]$geometry$coordinates[[1]][[j]][[1]]))
		longs <- c(coords, unlist(map_data[2]$features[[i]]$geometry$coordinates[[1]][[j]][[2]]))
	}
}
c <- unlist(coords)
