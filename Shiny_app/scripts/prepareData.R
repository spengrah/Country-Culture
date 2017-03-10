require(dplyr); require(maps); require(rgdal); require(leaflet); require(jsonlite)
require(geojsonio)

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
for (i in 1:length(countriesJSON[2]$features)) {
	for (j in 1:length(countriesJSON[2]$features[[i]]$geometry$coordinates[[1]])) {
		lats <- c(coords, unlist(countriesJSON[2]$features[[i]]$geometry$coordinates[[1]][[j]][[1]]))
		longs <- c(coords, unlist(countriesJSON[2]$features[[i]]$geometry$coordinates[[1]][[j]][[2]]))
	}
}
c <- unlist(coords)
