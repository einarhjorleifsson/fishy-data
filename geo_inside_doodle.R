# more doodle

library(sp)
library(dplyr)
library(ggplot2)
library(ggpolypath)

ices.area <- rgdal::readOGR(dsn = "/home/einarhj/stasi/gis/ices/shapes_modified",
                            layer = "ICES_areas_simplifed_tolerance005")
map <- ices.area[ices.area$ICES_area %in% c("Va1", "Va2"),]
class(map)
map@data

# an improved function
# lon a vector
# lat a vector
# map a SpatialPolygonsDataFrame
# variable a character vector containing the variable name whose
#  value to return (any one of the variable in map@data).
geo_inside <- function(lon, lat, map, variable = "ICES_area") {

  x <- sp::SpatialPoints(data.frame(long = lon,
                                    lat = lat),
                         proj4string = sp::CRS(proj4string(map)))

  x <- sp::over(x, map)

  x <- as.character(x[,variable])

  return(x)

}

station <-
  data_frame(long = runif(1000, -28, -9),
             lat  = runif(1000, 61.5, 68.5),
             id   = 1:1000,
             stringsAsFactors = FALSE) %>%
  mutate(strata = geo_inside(long, lat, map))

ggplot(map, aes(x = long, y = lat)) +
  theme_bw() +
  geom_polypath(aes(group = group), fill = "grey") +
  geom_path(aes(group = group), col = "yellow") +
  geom_point(data = station, aes(col = strata), size = 1)+
  coord_quickmap()
