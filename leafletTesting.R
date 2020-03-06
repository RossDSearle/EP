library(leaflet)
library(rgdal)

p <- readOGR('C:/Projects/EP/Soil Data/EPSoils.shp')

p <- readOGR('C:/Projects/EP/Soil Data/EPSimplified3.shp')

leaflet() %>%
  addPolygons(data=p2, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = 'blue',
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
              )


library(viridis) # My favorite palette for maps
ppal <- colorFactor(viridis(7), p2$LANSLU) # I'm assuming the variable ward contains the names of the communities.

p2 <- sf::read_sf('C:/Projects/EP/Soil Data/EPSoils4.shp')
leaflet(p2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
                   fillColor = ~ppal(LANSLU),
                   stroke=T,
                   fillOpacity = 0.8,
                   #smoothFactor = 10,
                   weight = 1,
                   color = 'black'
                   ) #%>%
  # addLegend("bottomright", pal = ppal, values = ~LANSLU,
  #           title = "Est. GDP (2010)",
  #           labFormat = labelFormat(prefix = "$"),
  #           opacity = 1
  #)


library("sf")
regions <- sf::read_sf('C:/Projects/EP/Soil Data/EPSoils.shp')
plot(regions[1])

simplepolys <- rmapshaper::ms_simplify(input = regions) %>% st_as_sf()
plot(simplepolys[1])
write_sf(simplepolys, 'C:/Projects/EP/Soil Data/EPSoils4.shp')




p <- readOGR('C:/Projects/EP/Soil Data/EPSoils4.shp')
plot(p)


