library(tidyverse)
library(tidycensus)
library(leaflet)

acs_vars <- load_variables(2019, "acs5")

state_population <- get_acs(
  geography = "state",
  #state = "VT",
  variables = "B01001_001",
  year = 2019,
  geometry = TRUE
)

## For Leaflet


color <- colorNumeric(palette = "viridis",
                      domain = state_population$estimate)


leaflet() |> 
  addProviderTiles(providers$Stamen.TonerLite) |> 
  addPolygons(data = state_population,
              color = "black",
              opacity = 1,
              fillColor = ~color(estimate),
              fillOpacity = 1,
              stroke = 1,
              weight = 1,
              label = ~paste(NAME,": ", estimate)) |> 
  setView(-96, 37.8, 3.5) |> 
  addLegend(
    position = "bottomright",
    pal = color,
    values = state_population$estimate,
    title = "State Population"
  )
