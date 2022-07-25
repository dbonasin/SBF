require("tmap")
require("rnaturalearth")
require("dplyr")
require("raster")
require("sf")
require("shiny")
source("AOIGeneration.r")
  
# cell_size represents degrees of longitude and latitude
cell_size <- 0.1
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
map <- worldmap[worldmap$name == "Croatia",]
# making grid over the map and size of cell is expressed in degrees
grid <- st_make_grid(map, crs = 4326, cellsize = cell_size) %>%
  st_sf('geometry' = ., data.frame('ID' = 1:length(.)))

min_max_lat_lon <- function(grid){
  longs <- c()
  lats <- c()
  for (i in grid[[2]]) {
    m <- i[[1]]
    longs <- c(longs, m[,1])
    lats <- c(lats, m[,2])
  }
  return_list <- list("min_lat" = min(lats), "min_lon" = min(longs), "min_lat" = max(lats), "max_lon" = max(longs))
  return(return_list)
}


S <- coverageAOI(grid, point, radius, cell_size, n)

# Draw areas of interest on interactive map
buffer <- st_buffer(point, radius)

country_names <- c("Croatia", worldmap[which(worldmap$name != "Croatia"),]$name)

ui <- fluidPage(
  tmapOutput("map"),
  selectInput("var", "Variable", country_names)
)

server <- function(input, output, session) {
  output$map <- renderTmap({
    tm_shape(S)+tm_fill(col="label", alpha = 0.45, zindex = 401)+tm_polygons("blue", alpha = 0, zindex = 402)+
      tm_shape(buffer)+tm_borders("red", zindex = 403)+
      tm_shape(point)+tm_dots("red", zindex = 404)
  })
  
  observe({

    var <- input$var

    map <- worldmap[worldmap$name == var,]
    # making grid over the map and size of cell is expressed in degrees
    grid <- st_make_grid(map, crs = 4326, cellsize = cell_size) %>%
      st_sf('geometry' = ., data.frame('ID' = 1:length(.)))
    
    S <- coverageAOI(grid, point, radius, cell_size, n)
    tmapProxy("map", session, {
      tm_remove_layer(401)+tm_remove_layer(402)+tm_shape(S)+tm_fill(col="label", alpha = 0.45, zindex = 401)+tm_polygons("blue", alpha = 0, zindex = 402)+
        tm_remove_layer(403)+tm_shape(buffer)+tm_borders("red", zindex = 403)+
        tm_remove_layer(404)+tm_shape(point)+tm_dots("red", zindex = 404)
    })
  })
}	


shinyApp(ui, server)