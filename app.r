rm(list=ls())
# Load packages
packlage <- require("rnaturalearth")
if (!packlage) install.packages("rnaturalearth")
packlage <- require("shiny")
if (!packlage) install.packages("shiny")
packlage <- require("tmap")
if (!packlage) install.packages("tmap")
packlage <- require("dlpyr")
if (!packlage) install.packages("dlpyr")
packlage <- require("sf")
if (!packlage) install.packages("sf")
packlage <- require("raster")
if (!packlage) install.packages("raster")
packlage <- require("fasterize")
if (!packlage) install.packages("fasterize")
source("AOIGeneration.r")
source("hashGeneration.r")
source("spatialBF.r")
source("ui.r")

# cell_size represents degrees of longitude and latitude
cell_size <- 0.1

lon_poi <- 13.850165
lat_poi <- 44.873084
lon_test <- 14.457664
lat_test <- 45.328979
radius <- 25000
n <- 3
k <- 10
m <- 800
algorithms <- c("murmur32", "md5", "sha512", 'sha1', 'sha256', 'crc32', 'xxhash32','spookyhash')

worldmap <- ne_countries(scale = 'medium',
                         type = 'map_units',
                         returnclass = 'sf'
                         )

country_names <- c("Croatia", 
                   worldmap[which(worldmap$name != "Croatia"),]$name
                   )

create_grid <- function(country_name, cell_size){
  map <- worldmap[worldmap$name == country_name,]
  # making grid over the map and size of cell is expressed in degrees
  grid <- st_make_grid(map, crs = 4326, cellsize = cell_size) %>%
    st_sf('geometry' = ., data.frame('ID' = 1:length(.)))
  return(grid)
}

grid <- create_grid("Croatia", cell_size)

point <- function(lat, lon){
  return(data.frame(lng = lon, lat = lat) %>%
           st_as_sf(coords = c("lng", "lat"), crs = 4326))
}

poi_point <- point(lat_poi, lon_poi)
test_point <- point(lat_test, lon_test)

S <- coverageAOI(grid, poi_point, radius, cell_size, n)

# Draw areas of interest on interactive map
buffer <- st_buffer(poi_point, radius)

min_max_lat_lon <- function(grid){
  lons <- c()
  lats <- c()
  first_last_cell <- c(grid[[2]][[1]], grid[[2]][[length(grid[[2]])]])
  for (i in first_last_cell) {
    m <- i[[1]]
    
    print(m)
    lons <- c(lons, m[,1])
    lats <- c(lats, m[,2])
  }
  return_list <- list("min_lat" = min(lats), 
                      "min_lon" = min(lons), 
                      "max_lat" = max(lats), 
                      "max_lon" = max(lons)
                      )
  return(return_list)
}

create_SBF <- function(k, algorithm, S, m){
  H <- generateHashSetSalts(k, algorithm)
  
  b_vec_and_c_mat <- insert(S, H, m)
  b_vector <- b_vec_and_c_mat$b_vector
  return_list <- list("b_vector" = b_vec_and_c_mat$b_vector,
                      "col_mat" = b_vec_and_c_mat$col_mat,
                      "H" = H
                      )
  return(return_list)
}

check_point <- function(grid, test_point, b_vector, H){
  grid$has_test_point <- ifelse(sf::st_intersects(grid, test_point, sparse = F),
            "Yes",
            "No")
  cell <- grid[which(grid$has_test_point == "Yes"),]
  result <- check(b_vector, H, cell$ID)
  return(result)
}

ui <- ui(country_names, cell_size, lon_poi, lat_poi, lat_test, lon_test, radius, n, algorithms, k, m)

render_initial_map <- function(output){
  output$map <- renderTmap({
    tm_shape(S)+
      tm_fill(col="label", alpha = 0.45)+
      tm_polygons("blue", alpha = 0)+
      tm_shape(buffer)+tm_borders("red")+
      tm_shape(poi_point)+tm_dots("red")+
      tm_shape(test_point)+tm_dots("blue")
  })
  
  sbf_vector <- create_SBF(k, algorithms[1], S, m)
  
  output$SBF_label <- renderText({ 
    paste("Label of that test point is: ",
          check_point(grid,
                      test_point,
                      sbf_vector$b_vector,
                      sbf_vector$H
          )
    )
  })
}

error_message <- function(e){
  message(paste("Following error occured:\n",e))
  if(e$message == "replacement has 1 row, data has 0"){
    output$error_text <- renderText({
      paste("<font color=\"#FF0000\">",
            "Please enter bigger radius, 
                  lower number of areas or choose smaller size of cells"
      )
    })
  }
}

server <- function(input, output, session) {
  
  render_initial_map(output)
  
  observeEvent(input$go, {
    
    output$error_text <- renderText({})
    
    o_gird <- create_grid(input$country, input$cell_size)
    
    list <- min_max_lat_lon(o_gird)
    min_lat <- list$min_lat
    min_lon <- list$min_lon
    max_lat <- list$max_lat
    max_lon <- list$max_lon

    lat_lon_poi_ok <- FALSE
    lat_lon_test_ok <- FALSE
    
    point_error_txt <- ""
    
    error_text <- function(point_name){
      paste(
        "<font color=\"#FF0000\">",
        "Please enter ",
        point_name,
        " between ",
        min_lat, 
        " - ", 
        max_lat, 
        " of latitude and ", 
        min_lon, 
        " - ", 
        max_lon, 
        "of longitude"
        )
    }
    
    if (min_lat < input$lat_poi && 
        min_lon < input$lon_poi && 
        max_lat > input$lat_poi && 
        max_lon > input$lon_poi
        ) {
      lat_lon_poi_ok <- TRUE
    }
    else {
      point_error_txt <- paste(error_text("POI"), "<br>")
    }
    if (min_lat < input$lat_test && 
        min_lon < input$lon_test && 
        max_lat > input$lat_test && 
        max_lon > input$lon_test
        ) {
      lat_lon_test_ok <- TRUE
    }
    else{
      point_error_txt <- paste(point_error_txt, error_text("test point"))
    }
    
    output$error_text <- renderText({
      point_error_txt
    })
    
    if (lat_lon_poi_ok && lat_lon_test_ok) {
      
      poi_point <- point(input$lat_poi, input$lon_poi)
      test_point <- point(input$lat_test, input$lon_test)
      
      error <- tryCatch(
        {
          S <- coverageAOI(o_gird, 
                           poi_point, 
                           input$radius,
                           input$cell_size, 
                           input$n
                           )
          
          sbf_vector <- create_SBF(input$k, input$algorithm, S, input$m)
          
          buffer <- st_buffer(poi_point, input$radius)
          
          output$map <- renderTmap({
            tm_shape(S)+
            tm_fill(col="label", alpha = 0.45)+
            tm_polygons("blue", alpha = 0)+
            tm_shape(buffer)+tm_borders("red")+
            tm_shape(poi_point)+tm_dots("red")+
            tm_shape(test_point)+tm_dots("blue")
            })
          
          output$SBF_label <- renderText({ 
            paste("Label of test point is: ",
                  check_point(o_gird,
                              test_point,
                              sbf_vector$b_vector,
                              sbf_vector$H
                  )
            )
          })
          
        },
        error = function(e)
        {
          error_message(e)
        } 
      )
    }
  })
  
  observeEvent(input$goCSV, {
    output$error_text <- renderText({})
    
    o_gird <- create_grid(input$country, input$cell_size)
    
    list <- min_max_lat_lon(o_gird)
    min_lat <- list$min_lat
    min_lon <- list$min_lon
    max_lat <- list$max_lat
    max_lon <- list$max_lon
    
    lat_lon_poi_ok <- FALSE
    lat_lon_test_ok <- FALSE
    
    point_error_txt <- ""
    
    error_text <- function(point_name){
      paste(
        "<font color=\"#FF0000\">",
        "Please enter ",
        point_name,
        " between ",
        min_lat, 
        " - ", 
        max_lat, 
        " of latitude and ", 
        min_lon, 
        " - ", 
        max_lon, 
        "of longitude"
      )
    }
    
    points_from_csv <- read.csv(file = input$csvFiles$datapath)
    print(points_from_csv)
    
    error_text_multiple <- function(point_name){
      paste(
        "<font color=\"#FF0000\">",
        "Please make sure that all the points in CSV are between ",
        min_lat,
        " - ",
        max_lat,
        " of latitude and ",
        min_lon,
        " - ",
        max_lon,
        "of longitude"
      )
    }

    if (min_lat < input$lat_poi &&
        min_lon < input$lon_poi &&
        max_lat > input$lat_poi &&
        max_lon > input$lon_poi
    ) {
      lat_lon_poi_ok <- TRUE
    }
    else {
      point_error_txt <- paste(error_text("POI"), "<br>")
    }
    
    for (i in 1:length(points_from_csv[,1])) {
      if (min_lat < points_from_csv[i,]$lat_test &&
          min_lon < points_from_csv[i,]$lon_test &&
          max_lat > points_from_csv[i,]$lat_test &&
          max_lon > points_from_csv[i,]$lon_test
          )
        {
          lat_lon_test_ok <- TRUE
          }
      else{
        point_error_txt <- paste(point_error_txt, error_text(points_from_csv[i,]$name))
        lat_lon_test_ok <- FALSE
        }
    }


    if (lat_lon_poi_ok && lat_lon_test_ok) {
      test_points <- st_as_sf(points_from_csv,
                              coords = c("lon_test", "lat_test")
                              )
      
      poi_point <- point(input$lat_poi, input$lon_poi)
      
      error <- tryCatch(
        {
          S <- coverageAOI(o_gird,
                           poi_point,
                           input$radius,
                           input$cell_size,
                           input$n
          )

          sbf_vector <- create_SBF(input$k, input$algorithm, S, input$m)

          buffer <- st_buffer(poi_point, input$radius)

          output$map <- renderTmap({
            tm_shape(S)+
              tm_fill(col="label", alpha = 0.45)+
              tm_polygons("blue", alpha = 0)+
              tm_shape(buffer)+tm_borders("red")+
              tm_shape(poi_point)+tm_dots("red")+
              tm_shape(test_points)+tm_dots("blue")+tm_text("name", just = "top")
          })

          output_text <- ""
          print(length(points_from_csv[,1]))
          for (i in 1:length(points_from_csv[,1])) {
            print(points_from_csv[i,]$name)
            output_text <- paste(output_text,
                                 "<br>",
                                 paste("Label of test point= ",points_from_csv[i,]$name ," is: ",
                                        check_point(o_gird,
                                                    point(points_from_csv[i,]$lat_test, points_from_csv[i,]$lon_test),
                                                    sbf_vector$b_vector,
                                                    sbf_vector$H
                                                    )
                                        )
                          )
          }

          output$SBF_label <- renderText({output_text})
        },

        error = function(e)
        {
          error_message(e)
        }
      )
    }

    else{
      output$error_text <- renderText({
        point_error_txt
      })
    }
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)