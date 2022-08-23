rm(list=ls()) # cleaning workspace

# load all necessary scripts
source("installNecessaryLibraries.r")
source("AOIGeneration.r")
source("spatialBF.r")
source("ui.r")
source("utilFunctions.r")

# Load packages
library(rnaturalearth)
library(shiny)
library(tmap)
library(sf)
library(raster)
library(fasterize)

# Define initial point of interest
lon_poi <- 13.850165
lat_poi <- 44.873084

# Define initial test point
lon_test <- 14.457664
lat_test <- 45.328979

radius <- 25000 # radius

## SBF settings
n <- 3 # areas of interest
k <- 10 # amount of hash functions
m <- 800 # size of SBF

# Size of grid cell
cell_size <- 0.1 # in degrees of longitude and latitude
degRadius <- 0.1 # Radius of the random generated points in degrees for normal distribution

num_rnd_points <- 100 # initial number of random test points
num_iter <- 500 # initial number of iterations for testing grid search, etc.

# List of hash algorithms
algorithms <- c("murmur32", "md5", "sha512", 'sha1', 'sha256', 'crc32', 'xxhash32','spookyhash')

randomGenerationModes <- c("Uniform", "Normal")

worldmap <- ne_countries(scale = 'medium',
                         type = 'map_units',
                         returnclass = 'sf'
                         )

country_names <- c("Croatia", 
                   worldmap[which(worldmap$name != "Croatia"),]$name
                   )

ui <- ui(country_names,
         cell_size,
         lon_poi,
         lat_poi, 
         lat_test,
         lon_test,
         radius,
         n,
         algorithms,
         k,
         m,
         num_rnd_points, 
         randomGenerationModes,
         degRadius,
         num_iter)

render_initial_map <- function(output){
  #TODO koristiti stvari iz util functions skripte
  
  grid <- createGrid("Croatia", cell_size, worldmap)
  
  df_test_point <- data.frame(lat_test = lat_test, lon_test = lon_test, name = "test point")
  
  input = list ("lon_poi" = lon_poi,
                "lat_poi" = lat_poi,
                "radius" = radius,
                "cell_size" = cell_size,
                "n" = n,
                "k" = k,
                "algorithm" = "murmur32",
                "m" = m)
  
  testPoints(df_test_point, grid, input, output)
}

server <- function(input, output, session) {
  
  render_initial_map(output)
  
  observeEvent(input$go, {
    
    output$error_text <- renderText({})
    
    grid <- createGrid(input$country, input$cell_size, worldmap)
    
    bounds <- getBounds(grid)
    
    if(checkIfPointIsOutOfBounds(bounds, input$lat_poi, input$lon_poi))
    {
      output$error_text <- renderText({createWarningStringForOutOfBoundsPoint("POI", bounds)})
      return()
    }
    if(checkIfPointIsOutOfBounds(bounds, input$lat_test, input$lon_test))
    {
      output$error_text <- renderText({createWarningStringForOutOfBoundsPoint("test point", bounds)})
      return()
    }

    df_test_point <- data.frame(lat_test = input$lat_test, lon_test = input$lon_test, name = "test point")
    
    testPoints(df_test_point, grid, input, output)
    
  })
  
  observeEvent(input$goCSV, {
    if (length(input$csvFiles$datapath) == 0){
      output$error_text <- renderText({paste("<font color=\"#FF0000\">",
                                             "Please sepcify CSV file path."
                                             )
        })
    } else {
      output$error_text <- renderText({})
      
      grid <- createGrid(input$country, input$cell_size, worldmap)
      
      bounds <- getBounds(grid)
      
      test_points_ok <- FALSE
      
      point_error_txt <- ""
      
      
      df_test_points <- read.csv(file = input$csvFiles$datapath)
      print(df_test_points)
      
      if(checkIfPointIsOutOfBounds(bounds, input$lat_poi, input$lon_poi))
      {
        output$error_text <- renderText({createWarningStringForOutOfBoundsPoint("POI", bounds)})
        return()
      }
      
      for (i in 1:length(df_test_points[,1])) {
        if (checkIfPointIsOutOfBounds(bounds, df_test_points[i,]$lat_test, df_test_points[i,]$lon_test))
        {
          point_error_txt <- paste(point_error_txt, createWarningStringForOutOfBoundsPoint(df_test_points[i,]$name, bounds),df_test_points[i,]$lat_test, df_test_points[i,]$lon_test , "<br>")
          test_points_ok <- FALSE
          
        } else {
          test_points_ok <- TRUE
        }
      }
      
      output$error_text <- renderText({point_error_txt})
      
      if (test_points_ok) {
        testPoints(df_test_points, grid, input, output)
      } else {
        output$error_text <- renderText({
          point_error_txt
        })
      }
    }
  })
  
  observeEvent(input$goRnd, {
    
    output$error_text <- renderText({})
    
    grid <- createGrid(input$country, input$cell_size, worldmap)
    
    bounds <- getBounds(grid)
    
    rnd_test_points <- generateRandomPoints(bounds, input$num_rnd_points, input$randomGenerationModes, input$lon_poi, input$lat_poi, input$degRadius)
    
    
    testPoints(rnd_test_points, grid, input, output)
  })
  
  observeEvent(input$goTst, {
    
    output$error_text <- renderText({})
    
    grid <- createGrid(input$country, input$cell_size, worldmap)
    
    test_k_and_m(input, grid)
    
  })
}

shinyApp(ui = ui, server = server)