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
library(stringr)
# 
parameters <- readParameters()
# Define initial point of interest
lon_poi <- parameters$lon_poi
lat_poi <- parameters$llat_poi

# Define initial test point
lon_test <- parameters$lon_test
lat_test <- parameters$lat_test

radius <- parameters$radius # radius

## SBF settings
n <- parameters$n # areas of interest
k <- parameters$k # amount of hash functions
m <- parameters$m # size of SBF

# Size of grid cell
cell_size <- parameters$cell_size # in degrees of longitude and latitude
degRadius <- parameters$degRadius # Radius of the random generated points in degrees for normal distribution

num_rnd_points <- parameters$num_rnd_points # initial number of random test points
# num_iter <- 500 # initial number of iterations for testing grid search, etc.

# List of hash algorithms
algorithms <<- c("murmur32",
                 "md5",
                 "sha512",
                 'sha1',
                 'sha256',
                 'crc32',
                 'xxhash32',
                 'spookyhash'
                 )

randomGenerationModes <<- c("Uniform", "Normal")

worldmap <- ne_countries(scale = 'medium',
                         type = 'map_units',
                         returnclass = 'sf'
                         )

country_names <<- c("Croatia", 
                   worldmap[which(worldmap$name != "Croatia"),]$name
                   )

rnd_test_points <- 0

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
         degRadius
         )

server <- function(input, output, session) {
  
  renderInitialMap(input, output, session, worldmap, parameters)
  
  observeEvent(input$saveParameters, {
    saveParametersToCSV(input, output)
    
  })
  
  observeEvent(input$loadParameters, {
    loadParametersFromCSV(input, output, session)
  })
  
  observeEvent(input$go, {
    cleanOutputs(output)
    
    grid <- createGrid(input$country, input$cell_size, worldmap)
    bounds <- getBounds(grid)
    
    if(checkIfPointIsOutOfBounds(bounds, input$lat_poi, input$lon_poi))
    {
      printMessage(createWarningStringForOutOfBoundsPoint("POI", bounds),
                   "warning"
                   )
      return()
    }
    if(checkIfPointIsOutOfBounds(bounds, input$lat_test, input$lon_test))
    {
      printMessage(createWarningStringForOutOfBoundsPoint("test point", bounds),
                   "warning"
                   )
      return()
    }

    df_test_point <- data.frame(lat_test = input$lat_test,
                                lon_test = input$lon_test,
                                name = "test point"
                                )

    testPoints(df_test_point, grid, input, output)
    
  })
  
  observeEvent(input$goCSV, {
    if (length(input$csvFiles$datapath) == 0){
      printMessage("Please sepcify CSV file path.", output, "warning")
    } else {
      cleanOutputs(output)
      
      grid <- createGrid(input$country, input$cell_size, worldmap)
      bounds <- getBounds(grid)
      
      error <- tryCatch(
        {
          df_test_points <- read.csv(file = input$csvFiles$datapath)
          print(df_test_points)
        },
        error = function(e)
        {
          printMessage(e, output, "error")
        })
      
      if(checkIfPointIsOutOfBounds(bounds, input$lat_poi, input$lon_poi)){
        printMessage(createWarningStringForOutOfBoundsPoint("POI", bounds),
                     output,
                     "warning"
                     )
        return()
      }
      
      test_points_ok <- FALSE
      point_error_txt <- ""
    
      for (i in 1:length(df_test_points[,1])) {
        is_out_of_bounds <- checkIfPointIsOutOfBounds(bounds,
                                                      df_test_points[i,]$lat_test,
                                                      df_test_points[i,]$lon_test
                                                      )
        if (is_point_out_of_bounds)
        {
          point_error_txt <- paste(point_error_txt,
                                   # createWarningStringForOutOfBoundsPoint(df_test_points[i,]$name,
                                   #                                        bounds),
                                   df_test_points[i,]$lat_test,
                                   df_test_points[i,]$lon_test,
                                   "<br>"
                                   )
          test_points_ok <- FALSE
        } else {
          test_points_ok <- TRUE
        }
      }
  
      if (test_points_ok) {
        testPoints(df_test_points, grid, input, output)
      } else {
        printMessage(point_error_txt, output, "warning")
      }
    }
  })
  
  observeEvent(input$goRnd, {
    cleanOutputs(output)
    
    grid <- createGrid(input$country, input$cell_size, worldmap)
    bounds <- getBounds(grid)
    
    rnd_test_points <<- generateRandomPoints(bounds,
                                             input$num_rnd_points,
                                             input$randomGenerationModes,
                                             input$lon_poi,
                                             input$lat_poi,
                                             input$degRadius
                                             )
    
    testPoints(rnd_test_points, grid, input, output)
  })
  
  observeEvent(input$saveRndPnts, {
    output$error_text <- renderText({})
    if (class(rnd_test_points) == "numeric"){
      printMessage("First press  \"Test random points\" button.", output, "warning")
    } else {
      error <- tryCatch(
        {
          write.csv(rnd_test_points,
                    paste0("saved_random_points_", 
                           str_replace_all(Sys.time(), 
                                           c("-" = "_",
                                             " " = "-",
                                             ":" = "_")
                                           ),
                           "_.csv"
                    ),
                    row.names = FALSE
          )
          printMessage("Random points are saved.", output, "sucess")
        },
        error = function(e)
        {
          printMessage(e, output, "error")
        })
    }
  })
  
  # observeEvent(input$goTst, {
  #   
  #   output$error_text <- renderText({})
  #   output$SBF_label <- renderText({})
  #   output$confusion_matrix <- renderTable({})
  #   
  #   grid <- createGrid(input$country, input$cell_size, worldmap)
  #   
  #   test_k_and_m(input, grid)
  #   
  # })
}

shinyApp(ui = ui, server = server)