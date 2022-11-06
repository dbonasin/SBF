rm(list = ls()) # cleaning workspace

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
lat_poi <- parameters$lat_poi

# Define initial test point
lon_test <- parameters$lon_test
lat_test <- parameters$lat_test

radius <- parameters$radius # radius

## SBF settings
n <- parameters$n # areas of interest
k <- parameters$k # amount of hash functions
m <- parameters$m # size of SBF

# Size of grid cell
cell_size <-
  parameters$cell_size # in degrees of longitude and latitude
degRadius <-
  parameters$degRadius # Radius of the random generated points in degrees for normal distribution

num_rnd_points <-
  parameters$num_rnd_points # initial number of random test points
# num_iter <- 500 # initial number of iterations for testing grid search, etc.

# List of hash algorithms
algorithms <<- c("murmur32",
                 "md5",
                 "sha512",
                 'sha1',
                 'sha256',
                 'crc32',
                 'xxhash32',
                 'spookyhash')
# List of distributions for generating random points on map
randomGenerationModes <<- c("Uniform", "Normal")

# Getting data for world map
worldmap <- ne_countries(scale = 'medium',
                         type = 'map_units',
                         returnclass = 'sf')
# List of countries in the world
country_names <<- c("Croatia",
                    worldmap[which(worldmap$name != "Croatia"),]$name)
# Initialising number of random test points
rnd_test_points <- 0
# Calling function to build ui
ui <- ui(
  country_names,
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
  # Creating initial map at the startup of the script with demo example
  renderInitialMap(input, output, session, worldmap, parameters)
  # When Save parameters button is pressed save parameters to CSV file
  observeEvent(input$saveParameters, {
    saveParametersToCSV(input, output, path = ".")
  })
  # When Load parameters button is pressed load parameters from CSV file
  observeEvent(input$loadParameters, {
    loadParametersFromCSV(input, output, session)
  })
  # When Test single point button is pressed calculate SBF for single point
  observeEvent(input$go, {
    # Erase content in text outputs
    cleanOutputs(output)
    # Initialise grid for specified country
    grid <- createGrid(input$country, input$cell_size, worldmap)
    # Get bounds of the grid
    bounds <- getBounds(grid)
    # If POI point is out of bounds stop and write warning
    if (checkIfPointIsOutOfBounds(bounds, input$lat_poi, input$lon_poi))
    {
      printMessage(createWarningStringForOutOfBoundsPoint("POI", bounds),
                   "warning")
      return()
    }
    # If test point is out of bounds stop and write warning
    if (checkIfPointIsOutOfBounds(bounds, input$lat_test, input$lon_test))
    {
      printMessage(createWarningStringForOutOfBoundsPoint("test point", bounds),
                   "warning")
      return()
    }
    # Create data frame for test point and add a name to it
    df_test_point <- data.frame(
      lat_test = input$lat_test,
      lon_test = input$lon_test,
      name = "test point"
    )
    # Start test
    testPoints(df_test_point, grid, input, output)
    
  })
  
  observeEvent(input$goCSV, {
    path <- input$csvFiles$datapath
    # If no file is chosen
    if (length(path) == 0) {
      printMessage("Please sepcify CSV file path.", output, "warning")
      # If it's not CSV file
    } else if (substr(path, nchar(path) - 3, nchar(path)) != ".csv") {
      print(substr(path, nchar(path) - 3, nchar(path)))
      printMessage("Please use CSV file", output, "warning")
      # If it's CSV file
    } else {
      df_test_points <- readCSV(path)
      necessary_columns <- c("lat_test",
                             "lon_test",
                             "name")
      # If CSV file is not well formatted
      if (!checkCSVFormat(necessary_columns, df_test_points, output)) {
        return()
      }
      # Erase content in text outputs
      cleanOutputs(output)
      # Initialize grid for specified country
      grid <- createGrid(input$country, input$cell_size, worldmap)
      # Get bounds of the grid
      bounds <- getBounds(grid)
      # Try to open file. Catch error and write it in application otherwise
      # If POI point is out of bounds stop and write warning
      if (checkIfPointIsOutOfBounds(bounds, input$lat_poi, input$lon_poi)) {
        printMessage(createWarningStringForOutOfBoundsPoint("POI", bounds),
                     output,
                     "warning")
        return()
      }
      # Initialize flag that enables start of the test
      test_points_ok <- FALSE
      # Initialize text that will be written in case any test point is out of bounds
      point_error_txt <- ""
      # for each test point if it's outside bounds write on console
      # and stop execution of test
      for (i in 1:length(df_test_points[, 1])) {
        is_out_of_bounds <- checkIfPointIsOutOfBounds(bounds,
                                                      df_test_points[i, ]$lat_test,
                                                      df_test_points[i, ]$lon_test)
        if (is_out_of_bounds)
        {
          point_error_txt <- paste(
            point_error_txt,
            df_test_points[i,]$lat_test,
            df_test_points[i,]$lon_test,
            "<br>"
          )
          test_points_ok <- FALSE
        } else {
          test_points_ok <- TRUE
        }
      }
      # If test points are in bounds start the test otherwise write warning
      if (test_points_ok) {
        # Start test
        testPoints(df_test_points, grid, input, output)
      } else {
        printMessage(point_error_txt, output, "warning")
      }
    }
  })
  
  observeEvent(input$goRnd, {
    # Erase content in text outputs
    cleanOutputs(output)
    
    # Initialise grid for specified country
    grid <- createGrid(input$country, input$cell_size, worldmap)
    # Get bounds of the grid
    bounds <- getBounds(grid)
    #Generate random points
    rnd_test_points <<- generateRandomPoints(
      bounds,
      input$num_rnd_points,
      input$randomGenerationModes,
      input$lon_poi,
      input$lat_poi,
      input$degRadius
    )
    # Start test
    testPoints(rnd_test_points, grid, input, output)
  })
  
  observeEvent(input$saveRndPnts, {
    # Reset error text
    output$error_text <- renderText({})
    
    # If there is no pints write warning, else save the points
    if (class(rnd_test_points) == "numeric") {
      printMessage("First press  \"Test random points\" button.",
                   output,
                   "warning")
    } else {
      error <- tryCatch({
        write.csv(
          rnd_test_points,
          paste0(
            "saved_random_points_",
            str_replace_all(Sys.time(),
                            c(
                              "-" = "_",
                              " " = "-",
                              ":" = "_"
                            )),
            "_",
            input$randomGenerationModes,
            "_",
            input$degRadius,
            ".csv"
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
}

shinyApp(ui = ui, server = server)