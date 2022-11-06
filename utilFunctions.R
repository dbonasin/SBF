library('plot.matrix')

# Check if point is inside bounds of the grid
#
# Parameters
# ----------
# bounds : list
#   list of the minimum longitude and maximum longitude, minimum latitude and
#   maximum latitude
# point_lat : numeric
#   latitude of the point that is being checked
# point_lon : numeric
#   longitude of the point that is being checked
#
# Returns
# -------
# boolean
#   a TRUE statement if the point is inside the bounds and a FALSE statement
#   otherwise
checkIfPointIsOutOfBounds <-
  function(bounds, point_lat, point_lon) {
    return (
      !(
        bounds$min_lat < point_lat
        && bounds$min_lon < point_lon
        && bounds$max_lat > point_lat
        && bounds$max_lon > point_lon
      )
    )
  }

# Creates spatial point from latitude and longitude
#
# Parameters
# ----------
# lat : numeric
#   latitude of the spatial point that is being created
# lon : numeric
#   longitude of the spatial point that is being created
#
# Returns
# -------
# SimpleFeatures
#   a spatial pint
createSFPoint <- function(lat, lon) {
  return(data.frame(lng = lon, lat = lat) %>%
           st_as_sf(coords = c("lng", "lat"), crs = 4326))
}

# Creates spatial grid with length and the width of the selected country.
#
# Parameters
# ----------
# country_name : string
#   name of the country for which the grid is being made
# cell_size : numeric
#   represents size of the cell in degrees
# worldmap : SimpleFeatures
#   world country polygons
#
# Returns
# -------
# SimpleFeatures
#   a spatial grid
createGrid <- function(country_name, cell_size, worldmap) {
  map <- worldmap[worldmap$name == country_name,]
  grid <- st_make_grid(map, crs = 4326, cellsize = cell_size) %>%
    st_sf('geometry' = ., data.frame('ID' = 1:length(.)))
  return(grid)
}

# Extracts maximum and minimum numbers from the spatial grid.
#
# Parameters
# ----------
# grid : SimpleFeatures
#   spatial grid
#
# Returns
# -------
# list
#   a list with the minimum longitude and maximum longitude, minimum latitude
#   and maximum latitude
getBounds <- function(grid) {
  lons <- c()
  lats <- c()
  first_last_cell <-
    c(grid[[2]][[1]], grid[[2]][[length(grid[[2]])]])
  for (i in first_last_cell) {
    m <- i[[1]]
    
    
    lons <- c(lons, m[, 1])
    lats <- c(lats, m[, 2])
  }
  return_list <- list(
    "min_lat" = min(lats),
    "min_lon" = min(lons),
    "max_lat" = max(lats),
    "max_lon" = max(lons)
  )
  return(return_list)
}

# Checks in which area is the cell
#
# Parameters
# ----------
# grid : SimpleFeatures
#   spatial grid
# test_point : SimpleFeatures
#   spatial point for testing if it is contained in SBF vector
# b_vector : vector
#   SBF vector
# H : numeric
#   List of hash functions
#
# Returns
# -------
# list
#   a list with a cell and a number of the area the test_point is located
checkIfPointIsInSBF <- function(grid, test_point, b_vector, H) {
  
  options(digits.secs = 6)
  start_time <- Sys.time()
  
  grid$has_test_point <-
    ifelse(sf::st_intersects(grid, test_point, sparse = F),
           "Yes",
           "No")
  
  end_time <- Sys.time()
  message(paste0("Time for check intersect:", as.numeric(difftime(end_time, start_time, units="secs")) * 1000))
  
  cell <- grid[which(grid$has_test_point == "Yes"),]
  result <- check(b_vector, H, cell$ID)
  return_list <- list("cell" = cell,
                      "result" = result)
  
  return(return_list)
}

# Calls checkIfPointIsInSBF function for each point in the test set and writes
# the results in the confusion matrix.
#
# Parameters
# ----------
# grid : SimpleFeatures
#   spatial grid
# n : numeric
#   number of sub-areas in area of interest
# S : data.frame
#   area for which SBF filter should be created
# H : list
#   List of hash functions
# b_vector : vector
#   SBF vector
# df_test_points : data.frame
#   dataframe with the information about points to test
#
# Returns
# -------
# matrix
#   a confusion matrix
checkIfPointsAreInSBF <-
  function(grid, n, S, H, b_vector, df_test_points) {
    confusion_mat <- matrix(0, nrow = n + 1, ncol =  n + 1)
    colnames(confusion_mat) <- 0:n
    rownames(confusion_mat) <- 0:n
    
    for (i in 1:length(df_test_points[, 1])) {
      options(digits.secs = 6)
      start_time <- Sys.time()
      
      cell_and_result <- checkIfPointIsInSBF(
        grid,
        createSFPoint(df_test_points[i,]$lat_test,
                      df_test_points[i,]$lon_test),
        b_vector,
        H
      )
      result <- cell_and_result$result
      correct_label <-
        S[which(cell_and_result$cell$ID == S$ID),]$label
      # If there is no such ID in S then it will return zero length vector.
      # That zero length vector needs to become 0
      if (length(correct_label) == 0)
        correct_label = 0
      
      confusion_mat[correct_label + 1, result + 1] <-
        confusion_mat[correct_label + 1, result + 1] + 1
      
      end_time <- Sys.time()
      message(paste0("Point :", i, " whole time:", as.numeric(difftime(end_time, start_time, units="secs")) * 1000))
    }
    
    df_confusion_mat <- as.data.frame(confusion_mat)
    colnames(confusion_mat) <- 0:n
    rownames(confusion_mat) <- 0:n
    
    return(df_confusion_mat)
  }

# Generates the test points based on normal distribution and with point of
# interest as center/mean value
#
# Parameters
# ----------
# num_rnd_points : numeric
#   number of test points to generate
# lon : numeric
#   longitude of the point of interest
# lat : numeric
#   latitude of the point of interest
# degRadius : numeric
#   radius in which we want the points to be generated, in degrees
#
# Returns
# -------
# data.frame
#   a data frame with random points generated with normal distribution centered
#   around point of interest
normGenerator <- function(num_rnd_points, lon, lat, degRadius) {
  # mean and standard deviation in degrees
  rlon <- rnorm(num_rnd_points, mean = lon, sd = degRadius)
  # mean and standard deviation in degrees
  rlat <-
    rnorm(num_rnd_points, mean = lat, sd = degRadius) 
  df_test_points <-
    cbind.data.frame(longitude = rlon, latitude = rlat)
  names(df_test_points)[1] <- "lon_test"
  names(df_test_points)[2] <- "lat_test"
  df_test_points$name <-
    paste("ID: ", 1:length(df_test_points[, 1]))
  return(df_test_points)
}

# Calls function for creating set of hash functions necessary for creation of
# the SBF filter and a function for creating SBF vector.
#
# Parameters
# ----------
# bounds : list
#   list of the minimum longitude and maximum longitude, minimum latitude and
#   maximum latitude
# num_rnd_points : numeric
#   number of test points to generate
#
# Returns
# -------
# data.frame
#   a data frame with random points generated with uniform distribution
uniformGenerator <- function(bounds, num_rnd_points) {
  polygon =
    list(matrix(
      c(
        bounds$min_lon,
        bounds$min_lat,
        bounds$max_lon,
        bounds$min_lat,
        bounds$max_lon,
        bounds$max_lat,
        bounds$min_lon,
        bounds$max_lat,
        bounds$min_lon,
        bounds$min_lat
      ),
      ncol = 2,
      byrow = T
    ))
  polygon = sf::st_polygon(polygon)
  test_points = sf::st_sample(polygon, size = num_rnd_points)
  df_test_points <-
    as.data.frame(as(test_points , "Spatial")@coords)
  names(df_test_points)[1] <- "lon_test"
  names(df_test_points)[2] <- "lat_test"
  df_test_points$name <-
    paste("ID: ", 1:length(df_test_points[, 1]))
  return(df_test_points)
}

# Calls uniformGenerator function or normGenerator function depending which
# distribution user decides to use
#
# Parameters
# ----------
# bounds : list
#   list of the minimum longitude and maximum longitude, minimum latitude and
#   maximum latitude
# num_rnd_points : numeric
#   number of test points to generate
# mode : string
#   name of the distribution
# center_lon : numeric
#   longitude of the point of interest
# center_lat : numeric
#   latitude of the point of interest
# degRadius : numeric
#   radius in which we want the points to be generated, in degrees
#
# Returns
# -------
# data.frame
#   a data.frame of either uniformly or normally generated random points
generateRandomPoints <- function(bounds,
                                 num_rnd_points,
                                 mode,
                                 center_lon,
                                 center_lat,
                                 degRadius) {
  if (mode == "Uniform") {
    return(uniformGenerator(bounds, num_rnd_points))
  }
  if (mode == "Normal") {
    return(normGenerator(num_rnd_points, center_lon, center_lat, degRadius))
  }
}

# Takes all the input from GUI and it prints results of thests back in GUI
#
# Parameters
# ----------
# df_test_points : data.frame
#   dataframe with the information about points to test
# grid : SimpleFeatures
#   spatial grid
# input : list
#   list-like object generated by Shiny, it contains all input elements
# output : list
#   list-like object generated by Shiny, it contains all output elements
# is_initial : boolean
#   set to false so it doesn't write F1_score and Confusion_marix csv filesat
#   the startup
testPoints <-
  function(df_test_points,
           grid,
           input,
           output,
           is_initial = FALSE) {
    # Translate points from dataframe to simple features object
    test_points <- st_as_sf(df_test_points,
                            coords = c("lon_test", "lat_test"),
                            crs = 4326)
    # Translate point of interest from lat and lon numbers to simple features
    # object
    poi_point <- createSFPoint(input$lat_poi, input$lon_poi)
    
    error <- tryCatch({
      # Create area of interest
      S <- coverageAOI(grid,
                       poi_point,
                       input$radius,
                       input$cell_size,
                       input$n)
      
      # Create sbf vector
      sbf_vector <- createSBF(input$k, input$algorithm, S, input$m)
      
      # Create spatial radius
      buffer <- st_buffer(poi_point, input$radius)
      
      # Show areas of interests and other spatial information on a map
      output$map <- renderTmap({
        tm_shape(S) +
          tm_fill(col = "label", alpha = 0.45) +
          tm_polygons("blue", alpha = 0) +
          tm_shape(buffer) + tm_borders("red") +
          tm_shape(poi_point) + tm_dots("red") +
          tm_shape(test_points) + tm_dots("blue") + tm_text("name", just = "top")
      })
      
      # Initialise output text
      output_text <- ""
      
      # In case there is less then 10 points write in text in which area each
      # point belongs to
      if (length(df_test_points[, 1]) < 10) {
        for (i in 1:length(df_test_points[, 1])) {
          output_text <- paste(
            output_text,
            "<br>",
            paste(
              "Label of test point= ",
              df_test_points[i,]$name,
              " is: ",
              # Return label of the test point
              checkIfPointIsInSBF(
                grid,
                createSFPoint(df_test_points[i,]$lat_test,
                              df_test_points[i,]$lon_test),
                sbf_vector$b_vector,
                sbf_vector$H
              )$result
            )
          )
        }
        output$SBF_label <- renderText({
          output_text
        })
      }
       
      # Create confusion matrix
      confusion_mat <- checkIfPointsAreInSBF(grid,
                                             input$n,
                                             S,
                                             sbf_vector$H,
                                             sbf_vector$b_vector,
                                             df_test_points)
      # Initialize table with for F1 values
      df_F1 <- data.frame(
        area = integer(),
        acc = double(),
        recall = double(),
        precision = double(),
        F1 = double(),
        type_1_error = double(),
        type_2_error = double()
      )
      
      # For each area of interest calculate micro F1, recall, precision,
      # type_1_error and type_2_error
      for (area in 1:(input$n + 1)) {
        print(area)
        TP <- 0
        TN <- 0
        FP <- 0
        FN <- 0
        TP <- TP + confusion_mat[area, area]
        df_tn <- confusion_mat[-area, -area]
        df_dim <- dim(df_tn)
        if (is.null(df_dim)) {
          TN <- sum(unlist(df_tn))
        } else {
          TN <- sum(unlist(colSums(df_tn)))
        }
        for (i in 1:(length(confusion_mat[1,]))) {
          if (i != area) {
            FP <- FP + confusion_mat[i, area]
            FN <- FN + confusion_mat[area, i]
          }
        }
        print(paste0("TP ", TP, "; TN ", TN, "; FP ", FP, "; FN ", FN))
        
        acc <- (TP + TN) / (TP + TN + FP + FN)
        recall <- TP / (TP + FN)
        precision <- TP / (TP + FP)
        F1_micro <- (2 * precision * recall) / (precision + recall)
        type_1_error <- FP / (FP + TN)
        type_2_error <- FN / (FN + TP)
        
        # edge cases which could cause division by 0
        if (TP == 0 & FP == 0 & FN == 0) {
          recall <- 1
          precision <- 1
          F1_micro <- 1
        } else if (TP == 0 &
                   ((FP == 0 & FN > 0) | (FP  > 0 & FN == 0))) {
          recall <- 0
          precision <- 0
          F1_micro <- 0
        }
        
        if (is.nan(type_1_error) & F1_micro == 1) {
          type_1_error = 0
        }
        if (is.nan(type_2_error) & F1_micro == 1) {
          type_2_error = 0
        }
        if (is.nan(type_1_error) & FN == 0 & TP == 0) {
          type_1_error = 0
        }
        if (is.nan(type_2_error) & FN == 0 & TP == 0) {
          type_2_error = 0
        }
        
        # Write all calculated values in dataframe
        df_F1[nrow(df_F1) + 1,] <- c(area - 1,
                                     acc,
                                     recall,
                                     precision,
                                     F1_micro,
                                     type_1_error,
                                     type_2_error)
      }
      
      # Calculate F1 makro
      df_F1_micro_colSums <- colSums(df_F1)
      df_F1$F1_makro <- df_F1_micro_colSums[[5]] / (input$n + 1)
      
      # Show confusion matrix
      output$confusion_matrix <- renderTable({
        confusion_mat[] <- lapply(confusion_mat, as.character)
        confusion_mat
      }, rownames = TRUE)
      
      # Show F1 table
      output$F1 <- renderTable({
        df_F1[] <- lapply(df_F1, as.character)
        df_F1
      }, rownames = FALSE)
      
      # If the run isn't first one after starting the app then save the 
      # confusion matrix and F1 results and histogram and matrix representation
      # of SBF vector
      if (!is_initial) {
        dir.create(".\\tmp")
        path <- paste0(".\\tmp\\",
                       str_replace_all(Sys.time(),
                                       c(
                                         "-" = "_",
                                         " " = "-",
                                         ":" = "_"
                                       )))
        dir.create(path)
        write.csv(confusion_mat,
                  paste0(path,
                         "\\Confusion_Matrix_.csv"),
                  row.names = FALSE,
        )
        write.csv(df_F1,
                  paste0(path,
                         "\\F1_score_.csv"),
                  row.names = FALSE, )
        saveParametersToCSV(input, output, path)
        
        # Create matrix representation of SBF vector
        b_vec_mat <- t(matrix(sbf_vector$b_vector, ncol = 10))
        # Save matrix representation of SBF vector as picture
        jpeg(file = paste0(path, "\\b_vec_plot.jpg"))
        plot(b_vec_mat, col = topo.colors, breaks = length(unique(sbf_vector$b_vector)))
        dev.off()
        # Save histogram of SBF vector as picture
        jpeg(file = paste0(path, "\\b_vec_hist.jpg"))
        hist(
          b_vec_mat,
          breaks = seq(min(b_vec_mat) - 0.5, max(b_vec_mat) + 0.5, by = 1),
          main = "Histogram oznaka u PBF-u",
          xlab = "Oznake",
          ylab = "Frekvencija pojavljivanja",
          labels = T
        )
        dev.off()
        # Save SBF vector
        write.csv(sbf_vector$b_vector,
                  paste0(path,
                         "\\SBF_vector_.csv"),
                  row.names = FALSE,
        )
      }
      
    },
    
    error = function(e)
    {
      printMessage(e, output, path)
    })
  }

# Prints error message in GUI when it can't put all sub-areas inside area of interest
#
# Parameters
# ----------
# e : error
#   error object
# output : list
#   list-like object generated by Shiny, it contains all output elements
# type_of_msg : character
#   describes whether the message is error, warning or information about success
printMessage <- function(message, output, type_of_msg) {
  if (type_of_msg == "error") {
    message(paste("Following error occured:\n", message))
    output$error_text <- renderText({
      paste("<font color=\"#FF0000\">",
            "Following error occured:\n",
            message)
    })
    if (class(message) != "character") {
      if (message$message == "replacement has 1 row, data has 0") {
        output$error_text <- renderText({
          paste(
            "<font color=\"#FF0000\">",
            "Please enter bigger radius,
                    lower number of areas or choose smaller size of cells"
          )
        })
      }
    }
  } else if (type_of_msg == "warning") {
    output$error_text <- renderText({
      paste("<font color=\"#FF0000\">", message)
    })
  } else if (type_of_msg == "sucess") {
    output$error_text <- renderText({
      paste("<font color=\"#01A66F\">", message)
    })
  }
}

# Creates a string to print on GUI when the point is out of bounds
#
# Parameters
# ----------
# point_name : string
#   name of the point which is out of bound
# bounds : string
#   which algorithm should be used for hashing
#
# Returns
# -------
# string
#   warning string
createWarningStringForOutOfBoundsPoint <-
  function(point_name, bounds) {
    paste(
      "<font color=\"#FF0000\">",
      "Please enter ",
      point_name,
      " between ",
      bounds$min_lat,
      " - ",
      bounds$max_lat,
      " of latitude and ",
      bounds$min_lon,
      " - ",
      bounds$max_lon,
      "of longitude"
    )
  }

readCSV <- function(path) {
  error <- tryCatch({
    parameters <- read.csv(path)
    return(parameters)
  },
  error = function(e)
  {
    printMessage(e, output, "error")
  })
}

# Creates initial test point when the app starts
#
# Parameters
# ----------
# input : list
#   list-like object generated by Shiny, it contains all input elements
# output : list
#   list-like object generated by Shiny, it contains all output elements
# session : object
#   The session object is an environment that can be used to access information and functionality relating to the session.
# worldmap : dataframe
#   Contains world country polygons at a specified scale, or points of tiny_countries
# parameters : dataframe
#   Contains saved parameters
renderInitialMap <-
  function(input,
           output,
           session,
           worldmap,
           parameters) {
    loadParametersFromCSV(input, output, session)
    
    grid <-
      createGrid(parameters$country, parameters$cell_size, worldmap)
    
    df_test_point <- data.frame(
      lat_test = parameters$lat_test,
      lon_test = parameters$lon_test,
      name = "test point"
    )
    
    input = list (
      "lon_poi" = parameters$lon_poi,
      "lat_poi" = parameters$lat_poi,
      "radius" = parameters$radius,
      "cell_size" = parameters$cell_size,
      "n" = parameters$n,
      "k" = parameters$k,
      "algorithm" = parameters$algorithm,
      "m" = parameters$m
    )
    
    testPoints(df_test_point, grid, input, output, TRUE)
  }

# Checks formatting inside CSV file
#
# Parameters
# ----------
# neccesary_fields : list
#   list of fields that the CSV file should contain
# csv_as_dataframe : dataframe
#   Content of CSV file as dataframe
# output : list
#   list-like object generated by Shiny, it contains all output elements
#
# Returns
# -------
# boolean
#   True if format is good, otherwise false
checkCSVFormat <- function(neccesary_fields, csv_as_dataframe, output){
  # If file doesn't have good format write error
  if (length(setdiff(neccesary_fields, colnames(csv_as_dataframe))) != 0){
    printMessage("CSV doesn't have good format", output, "warning")
    return(FALSE)
  }
  return(TRUE)
}

# Loads parameters from CSV file
#
# Parameters
# ----------
# input : list
#   list-like object generated by Shiny, it contains all input elements
# output : list
#   list-like object generated by Shiny, it contains all output elements
# session : object
#   The session object is an environment that can be used to access information and functionality relating to the session.
loadParametersFromCSV <- function(input, output, session) {
  observe({
    output$error_text <- renderText({
      
    })
    # Try to read file, if error write in app
    error <- tryCatch({
      parameters <- readCSV("parameters.csv")
      
      neccessary_parameters <- c(
        "country",
        "cell_size",
        "lon_poi",
        "lat_poi",
        "lat_test",
        "lon_test",
        "radius",
        "n",
        "algorithm",
        "k",
        "m",
        "num_rnd_points",
        "randomGenerationModes",
        "degRadius"
      )
      # If file doesn't have good formatting don't do anything
      if (checkCSVFormat(neccessary_parameters, parameters, output)){
        updateSelectInput(session, "country",
                          selected = parameters$country)
        updateNumericInput(session, "cell_size", value = parameters$cell_size)
        updateNumericInput(session, "lat_test", value = parameters$lat_test)
        updateNumericInput(session, "lon_test", value = parameters$lon_test)
        updateNumericInput(session, "lon_poi", value = parameters$lon_poi)
        updateNumericInput(session, "lat_poi", value = parameters$lat_poi)
        updateNumericInput(session, "radius", value = parameters$radius)
        updateNumericInput(session, "cell_size", value = parameters$cell_size)
        updateNumericInput(session, "n", value = parameters$n)
        updateNumericInput(session, "k", value = parameters$k)
        updateSelectInput(session,
                          "algorithm",
                          choices = algorithms,
                          selected = parameters$algorithm)
        updateNumericInput(session, "m", value = parameters$m)
        updateNumericInput(session,
                           "num_rnd_points",
                           value = parameters$num_rnd_points)
        updateSelectInput(
          session,
          "randomGenerationModes",
          choices = randomGenerationModes,
          selected = parameters$randomGenerationModes
        )
        updateNumericInput(session, "degRadius", value = parameters$degRadius)
        # print message if loading parameters successful
        printMessage("Parameters are loaded.", output, "sucess")
      }
      
    },
    error = function(e)
    {
      printMessage(e, output, "error")
      return()
    })
  })
}

# Saves parameters to CSV file
#
# Parameters
# ----------
# input : list
#   list-like object generated by Shiny, it contains all input elements
# output : list
#   list-like object generated by Shiny, it contains all output elements
# path : string
#   Location whre to save the file
saveParametersToCSV <- function(input, output, path) {
  parameters <- data.frame(
    "country" = input$country,
    "cell_size" = input$cell_size,
    "lat_test" = input$lat_test,
    "lon_test" = input$lon_test,
    "lon_poi" = input$lon_poi,
    "lat_poi" = input$lat_poi,
    "radius" = input$radius,
    "n" = input$n,
    "k" = input$k,
    "algorithm" = input$algorithm,
    "m" = input$m,
    "num_rnd_points" = input$num_rnd_points,
    "randomGenerationModes" = input$randomGenerationModes,
    "degRadius" = input$degRadius
  )
  
  output$error_text <- renderText({})
  error <- tryCatch({
    write.csv(parameters, paste0(path, "\\parameters.csv"), row.names = FALSE)
    printMessage("Parameters are saved.", output, "sucess")
  },
  error = function(e)
  {
    printMessage(e, output, "error")
  })
}

# Cleans the Error text and SBF label fields, and also confusion matrix table
#
# Parameters
# ----------
# output : list
#   list-like object generated by Shiny, it contains all output elements
cleanOutputs <- function(output) {
  output$error_text <- renderText({
    
  })
  output$SBF_label <- renderText({
    
  })
  output$confusion_matrix <- renderTable({
    
  })
}