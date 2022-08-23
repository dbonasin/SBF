# Check if point is inside bounds of the grid
# 
# Parameters
# ----------
# bounds : list
#   list of the minimum longitude and maximum longitude, minimum latitude and maximum latitude
# point_lat : numeric
#   latitude of the point that is being checked
# point_lon : numeric
#   longitude of the point that is being checked 
# 
# Returns
# -------
# boolean
#   a TRUE statement if the point is inside the bounds and a FALSE statement otherwise
checkIfPointIsOutOfBounds <- function(bounds, point_lat, point_lon){
  return (!(bounds$min_lat < point_lat
            && bounds$min_lon < point_lon
            && bounds$max_lat > point_lat
            && bounds$max_lon > point_lon))
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
createSFPoint <- function(lat, lon){
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
createGrid <- function(country_name, cell_size, worldmap){
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
#   a list with the minimum longitude and maximum longitude, minimum latitude and maximum latitude
getBounds <- function(grid){
  lons <- c()
  lats <- c()
  first_last_cell <- c(grid[[2]][[1]], grid[[2]][[length(grid[[2]])]])
  for (i in first_last_cell) {
    m <- i[[1]]
    
    
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

# Calls function for crating set of hash functions necessary for creation of the SBF filter and
# a function for creating SBF vector.
# 
# Parameters
# ----------
# k : numeric
#   number of hash algorithms
# algorithm : string
#   which algorithm should be used for hashing
# S : data.frame
#   area for which we want to create SBF vector
# m : numeric
#   size of the SBF vector 
# 
# Returns
# -------
# list
#   a list with SBF vector, collision matrix and set of hash functions used to create SBF filter
createSBF <- function(k, algorithm, S, m){
  H <- generateHashSetSalts(k, algorithm)
  
  b_vec_and_c_mat <- insert(S, H, m)
  return_list <- list("b_vector" = b_vec_and_c_mat$b_vector,
                      "col_mat" = b_vec_and_c_mat$col_mat,
                      "H" = H
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
checkIfPointIsInSBF <- function(grid, test_point, b_vector, H){
  grid$has_test_point <- ifelse(sf::st_intersects(grid, test_point, sparse = F),
                                "Yes",
                                "No")
  cell <- grid[which(grid$has_test_point == "Yes"),]
  result <- check(b_vector, H, cell$ID)
  
  return_list <- list("cell" = cell, 
                      "result" = result 
  )
  
  return(return_list)
}

# Calls checkIfPointIsInSBF function for each point in the test set and writes the results in the confusion matrix.
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
checkIfPointsAreInSBF <- function(grid, n, S, H, b_vector, df_test_points){
  
  confusion_mat <- matrix(0, nrow = n + 1, ncol =  n + 1)
  colnames(confusion_mat) <- 0:n
  rownames(confusion_mat) <- 0:n
  
  for (i in 1:length(df_test_points[,1])) {
    cell_and_result <- checkIfPointIsInSBF(grid, createSFPoint(df_test_points[i,]$lat_test, df_test_points[i,]$lon_test), b_vector, H)
    result <- cell_and_result$result
    correct_label <- S[which(cell_and_result$cell$ID == S$ID),]$label
    # If there is no such ID in S then it will return zero length vector.
    # That zero length vector needs to become 0
    if(length(correct_label) == 0) correct_label = 0
    
    confusion_mat[correct_label + 1, result + 1] <- confusion_mat[correct_label + 1, result + 1] + 1
  }
  
  return(confusion_mat)
}

# Generates the test points based on normal distribution and with point of interest as center/mean value
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
#   a data frame with random points generated with normal distribution centered around point of interest
normGenerator <- function(num_rnd_points, lon, lat, degRadius){
  rlon <- rnorm(num_rnd_points, mean=lon, sd=degRadius) # longitudinal degree error adjusted for latitude
  rlat <- rnorm(num_rnd_points, mean=lat, sd=degRadius) # mean and standard deviation in degrees
  df_test_points <- cbind.data.frame(longitude=rlon, latitude=rlat)
  names(df_test_points)[1] <- "lon_test"
  names(df_test_points)[2] <- "lat_test"
  df_test_points$name <- paste("ID: ",1:length(df_test_points[,1]))
  return(df_test_points)
}

# Calls function for creating set of hash functions necessary for creation of the SBF filter and
# a function for creating SBF vector.
# 
# Parameters
# ----------
# bounds : list
#   list of the minimum longitude and maximum longitude, minimum latitude and maximum latitude
# num_rnd_points : numeric
#   number of test points to generate
# 
# Returns
# -------
# data.frame
#   a data frame with random points generated with uniform distribution
uniformGenerator <- function(bounds, num_rnd_points){
  polygon =
    list(
      matrix(
        c(bounds$min_lon, bounds$min_lat, bounds$max_lon, bounds$min_lat, bounds$max_lon, bounds$max_lat, bounds$min_lon, bounds$max_lat, bounds$min_lon, bounds$min_lat),
        ncol=2, byrow=T
      )
    ) 
  polygon = sf::st_polygon(polygon)
  test_points = sf::st_sample(polygon, size=num_rnd_points)
  df_test_points <- as.data.frame(as(test_points , "Spatial")@coords)
  names(df_test_points)[1] <- "lon_test"
  names(df_test_points)[2] <- "lat_test"
  df_test_points$name <- paste("ID: ",1:length(df_test_points[,1]))
  return(df_test_points)
}

# Calls uniformGenerator function or normGenerator function depending which distribution user decides to use
# 
# Parameters
# ----------
# bounds : list
#   list of the minimum longitude and maximum longitude, minimum latitude and maximum latitude
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
generateRandomPoints <- function(bounds, num_rnd_points, mode, center_lon, center_lat, degRadius){
  if (mode == "Uniform") {
    return(uniformGenerator(bounds, num_rnd_points))
  }
  if (mode == "Normal"){
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
testPoints <- function(df_test_points, grid, input, output){
  
  test_points <- st_as_sf(df_test_points,
                          coords = c("lon_test", "lat_test"),
                          crs = 4326 
  )
  
  poi_point <- createSFPoint(input$lat_poi, input$lon_poi)
  
  error <- tryCatch(
    {
      S <- coverageAOI(grid,
                       poi_point,
                       input$radius,
                       input$cell_size,
                       input$n
      )
      
      sbf_vector <- createSBF(input$k, input$algorithm, S, input$m)

      buffer <- st_buffer(poi_point, input$radius)

      output$map <- renderTmap({
        tm_shape(S)+
          tm_fill(col="label", alpha = 0.45)+
          tm_polygons("blue", alpha = 0)+
          tm_shape(buffer)+tm_borders("red")+
          tm_shape(poi_point)+tm_dots("red")+
          tm_shape(test_points)+tm_dots("blue")+tm_text("name", just = "top")
      })

      if(length(df_test_points[,1]) < 10){
        output_text <- ""
        for (i in 1:length(df_test_points[,1])) {
          output_text <- paste(output_text,
                               "<br>",
                               paste("Label of test point= ",df_test_points[i,]$name ," is: ",
                                     checkIfPointIsInSBF(grid,
                                                         createSFPoint(df_test_points[i,]$lat_test, df_test_points[i,]$lon_test),
                                                         sbf_vector$b_vector,
                                                         sbf_vector$H
                                     )$result
                               )
          )
        }
        output$SBF_label <- renderText({output_text})
      }

      confusion_mat<- checkIfPointsAreInSBF(grid, input$n, S, sbf_vector$H, sbf_vector$b_vector, df_test_points)
      mode(confusion_mat) <- "integer"

      output$confusion_matrix <- renderTable({
        confusion_mat
      }, rownames = TRUE)
    },
    
    error = function(e)
    {
      printErrorMessage(e, output)
    }
  )
}

# Prints error message in GUI when it can't put all sub-areas inside area of interest
# 
# Parameters
# ----------
# e : error
#   error object
# output : list
#   list-like object generated by Shiny, it contains all output elements
printErrorMessage <- function(e, output){
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
createWarningStringForOutOfBoundsPoint <- function(point_name, bounds){
  
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
#TODO clean up
#TODO try to figure out where is biggest bottleneck
test_k_and_m <- function(input, grid){
  poi_point <- createSFPoint(input$lat_poi, input$lon_poi)
  message("Creating S\n")
  S <- coverageAOI(grid,
                   poi_point,
                   input$radius,
                   input$cell_size,
                   input$n
  )
  message("Creating S2\n")
  S2 <- testAOI(grid, input$cell_size, S)
  k_seq <- 1:30
  m_seq <- 5:10
  # k_seq <- 1:3
  # m_seq <- seq(500, 600, by=20)
  accuracy <- data.frame(id = 1:(length(k_seq) * length(m_seq)))
  id = 1
  
  message("Testing k and m\n")
  for (k in k_seq) {
    # Generate a set of hashes
    ###########################
    # source("hashGeneration.r")
    H <- generateHashSetSalts(k, input$algorithm)
    for (m in m_seq) {
      # Create SBF
      ########################
      # source("spatialBF.r")
      b_vec_and_c_mat <- insert(S,H,m)
      b_vector <- b_vec_and_c_mat$b_vector
      results <- test(S2, H, b_vector) # <- problem with S2, možda da stavim tu taj test i da stavim da koristi random točke
      
      accuracy[id, "num_of_k"] <- k
      accuracy[id, "size_of_vector"] <- 2^m
      accuracy[id, "diff"] <- results$accuracy
      accuracy[id, "lower_label"] <- results$lower_label
      accuracy[id, "higher_label"] <- results$higher_label
      accuracy[id, "should_be_zero"] <- results$should_be_zero
      
      id <- id + 1
    }
  }
  message("Writing results in file\n")
  write.csv(accuracy,paste("[MAD_method]grid_search_for_k_and_m(radius=",input$radius,", algorithm=",input$algorithm,").csv", sep=""), row.names = FALSE)
  message("Done\n")
}

test <- function(S, H, b_vector){
  correct <- 0
  lower_label <- 0
  higher_label <- 0
  should_be_zero <- 0
  correct_and_in_filter <- 0
  
  mat <- matrix(0, nrow = n + 1, ncol =  n + 1)
  colnames(mat) <- 0:n
  rownames(mat) <- 0:n
  
  for (i in 1:nrow(S)) {
    label <- S[i,]$label
    
    check_start <- Sys.time()
    result <- check(b_vector, H, S[i,]$ID)
    check_end <- Sys.time()
    
    mat[label + 1,result + 1] <- mat[label + 1,result + 1] + 1
    if (result == label) correct <- correct + 1
    if (result == label & label > 0) correct_and_in_filter <- correct_and_in_filter + 1 
    
    if(result<label & result != 0){
      # cat("Guessed lable is lower than real label\nReal label: ", label, ", result: ", result, "\n")
      lower_label <- lower_label + 1
    }
    if(result != 0 & label == 0){
      # cat("Guessed lable should be 0\nReal label: ", label, ", result: ", result, "\n")
      should_be_zero <- should_be_zero + 1
    }
    else if(result>label & result != 0){
      # cat("Guessed lable is higher than real label\nReal label: ", label, ", result: ", result, "\n")
      higher_label <- higher_label + 1
    }
  }
  return_list <- list("accuracy" = nrow(S) - correct, "higher_label" = higher_label, "lower_label" = lower_label, "should_be_zero" = should_be_zero, "check_time" = difftime(check_end, check_start, units = "secs"[[1]]))
  return(return_list)
}
