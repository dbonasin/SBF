library(sf)
library(raster)
library(fasterize)
library(tmaptools)
library(rnaturalearth)

coverageAOI <- function(grid, point, radius, size_of_cell, partition_count){
  # buffer is circle around the center point
  buffer <- st_buffer(point, radius)
  
  rasterizedGrid <- fasterize::raster(grid, resolution=c(size_of_cell,size_of_cell), crs = 4326)

  # get X and Y from row and col numbers
  grid$X <- raster::colFromCell(rasterizedGrid, grid$ID)
  grid$Y <- raster::rowFromCell(rasterizedGrid, grid$ID)


  # intersection with buffer as a polygon
  grid$intersects_buffer <- ifelse(sf::st_intersects(grid, buffer, sparse = F),
                               "Yes",
                               "No")

  # does the cell have center
  grid$has_center <- ifelse(sf::st_intersects(grid, point, sparse = F),
                           "Yes",
                           "No")

  # Selecting only those cells which contain the buffer
  S <- grid[which(grid$intersects_buffer == "Yes"),]
  
  return(partitionAOI(manhattanDistanceAOI(S), partition_count))
}

manhattanDistanceAOI <- function(S){
  x <- S[which(S$has_center == "Yes"),]$X
  y <- S[which(S$has_center == "Yes"),]$Y
  
  S["manDist"] <- NA

  for (i in 1:nrow(S)) {
    S[i,]$manDist <- abs(x - S[i,]$X) + abs(y - S[i,]$Y)
  }
  return(S)
}

partitionAOI <- function(S, partition_count){
  S["label"] <- NA
  max_distance <- max(S$manDist)
  partition_length <- floor((max_distance + 1) / partition_count)
  m <- (max_distance + 1) %% partition_count
  
  for (i in 1:partition_count) {
    if (m != 0){
      S[which(S$manDist >= (max_distance - partition_length) & S$manDist <= max_distance),]$label <- i
      
      max_distance <- max_distance - (partition_length + 1)
      m <- m - 1
    } else {
      S[which(S$manDist >= (max_distance - partition_length + 1) & S$manDist <= max_distance),]$label <- i
      
      max_distance <- max_distance - partition_length
    }
  }
  return(S)
}

testAOI <- function(grid, size_of_cell, S){
  # It takes 80% of random cells from whole grid for testing
  num_of_rows <- nrow(grid)
  grid[sample(num_of_rows, num_of_rows * 0.8), ]
  
  grid["label"] <- 0
  
  # It transfers the labels for cells from S(they are in bloom filter) to grid
  # This way we can know which label they have in bloom filter
  common <- intersect(grid$ID, S$ID) 
  for (i in common) {
    grid[which(grid$ID == i),]$label <- S[which(S$ID == i),]$label
  }
  
  return(grid)
}