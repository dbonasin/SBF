rm(list = ls())

library(sf)
library(tmap)
library(tmaptools)
library(rnaturalearth)

# n represents how many areas of interest the program will make
n <- 3
# k represents number of hash functions
k <- 9
# m represents size of the SBF vector
m <- 610
# radius is in meters
radius <- 25000
# cell_size represents degrees of longitude and latitude
cell_size <- 0.1

# Amfiteatar in Pula: 44.873084, 13.850165
long <- 13.850165
lati <- 44.873084

# Generate areas of interest
############################
source("AOIGeneration.r")
worldmap <- ne_countries(scale = 'medium',
                         type = 'map_units',
                         returnclass = 'sf')
map <- worldmap[worldmap$name == 'Croatia', ]

# making grid over the map and size of cell is expressed in degrees
grid <- st_make_grid(map, crs = 4326, cellsize = cell_size) %>%
  st_sf('geometry' = ., data.frame('ID' = 1:length(.)))

point <- data.frame(lng = long, lat = lati) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

S <- coverageAOI(grid, point, radius, cell_size, n)

# Draw areas of interest on interactive map
buffer <- st_buffer(point, radius)

tmap_mode("view")
tm_shape(grid) + tm_borders() +
  tm_shape(S) + tm_polygons("intersects_buffer",
                            alpha = 0.65,
                            legend.show = FALSE) +
  tm_shape(buffer) + tm_borders("red") +
  tm_shape(point) + tm_dots("red")

tmap_mode("view")
tm_shape(S) + tm_fill(alpha = 0.65) + tm_polygons(col = "blue", alpha = 0) +
  tm_text("manDist") +
  tm_shape(buffer) + tm_borders("red") +
  tm_shape(point) + tm_dots("red")

tmap_mode("view")
tm_shape(S) + tm_fill(alpha = 0.65) + tm_polygons(col = "blue", alpha = 0) +
  tm_text("label") +
  tm_shape(buffer) + tm_borders("red") +
  tm_shape(point) + tm_dots("red")

tmap_mode("view")
tm_shape(S) + tm_fill(col = "label", alpha = 0.45) + tm_polygons(col = "blue", alpha = 0) +
  tm_shape(buffer) + tm_borders("red") +
  tm_shape(point) + tm_dots("red")

# Test SBF
########################
test <- function(S, H, b_vector) {
  correct <- 0
  lower_label <- 0
  higher_label <- 0
  should_be_zero <- 0
  correct_and_in_filter <- 0
  
  mat <- matrix(0, nrow = n + 1, ncol =  n + 1)
  colnames(mat) <- 0:n
  rownames(mat) <- 0:n
  
  for (i in 1:nrow(S)) {
    label <- S[i, ]$label
    
    check_start <- Sys.time()
    result <- check(b_vector, H, S[i, ]$ID)
    check_end <- Sys.time()
    
    mat[label + 1, result + 1] <- mat[label + 1, result + 1] + 1
    if (result == label)
      correct <- correct + 1
    if (result == label &
        label > 0)
      correct_and_in_filter <- correct_and_in_filter + 1
    
    if (result < label & result != 0) {
      # cat("Guessed lable is lower than real label\nReal label: ", label, ", result: ", result, "\n")
      lower_label <- lower_label + 1
    }
    if (result != 0 & label == 0) {
      # cat("Guessed lable should be 0\nReal label: ", label, ", result: ", result, "\n")
      should_be_zero <- should_be_zero + 1
    }
    else if (result > label & result != 0) {
      # cat("Guessed lable is higher than real label\nReal label: ", label, ", result: ", result, "\n")
      higher_label <- higher_label + 1
    }
  }
  # print(mat)
  # cat("Correct", correct, "/", nrow(S), "\n")
  # cat("Correct and in bloom filter:", correct_and_in_filter, "/", nrow(S[which(S$label > 0),]), "\n")
  # cat("Guessed higher label", higher_label, "/", nrow(S), "\n")
  # cat("Guessed lower label", lower_label, "/", nrow(S), "\n")
  # cat("Should be 0:", should_be_zero, "/", nrow(S), "\n")
  # return(nrow(S) - correct)
  return_list <-
    list(
      "accuracy" = nrow(S) - correct,
      "higher_label" = higher_label,
      "lower_label" = lower_label,
      "should_be_zero" = should_be_zero,
      "check_time" = difftime(check_end, check_start, units = "secs"[[1]])
    )
  return(return_list)
}

num_of_tests <- 500
S2 <- testAOI(grid, cell_size, S)
algorithms <-
  c("murmur32",
    "md5",
    "sha512",
    'sha1',
    'sha256',
    'crc32',
    'xxhash32',
    'spookyhash')
alogrithms_delta_df <- data.frame(id = 1:num_of_tests)

for (algorithm in algorithms) {
  print(algorithm)
  v <- c()
  
  algorithms_distribution_df <- data.frame(id = 1:num_of_tests)
  for (i in 1:(m + 2)) {
    if (i < (m + 1)) {
      algorithms_distribution_df[as.character(i)] <- NA
    }
    else if (i == (m + 1)) {
      algorithms_distribution_df["insert_time"] <- NA
    }
    else if (i == (m + 2)) {
      algorithms_distribution_df["test_time"] <- NA
    }
  }
  
  for (i in 1:num_of_tests) {
    # Generate a set of hashes
    ###########################
    source("hashGeneration.r")
    H <- generateHashSetSalts(k, algorithm)
    
    # Create SBF
    ########################
    source("spatialBF.r")
    insert_start <- Sys.time()
    b_vector <- insert(S, H, m)
    insert_end <- Sys.time()
    
    insertion_counter <- b_vec_and_c_mat$insertion_counter
    
    test_results <- test(S2, H, b_vector)
    
    v <- c(v, test_results$accuracy)
    
    algorithms_distribution_df[i, ] <-
      c(
        i,
        insertion_counter,
        difftime(insert_end, insert_start, units = "secs"[[1]]),
        test_results$check_time
      )
  }
  write.csv(algorithms_distribution_df,
            paste(
              "distributions",
              paste(algorithm, "distribution_1.csv", sep = "_"),
              sep = "/"
            ),
            row.names = FALSE)
  matplot(t(subset(
    algorithms_distribution_df,
    select = -c(id, insert_time, test_time)
  )), type = "b")
  alogrithms_delta_df[algorithm] <- v
}

plot(
  alogrithms_delta_df$id,
  alogrithms_delta_df$murmur32,
  type = "l",
  col = 1,
  ylim = c(0, 20)
)
lines(alogrithms_delta_df$id,
      alogrithms_delta_df$md5,
      type = "l",
      col = 2)
lines(
  alogrithms_delta_df$id,
  alogrithms_delta_df$sha512,
  type = "l",
  col = 3
)
legend(
  2,
  20,
  legend = algorithms,
  col = 1:length(algorithms),
  lty = 1,
  cex = 0.8
)

write.csv(alogrithms_delta_df,
          "hash_algorithms_delta.csv",
          row.names = FALSE)

alogrithms_delta_df <- read.csv("hash_algorithms_delta.csv")
boxplot(
  alogrithms_delta_df$murmur32,
  alogrithms_delta_df$md5,
  alogrithms_delta_df$sha512,
  alogrithms_delta_df$sha1,
  alogrithms_delta_df$sha256,
  alogrithms_delta_df$crc32,
  alogrithms_delta_df$xxhash32,
  alogrithms_delta_df$spookyhash,
  names = algorithms,
  xlab = "hash algoritmi",
  ylab = "Broj krivo pogođenih polja"
)

# Trying different values for m and k
###########################################
k_seq <- 1:30
m_seq <- seq(100, 600, by = 20)
accuracy <- data.frame(id = 1:(length(k_seq) * length(m_seq)))
id = 1
for (k in k_seq) {
  # Generate a set of hashes
  ###########################
  source("hashGeneration.r")
  H <- generateHashSetSalts(k, "murmur32")
  for (m in m_seq) {
    # Create SBF
    ########################
    source("spatialBF.r")
    b_vector <- insert(S, H, m)
    results <- test(S2, H, b_vector)
    
    accuracy[id, "num_of_k"] <- k
    accuracy[id, "size_of_vector"] <- m
    accuracy[id, "diff"] <- results$accuracy
    accuracy[id, "lower_label"] <- results$lower_label
    accuracy[id, "should_be_zero"] <- results$should_be_zero
    
    cat("done with k=", k, ", m=", m, "\n")
    id <- id + 1
  }
}
write.csv(
  accuracy,
  paste("grid_search_for_k_and_m(radius=", radius, ").csv", sep = ""),
  row.names = FALSE
)
accuracy <- read.csv("grid_search_for_k_and_m.csv")
require("dplyr")
optimal <-
  accuracy %>% group_by(num_of_k) %>% filter(diff == 0) %>% slice_min(order_by = size_of_vector)
plot(optimal$num_of_k,
     optimal$size_of_vector,
     type = "l",
     col = 1)

# Printing sbf vector as matrix
###########################################

require("ggplot2")
# Load vector
paths =c("C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_09_28-23_41_34\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_09_28-23_42_31\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_09_28-23_43_09\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_09_29-00_00_44\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_09_29-00_01_10\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_09_29-00_12_43\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_09_29-00_12_58\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_10_11-23_23_21\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_10_11-23_25_18\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_10_23-21_18_34\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_10_23-21_20_03\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_10_23-21_21_34\\",
         "C:\\Users\\Daniel\\Desktop\\diplomski r code\\tmp\\2022_10_23-21_23_33\\"
         
        )

for (path in paths) {
  print(path)
  sbf_vec <- read.csv(paste0(path, "SBF_vector_.csv"))
  # Create matrix representation of SBF vector
  b_vec_mat <- t(matrix(sbf_vec$x, ncol = 10))
  # Save matrix representation of SBF vector as picture
  jpeg(file = paste0(path, "b_vec_plot2.jpg"))
  plot(b_vec_mat, col = topo.colors, breaks = length(unique(sbf_vec$x)),
       main = "Prikaz PBF vektora pomoću matrice.",
       xlab = "Stupac",
       ylab = "Red",
       labels=c("0","1","2","3"))
  # dev.off()
}

