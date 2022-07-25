rm(list=ls())

package <- require("sf")
if (!package) install.packages("sf")
package <- require("tmap")
if (!package) install.packages("tmap")
package <- require("tmaptools")
if (!package) install.packages("tmaptools")
package <- require("rnaturalearth")
if (!package) install.packages("rnaturalearth")
package <- require("dplyr")
if (!package) install.packages("dplyr")

# n represents how many areas of interest the program will make
n <- 6
# k represents number of hash functions
k <- 10
# m represents size of the SBF vector
m <- 400
# radius is in meters
radius <- 25000
# cell_size represents degrees of longitude and latitude
cell_size <- 0.1

# Calculating probability, optimal amount of hash functions and 
# optimal size of bloom filter vector
# source("statistics.r")
# calculated_p <- calculateP(k, m, n)


# Amfiteatar in Pula: 44.873084, 13.850165
long <- 13.850165
lati <- 44.873084

# Generate areas of interest
############################
source("AOIGeneration.r")
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
map <- worldmap[worldmap$name == 'Croatia',]

# making grid over the map and size of cell is expressed in degrees
grid <- st_make_grid(map, crs = 4326, cellsize = cell_size) %>%
  st_sf('geometry' = ., data.frame('ID' = 1:length(.)))

point <- data.frame(lng = long, lat = lati) %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326)

S <- coverageAOI(grid, point, radius, cell_size, n)

# Draw areas of interest on interactive map
buffer <- st_buffer(point, radius)

tmap_mode("view")
tm_shape(S)+tm_fill(col="label", alpha = 0.45)+tm_polygons("blue", alpha = 0)+
  tm_shape(buffer)+tm_borders("red")+
  tm_shape(point)+tm_dots("red")

# Test SBF
########################
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
    result <- check(b_vector, H, S[i,]$ID)
    
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
  # print(mat)
  # cat("Correct", correct, "/", nrow(S), "\n")
  # cat("Correct and in bloom filter:", correct_and_in_filter, "/", nrow(S[which(S$label > 0),]), "\n")
  # cat("Guessed higher label", higher_label, "/", nrow(S), "\n")
  # cat("Guessed lower label", lower_label, "/", nrow(S), "\n")
  # cat("Should be 0:", should_be_zero, "/", nrow(S), "\n")
  # return(nrow(S) - correct)
  return_list <- list("accuracy" = nrow(S) - correct, "higher_label" = higher_label, "lower_label" = lower_label, "should_be_zero" = should_be_zero)
  return(return_list)
}

S2 <- testAOI(grid, cell_size, S)
algorithms <- c("murmur32", "md5", "sha512", 'sha1', 'sha256', 'crc32', 'xxhash32','spookyhash')
df <- data.frame(id = 1:500)
for (algorithm in algorithms) {
  print(algorithm)
  v <- c()
  for (i in 1:500) {
    # Generate a set of hashes
    ###########################
    source("hashGeneration.r")
    H <- generateHashSetSalts(k, algorithm)
    
    # Create SBF
    ########################
    source("spatialBF.r")
    b_vec_and_c_mat <- insert(S,H,m)
    b_vector <- b_vec_and_c_mat$b_vector
    # col_mat <- b_vec_and_c_mat$col_mat
    
    v <- c(v, test(S2, H, b_vector))
  }
  df[algorithm] <- v
}

plot(df$id, df$murmur32, type = "l", col = 1,ylim = c(0,20))
lines(df$id, df$md5, type = "l", col = 2)
lines(df$id, df$sha512, type = "l", col = 3)
legend(2, 20, legend=algorithms,
       col=1:length(algorithms), lty=1, cex=0.8)

write.csv(df,"hash_algorithms_delta.csv", row.names = FALSE)

boxplot(df$murmur32, df$md5, df$sha512, df$sha1, df$sha256, df$crc32, df$xxhash32, df$spookyhash, names = algorithms, xlab="hash algorithms",
        ylab="difference between all fields and correctly guessed fields")

# Trying different values for m and k
###########################################
k_seq <- 1:30
m_seq <- seq(10, 1010, by=100)
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
    b_vec_and_c_mat <- insert(S,H,m)
    b_vector <- b_vec_and_c_mat$b_vector
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
write.csv(accuracy,"grid_search_for_k_and_m.csv", row.names = FALSE)

require("dplyr")
optimal <- accuracy %>% group_by(num_of_k) %>% filter(diff == 0) %>% slice_min(order_by = size_of_vector)
plot(optimal$num_of_k, optimal$size_of_vector, type = "l", col = 1)