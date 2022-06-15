rm(list=ls())

# n represents how many areas of interest the program will make
n <- 6
k <- 100
m <- 58
# radius is in meters
radius <- 25000
# cell_size represents degrees of longitude and latitude
cell_size <- 0.1
p <- 0.01

# Calculating probability, optimal amount of hash functions and 
# optimal size of bloom filter vector
source("probability.r")
k_and_m <- calculateK_M(n, p)
calculated_p <- calculateP(k_and_m$k, k_and_m$m, n)


# Amfiteatar in Pula: 44.873084, 13.850165
long <- 13.850165
lati <- 44.873084

# Generate a set of hashes
###########################
source("hashGeneration.r")
H <- generateHashSet(k_and_m$k)

# Generate areas of interest
############################
source("AOIGeneration.r")
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
cro <- worldmap[worldmap$name == 'Croatia',]
point <- data.frame(lng = long, lat = lati) %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326)

S <- coverageAOI(cro, point, radius, cell_size, n)
buffer <- st_buffer(point, radius)

library(tmap)
tmap_mode("view")
tm_shape(S)+tm_fill(col="label", alpha = 0.45)+tm_polygons("blue", alpha = 0)+
  tm_shape(buffer)+tm_borders("red")+
  tm_shape(point)+tm_dots("red")

# Create SBF
########################
source("spatialBF.r")
b_vector <- insert(S,H,k_and_m$m)
b_vector
plot(b_vector)

# Check SBF
########################
# TODO: make apriori and posteriori calculations for each area. 
# As they did in python code, sbf.py lines: 697 - EOF (or maybe just probabilities, 778)
test <- function(S){
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
      cat("Guessed lable is lower than real label\nReal label: ", label, ", result: ", result, "\n")
      lower_label <- lower_label + 1
    }
    if(result != 0 & label == 0){
      cat("Guessed lable should be 0\nReal label: ", label, ", result: ", result, "\n")
      should_be_zero <- should_be_zero + 1
    }
    else if(result>label & result != 0){
      cat("Guessed lable is higher than real label\nReal label: ", label, ", result: ", result, "\n")
      higher_label <- higher_label + 1
    }
  }
  print(mat)
  cat("Correct", correct, "/", nrow(S), "\n")
  cat("Correct and in bloom filter:", correct_and_in_filter, "/", nrow(S[which(S$label > 0),]), "\n")
  cat("Guessed higher label", higher_label, "/", nrow(S), "\n")
  cat("Guessed lower label", lower_label, "/", nrow(S), "\n")
  cat("Should be 0:", should_be_zero, "/", nrow(S), "\n")
  # cat("\ncalculated_p: ", calculated_p, "\np in example:", (lower_label + should_be_zero) / nrow(S2))
}

S2 <- testAOI(cro, cell_size, S)
test(S2)