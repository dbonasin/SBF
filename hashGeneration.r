is_digest_available <- require("digest")
if (!is_digest_available) install.packages("digest")

#Generate salts
generateSalts <- function(num_salts){
  result <- c()
  for (i in 1:num_salts) {
    result <- c(result, i)
  }
  return(result)
}

generateHashSet <- function(num_hashes){
  salts <- generateSalts(num_hashes)
  result <- lapply(salts, function(i) {force(i); function(area) return(digest(area, "murmur32", seed = i))})#murmur32 
  return(result)
}