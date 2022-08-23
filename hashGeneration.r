library(digest)
library(data.table)
library(readr)

#Generate seeds
generateSeeds <- function(num_seeds){
  result <- c()
  for (i in 1:num_seeds) {
    result <- c(result, i)
  }
  return(result)
}

generateHashSet <- function(num_hashes){
  seeds <- generateSeeds(num_hashes)
  result <- lapply(seeds, function(i) {force(i); function(area) return(digest(area, "murmur32", seed = i))})#murmur32 
  return(result)
}

readSalts <- function(num_hashes){
  file <- "salts.txt"

  if (file.exists(file)) {
    salts <- readLines(file)

    if(length(salts) != num_hashes){
      salts <- generateSalts(num_hashes)
      fwrite(list(salts), file = file)
    }

    return(salts)
  } else {
    file.create(file)
    salts <- generateSalts(num_hashes)
    fwrite(list(salts), file = file)
    return(salts)
  }
}

# Generate salts
generateSalts <- function(num_salts){
  a <- do.call(paste0, replicate(5, sample(LETTERS, num_salts, TRUE), FALSE))
  return(paste0(a, sprintf("%04d", sample(9999, num_salts, TRUE)), sample(LETTERS, num_salts, TRUE)))
}

generateHashSetSalts <- function(num_hashes, hash_alg){
  salts <- readSalts(num_hashes)
  result <- lapply(salts, function(salt, hash_alg) {force(salt); function(area) return(digest(c(area, salt), hash_alg))}, hash_alg=hash_alg)#murmur32
  return(result)
}