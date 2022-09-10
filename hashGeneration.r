library(digest)
library(stringi)
library(data.table)
library(readr)

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
  return(stri_rand_strings(num_salts, 5, pattern = "[A-Za-z0-9]"))
}

generateHashSetSalts <- function(num_hashes, hash_alg){
  salts <- readSalts(num_hashes)
  H <- lapply(salts, function(salt, hash_alg) {force(salt); function(area) return(digest(c(area, salt), hash_alg))}, hash_alg=hash_alg)#murmur32
  return(H)
}