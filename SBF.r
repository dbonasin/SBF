library(digest)
# TODO odlučiti pustiti ovaj mad ili ne (možda dopustiti da je povećanje polja moguće samo za potenciju broja 2 i onda uštimati a i b)
# Creates a SBF vector
# 
# Parameters
# ----------
# S : data.frame
#   areas for which we want to create SBF vector
# H : list
#   lsit of hash functions
# size : numeric
#   size of the SBF vector
#
# Returns
# -------
# list
#   a list with SBF vector, collision matrix and vector of how many times it was written in each field
insert <- function(S, H, size){
  labels <- sort(unique(S$label))
  
  #initialise SBF vector
  b_vector <- integer(size)
  insertion_counter <- integer(size)
  
  # collision matrix: rows is the old value and the columns is new value
  col_mat <- matrix(0, nrow = length(labels), ncol =  length(labels))
  colnames(col_mat) <- 1:length(labels)
  rownames(col_mat) <- 1:length(labels)
  
  for (label in labels) {
    for (element in S[S$label == label,]$ID) {
      for (h in H) {
        # id <- as.integer(paste("0x",substr(h(element), 1, 4), sep=""))%%size + 1
        id <- mad_compress_function(h(element), size)
        if(b_vector[id] != 0){
          col_mat[b_vector[id],label] <- col_mat[b_vector[id],label] + 1
        }
        
        insertion_counter[id] <- insertion_counter[id] + 1
        b_vector[id] <- label
        
        if (id == 0) cat("id: ", id, "b_vector[id]: ", b_vector[id], "\n")
      }
    }
  }
  
  return_list <- list("b_vector" = b_vector, "col_mat" = col_mat, "insertion_counter" = insertion_counter)
  return(return_list)
}

# Checks if the element is in SBF vector
# 
# Parameters
# ----------
# b_vector : vector
#   SBF vector
# H : list
#   lsit of hash functions
# element : string
#   ID of the element
#
# Returns
# -------
# numeric
#   ID of the area that contains the element
check <- function(b_vector, H, element){
  i <- max(b_vector)
  size <- length(b_vector)
  for (h in H) {
    # id <- as.integer(paste("0x",substr(h(element), 1, 4), sep=""))%%size + 1
    id <- mad_compress_function(h(element), size)
    if (b_vector[id] == 0){
      return(0)
    } else {
      if (b_vector[id] < i) i <- b_vector[id]
       
    }
  }
  return(i)
}

