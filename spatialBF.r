is_sf_available <- require("digest")
if (!is_sf_available) install.packages("digest")
# TODO: calculate collisions
insert <- function(S, H, size){
  labels <- sort(unique(S$label))
  
  #initialise SBF vector
  b_vector <- integer(size)
  
  # collision matrix: rows is the old value and the columns is new value
  col_mat <- matrix(0, nrow = length(labels), ncol =  length(labels))
  colnames(col_mat) <- 1:length(labels)
  rownames(col_mat) <- 1:length(labels)
  
  for (label in labels) {
    for (element in S[S$label == label,]$ID) {
      for (h in H) {
        id <- as.integer(paste("0x",substr(h(element), 1, 4), sep=""))%%size + 1
        
        if(b_vector[id] != 0){
          col_mat[b_vector[id],label] <- col_mat[b_vector[id],label] + 1
        }
        
        b_vector[id] <- label
        
        if (id == 0) cat("id: ", id, "b_vector[id]: ", b_vector[id], "\n")
      }
    }
  }
  
  return_list <- list("b_vector" = b_vector, "col_mat" = col_mat)
  return(return_list)
}

check <- function(b_vector, H, element){
  i <- max(b_vector)
  size <- length(b_vector)
  for (h in H) {
    id <- as.integer(paste("0x",substr(h(element), 1, 4), sep=""))%%size + 1
    if (b_vector[id] == 0){
      return(0)
    } else {
      if (b_vector[id] < i) i <- b_vector[id]
       
    }
  }
  return(i)
}