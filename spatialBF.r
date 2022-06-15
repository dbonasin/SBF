insert <- function(S, H, size){
  #initialise SBF vector
  b_vector <- integer(size)
  
  for (label in sort(unique(S$label))) {
    for (element in S[S$label == label,]$ID) {
      for (h in H) {
        id <- as.integer(paste("0x",substr(h(element), 1, 4), sep=""))%%size + 1
        b_vector[id] <- label
        if (id == 0) cat("id: ", id, "b_vector[id]: ", b_vector[id], "\n")
      }
    }
  }
  return(b_vector)
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