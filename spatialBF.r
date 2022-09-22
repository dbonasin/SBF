source("hashGeneration.r")

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
insert <- function(S, H, size) {
  labels <- sort(unique(S$label))
  
  # initialise SBF vector
  b_vector <- integer(size)
  
  for (label in labels) {
    for (element in S[S$label == label, ]$ID) {
      for (h in H) {
        id <- as.integer(paste("0x",
                               substr(h(element),
                                      1,
                                      4),
                               sep = "")) %% size + 1
        b_vector[id] <- label
      }
    }
  }
  return(b_vector)
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
check <- function(b_vector, H, element) {
  i <- max(b_vector)
  size <- length(b_vector)
  for (h in H) {
    id <- as.integer(paste("0x",
                           substr(h(element),
                                  1,
                                  4),
                           sep = "")) %% size + 1
    if (b_vector[id] == 0) {
      return(0)
    } else {
      if (b_vector[id] < i)
        i <- b_vector[id]
    }
  }
  return(i)
}

# Calls function for crating set of hash functions necessary for creation of the SBF filter and
# a function for creating SBF vector.
#
# Parameters
# ----------
# k : numeric
#   number of hash algorithms
# algorithm : string
#   which algorithm should be used for hashing
# S : data.frame
#   area for which we want to create SBF vector
# m : numeric
#   size of the SBF vector
#
# Returns
# -------
# list
#   a list with SBF vector, collision matrix and set of hash functions used to create SBF filter
createSBF <- function(k, algorithm, S, m) {
  H <- generateHashSetSalts(k, algorithm)
  return_list <- list("b_vector" = insert(S, H, m), "H" = H)
  return(return_list)
}
