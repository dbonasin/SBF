# is_rnaturalearth_available <- require("dplyr")
# if (!is_rnaturalearth_available) install.packages("dplyr")
calculateP <- function(k, m, n){
  return((1 - exp(-k / (m / n)))^k)
}

# TODO: a-posteriori false positives probability for each area
# TODO: a-priori false positives probability for each area
# TODO: Computes the expected number of cells for each area
# TODO: Returns the sparsity (ratio of non zero cells to the total number of cells) for the entire SBF filter.
# TODO: Computes a-posteriori false positives probability for the filter.
# TODO: Computes a-priori false positives probability for the filter.

# Computes the expected emersion value for an area.
expectedAreaEmersion <- function(label, S, k, b_vector_size){
  # Area members are how many small deltas are in the set with the same label,
  nfill <- 0
  
  for (l in unique(S$label)) {
    nfill <- nfill + nrow(S[which(S$label == l),])
  }
  
  p <- 1 - (1/b_vector_size)
  return(p <- p^(k*nfill))
}
expectedAreaEmersion(3, S, k, length(b_vector))

# Computes the emersion value for an area.
areaEmersion <- function(label, S, k, b_vector, col_mat){
  # Area members are how many small deltas are in the set with the same label,
  # while area_cells are number of cells in bloom filter vector with same label 
  area_members <- nrow(S[which(S$label == label),])
  
  area_cells_freq <- as.data.frame(table(b_vector))
  colnames(area_cells_freq) <- c("label", "freq")
  area_cells <- area_cells_freq[which(area_cells_freq$label == label),]$freq
  
  self_collisions <- col_mat[label, label]
  
  cat("(",area_cells,"/((",area_members,"*",k,")-",self_collisions,"))")
  if (area_members == 0) {
    return(-1)
  } else {
    return(area_cells/((area_members*k)-self_collisions))
  }
}
print(areaEmersion(3, S, k, b_vector, col_mat))