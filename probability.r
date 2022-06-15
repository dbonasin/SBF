calculateP <- function(k, m, n){
  return((1 - exp(-k / (m / n)))^k)
}

calculateK_M <- function(n, p){
  m <- ceiling((n * log(p)) / log(1 / 2^log(2)))
  k <- round((m / n) * log(2))
  tmp_list <- list("m"=m, "k"=k)
  return(tmp_list)
}