calculateP <- function(k, m, n){
  return((1 - exp(-k / (m / n)))^k)
}