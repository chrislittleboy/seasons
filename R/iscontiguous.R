iscontiguous <- function(v){
  l <- length(unique(v))
  n <- length(v)
  c <- rep(0, n)
  i <- 1
  while(i < n){
    c[i] <- ifelse(v[i] == v[i+1], 1,0)
    i <- i + 1
  }
  ifelse(v[n] == v[1],1,0)
  c[n] <- ifelse(v[n] == v[1],1,0)
  return(n-sum(c) == l)
}