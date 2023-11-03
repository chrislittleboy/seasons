smoothing <- function(v,f,n,sample){
  i <- 2
  while(i < n){
    f[i] <- getmode(v = c(v[i], v[i-1], v[i+1]), sample = sample)
    i <- i + 1
  }
  f[1] <- getmode(c(v[1], v[n], v[2]), sample = sample)
  f[n] <- getmode(c(v[n], v[n-1], v[1]), sample = sample)
  return(f)
}