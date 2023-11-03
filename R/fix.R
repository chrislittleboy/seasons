fix <- function(v){
# initialise
  l <- length(unique(v))
  n <- length(v)
  f <- rep(0, n)
# run smoothing in ideal scenario
  f <- smoothing(v,f,n,sample = 0)
# check if works
  checks <- dochecks(f,l)
# if doesn't work, try different sensible iterations until it does work
  while(checks != 2){
    f <- rep(0, n)
    f <- smoothing(v,f,n,sample = 1)
    checks <- dochecks(f,l)
  }
# take the working fix, and try to see if it still works if the seasons are changed back to original clusters
  i <- 1
  while(i <= 12){
    newf <- f
    newf[i] <- v[i]
    checks <- dochecks(newf,l)
    f[i] <- ifelse(checks==2, newf[i], f[i])
    newf[i] <- ifelse(checks==2, v[i], f[i])
    i <- i + 1
  }
  return(f)
}
