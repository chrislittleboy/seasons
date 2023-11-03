getmode <- function(v,sample) {
  if(sample == 0){
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  }
  if(sample == 1){
    uniqv <- sample(unique(v), 3, replace = T)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  }
}
