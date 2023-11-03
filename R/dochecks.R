dochecks <- function(f,l){
rightlength <- length(unique(f)) == l
contiguous <- iscontiguous(f)
sum(c(rightlength, contiguous))
}