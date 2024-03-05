replace.values <- function(search, replace, x){
  stopifnot(length(search) == length(replace))
  xnew <- replace[ match(x, search) ]
  takeOld <- is.na(xnew) & !is.na(x)
  xnew[takeOld] <- x[takeOld]
  return(xnew)
}