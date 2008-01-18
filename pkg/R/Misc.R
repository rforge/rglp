## Miscellaneous functions used in this package

## get the right representation of integer vector
## this function takes an index vector and returns a vector of
## logicals with length 'n' 
glp_integers <- function(x, n){
  if(!all(x<=n))
    stop("At least one integer index is greater than the number of objective coefficients!")
  out <- logical(n)
  out[x] <- TRUE
  out
}
