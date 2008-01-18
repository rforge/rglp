## Rglpk (sparse) matrix represenations

## get right representation of constraint matrix called 'index form'

## a generic function which allows to take different dense
## and sparse representations of matrices
glp_matrix <- function(x){
  UseMethod("glp_matrix")
}

## no default represenation
glp_matrix.default <- function(x){
 stop("There is no default method for constraint matrix representations!")
}

## standard matrix representation -> index form
glp_matrix.matrix <- function(x){
  if(!is.matrix(x))
    stop("'x' must be a matrix")
  out <- list(i = NULL, j = NULL, v = NULL)
  ## which coefficients not 0
  ind <- which(x != vector(typeof(x), 1L), arr.ind=TRUE)
  ## build the matrix (index form)
  out$i <- ind[, 1L]
  out$j <- ind[, 2L]
  out$v <- x[ind]
  out
}

## FIXME: should this function reside in this package?
## simple triplet representaion -> index form
glp_matrix.simple_triplet_matrix <- function(x){
  if(!inherits("simple_triplet_matrix"))
    stop("'x' must be of class 'simple_triplet_matrix'")
  out <- list(i = x$i, j = x$j, v = x$v)
  out
}
