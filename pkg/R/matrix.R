## Rglpk (sparse) matrix represenations

## glp_matrix is the representation of the constraint matrix for
## use in connection with the GLPK solver.
## This represenation can only be part of a complete MILP
## specification (a constraint matrix has at least one value
## in each row and column).

glp_matrix <-
function(i, j, v)
{
  out <- list(i = i, j = j, v = v)
  class(out) <- "glp_matrix"
  out
}

## Get right representation of constraint matrix called 'index form'

## A generic function which allows to take different dense and sparse
## representations of matrices.

as.glp_matrix <-
function(x)
  UseMethod("as.glp_matrix")

## No default representation.
as.glp_matrix.default <-
function(x)
 stop("There is no default method for constraint matrix representations.")

## Standard matrix representation -> index form
as.glp_matrix.matrix <-
function(x)
{
  if(!is.matrix(x))
    stop("'x' must be a matrix")
  out <- list(i = NULL, j = NULL, v = NULL)
  ## which coefficients not 0
  ind <- which(x != vector(typeof(x), 1L), arr.ind=TRUE)
  ## build the matrix (index form)
  out$i <- ind[, 1L]
  out$j <- ind[, 2L]
  out$v <- x[ind]
  class(out) <- "glp_matrix"
  out
}

as.glp_matrix.glp_matrix <- function(x)
  x

## FIXME: should this function reside in this package?
## simple triplet representaion -> index form
as.glp_matrix.simple_triplet_matrix <-
function(x)
{
  if(!inherits(x, "simple_triplet_matrix"))
    stop("'x' must be of class 'simple_triplet_matrix'")
  out <- list(i = x$i, j = x$j, v = x$v)
  class(out) <- "glp_matrix"
}
