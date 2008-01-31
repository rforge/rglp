## Miscellaneous functions used in this package

## Get the right representation of integer vector.
## This function takes an index vector and returns a vector of logicals
## with length 'n'.

glp_integers <-
function(x, n)
{
  if(!all(x <= n))
    stop("Indices must not exceed the number of objective coefficients.")
  out <- logical(n)
  out[x] <- TRUE
  out
}

print.MILP <- function(x){
  if(!inherits(x,"MILP"))
     stop("'x' must be of class 'MILP'")
  writeLines(paste("A mixed integer linear program with", x$n_objective_vars, "objective variables,"))
  ## writeLines(paste(x$n_integer_vars, "of which are integer and ", x$n_binary_vars, "are binary varibles."))
  writeLines(paste("This problem has", x$n_constraints, "constrains"))
}
