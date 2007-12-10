## the R-ported GNU Linear Programming kit
## solve function --- C Interface

glp_solve <- function(obj, mat, dir, rhs, int = NULL, max = FALSE,
                      bounds = list(lower=list(NULL,NULL),upper=list(NULL,NULL)),
                      verbose = FALSE){
  
  ## direction of optimization
  if(!is.logical(max))
    stop("'max' can be either TRUE or FALSE")
  direction_of_optimization <- as.integer(max)

  ## verbosity flag
  if(!is.logical(verbose))
    stop("'verbose' can be either TRUE or FALSE")
  verb <- as.integer(verbose)
 
  ## match direction of constraints
  n_of_constraints <- length(dir)
  ## match relational operators to requested input
  direction_of_constraints <- match(dir, c("<", "<=", ">", ">=", "=="))
  if(any(is.na(direction_of_constraints)))
    stop("'dir' must be either '<', '<=', '>', '>=' or '=='!")
  
  n_of_objective_vars <- length(obj)

  constraint_matrix <- glp_matrix(mat)

  ## need a TRUE/FALSE integer representation
  integers <- glp_integers(int, n_of_objective_vars)
  ## do we have a mixed integer linear program?
  is_integer <- any(integers)

  bounds <- glp_bounds(bounds, n_of_objective_vars)
  ## call the C interface - this actually runs the solver
  x <- glp_call_interface(obj, n_of_objective_vars, constraint_matrix$i,
                          constraint_matrix$j, constraint_matrix$v, length(constraint_matrix$v),
                          rhs, direction_of_constraints, n_of_constraints, is_integer,
                          integers, direction_of_optimization, bounds$lower[[1L]],
                          bounds$lower[[2L]], bounds$upper[[1L]], bounds$upper[[2L]], verb)
  out <- list(optimum=NULL, solution=NULL)
  out$optimum <- x$lp_optimum
  out$solution <- x$lp_objective_vars_values
  ## match status of solution
  ## 0 -> optimal solution (5 in GLPK) else 1
  out$status <- as.integer(x$lp_status != 5L)
  out
}

## get right representation of constraint matrix 
glp_matrix <- function(x){
  UseMethod("glp_matrix")
}

glp_matrix.default <- function(x){
 stop("There is no method for this class of constraint matrix!")
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
  if(!(class(x)=="simple_triplet_matrix"))
    stop("'x' must be of class 'simple_triplet_matrix'")
  out <- list(i = x$i, j = x$j, v = x$v)
  out
}

## get right representation of integer vector
## index vector to logicals
glp_integers <- function(x, n){
  if(!all(x<=n))
    stop("At least one integer index is greater than the number of objective variables!")
  out <- logical(n)
  out[x] <- TRUE
  out
}

glp_bounds <- function(x, n){
  ## Input validation
  if(!is.list(x))
    stop("Bounds have to be of type list")
  if((length(x)!=2) || (is.null(x$upper)) || (is.null(x$lower)))
    stop("Bounds have to be of type list with 2 list elements called upper and lower")
#  if(!is.integer(x$lower[[1]]) || !is.integer(x$upper[[1]]))
#    stop("Bound indices have to be of type integer")
#  if((max(x$lower[[1]]) > n) || (max(x$upper[[1]]) > n))
#    stop("Bound indices must not exceed number of objective variables")
  if((length(x$lower[[1]]) != length(x$lower[[2]])) || (length(x$upper[[1]]) != length(x$upper[[2]])))
    stop("Length of bound indices must be equal to the length of the corresponding bound values")
  if(any(duplicated(x$lower[[1]])) || any(duplicated(x$upper[[1]])))
    stop("Duplicated entries in bound indices")
  if(any(x$lower[[1]]==Inf) || any(x$upper[[1]]==-Inf))
    stop("Lower bound cannot be 'Inf'; upper bound cannot be '-Inf'")
  ## reorder bounds, bound indices must be ascending
  if(!is.null(x$lower[[1]])){
    ord <- order(x$lower[[1]])
    x$lower[[1]] <- x$lower[[1]][ord]
    x$lower[[2]] <- x$lower[[2]][ord]
  }
  if(!is.null(x$upper[[1]])){
    ord <- order(x$upper[[1]])
    x$upper[[1]] <- x$upper[[1]][ord]
    x$upper[[2]] <- x$upper[[2]][ord]
  } 
  x
}
  

glp_call_interface <- function(lp_objective_coefficients, lp_n_of_objective_vars,
                               lp_constraint_matrix_i, lp_constraint_matrix_j, lp_constraint_matrix_v,
                               lp_n_of_values_in_constraint_matrix, lp_right_hand_side,
                               lp_direction_of_constraints, lp_n_of_constraints, lp_is_integer,
                               lp_objective_var_is_integer, lp_direction_of_optimization,
                               lp_lower_bounds_i, lp_lower_bounds_v, lp_upper_bounds_i, lp_upper_bounds_v,
                               verbose){
  out <- .C("R_glp_solve",
            lp_direction_of_optimization= as.integer(lp_direction_of_optimization),
            lp_n_of_constraints         = as.integer(lp_n_of_constraints),
            lp_direction_of_constraints = as.integer(lp_direction_of_constraints),
            lp_right_hand_side          = as.double(lp_right_hand_side),
            lp_n_of_objective_vars      = as.integer(lp_n_of_objective_vars),
            lp_objective_coefficients   = as.double(lp_objective_coefficients),
            lp_objective_var_is_integer = as.integer(lp_objective_var_is_integer),
            lp_is_integer               = as.integer(lp_is_integer),
            lp_n_of_values_in_constraint_matrix = as.integer(lp_n_of_values_in_constraint_matrix),
            lp_constraint_matrix_i      = as.integer(lp_constraint_matrix_i),
            lp_constraint_matrix_j      = as.integer(lp_constraint_matrix_j),
            lp_constraint_matrix_values = as.double(lp_constraint_matrix_v),
            lp_lower_bounds_i           = as.integer(lp_lower_bounds_i),
            lp_lower_bounds_v           = as.double(lp_lower_bounds_v),
            lp_n_of_bounds_l            = as.integer(length(lp_lower_bounds_i)),
            lp_upper_bounds_i           = as.integer(lp_upper_bounds_i), 
            lp_upper_bounds_v           = as.double(lp_upper_bounds_v),
            lp_n_of_bounds_u            = as.integer(length(lp_upper_bounds_i)),
            lp_optimum                  = double(1),
            lp_objective_vars_values    = double(lp_n_of_objective_vars),
            lp_verbosity                = as.integer(verbose),
            lp_status                   = integer(1),
            NAOK = TRUE, PACKAGE = "Rglp")
  out
}
