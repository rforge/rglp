## the R-ported GNU Linear Programming kit
## solve function --- C Interface

Rglpk_solve <- function(obj, mat, dir, rhs, int = NULL, max = FALSE,
                      bounds = NULL,
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

  if(is.null(bounds))
    bounds <- list()
  #list(lower=list(NULL,NULL),upper=list(NULL,NULL))
  bounds <- glp_bounds(bounds, n_of_objective_vars)


  ## call the C interface - this actually runs the solver
  x <- glp_call_interface(obj, n_of_objective_vars, constraint_matrix$i,
                          constraint_matrix$j, constraint_matrix$v, length(constraint_matrix$v),
                          rhs, direction_of_constraints, n_of_constraints, is_integer,
                          integers, direction_of_optimization, bounds[,1],
                          bounds[,2], bounds[,3], verb)
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

## fixes the GLPK bound types given a data.frame with bounds
## GLP_FR 1 free variable
## GLP_LO 2 variable with lower bound
## GLP_UP 3 variable with upper bound
## GLP_DB 4 double-bounded variable
## GLP_FX 5 fixed variable
glp_fix_bound_type <- function(x){
  if(!inherits(x,"bound_table"))
    stop("'x' is not of class 'bound_table'")
  x$type <- ifelse(is.finite(x$lower),
                   ifelse(is.finite(x$upper), 4L, 3L),
                   ifelse(is.finite(x$upper), 2L, 1L))
  x$type[x$upper==x$lower] <- 5L
  x
}

## TODO: should be a generic function providing methods for
## different representations (e.g., a matrix, list of vectors, ...)
##                   
glp_bounds <- function(x, n){
  ## Input validation
  if(!is.list(x))
    stop("Bounds have to be of type list")

  ## Initialize default matrix
  bound_table <- expand.grid(type=rep.int(2L,n), upper=0.0, lower=Inf)
  class(bound_table) <- c("bound_table", class(bound_table))
  
  ## Lower bounds
  lower <- x$lower
  if(!is.null(lower)){
    if(!is.integer(lower[[1]]))
      stop("Bound indices have to be of type integer")
    if(length(lower[[1]]) != length(lower[[2]]))
      stop("Length of bound indices must be equal to the length of the corresponding bound values")
    if(any(lower[[1]]==Inf))
      stop("Lower bound cannot be 'Inf'")
    if(any(duplicated(lower[[1]])))
      stop("Duplicated entries in bound indices")
    if((max(lower[[1]]) > n))
      stop("Bound indices must not exceed number of objective variables") 
    ## if everything is OK set new lower bounds
    bound_table[lower[[1]],2] <- lower[[2]]
  }

  ## Upper bounds
  upper <- x$upper
  if(!is.null(upper)){
    if(!is.integer(upper[[1]]))
      stop("Bound indices have to be of type integer")
    if(length(upper[[1]]) != length(upper[[2]]))
      stop("Length of bound indices must be equal to the length of the corresponding bound values")
    if(any(upper[[1]]==-Inf))
      stop("Upper bound cannot be '-Inf'")
    if(any(duplicated(upper[[1]])))
      stop("Duplicated entries in bound indices")
    if((max(upper[[1]]) > n))
      stop("Bound indices must not exceed number of objective variables")
    ## so far, the same as with lower bounds but in addition we have to be
    ## sure that upper bounds are greater than or equal to lower bounds
    if(any(bound_table[upper[[1]],2] > upper[[2]]))
      stop("Upper bounds have to be greater than or equal to lower bounds")
    bound_table[upper[[1]],3] <- upper[[2]]
  }

  ## Fix bound types
  out <- glp_fix_bound_type(bound_table)
  out
}
  

glp_call_interface <- function(lp_objective_coefficients, lp_n_of_objective_vars,
                               lp_constraint_matrix_i, lp_constraint_matrix_j, lp_constraint_matrix_v,
                               lp_n_of_values_in_constraint_matrix, lp_right_hand_side,
                               lp_direction_of_constraints, lp_n_of_constraints, lp_is_integer,
                               lp_objective_var_is_integer, lp_direction_of_optimization,
                               lp_bounds_type, lp_bounds_lower, lp_bounds_upper,
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
            lp_bounds_type              = as.integer(lp_bounds_type),
            lp_bounds_lower             = as.double(lp_bounds_lower),
            ## lp_n_of_bounds_l            = as.integer(length(lp_lower_bounds_i)),
            lp_bounds_upper             = as.double(lp_bounds_upper), 
            ## lp_n_of_bounds_u            = as.integer(length(lp_upper_bounds_i)),
            lp_optimum                  = double(1),
            lp_objective_vars_values    = double(lp_n_of_objective_vars),
            lp_verbosity                = as.integer(verbose),
            lp_status                   = integer(1),
            NAOK = TRUE, PACKAGE = "Rglpk")
  out
}
