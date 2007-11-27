## the R-ported GNU Linear Programming kit
## solve function --- C Interface

glp_solve <- function(objective_coef, constr_mat, rhs, directions, integers,
                      maximize = FALSE, verbose = FALSE){
  
  ## direction of optimization
  if(!is.logical(maximize))
    stop("'maximize' can be either TRUE or FALSE")
  direction_of_optimization <- 0
  if(maximize)
    direction_of_optimization <- 1
  ## verbosity flag
  if(!is.logical(verbose))
    stop("'verbose' can be either TRUE or FALSE")
  verb <- 0
  if(verbose)
    verb <- 1
  ## match direction of constraints
  n_of_constraints <- length(directions)
  direction_of_constraints <- glp_match_directions(directions, n_of_constraints)
  
  n_of_objective_vars <- length(objective_coef)

  constraint_matrix <- glp_matrix(constr_mat)

  ## do we have a mixed integer linear program?
  is_integer <- any(integers)

  ## call the C interface - this actually runs the solver
  x <- glp_call_interface(objective_coef, n_of_objective_vars, constraint_matrix$i,
                          constraint_matrix$j,constraint_matrix$v, length(constraint_matrix$v),
                          rhs, direction_of_constraints, n_of_constraints, is_integer,
                          integers, direction_of_optimization, verb)
  out <- list(optimum=NULL, solution=NULL)
  out$optimum <- x$lp_optimum
  out$solution <- x$lp_objective_vars_values
  out
}

## match relational operators to requested input
glp_match_directions <- function(directions, n){
  out <- integer(n)
  for(i in 1:n)
    switch(directions[i],
           "default"=stop("'dir' must be either '<', '<=', ..."),
           "<"  = out[i] <- 1L,
           "<=" = out[i] <- 2L,
           ">"  = out[i] <- 3L,
           ">=" = out[i] <- 4L,
           "==" = out[i] <- 5L
           )
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


glp_call_interface <- function(lp_objective_coefficients, lp_n_of_objective_vars,
                               lp_constraint_matrix_i, lp_constraint_matrix_j, lp_constraint_matrix_v,
                               lp_n_of_values_in_constraint_matrix, lp_right_hand_side,
                               lp_direction_of_constraints, lp_n_of_constraints, lp_is_integer,
                               lp_objective_var_is_integer, lp_direction_of_optimization, verbose){
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
            lp_optimum                  = double(1),
            lp_objective_vars_values    = double(lp_n_of_objective_vars),
            lp_verbosity                = as.integer(verbose),
            PACKAGE = "Rglp")
  out
}
