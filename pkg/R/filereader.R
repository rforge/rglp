## Reads linear programs from MPS files
## uses GLPK's facilities for reading these files
## Interface to GLPK's MPS file reader

## Input: Path to a file specifying a mathematical program (MP),
##        the model specification language


## Output: an object of class 'MP_data_from_file' describing the MP 

## Mathematical Programming (MP) Data Object
##$file                     ... absolute path to data file 
##$type                     ... file type (currently 'MPS-fixed', 'MPS-free', 'CPLEX LP'
##$objective_coefficients   ... a vector of the objective coefficients
##$constraint_matrix        ... specifies the constraint matrixin simple triplet form
##$direction_of_constraints ... contains the direction of constraints
##$right_hand_side          ... vector of right hand side values
##$objective_var_is_integer ... a vector of logicals specifying which objective variable is of type 'integer'
##$objective_var_is_binary  ... a vector of logicals specifying which objective variable is of type 'binary'
##$direction_of_optimization... can be either 'min' or 'max'
##$bounds                   ... upper and lower bounds of objective variables
##$n_objective_vars         ... number of objective variables    
##$n_integer_vars           ... number of variables which are of type 'integer'
##$n_binary_vars            ... number of variables which are of type 'binary'
##$n_constraints            ... number of constraints    
##$n_values_in_constraint_matrix ... number of values in constraint matrix
##$problem_name             ... name of the problem     
##


Rglpk_read_file <- function(file, type = c("MPS_fixed", "MPS_free", "CPLEX_LP"), verbose = FALSE){
  if(!file.exists(file))
    stop(paste("There is no file called", file, "!"))
  type <- match.arg(type)
  type_db <- c("MPS_fixed" = 1L,
               "MPS_free"  = 2L,
               "CPLEX_LP"  = 3L
               )
  type <- type_db[type]
  obj <- list(file = tools::file_path_as_absolute(file),
              type = type)
  meta_data <- glp_get_meta_data_from_file(obj, verbose)
  milp_data <- glp_retrieve_MP_from_file(meta_data, verbose)
  out <- glp_merge_MP_data(meta_data, milp_data)
  ## Post processing
  out$type <- names(type_db[type_db==out$type])
  class(out$bounds) <- c("bound_table", class(out$bounds))
  dir_db <- c("<" = 1L, "<=" = 2L, ">" = 3L, ">=" = 4L, "==" = 5L)
  out$direction_of_constraints <- names(dir_db[out$direction_of_constraints])
  class(out) <- "MP_data_from_file"
  out
}

## First parse file to get some meta data of the LP/MILP
## (number of constraints/objective variables, direction of optimization, ...)
glp_get_meta_data_from_file <- function(x, verbose = FALSE){
  res <- .C("Rglpk_read_file",
            file                          = as.character(x$file),
            type                          = as.integer(x$type),
            problem_name                  = character(1L),
            ## objective_function_name       = character(1L),
            direction_of_optimization     = integer(1L),
            n_constraints                 = integer(1L),         
            n_objective_vars              = integer(1L),
            n_values_in_constraint_matrix = integer(1L),
            n_integer_vars                = integer(1L),
            n_binary_vars                 = integer(1L),
            verbosity                     = as.integer(verbose),
            PACKAGE = "Rglpk")
  res
}

## Retrieve all missing elements of the LP/MILP
glp_retrieve_MP_from_file <- function(x, verbose = FALSE){
  res <- .C("Rglpk_retrieve_MP_from_file",
            file                     = as.character(x$file),
            type                     = as.integer(x$type),
            n_constraints            = x$n_constraints,         
            n_objective_vars         = x$n_objective_vars,
            ##n_values_in_constraint_matrix = x$n_values_in_constraint_matrix,
            ##n_integer_vars           = x$n_integer_vars,
            ##n_binary_vars            = x$n_binary_vars,
            objective_coefficients   = double(x$n_objective_vars),
            constraint_matrix_i      = integer(x$n_values_in_constraint_matrix),
            constraint_matrix_j      = integer(x$n_values_in_constraint_matrix),
            constraint_matrix_values = double(x$n_values_in_constraint_matrix),
            direction_of_constraints = integer(x$n_constraints),
            right_hand_side          = double(x$n_constraints),           
            objective_var_is_integer = integer(x$n_objective_vars),
            objective_var_is_binary  = integer(x$n_objective_vars),
            bounds_type              = integer(x$n_objective_vars),
            bounds_lower             = double(x$n_objective_vars),
            bounds_upper             = double(x$n_objective_vars),
            verbosity                = as.integer(verbose),
            PACKAGE = "Rglpk")
  ## lp_is_integer               = as.integer(lp_is_integer),
  res
}
                        
glp_merge_MP_data <- function(x, y){
  out <- list(objective_coefficients        = y$objective_coefficients,
              constraint_matrix             = glp_matrix(y$constraint_matrix_i,
                                                         y$constraint_matrix_j,
                                                         y$constraint_matrix_values),
              direction_of_constraints      = y$direction_of_constraints,
              right_hand_side               = y$right_hand_side,
              objective_var_is_integer      = as.logical(y$objective_var_is_integer),
              objective_var_is_binary       = as.logical(y$objective_var_is_binary),
              ## minimization if GLP_MIN (1L) or max if GLP_MAX (2L)
              maximize                      = x$direction_of_optimization == 2L,
              bounds                        = data.frame(type  = y$bounds_type,
                                                         lower = y$bounds_lower,
                                                         upper = y$bounds_upper),
              n_objective_vars              = x$n_objective_vars,
              n_integer_vars                = x$n_integer_vars,
              n_binary_vars                 = x$n_binary_vars,
              n_constraints                 = x$n_constraints,
              n_values_in_constraint_matrix = x$n_values_in_constraint_matrix,
              problem_name                  = x$problem_name,
              file                          = x$file,
              type                          = x$type
              )
  out
}
