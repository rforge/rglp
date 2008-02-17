## Reads linear programs from MPS files
## uses GLPK's facilities for reading these files
## Interface to GLPK's MPS file reader

## Input: Path to MPS file, MPS file type, turn on/off verbosity
## Output: a list with elements describing the MILP 

Rglpk_read_MPS <- function(file, type = "Standard", verbose=FALSE) {
  if(!file.exists(file))
    stop(paste("There is no file", file, "!"))
  obj <- list(file = tools::file_path_as_absolute(file), type = type)
  meta_data <- glp_call_MPS_reader(obj, verbose)
  milp_data <- glp_retrieve_MILP_from_MPS(meta_data, verbose)
  out <- glp_merge_MPS_data(meta_data, milp_data)
  out
}

## First parse file to get core elements of the LP/MILP
## (number of constraints/objective variables, direction of optimization, ...)
glp_call_MPS_reader <- function(x, verbose = FALSE){
  res <- .C("Rglpk_read_MPS",
            file                          = as.character(x$file),
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
glp_retrieve_MILP_from_MPS <- function(x, verbose = FALSE){
  res <- .C("Rglpk_retrieve_MILP_from_MPS",
            MPS_file                 = as.character(x$file),
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
            verbosity = as.integer(verbose),
            PACKAGE = "Rglpk")
  ## lp_is_integer               = as.integer(lp_is_integer),
  res
}
                        
glp_merge_MPS_data <- function(x, y){
  out <- list(objective_coefficients        = y$objective_coefficients,
              constraint_matrix             = glp_matrix(y$constraint_matrix_i,
                                                         y$constraint_matrix_j,
                                                         y$constraint_matrix_values),
              direction_of_constraints      = y$direction_of_constraints,
              right_hand_side               = y$right_hand_side,
              objective_var_is_integer      = y$objective_var_is_integer,
              objective_var_is_binary       = y$objective_var_is_binary,
              direction_of_optimization     = x$direction_of_optimization,
              bounds                        = data.frame(type  = y$bounds_type,
                                                         lower = y$bounds_lower,
                                                         upper = y$bounds_upper),
              n_objective_vars              = x$n_objective_vars,
              n_integer_vars                = x$n_integer_vars,
              n_binary_vars                 = x$n_binary_vars,
              n_constraints                 = x$n_constraints,
              n_values_in_constraint_matrix = x$n_values_in_constraint_matrix,
              problem_name                  = x$problem_name,
              file                          = x$file
              )
  class(out) <- "MPS_data"
  out
}
