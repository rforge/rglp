/* This is the GLPK C Interface
 */

#include "glpk.h"
#include <stdio.h>
#include <R.h>

// this is the solve function called from R
void R_glp_solve (int *lp_direction, int *lp_number_of_constraints,
		  int *lp_direction_of_constraints, double *lp_right_hand_side,
		  int *lp_number_of_objective_vars,
		  double *lp_objective_coefficients,
		  int *lp_objective_var_is_integer, int *lp_is_integer, //should
									//be
									//boolean 
		  int *lp_number_of_values_in_constraint_matrix,
		  int *lp_constraint_matrix_i, int *lp_constraint_matrix_j,
		  double *lp_constraint_matrix_values,
		  int *lp_lower_bounds_i, double *lp_lower_bounds_v,
		  int *lp_n_of_bounds_l,
		  int *lp_upper_bounds_i, double *lp_upper_bounds_v,
		  int *lp_n_of_bounds_u,
		  double *lp_optimum,
		  double *lp_objective_vars_values,
		  int *lp_verbosity, int *lp_status) {

  glp_prob *lp;
  int i, kl, ku;
  // create problem object 
  lp = glp_create_prob();

  // Turn on/off Terminal Output
  if(*lp_verbosity==1)
    glp_term_out(GLP_ON);
  else
    glp_term_out(GLP_OFF);
  
  // direction of optimization
  if(*lp_direction==1)
    glp_set_obj_dir(lp, GLP_MAX);
  else
    glp_set_obj_dir(lp, GLP_MIN);
  
  // is it a mixed integer problem? -- seems to be an R glpk function
    //if(lp_integer)
      //lpx_set_class(lp, LPX_MIP);
  // add rows to the problem object
  glp_add_rows(lp, *lp_number_of_constraints);
  for(i = 0; i < *lp_number_of_constraints; i++)
    switch(lp_direction_of_constraints[i]){
    case 1: 
      glp_set_row_bnds(lp, i+1, GLP_UP, 0.0, lp_right_hand_side[i]);
      break;
    case 2: 
      glp_set_row_bnds(lp, i+1, GLP_UP, 0.0, lp_right_hand_side[i]);
      break;
    case 3: 
      glp_set_row_bnds(lp, i+1, GLP_LO, lp_right_hand_side[i], 0.0);
      break;
    case 4: 
      glp_set_row_bnds(lp, i+1, GLP_LO, lp_right_hand_side[i], 0.0);
      break;
    case 5: 
      glp_set_row_bnds(lp, i+1, GLP_FX, lp_right_hand_side[i],
		       lp_right_hand_side[i]);
      break;
    }
  
  // add columns to the problem object
  glp_add_cols(lp, *lp_number_of_objective_vars);
  kl = ku = 0;
  for(i = 0; i < *lp_number_of_objective_vars; i++) {
        
    /* set column bounds
       we get 4 vectors containing indices and positions of bounds
       from R.
       we have to test if the index vectors are empty and apply
       different parts of the code on the remaining:
       -- if all are non-empty we have to look for
          GLP_FR (only the case when lower == -Inf and upper == Inf; same
	  index)
	  GLP_LO (if there is a lower but no upper bound; same index)
	  GLP_UP (if there is an upper but no lower bound; same index)
	  GLP_DB (if there is a lower and an upper bound; same index)
	  GLP_FX (if there is a lower and an upper bound, both have same
	  values) 
       -- if only one of them is non-empty we have to look for
          either GLP_LO or GLP_UP
       -- default 
          GLP_LO (between 0.0 and Inf)
     */
    if ((*lp_n_of_bounds_l > 0) && (*lp_n_of_bounds_u > 0))
      if (lp_lower_bounds_i[kl] == i+1) {
	if (lp_upper_bounds_i[ku] == i+1) {
	  if (lp_lower_bounds_v[kl] == lp_upper_bounds_v[ku]){
	    if(*lp_verbosity==1)
	      Rprintf("GLP_FX: %f \n", lp_lower_bounds_v[kl]);
	    glp_set_col_bnds(lp, i+1, GLP_FX, lp_lower_bounds_v[kl], 0.0);
	  }
	  else {
	    if((lp_lower_bounds_v[kl]==R_NegInf) && (lp_upper_bounds_v[ku]==R_PosInf)){
	      if(*lp_verbosity==1)
		Rprintf("GLP_FR: -oo, oo \n");
	      glp_set_col_bnds(lp, i+1, GLP_FR, 0.0, 0.0);
	    }
	    else{
	      if(*lp_verbosity==1)
		Rprintf("GLP_DB: %f, %f \n", lp_lower_bounds_v[kl], lp_upper_bounds_v[ku]);
	      glp_set_col_bnds(lp, i+1, GLP_DB, lp_lower_bounds_v[kl], lp_upper_bounds_v[ku]);
	    }
	  }
	  if (ku < *lp_n_of_bounds_u)
	    ku++;
	} else{
	  if(*lp_verbosity==1)
	    Rprintf("GLP_LO: %f \n", lp_lower_bounds_v[kl]);
	  glp_set_col_bnds(lp, i+1, GLP_LO, lp_lower_bounds_v[kl], 0.0);
	}
	if (kl < *lp_n_of_bounds_l)
	  kl++;
      } else if (lp_upper_bounds_i[ku] == i+1) {
	if(*lp_verbosity==1)
	  Rprintf("GLP_UP: %f \n", lp_lower_bounds_v[kl], lp_upper_bounds_v[ku]);
	glp_set_col_bnds(lp, i+1, GLP_UP, 0.0, lp_upper_bounds_v[ku]);
	if (ku < *lp_n_of_bounds_u)
	  ku++;
      } else {
      // default: 0 <= x < oo
	if(*lp_verbosity==1)
	  Rprintf("default double: GLP_LO: 0, oo \n");
	glp_set_col_bnds(lp, i+1, GLP_LO, 0.0, 0.0);
      }
    else if (*lp_n_of_bounds_l > 0)
      if (lp_lower_bounds_i[kl] == i+1) {
	if(*lp_verbosity==1)
	  Rprintf("GLP_LO only: %f \n", lp_lower_bounds_v[kl]);
	glp_set_col_bnds(lp, i+1, GLP_LO, lp_lower_bounds_v[kl], 0.0);
	if (kl < *lp_n_of_bounds_l)
	  kl++;
      }else {
	if(*lp_verbosity==1)
	  Rprintf("default only: GLP_LO: 0, oo \n");
	// default: 0 <= x < oo
	glp_set_col_bnds(lp, i+1, GLP_LO, 0.0, 0.0);
      }
    else if (*lp_n_of_bounds_u > 0)
      if (lp_upper_bounds_i[ku] == i+1) {
	if(*lp_verbosity==1)
	  Rprintf("GLP_UP only: %f \n", lp_upper_bounds_v[ku]);
	glp_set_col_bnds(lp, i+1, GLP_UP, 0.0, lp_upper_bounds_v[ku]);
	if (ku < *lp_n_of_bounds_u)
	  ku++;
      }else{
	if(*lp_verbosity==1)
	  Rprintf("default only: GLP_LO: 0, oo \n");
	// default: 0 <= x < oo
	glp_set_col_bnds(lp, i+1, GLP_LO, 0.0, 0.0);
      }
    else{
      if(*lp_verbosity==1)
	Rprintf("default nothing: GLP_LO: 0, oo \n");
      // default: 0 <= x < oo
      glp_set_col_bnds(lp, i+1, GLP_LO, 0.0, 0.0);
    }
    
    // set objective coefficients and integer if necessary
    glp_set_obj_coef(lp, i+1, lp_objective_coefficients[i]);
    if (lp_objective_var_is_integer[i])
      glp_set_col_kind(lp, i+1, GLP_IV);
  }
  // load the matrix
  // IMPORTANT: as glp_load_matrix requires triplets as vectors of the
  // form: ia[1] ... ia[n], we have to pass the pointer to the adress
  // [-1] of the corresponding vector 

  glp_load_matrix(lp, *lp_number_of_values_in_constraint_matrix,
		  &lp_constraint_matrix_i[-1],
                  &lp_constraint_matrix_j[-1], &lp_constraint_matrix_values[-1]);

  // run simplex method to solve linear problem
  glp_simplex(lp, NULL);
  
  // retrieve status of optimization
  *lp_status = glp_get_status(lp);
  // retrieve optimum
  *lp_optimum = glp_get_obj_val(lp);
  // retrieve values of objective vars
  for(i = 0; i < *lp_number_of_objective_vars; i++) {
    lp_objective_vars_values[i] = glp_get_col_prim(lp, i+1);
  }
  if(*lp_is_integer) {
    glp_intopt(lp, NULL);
    // retrieve status of optimization
    *lp_status = glp_mip_status(lp);
    
    // retrieve MIP optimum
    *lp_optimum = glp_mip_obj_val(lp);
    // retrieve MIP values of objective vars
    for(i = 0; i < *lp_number_of_objective_vars; i++){
      lp_objective_vars_values[i] = glp_mip_col_val(lp, i+1);
    }
  }
  // delete problem object
  glp_delete_prob(lp);
}


