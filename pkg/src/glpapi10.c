/* glpapi10.c (basis factorization and simplex tableau routines) */

/***********************************************************************
*  This code is part of GLPK (GNU Linear Programming Kit).
*
*  Copyright (C) 2000, 01, 02, 03, 04, 05, 06, 07 Andrew Makhorin,
*  Department for Applied Informatics, Moscow Aviation Institute,
*  Moscow, Russia. All rights reserved. E-mail: <mao@mai2.rcnet.ru>.
*
*  GLPK is free software: you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  GLPK is distributed in the hope that it will be useful, but WITHOUT
*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
*  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
*  License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with GLPK. If not, see <http://www.gnu.org/licenses/>.
***********************************************************************/

#include "glpapi.h"

/***********************************************************************
*  NAME
*
*  glp_bf_exists - check if the basis factorization exists
*
*  SYNOPSIS
*
*  int glp_bf_exists(glp_prob *lp);
*
*  RETURNS
*
*  If the basis factorization for the current basis associated with
*  the specified problem object exists and therefore is available for
*  computations, the routine glp_bf_exists returns non-zero. Otherwise
*  the routine returns zero. */

int glp_bf_exists(glp_prob *lp)
{     int ret;
      ret = (lp->m == 0 || lp->valid);
      return ret;
}

/***********************************************************************
*  NAME
*
*  glp_factorize - compute the basis factorization
*
*  SYNOPSIS
*
*  int glp_factorize(glp_prob *lp);
*
*  DESCRIPTION
*
*  The routine glp_factorize computes the basis factorization for the
*  current basis associated with the specified problem object.
*
*  RETURNS
*
*  0  The basis factorization has been successfully computed.
*
*  GLP_EBADB
*     The basis matrix is invalid, i.e. the number of basic (auxiliary
*     and structural) variables differs from the number of rows in the
*     problem object.
*
*  GLP_ESING
*     The basis matrix is singular within the working precision.
*
*  GLP_ECOND
*     The basis matrix is ill-conditioned. */

static int b_col(void *info, int j, int ind[], double val[])
{     glp_prob *lp = info;
      int m = lp->m;
      GLPAIJ *aij;
      int k, len;
      xassert(1 <= j && j <= m);
      /* determine the ordinal number of basic auxiliary or structural
         variable x[k] corresponding to basic variable xB[j] */
      k = lp->bhead[j];
      /* build j-th column of the basic matrix, which is k-th column of
         the scaled augmented matrix (I | -R*A*S) */
      if (k <= m)
      {  /* x[k] is auxiliary variable */
         len = 1;
         ind[1] = k;
         val[1] = 1.0;
      }
      else
      {  /* x[k] is structural variable */
         len = 0;
         for (aij = lp->col[k-m]->ptr; aij != NULL; aij = aij->c_next)
         {  len++;
            ind[len] = aij->row->i;
            val[len] = - aij->row->rii * aij->val * aij->col->sjj;
         }
      }
      return len;
}

int glp_factorize(glp_prob *lp)
{     int m = lp->m;
      int n = lp->n;
      GLPROW **row = lp->row;
      GLPCOL **col = lp->col;
      int *bhead = lp->bhead;
      int j, k, stat, ret;
      /* invalidate the basis factorization */
      lp->valid = 0;
      /* build the basis header */
      j = 0;
      for (k = 1; k <= m+n; k++)
      {  if (k <= m)
         {  stat = row[k]->stat;
            row[k]->bind = 0;
         }
         else
         {  stat = col[k-m]->stat;
            col[k-m]->bind = 0;
         }
         if (stat == GLP_BS)
         {  j++;
            if (j > m)
            {  /* too many basic variables */
               ret = GLP_EBADB;
               goto fini;
            }
            bhead[j] = k;
            if (k <= m)
               row[k]->bind = j;
            else
               col[k-m]->bind = j;
         }
      }
      if (j < m)
      {  /* too few basic variables */
         ret = GLP_EBADB;
         goto fini;
      }
      /* try to factorize the basis matrix */
      if (m > 0)
      {  _glp_access_bfd(lp);
         xassert(lp->bfd != NULL);
         switch (bfd_factorize(lp->bfd, m, lp->bhead, b_col, lp))
         {  case 0:
               /* ok */
               break;
            case BFD_ESING:
               /* singular matrix */
               ret = GLP_ESING;
               goto fini;
            case BFD_ECOND:
               /* ill-conditioned matrix */
               ret = GLP_ECOND;
               goto fini;
            default:
               xassert(lp != lp);
         }
         lp->valid = 1;
      }
      /* factorization successful */
      ret = 0;
fini: /* bring the return code to the calling program */
      return ret;
}

/***********************************************************************
*  NAME
*
*  glp_bf_updated - check if the basis factorization has been updated
*
*  SYNOPSIS
*
*  int glp_bf_updated(glp_prob *lp);
*
*  RETURNS
*
*  If the basis factorization has been just computed from scratch, the
*  routine glp_bf_updated returns zero. Otherwise, if the factorization
*  has been updated one or more times, the routine returns non-zero. */

int glp_bf_updated(glp_prob *lp)
{     int cnt;
      if (!(lp->m == 0 || lp->valid))
         xfault("glp_bf_update: basis factorization does not exist\n");
      cnt = (lp->m == 0 ? 0 : lp->bfd->upd_cnt);
      return cnt;
}

/***********************************************************************
*  NAME
*
*  glp_get_bfcp - retrieve basis factorization control parameters
*
*  SYNOPSIS
*
*  void glp_get_bfcp(glp_prob *lp, glp_bfcp *parm);
*
*  DESCRIPTION
*
*  The routine glp_get_bfcp retrieves control parameters, which are
*  used on computing and updating the basis factorization associated
*  with the specified problem object.
*
*  Current values of control parameters are stored by the routine in
*  a glp_bfcp structure, which the parameter parm points to. */

void glp_get_bfcp(glp_prob *lp, glp_bfcp *parm)
{     glp_bfcp *bfcp = lp->bfcp;
      if (bfcp == NULL)
      {  parm->type = GLP_BF_FT;
         parm->lu_size = 0;
         parm->piv_tol = 0.10;
         parm->piv_lim = 4;
         parm->suhl = GLP_ON;
         parm->eps_tol = 1e-15;
         parm->max_gro = 1e+10;
         parm->nfs_max = 50;
         parm->upd_tol = 1e-6;
         parm->nrs_max = 50;
         parm->rs_size = 0;
      }
      else
         memcpy(parm, bfcp, sizeof(glp_bfcp));
      return;
}

/***********************************************************************
*  NAME
*
*  glp_set_bfcp - change basis factorization control parameters
*
*  SYNOPSIS
*
*  void glp_set_bfcp(glp_prob *lp, const glp_bfcp *parm);
*
*  DESCRIPTION
*
*  The routine glp_set_bfcp changes control parameters, which are used
*  by internal GLPK routines in computing and updating the basis
*  factorization associated with the specified problem object.
*
*  New values of the control parameters should be passed in a structure
*  glp_bfcp, which the parameter parm points to.
*
*  The parameter parm can be specified as NULL, in which case all
*  control parameters are reset to their default values. */

static void copy_bfcp(glp_prob *lp)
{     glp_bfcp _parm, *parm = &_parm;
      BFD *bfd = lp->bfd;
      glp_get_bfcp(lp, parm);
      xassert(bfd != NULL);
      bfd->type = parm->type;
      bfd->lu_size = parm->lu_size;
      bfd->piv_tol = parm->piv_tol;
      bfd->piv_lim = parm->piv_lim;
      bfd->suhl = parm->suhl;
      bfd->eps_tol = parm->eps_tol;
      bfd->max_gro = parm->max_gro;
      bfd->nfs_max = parm->nfs_max;
      bfd->upd_tol = parm->upd_tol;
      bfd->nrs_max = parm->nrs_max;
      bfd->rs_size = parm->rs_size;
      return;
}

void glp_set_bfcp(glp_prob *lp, const glp_bfcp *parm)
{     glp_bfcp *bfcp = lp->bfcp;
      if (parm == NULL)
      {  /* reset to default values */
         if (bfcp != NULL)
            xfree(bfcp), lp->bfcp = NULL;
      }
      else
      {  /* set to specified values */
         if (bfcp == NULL)
            bfcp = lp->bfcp = xmalloc(sizeof(glp_bfcp));
         memcpy(bfcp, parm, sizeof(glp_bfcp));
         if (!(bfcp->type == GLP_BF_FT || bfcp->type == GLP_BF_BG ||
               bfcp->type == GLP_BF_GR))
            xfault("glp_set_bfcp: type = %d; invalid parameter\n",
               bfcp->type);
         if (bfcp->lu_size < 0)
            xfault("glp_set_bfcp: lu_size = %d; invalid parameter\n",
               bfcp->lu_size);
         if (!(0.0 < bfcp->piv_tol && bfcp->piv_tol < 1.0))
            xfault("glp_set_bfcp: piv_tol = %g; invalid parameter\n",
               bfcp->piv_tol);
         if (bfcp->piv_lim < 1)
            xfault("glp_set_bfcp: piv_lim = %d; invalid parameter\n",
               bfcp->piv_lim);
         if (!(bfcp->suhl == GLP_ON || bfcp->suhl == GLP_OFF))
            xfault("glp_set_bfcp: suhl = %d; invalid parameter\n",
               bfcp->suhl);
         if (!(0.0 <= bfcp->eps_tol && bfcp->eps_tol <= 1e-6))
            xfault("glp_set_bfcp: eps_tol = %g; invalid parameter\n",
               bfcp->eps_tol);
         if (bfcp->max_gro < 1.0)
            xfault("glp_set_bfcp: max_gro = %g; invalid parameter\n",
               bfcp->max_gro);
         if (!(1 <= bfcp->nfs_max && bfcp->nfs_max <= 32767))
            xfault("glp_set_bfcp: nfs_max = %d; invalid parameter\n",
               bfcp->nfs_max);
         if (!(0.0 < bfcp->upd_tol && bfcp->upd_tol < 1.0))
            xfault("glp_set_bfcp: upd_tol = %g; invalid parameter\n",
               bfcp->upd_tol);
         if (!(1 <= bfcp->nrs_max && bfcp->nrs_max <= 32767))
            xfault("glp_set_bfcp: nrs_max = %d; invalid parameter\n",
               bfcp->nrs_max);
         if (bfcp->rs_size < 0)
            xfault("glp_set_bfcp: rs_size = %d; invalid parameter\n",
               bfcp->nrs_max);
         if (bfcp->rs_size == 0)
            bfcp->rs_size = 20 * bfcp->nrs_max;
      }
      if (lp->bfd != NULL) copy_bfcp(lp);
      return;
}

BFD *_glp_access_bfd(glp_prob *lp)
{     /* access the basis factorization object */
      if (lp->bfd == NULL)
         lp->bfd = bfd_create_it(), copy_bfcp(lp);
      return lp->bfd;
}

/***********************************************************************
*  NAME
*
*  glp_get_bhead - retrieve the basis header information
*
*  SYNOPSIS
*
*  int glp_get_bhead(glp_prob *lp, int k);
*
*  DESCRIPTION
*
*  The routine glp_get_bhead returns the basis header information for
*  the current basis associated with the specified problem object.
*
*  RETURNS
*
*  If xB[k], 1 <= k <= m, is i-th auxiliary variable (1 <= i <= m), the
*  routine returns i. Otherwise, if xB[k] is j-th structural variable
*  (1 <= j <= n), the routine returns m+j. Here m is the number of rows
*  and n is the number of columns in the problem object. */

int glp_get_bhead(glp_prob *lp, int k)
{     if (!(lp->m == 0 || lp->valid))
         xfault("glp_get_bhead: basis factorization does not exist\n");
      if (!(1 <= k && k <= lp->m))
         xfault("glp_get_bhead: k = %d; index out of range\n", k);
      return lp->bhead[k];
}

/***********************************************************************
*  NAME
*
*  glp_get_row_bind - retrieve row index in the basis header
*
*  SYNOPSIS
*
*  int glp_get_row_bind(glp_prob *lp, int i);
*
*  RETURNS
*
*  The routine glp_get_row_bind returns the index k of basic variable
*  xB[k], 1 <= k <= m, which is i-th auxiliary variable, 1 <= i <= m,
*  in the current basis associated with the specified problem object,
*  where m is the number of rows. However, if i-th auxiliary variable
*  is non-basic, the routine returns zero. */

int glp_get_row_bind(glp_prob *lp, int i)
{     if (!(lp->m == 0 || lp->valid))
         xfault("glp_get_row_bind: basis factorization does not exist\n"
            );
      if (!(1 <= i && i <= lp->m))
         xfault("glp_get_row_bind: i = %d; row number out of range\n",
            i);
      return lp->row[i]->bind;
}

/***********************************************************************
*  NAME
*
*  glp_get_col_bind - retrieve column index in the basis header
*
*  SYNOPSIS
*
*  int glp_get_col_bind(glp_prob *lp, int j);
*
*  RETURNS
*
*  The routine glp_get_col_bind returns the index k of basic variable
*  xB[k], 1 <= k <= m, which is j-th structural variable, 1 <= j <= n,
*  in the current basis associated with the specified problem object,
*  where m is the number of rows, n is the number of columns. However,
*  if j-th structural variable is non-basic, the routine returns zero.*/

int glp_get_col_bind(glp_prob *lp, int j)
{     if (!(lp->m == 0 || lp->valid))
         xfault("glp_get_col_bind: basis factorization does not exist\n"
            );
      if (!(1 <= j && j <= lp->n))
         xfault("glp_get_col_bind: j = %d; column number out of range\n"
            , j);
      return lp->col[j]->bind;
}

/***********************************************************************
*  NAME
*
*  glp_ftran - perform forward transformation (solve system B*x = b)
*
*  SYNOPSIS
*
*  void glp_ftran(glp_prob *lp, double x[]);
*
*  DESCRIPTION
*
*  The routine glp_ftran performs forward transformation, i.e. solves
*  the system B*x = b, where B is the basis matrix corresponding to the
*  current basis for the specified problem object, x is the vector of
*  unknowns to be computed, b is the vector of right-hand sides.
*
*  On entry elements of the vector b should be stored in dense format
*  in locations x[1], ..., x[m], where m is the number of rows. On exit
*  the routine stores elements of the vector x in the same locations.
*
*  SCALING/UNSCALING
*
*  Let A~ = (I | -A) is the augmented constraint matrix of the original
*  (unscaled) problem. In the scaled LP problem instead the matrix A the
*  scaled matrix A" = R*A*S is actually used, so
*
*     A~" = (I | A") = (I | R*A*S) = (R*I*inv(R) | R*A*S) =
*                                                                    (1)
*         = R*(I | A)*S~ = R*A~*S~,
*
*  is the scaled augmented constraint matrix, where R and S are diagonal
*  scaling matrices used to scale rows and columns of the matrix A, and
*
*     S~ = diag(inv(R) | S)                                          (2)
*
*  is an augmented diagonal scaling matrix.
*
*  By definition:
*
*     A~ = (B | N),                                                  (3)
*
*  where B is the basic matrix, which consists of basic columns of the
*  augmented constraint matrix A~, and N is a matrix, which consists of
*  non-basic columns of A~. From (1) it follows that:
*
*     A~" = (B" | N") = (R*B*SB | R*N*SN),                           (4)
*
*  where SB and SN are parts of the augmented scaling matrix S~, which
*  correspond to basic and non-basic variables, respectively. Therefore
*
*     B" = R*B*SB,                                                   (5)
*
*  which is the scaled basis matrix. */

void glp_ftran(glp_prob *lp, double x[])
{     int m = lp->m;
      GLPROW **row = lp->row;
      GLPCOL **col = lp->col;
      int i, k;
      /* B*x = b ===> (R*B*SB)*(inv(SB)*x) = R*b ===>
         B"*x" = b", where b" = R*b, x = SB*x" */
      if (!(m == 0 || lp->valid))
         xfault("glp_ftran: basis factorization does not exist\n");
      /* b" := R*b */
      for (i = 1; i <= m; i++)
         x[i] *= row[i]->rii;
      /* x" := inv(B")*b" */
      if (m > 0) bfd_ftran(lp->bfd, x);
      /* x := SB*x" */
      for (i = 1; i <= m; i++)
      {  k = lp->bhead[i];
         if (k <= m)
            x[i] /= row[k]->rii;
         else
            x[i] *= col[k-m]->sjj;
      }
      return;
}

/***********************************************************************
*  NAME
*
*  glp_btran - perform backward transformation (solve system B'*x = b)
*
*  SYNOPSIS
*
*  void glp_btran(glp_prob *lp, double x[]);
*
*  DESCRIPTION
*
*  The routine glp_btran performs backward transformation, i.e. solves
*  the system B'*x = b, where B' is a matrix transposed to the basis
*  matrix corresponding to the current basis for the specified problem
*  problem object, x is the vector of unknowns to be computed, b is the
*  vector of right-hand sides.
*
*  On entry elements of the vector b should be stored in dense format
*  in locations x[1], ..., x[m], where m is the number of rows. On exit
*  the routine stores elements of the vector x in the same locations.
*
*  SCALING/UNSCALING
*
*  See comments to the routine glp_ftran. */

void glp_btran(glp_prob *lp, double x[])
{     int m = lp->m;
      GLPROW **row = lp->row;
      GLPCOL **col = lp->col;
      int i, k;
      /* B'*x = b ===> (SB*B'*R)*(inv(R)*x) = SB*b ===>
         (B")'*x" = b", where b" = SB*b, x = R*x" */
      if (!(m == 0 || lp->valid))
         xfault("glp_btran: basis factorization does not exist\n");
      /* b" := SB*b */
      for (i = 1; i <= m; i++)
      {  k = lp->bhead[i];
         if (k <= m)
            x[i] /= row[k]->rii;
         else
            x[i] *= col[k-m]->sjj;
      }
      /* x" := inv[(B")']*b" */
      if (m > 0) bfd_btran(lp->bfd, x);
      /* x := R*x" */
      for (i = 1; i <= m; i++)
         x[i] *= row[i]->rii;
      return;
}

/* eof */
