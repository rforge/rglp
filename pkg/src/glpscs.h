/* glpscs.h (segmented character string) */

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

#ifndef _GLPSCS_H
#define _GLPSCS_H

#include "glpdmp.h"

typedef struct SCS SCS;

#define SCS_SEG_SIZE 12
/* number of characters in one string segment */

struct SCS
{     /* segment of character string; the string itself is associated
         with its first segment */
      char c[SCS_SEG_SIZE];
      /* up to SCS_SEG_SIZE characters; the end of string is indicated
         by '\0' as usual; thus, if this segment does not contain '\0',
         there must be a next segment */
      SCS *next;
      /* pointer to the next segment of the string */
};

#define scs_new _glp_scs_new
SCS *scs_new(DMP *pool);
/* create segmented character string */

#define scs_set _glp_scs_set
SCS *scs_set(DMP *pool, SCS *x, const char *s);
/* assign value to segmented character string */

#define scs_get _glp_scs_get
char *scs_get(char *s, const SCS *x);
/* retrieve value of segmented character string */

#define scs_cmp _glp_scs_cmp
int scs_cmp(const SCS *x, const SCS *y);
/* compare segmented character strings */

#define scs_drop _glp_scs_drop
void scs_drop(DMP *pool, SCS *x);
/* delete segmented character string */

#endif

/* eof */
