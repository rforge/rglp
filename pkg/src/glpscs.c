/* glpscs.c (segmented character string) */

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

#include "glpscs.h"

/***********************************************************************
*  NAME
*
*  scs_new - create segmented character string
*
*  SYNOPSIS
*
*  #include "glpscs.h"
*  SCS *scs_new(DMP *pool);
*
*  DESCRIPTION
*
*  The routine scs_new creates a new segmented character string, which
*  initially is empty.
*
*  The parameter pool specifies a dynamic memory pool used to store the
*  string segments.
*
*  RETURNS
*
*  The routine returns a pointer to the segmented string created. */

SCS *scs_new(DMP *pool)
{     SCS *x;
      x = dmp_get_atom(pool, sizeof(SCS));
      x->c[0] = '\0';
      x->next = NULL;
      return x;
}

/***********************************************************************
*  NAME
*
*  scs_set - assign value to segmented character string
*
*  SYNOPSIS
*
*  #include "glpscs.h"
*  SCS *scs_set(DMP *pool, SCS *x, const char *s);
*
*  DESCRIPTION
*
*  The routine scs_set copies the plain character string s to the
*  segmented character string x.
*
*  The parameter pool must specify the same dynamic memory pool, which
*  was used on creating the segmented character string.
*
*  RETURNS
*
*  The routine returns a pointer to the segmented character string. */

SCS *scs_set(DMP *pool, SCS *x, const char *s)
{     SCS *t, *tt;
      int i, j;
      /* copy characters */
      for (t = x, i = j = 0; ; i++)
      {  if ((t->c[j++] = s[i]) == '\0') break;
         if (j == SCS_SEG_SIZE)
         {  j = 0;
            if (t->next == NULL)
            {  /* another segment needed */
               t->next = dmp_get_atom(pool, sizeof(SCS));
               t->next->next = NULL;
            }
            t = t->next;
         }
      }
      /* free unused segments */
      while (t->next != NULL)
      {  tt = t->next;
         t->next = tt->next;
         dmp_free_atom(pool, tt, sizeof(SCS));
      }
      return x;
}

/***********************************************************************
*  NAME
*
*  scs_get - retrieve value of segmented character string
*
*  SYNOPSIS
*
*  #include "glpscs.h"
*  char *scs_get(char *s, const SCS *x);
*
*  DESCRIPTION
*
*  The routine scs_get copies the segmented character string x to the
*  plain character string s.
*
*  The plain character string must be large enough to hold all copied
*  characters.
*
*  RETURNS
*
*  The routine returns a pointer to the plain character string. */

char *scs_get(char *s, const SCS *x)
{     int i, j;
      for (i = 0; ; x = x->next)
      {  xassert(x != NULL);
         for (j = 0; j < SCS_SEG_SIZE; j++)
            if ((s[i++] = x->c[j]) == '\0') goto done;
      }
done: xassert(x->next == NULL);
      return s;
}

/***********************************************************************
*  NAME
*
*  scs_cmp - compare segmented character strings
*
*  SYNOPSIS
*
*  #include "glpscs.h"
*  int scs_cmp(const SCS *x, const SCS *y);
*
*  DESCRIPTION
*
*  The routine scs_cmp compares segmented characters strings x and y
*  in the lexicographical order.
*
*  RETURNS
*
*  The routine returns one of the following codes:
*
*  < 0, if x is lexicographically less than y;
*  = 0, if x and y are identical;
*  > 0, if x is lexicographically greater than y. */

int scs_cmp(const SCS *x, const SCS *y)
{     int j, cx, cy;
      for (;; x = x->next, y = y->next)
      {  xassert(x != NULL);
         xassert(y != NULL);
         for (j = 0; j < SCS_SEG_SIZE; j++)
         {  cx = (unsigned char)x->c[j];
            cy = (unsigned char)y->c[j];
            if (cx < cy) return -1;
            if (cx > cy) return +1;
            if (cx == '\0') goto done;
         }
      }
done: return 0;
}

/***********************************************************************
*  NAME
*
*  scs_drop - delete segmented character string
*
*  SYNOPSIS
*
*  #include "glpscs.h"
*  void scs_drop(DMP *pool, SCS *x);
*
*  DESCRIPTION
*
*  The routine scs_drop deletes the segmented character string x and
*  returns all its segments to the specified memory pool.
*
*  The parameter pool must specify the same dynamic memory pool, which
*  was used on creating the segmented character string. */

void scs_drop(DMP *pool, SCS *x)
{     scs_set(pool, x, "");
      xassert(x->next == NULL);
      dmp_free_atom(pool, x, sizeof(SCS));
      return;
}

/* eof */
