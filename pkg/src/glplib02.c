/* glplib02.c (64-bit arithmetic) */

/***********************************************************************
*  This code is part of GLPK (GNU Linear Programming Kit).
*
*  Copyright (C) 2000, 01, 02, 03, 04, 05, 06, 07, 08 Andrew Makhorin,
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

#include "glplib.h"

/***********************************************************************
*  NAME
*
*  ulset - construct an unsigned long integer
*
*  SYNOPSIS
*
*  #include "glplib.h"
*  glp_ulong ulset(unsigned int hi, unsigned int lo);
*
*  RETURNS
*
*  The routine ulset returns the number ((hi << 32) | lo). */

glp_ulong ulset(unsigned int hi, unsigned int lo)
{     glp_ulong x;
      x.lo = lo, x.hi = hi;
      return x;
}

/***********************************************************************
*  NAME
*
*  uladd - add unsigned long integers
*
*  SYNOPSIS
*
*  #include "glplib.h"
*  glp_ulong uladd(glp_ulong x, glp_ulong y);
*
*  RETURNS
*
*  The routine uladd returns the sum x + y. */

glp_ulong uladd(glp_ulong x, glp_ulong y)
{     if (x.lo <= 0xFFFFFFFF - y.lo)
         x.lo += y.lo, x.hi += y.hi;
      else
         x.lo += y.lo, x.hi += y.hi + 1;
      return x;
}

/***********************************************************************
*  NAME
*
*  ulsub - subtract unsigned long integers
*
*  SYNOPSIS
*
*  #include "glplib.h"
*  glp_ulong ulsub(glp_ulong x, glp_ulong y);
*
*  RETURNS
*
*  The routine ulsub returns the difference x - y. */

glp_ulong ulsub(glp_ulong x, glp_ulong y)
{     if (x.lo >= y.lo)
         x.lo -= y.lo, x.hi -= y.hi;
      else
         x.lo += (0xFFFFFFFF - y.lo) + 1, x.hi -= y.hi + 1;
      return x;
}

/***********************************************************************
*  NAME
*
*  ulcmp - compare unsigned long integers
*
*  SYNOPSIS
*
*  #include "glplib.h"
*  int ulcmp(glp_ulong x, glp_ulong y);
*
*  RETURNS
*
*  The routine ulcmp returns the sign of the difference x - y. */

int ulcmp(glp_ulong x, glp_ulong y)
{     if (x.hi > y.hi) return +1;
      if (x.hi < y.hi) return -1;
      if (x.lo > y.lo) return +1;
      if (x.lo < y.lo) return -1;
      return 0;
}

/***********************************************************************
*  NAME
*
*  ulmul - multiply unsigned long integers
*
*  SYNOPSIS
*
*  #include "glplib.h"
*  glp_ulong ulmul(glp_ulong x, glp_ulong y);
*
*  RETURNS
*
*  The routine ulmul returns the product x * y. */

glp_ulong ulmul(glp_ulong x, glp_ulong y)
{     unsigned short xx[8], yy[4];
      xx[4] = (unsigned short)x.lo;
      xx[5] = (unsigned short)(x.lo >> 16);
      xx[6] = (unsigned short)x.hi;
      xx[7] = (unsigned short)(x.hi >> 16);
      yy[0] = (unsigned short)y.lo;
      yy[1] = (unsigned short)(y.lo >> 16);
      yy[2] = (unsigned short)y.hi;
      yy[3] = (unsigned short)(y.hi >> 16);
      bigmul(4, 4, xx, yy);
      x.lo = (unsigned int)xx[0] | ((unsigned int)xx[1] << 16);
      x.hi = (unsigned int)xx[2] | ((unsigned int)xx[3] << 16);
      return x;
}

/***********************************************************************
*  NAME
*
*  uldiv - divide unsigned long integers
*
*  SYNOPSIS
*
*  #include "glplib.h"
*  glp_uldiv uldiv(glp_ulong x, glp_ulong y);
*
*  RETURNS
*
*  The routine uldiv returns a structure of type glp_uldiv.
*
*  The structure has the following members: quot, which is set to the
*  quotient x div y, and rem, which is set to the remainder x mod y. */

glp_uldiv uldiv(glp_ulong x, glp_ulong y)
{     int m;
      glp_uldiv qr;
      unsigned short xx[8], yy[4];
      xx[0] = (unsigned short)x.lo;
      xx[1] = (unsigned short)(x.lo >> 16);
      xx[2] = (unsigned short)x.hi;
      xx[3] = (unsigned short)(x.hi >> 16);
      yy[0] = (unsigned short)y.lo;
      yy[1] = (unsigned short)(y.lo >> 16);
      yy[2] = (unsigned short)y.hi;
      yy[3] = (unsigned short)(y.hi >> 16);
      if (yy[3])
         m = 4;
      else if (yy[2])
         m = 3;
      else if (yy[1])
         m = 2;
      else if (yy[0])
         m = 1;
      else
         xfault("uldiv: divide by zero\n");
      bigdiv(4 - m, m, xx, yy);
      /* remainder in x[0], x[1], ..., x[m-1] */
      qr.rem.lo = (unsigned int)xx[0], qr.rem.hi = 0;
      if (m >= 2) qr.rem.lo |= (unsigned int)xx[1] << 16;
      if (m >= 3) qr.rem.hi = (unsigned int)xx[2];
      if (m >= 4) qr.rem.hi |= (unsigned int)xx[3] << 16;
      /* quotient in x[m], x[m+1], ..., x[4] */
      qr.quot.lo = (unsigned int)xx[m], qr.quot.hi = 0;
      if (m <= 3) qr.quot.lo |= (unsigned int)xx[m+1] << 16;
      if (m <= 2) qr.quot.hi = (unsigned int)xx[m+2];
      if (m <= 1) qr.quot.hi |= (unsigned int)xx[m+3] << 16;
      return qr;
}

/***********************************************************************
*  NAME
*
*  ultoa - convert unsigned long integer to character string
*
*  SYNOPSIS
*
*  #include "glplib.h"
*  char *ultoa(glp_ulong x, char *s, int radix);
*
*  DESCRIPTION
*
*  The routine ultoa converts the value unsigned long integer x to a
*  null-terminated character string and stores the result in s.
*
*  The parameter radix specifies the base to be used on converting. It
*  must be between 2 and 36, inclusive.
*
*  NOTE
*
*  The space allocated to s must be sufficient to hold the resulting
*  string, includeing the terminating null character.
*
*  RETURNS
*
*  The routine returns the pointer s. */

char *ultoa(glp_ulong x, char *s, int radix)
{     static const char *d = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      glp_ulong base;
      glp_uldiv t;
      int len = 0;
      xassert(2 <= radix && radix <= 36);
      base = ulset(0, radix);
      while (!(x.lo == 0 && x.hi == 0))
      {  t = uldiv(x, base);
         xassert(t.rem.lo < (unsigned int)radix && t.rem.hi == 0);
         s[len++] = d[t.rem.lo];
         x = t.quot;
      }
      if (len == 0) s[len++] = '0';
      s[len] = '\0';
      return strrev(s);
}

/**********************************************************************/

#if 0
#include <assert.h>
#include "glprng.h"

#define N_TEST 1000000
/* number of tests */

static glp_ulong random(RNG *rand)
{     glp_ulong x;
      int k;
      k = rng_unif_rand(rand, 4);
      assert(0 <= k && k <= 3);
      x.lo = rng_unif_rand(rand, 65536);
      if (k == 1 || k == 3)
      {  x.lo <<= 16;
         x.lo += rng_unif_rand(rand, 65536);
      }
      if (k <= 1)
         x.hi = 0;
      else
         x.hi = rng_unif_rand(rand, 65536);
      if (k == 4)
      {  x.hi <<= 16;
         x.hi += rng_unif_rand(rand, 65536);
      }
      return x;
}

int main(void)
{     RNG *rand;
      glp_ulong x, y;
      glp_uldiv z;
      int test;
      rand = rng_create_rand();
      for (test = 1; test <= N_TEST; test++)
      {  x = random(rand);
         y = random(rand);
         if (y.lo == 0 && y.hi == 0) y.lo = 1;
         /* z.quot := x div y, z.rem := x mod y */
         z = uldiv(x, y);
         /* x must be equal to y * z.quot + z.rem */
         assert(ulcmp(x, uladd(ulmul(y, z.quot), z.rem)) == 0);
      }
      fprintf(stderr, "%d tests successfully passed\n", N_TEST);
      rng_delete_rand(rand);
      return 0;
}
#endif

/* eof */
