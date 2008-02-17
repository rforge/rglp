/* glpmpl05.c */

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

#include "glpmpl.h"

#define CSV_FIELD_MAX 50
/* maximal number of fields in record */

#define CSV_FDLEN_MAX 255
/* maximal field length */

struct csv
{     /* comma-separated values file */
      int mode;
      /* 'R' = reading; 'W' = writing */
      char *fname;
      /* name of csv file */
      FILE *fp;
      /* stream assigned to csv file */
      jmp_buf jump;
      /* address for non-local go to in case of error */
      int count;
      /* record count */
      /*--------------------------------------------------------------*/
      /* used only for input csv file */
      int c;
      /* current character or EOF */
      int what;
      /* current marker: */
#define CSV_EOF   0  /* end-of-file */
#define CSV_EOR   1  /* end-of-record */
#define CSV_NUM   2  /* floating-point number */
#define CSV_STR   3  /* character string */
      char field[CSV_FDLEN_MAX+1];
      /* current field just read */
      int nf;
      /* number of fields in the csv file */
      int ref[1+CSV_FIELD_MAX];
      /* ref[k] = k', if k-th field of the csv file corresponds to
         k'-th field in the table statement; if ref[k] = 0, k-th field
         of the csv file is ignored */
};

#undef read_char

static void read_char(struct csv *csv)
{     /* read character from csv data file */
      int c;
      xassert(csv->c != EOF);
      if (csv->c == '\n') csv->count++;
loop: c = fgetc(csv->fp);
      if (ferror(csv->fp))
      {  xprintf("%s:%d: read error - %s\n", csv->fname, csv->count,
            strerror(errno));
         longjmp(csv->jump, 0);
      }
      if (feof(csv->fp))
      {  if (csv->c == '\n')
         {  csv->count--;
            c = EOF;
         }
         else
         {  xprintf("%s:%d: warning: missing final end-of-line\n",
               csv->fname, csv->count);
            c = '\n';
         }
      }
      else if (c == '\r')
         goto loop;
      else if (c == '\n')
         ;
      else if (iscntrl(c))
      {  xprintf("%s:%d: invalid control character 0x%02X\n",
            csv->fname, csv->count, c);
         longjmp(csv->jump, 0);
      }
      csv->c = c;
      return;
}

static void read_field(struct csv *csv)
{     /* read field from csv data file */
      /* check for end of file */
      if (csv->c == EOF)
      {  csv->what = CSV_EOF;
         strcpy(csv->field, "EOF");
         goto done;
      }
      /* check for end of record */
      if (csv->c == '\n')
      {  csv->what = CSV_EOR;
         strcpy(csv->field, "EOR");
         read_char(csv);
         if (csv->c == ',')
err1:    {  xprintf("%s:%d: empty field not allowed\n", csv->fname,
               csv->count);
            longjmp(csv->jump, 0);
         }
         if (csv->c == '\n')
         {  xprintf("%s:%d: empty record not allowed\n", csv->fname,
               csv->count);
            longjmp(csv->jump, 0);
         }
         goto done;
      }
      /* skip comma before next field */
      if (csv->c == ',')
         read_char(csv);
      /* read field */
      if (csv->c == '\'' || csv->c == '"')
      {  /* read a field enclosed in quotes */
         int quote = csv->c, len = 0;
         csv->what = CSV_STR;
         /* skip opening quote */
         read_char(csv);
         /* read field characters within quotes */
         for (;;)
         {  /* check for closing quote and read it */
            if (csv->c == quote)
            {  read_char(csv);
               if (csv->c == quote)
                  ;
               else if (csv->c == ',' || csv->c == '\n')
                  break;
               else
               {  xprintf("%s:%d: invalid field\n", csv->fname,
                     csv->count);
                  longjmp(csv->jump, 0);
               }
            }
            /* check the current field length */
            if (len == CSV_FDLEN_MAX)
err2:       {  xprintf("%s:%d: field too long\n", csv->fname,
                  csv->count);
               longjmp(csv->jump, 0);
            }
            /* add the current character to the field */
            csv->field[len++] = (char)csv->c;
            /* read the next character */
            read_char(csv);
         }
         /* the field has been read */
         if (len == 0) goto err1;
         csv->field[len] = '\0';
      }
      else
      {  /* read a field not enclosed in quotes */
         int len = 0;
         double temp;
         csv->what = CSV_NUM;
         while (!(csv->c == ',' || csv->c == '\n'))
         {  /* quotes within the field are not allowed */
            if (csv->c == '\'' || csv->c == '"')
            {  xprintf("%s:%d: invalid use of single or double quote wi"
                  "thin field\n", csv->fname, csv->count);
               longjmp(csv->jump, 0);
            }
            /* check the current field length */
            if (len == CSV_FDLEN_MAX) goto err2;
            /* add the current character to the field */
            csv->field[len++] = (char)csv->c;
            /* read the next character */
            read_char(csv);
         }
         /* the field has been read */
         if (len == 0) goto err1;
         csv->field[len] = '\0';
         /* check the field type */
         if (str2num(csv->field, &temp)) csv->what = CSV_STR;
      }
done: return;
}

static struct csv *csv_open_file(TABDCA *dca, int mode)
{     /* open csv data file */
      struct csv *csv;
      /* create control structure */
      csv = xmalloc(sizeof(struct csv));
      csv->mode = mode;
      csv->fname = NULL;
      csv->fp = NULL;
      if (setjmp(csv->jump)) goto fail;
      csv->count = 0;
      csv->c = '\n';
      csv->what = 0;
      csv->field[0] = '\0';
      csv->nf = 0;
      /* try to open the csv data file */
      if (mpl_tab_num_args(dca) < 2)
      {  xprintf("csv_driver: file name not specified\n");
         longjmp(csv->jump, 0);
      }
      csv->fname = xmalloc(strlen(mpl_tab_get_arg(dca, 2))+1);
      strcpy(csv->fname, mpl_tab_get_arg(dca, 2));
      if (mode == 'R')
      {  /* open the file for reading */
         int k;
         csv->fp = xfopen(csv->fname, "r");
         if (csv->fp == NULL)
         {  xprintf("csv_driver: unable to open %s - %s\n",
               csv->fname, strerror(errno));
            longjmp(csv->jump, 0);
         }
         /* skip fake new-line */
         read_field(csv);
         xassert(csv->what == CSV_EOR);
         /* read field names */
         xassert(csv->nf == 0);
         for (;;)
         {  read_field(csv);
            if (csv->what == CSV_EOR)
               break;
            if (csv->what != CSV_STR)
            {  xprintf("%s:%d: invalid field name\n", csv->fname,
                  csv->count);
               longjmp(csv->jump, 0);
            }
            if (csv->nf == CSV_FIELD_MAX)
            {  xprintf("%s:%d: too many fields\n", csv->fname,
                  csv->count);
               longjmp(csv->jump, 0);
            }
            csv->nf++;
            /* find corresponding field in the table statement */
            for (k = mpl_tab_num_flds(dca); k >= 1; k--)
            {  if (strcmp(mpl_tab_get_name(dca, k), csv->field) == 0)
                  break;
            }
            csv->ref[csv->nf] = k;
         }
         /* find dummy RECNO field in the table statement */
         for (k = mpl_tab_num_flds(dca); k >= 1; k--)
            if (strcmp(mpl_tab_get_name(dca, k), "RECNO") == 0) break;
         csv->ref[0] = k;
      }
      else if (mode == 'W')
      {  /* open the file for writing */
         int k, nf;
         csv->fp = xfopen(csv->fname, "w");
         if (csv->fp == NULL)
         {  xprintf("csv_driver: unable to create %s - %s\n",
               csv->fname, strerror(errno));
            longjmp(csv->jump, 0);
         }
         /* write field names */
         nf = mpl_tab_num_flds(dca);
         for (k = 1; k <= nf; k++)
            fprintf(csv->fp, "%s%c", mpl_tab_get_name(dca, k),
               k < nf ? ',' : '\n');
         csv->count++;
      }
      else
         xassert(mode != mode);
      /* the file has been open */
      return csv;
fail: /* the file cannot be open */
      if (csv->fname != NULL) xfree(csv->fname);
      if (csv->fp != NULL) xfclose(csv->fp);
      xfree(csv);
      return NULL;
}

static int csv_read_record(TABDCA *dca, struct csv *csv)
{     /* read next record from csv data file */
      int k, ret = 0;
      xassert(csv->mode == 'R');
      if (setjmp(csv->jump))
      {  ret = 1;
         goto done;
      }
      /* read dummy RECNO field */
      if (csv->ref[0] > 0)
         mpl_tab_set_num(dca, csv->ref[0], csv->count-1);
      /* read fields */
      for (k = 1; k <= csv->nf; k++)
      {  read_field(csv);
         if (csv->what == CSV_EOF)
         {  /* end-of-file reached */
            xassert(k == 1);
            ret = -1;
            goto done;
         }
         else if (csv->what == CSV_EOR)
         {  /* end-of-record reached */
            int lack = csv->nf - k + 1;
            if (lack == 1)
               xprintf("%s:%d: one field missing\n", csv->fname,
                  csv->count);
            else
               xprintf("%s:%d: %d fields missing\n", csv->fname,
                  csv->count, lack);
            longjmp(csv->jump, 0);
         }
         else if (csv->what == CSV_NUM)
         {  /* floating-point number */
            if (csv->ref[k] > 0)
            {  double num;
               xassert(str2num(csv->field, &num) == 0);
               mpl_tab_set_num(dca, csv->ref[k], num);
            }
         }
         else if (csv->what == CSV_STR)
         {  /* character string */
            if (csv->ref[k] > 0)
               mpl_tab_set_str(dca, csv->ref[k], csv->field);
         }
         else
            xassert(csv != csv);
      }
      /* now there must be NL */
      read_field(csv);
      xassert(csv->what != CSV_EOF);
      if (csv->what != CSV_EOR)
      {  xprintf("%s:%d: too many fields\n", csv->fname, csv->count);
         longjmp(csv->jump, 0);
      }
done: return ret;
}

static int csv_write_record(TABDCA *dca, struct csv *csv)
{     /* write next record to csv data file */
      int k, nf, ret = 0;
      const char *c;
      xassert(csv->mode == 'W');
      nf = mpl_tab_num_flds(dca);
      for (k = 1; k <= nf; k++)
      {  switch (mpl_tab_get_type(dca, k))
         {  case 'N':
               fprintf(csv->fp, "%.*g", DBL_DIG,
                  mpl_tab_get_num(dca, k));
               break;
            case 'S':
               fputc('"', csv->fp);
               for (c = mpl_tab_get_str(dca, k); *c != '\0'; c++)
               {  if (*c == '"')
                     fputc('"', csv->fp), fputc('"', csv->fp);
                  else
                     fputc(*c, csv->fp);
               }
               fputc('"', csv->fp);
               break;
            default:
               xassert(dca != dca);
         }
         fputc(k < nf ? ',' : '\n', csv->fp);
      }
      csv->count++;
      if (ferror(csv->fp))
      {  xprintf("%s:%d: write error - %s\n", csv->fname, csv->count,
            strerror(errno));
         ret = 1;
      }
      return ret;
}

static int csv_close_file(TABDCA *dca, struct csv *csv)
{     /* close csv data file */
      int ret = 0;
      xassert(dca == dca);
      if (csv->mode == 'W')
      {  fflush(csv->fp);
         if (ferror(csv->fp))
         {  xprintf("%s:%d: write error - %s\n", csv->fname,
               csv->count, strerror(errno));
            ret = 1;
         }
      }
      xfree(csv->fname);
      xfclose(csv->fp);
      xfree(csv);
      return ret;
}

/*====================================================================*/

#define TAB_CSV   1
#define TAB_ODBC  2

void mpl_tab_drv_open(MPL *mpl, int mode)
{     TABDCA *dca = mpl->dca;
      xassert(dca->id == 0);
      xassert(dca->link == NULL);
      xassert(dca->na >= 1);
      if (strcmp(dca->arg[1], "CSV") == 0)
      {  dca->id = TAB_CSV;
         dca->link = csv_open_file(dca, mode);
      }
      else if (strcmp(dca->arg[1], "ODBC") == 0)
      {  dca->id = TAB_ODBC;
         xprintf("ODBC table driver is not implemented\n");
      }
      else
         xprintf("Invalid table driver `%s'\n", dca->arg[1]);
      if (dca->link == NULL)
         error(mpl, "error on opening table %s",
            mpl->stmt->u.tab->name);
      return;
}

int mpl_tab_drv_read(MPL *mpl)
{     TABDCA *dca = mpl->dca;
      int ret;
      switch (dca->id)
      {  case TAB_CSV:
            ret = csv_read_record(dca, dca->link);
            break;
         case TAB_ODBC:
         default:
            xassert(dca != dca);
      }
      if (ret > 0)
         error(mpl, "error on reading data from table %s",
            mpl->stmt->u.tab->name);
      return ret;
}

void mpl_tab_drv_write(MPL *mpl)
{     TABDCA *dca = mpl->dca;
      int ret;
      switch (dca->id)
      {  case TAB_CSV:
            ret = csv_write_record(dca, dca->link);
            break;
         case TAB_ODBC:
         default:
            xassert(dca != dca);
      }
      if (ret)
         error(mpl, "error on writing data to table %s",
            mpl->stmt->u.tab->name);
      return;
}

void mpl_tab_drv_close(MPL *mpl)
{     TABDCA *dca = mpl->dca;
      int ret;
      switch (dca->id)
      {  case TAB_CSV:
            ret = csv_close_file(dca, dca->link);
            break;
         case TAB_ODBC:
         default:
            xassert(dca != dca);
      }
      dca->id = 0;
      dca->link = NULL;
      if (ret)
         error(mpl, "error on closing table %s",
            mpl->stmt->u.tab->name);
      return;
}

/* eof */
