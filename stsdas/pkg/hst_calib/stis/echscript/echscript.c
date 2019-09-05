#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <float.h>
#include <math.h>

#include <c_iraf.h>
#include <xtables.h>
#include <xclio.h>
#include <xselector.h>

#include "echscript.h"

#define  NPARAM     5          /* Number of CL parameters           */
#define  ZERO       0 
#define  MAXCHKEY  20          /* Maximum length of keyword value   */

/*****************************************************************************
 *
 * File:    echscript.c
 * Purpose: Read a STIS 3D binary FITS table containing extracted spectral
 *          data and generate an IGI script to plot the selected rows.
 *
 * Input:   "Table name[row selector syntax]" output_script_name plot_style
 *          All rows are plotted if no row selector is used.
 *          Plot_styles may be: single, multiple, panel (s, m, or p)
 *           single   = single spectral order per plot per page (landscape)
 *           multiple = all selected orders in one plot on one page (landscape)
 *           panel    = single spectral order per plot, each plot a panel on
 *                      a graphics page (portrait).  Currently, 4 panels/page.
 *           diagnostic = single spectral order per page (portrait) -- all
 *                        data arrays are plotted
 *
 * Output:  IGI script which generates a plot of the selected rows (orders).
 *
 * Author:  Michele D. De La Pena
 * Date:    28 April 1997
 * Mods:    23 September 1997 - Updated to use the row selector syntax and
 *          to be run as a native IRAF task.
 * Mods:    08 October 1997 - Updated name of file and increased MAXORD.
 * Mods:    09 October 1997 - Adding diagnostic plot.
 * Mods:    27 October 1997 - Modularizing.
 * Mods:    26 November 1997 - Code review: Dynamically allocate rowptr, fix 
 *             status pointers, fix search for VMS for FITS extension using
 *             brackets, use c_clgetd for input wavelengths, 's' mode utilizes
 *             input wave limits, correct error checking, added fillKeyword,
 *             and streamlined code sections.
 * Mods:    31 March 1998 - Source only for C-bindings actually used.
 * Mods:    04 June 1998 - Fixed an error when input has extension appended.
 *             Added "view only" capability; deletes temporary IGI script.
 * Mods:    04 December 1998 - MDD: File name shortened to echscript.c.  
 *             Header file name also shortened (e_echscript.h -> echscript.h).
 * Mods:    23 July 1999 - MDD: Use CVOS xselector library. Clean up includes.
 *
******************************************************************************/

void iraferr() {
   printf("IRAF error code %d\nIRAF message: %s\n",
           c_iraferr(), c_iraferrmsg());
}

int e_echscript (int argc, char **argv) {

   char   cl_argv[NPARAM][SZ_NAME];    /* Holds CL parameters                */
   char   colSelect[SZ_NAME];          /* Column selector string             */
   char   fluxCol[SZ_NAME];            /* Requested intensity column name    */
   char   hdTable[SZ_NAME];            /* Name of primary header unit        */
   int    i = 0;                       /* Loop index                         */
   char   input[SZ_NAME];              /* Input table name with row selector */
   char   inTable[SZ_NAME];            /* Input 3D binary table name         */
   int    j = 0;                       /* Loop index                         */
   int    length  = 0;                 /* Length of string                   */
   double minwave = 0.0;               /* Optional input minimum wavelength  */
   double maxwave = 0.0;               /* Optional input maximum wavelength  */
   int    nprefix = 0;                 /* No. of chars in name before "["    */
   int    nrow    = 0;                 /* No. unique and valid rows selected */
   char   outScript[SZ_NAME];          /* Output IGI script name             */
   char   plotStyle[SZ_NAME];          /* Plot style (s, m, p, d)            */
   int    *row = NULL;                 /* Row(s) selected from table         */
   char   rowSelect[SZ_NAME];          /* Row selector string                */
   int    status = 0;                  /* Return status                      */
   char   title[SZ_NAME];              /* Input optional title for plot      */
   Bool   viewonly = False;            /* View only mode - no IGI script     */

   PhdInfo     *phdptr  = NULL;        /* Pointer to the primary header      */
   TblDesc     *tabptr  = NULL;        /* Pointer to the table structure     */
   RowContents **rowptr = NULL;        /* Pointers to the column information */

   /*** Declare the function prototypes ***/
   int       allocRow  (TblDesc *, char *, RowContents *[], int *);
   TblDesc * allocTbl  (char *);
   PhdInfo * allocPhd  (char *);
   void      rangeFind (double *, short, double *, double *);

   input[0]     = '\0';
   outScript[0] = '\0';
   plotStyle[0] = '\0';
   fluxCol[0]   = '\0';
   title[0]     = '\0';
   hdTable[0]   = '\0';
   rowSelect[0] = '\0';
   colSelect[0] = '\0';
   inTable[0]   = '\0';

   /* Read CL parameters */
   c_clgstr ("input",     cl_argv[0], SZ_NAME);
   c_clgstr ("output",    cl_argv[1], SZ_NAME);
   c_clgstr ("plotstyle", cl_argv[2], SZ_NAME);
   c_clgstr ("flux_col",  cl_argv[3], SZ_NAME);
   c_clgstr ("title",     cl_argv[4], SZ_NAME);
   minwave  = c_clgetd ("minwave");
   maxwave  = c_clgetd ("maxwave");
   viewonly = c_clgetb ("viewonly");

   /* Assign the input parameters */
   strcpy (input,     cl_argv[0]);
   strcpy (outScript, cl_argv[1]);
   strcpy (plotStyle, cl_argv[2]);
   strcpy (fluxCol,   cl_argv[3]);
   strcpy (title,     cl_argv[4]);

   /* Parse the input file name and extract the row and column selectors *
    * Note: if present, column selectors are ignored; column selected    *
    * input parameter                                                    */
   c_rdselect(input, inTable, rowSelect, colSelect, SZ_NAME);

   /* Check for the existence of the input table */
   if (!c_tbtacc(inTable)) {
       iraferr();
       sprintf (ErrMsg, "Input table %s does not exist.\n", inTable);
       e_error (ErrMsg);
       return  (-1);
   }

   /* Allocate the table structure and determine column pointers */
   tabptr = allocTbl (inTable);
   if (tabptr == NULL) {
       sprintf (ErrMsg, "Error on return from table allocation.\n");
       e_error (ErrMsg);
       return  (-1);
   }
   
   /* Allocate an array to store the actual rows requested */
   row  = (int *) calloc (tabptr->nrows, sizeof(int));
   if (row == NULL) {
       sprintf (ErrMsg, "No memory to allocate row pointers.\n");
       e_error (ErrMsg);
       return  (-1);
   }

   /* Allocate enough pointers to the row structures according to the size  * 
    * of the table and allocate the individual pointers to the row contents */
   rowptr = (RowContents **) calloc (tabptr->nrows, sizeof(RowContents *));
   for (i = 0; i < tabptr->nrows; i++) {
        rowptr[i] = (RowContents *) calloc (1, sizeof(RowContents));
        if (rowptr[i] == NULL) {
            sprintf (ErrMsg, "No memory to allocate pointers to row contents.\n");
            e_error (ErrMsg);
            return  (-1);
        }
   }

   /* Determine the number of rows actually selected */
   nrow = allocRow (tabptr, rowSelect, rowptr, row);
   if (nrow <= 0) {
       sprintf (ErrMsg, "No rows selected from table.\n");
       e_error (ErrMsg);
       return  (-1);
   }

   /* Reallocate the row structures to use only the number of pointers *
    * actually needed.  OK to reuse pointer as smaller than original   */
   rowptr = (RowContents **) realloc (rowptr, sizeof(RowContents *) * nrow);

   /* Construct the primary header name from the input table name */
   length = strlen(inTable);
   if (inTable[length-1] == ']') {
       for (i = length - 2; i >= 0; i--) {
            if (inTable[i] == '[') {
                nprefix = i;
                break;
            }
       }
       strncpy (hdTable, inTable, nprefix);
       hdTable[nprefix] = '\0';
   }
   else
       strcpy (hdTable, inTable);

   strcat  (hdTable,"[0]");

   /* Allocate the primary header structure and acquire keyword information */
   phdptr = allocPhd (hdTable);
   if (phdptr == NULL) {
       sprintf (ErrMsg, "Error on return from table header allocation.\n");
       e_error (ErrMsg);
       return  (-1);
   }

   /* Close the open header and table structures */
   c_tbtclo (phdptr->hd);
   c_tbtclo (tabptr->tp);

   /*                                                         */
   /*      Set up for generating the IGI graphics script      */
   /*                                                         */

   /* Convert the requested intensity column name to upper case */
   i = 0; 
   while (*(fluxCol + i) != '\0') {
          *(fluxCol + i) = toupper(*(fluxCol + i));
          i++;
   }

   /* Call the appropriate routine for the plot style selected */
   *plotStyle = toupper(*plotStyle);
   switch (*plotStyle) {

      case 'S':

         printf ("\nThe SINGLE ORDER PER PLOT style has been selected.\n\n");

         status = igiSingle (outScript, inTable, phdptr, fluxCol, title, 
                     rowptr, row, nrow, minwave, maxwave, viewonly);
         if (status < 0) {
             sprintf (ErrMsg, "Error in SINGLE graphics routine.\n");
             e_error (ErrMsg);
             return  (-1);
         }
         break;

      case 'M':

         printf ("\nThe MULTIPLE ORDER PER PLOT style has been selected.\n\n");

         status = igiMultiple (outScript, inTable, phdptr, fluxCol, title, 
                     rowptr, row, nrow, minwave, maxwave, viewonly);
         if (status < 0) {
             sprintf (ErrMsg, "Error in MULTIPLE graphics routine.\n");
             e_error (ErrMsg);
             return  (-1);
         }
         break;

      case 'P':

         printf ("\nThe SINGLE ORDER PER PLOT in a PANEL style has been selected.\n\n");
         status = igiPanel (outScript, inTable, phdptr, fluxCol, title, rowptr, 
                     row, nrow, viewonly);
         if (status < 0) {
             sprintf (ErrMsg, "Error in PANEL graphics routine.\n");
             e_error (ErrMsg);
             return  (-1);
         }
         break;

      case 'D':

         printf ("\nThe DIAGNOSTIC plot has been selected.\n\n");

         status = igiDiag (outScript, inTable, phdptr, title, rowptr, 
                     row, nrow, viewonly);
         if (status < 0) {
             sprintf (ErrMsg, "Error in DIAGNOSTIC graphics routine.\n");
             e_error (ErrMsg);
             return  (-1);
         }
         break;

      default:

         sprintf (ErrMsg, "Selected plot style - %s - is invalid.\n", plotStyle);
         e_error (ErrMsg);
         return  (-1);
         break;
   }
  
   /* Clean up */
   free (row);
   free (tabptr);
   free (phdptr);

   for (i = 0; i < nrow; i++) {
        free (rowptr[i]->wave);
        free (rowptr[i]->gross);
        free (rowptr[i]->back);
        free (rowptr[i]->net);
        free (rowptr[i]->flux);
        free (rowptr[i]->error);
        free (rowptr[i]->dq);
        free (rowptr[i]);
   }

   return (0);
}               /* end function MAIN */

/***************************************************************************
 *
 * rangeFind
 * Routine to determine the valid minimum and maximum values of an array.
 *
****************************************************************************/

void
rangeFind (double *data, short length, double *data_min, double *data_max) {

   int i;      /* Loop index */

   double temp_min = DBL_MAX;
   double temp_max = -DBL_MIN;

   /* Loop over the input data array */
   for (i = 0; i < length; i++) {
      if (*(data + i) < temp_min)
         temp_min = *(data + i);
      if (*(data + i) > temp_max)
         temp_max = *(data + i);
   }

   *data_min = temp_min;
   *data_max = temp_max;

   return;
}

/***************************************************************************
 *
 * allocTbl
 * Routine to allocate the table pointer and assign the pointers to the
 * appropriate columns in the table
 *
****************************************************************************/

TblDesc *
allocTbl (char *inTable) {

   TblDesc *tabptr = NULL;    

   /* Allocate memory for the table pointer */
   tabptr = (TblDesc *) calloc (1, sizeof(TblDesc));
   if (tabptr == NULL) {
      sprintf (ErrMsg, "No memory to allocate table pointer.\n");
      e_error (ErrMsg);
      return  (NULL);
   }

   /* Open the table */
   tabptr->tp = c_tbtopn (inTable, IRAF_READ_ONLY, ZERO);
   if (c_iraferr()) {
       iraferr();
       sprintf (ErrMsg, "Input table - %s - could not be opened.\n", inTable);
       e_error (ErrMsg);
       return  (NULL);
   }

   /* Determine the number of rows */
   tabptr->nrows = c_tbpsta (tabptr->tp, TBL_NROWS);
   if (c_iraferr()) {
       iraferr();
       sprintf  (ErrMsg, "Cannot determine the number of rows in table - %s.\n",
                 inTable);
       e_error  (ErrMsg);
       c_tbtclo (tabptr->tp);
       return   (NULL);
   }
 
   /* Set up the pointers to the columns */
   c_tbcfnd1 (tabptr->tp, "SPORDER", &tabptr->isporder);
   c_tbcfnd1 (tabptr->tp, "NELEM", &tabptr->inelem);
   c_tbcfnd1 (tabptr->tp, "WAVELENGTH", &tabptr->iwavelength);
   c_tbcfnd1 (tabptr->tp, "GROSS", &tabptr->igross);
   c_tbcfnd1 (tabptr->tp, "BACKGROUND", &tabptr->ibackground);
   c_tbcfnd1 (tabptr->tp, "NET", &tabptr->inet);
   c_tbcfnd1 (tabptr->tp, "FLUX", &tabptr->iflux);
   c_tbcfnd1 (tabptr->tp, "ERROR", &tabptr->ierror);
   c_tbcfnd1 (tabptr->tp, "DQ", &tabptr->idq);
   if ((tabptr->isporder == 0)    || (tabptr->inelem == 0) ||
       (tabptr->iwavelength == 0) || (tabptr->igross == 0) ||
       (tabptr->ibackground == 0) || (tabptr->inet == 0)   ||
       (tabptr->iflux == 0)       || (tabptr->ierror == 0) ||
       (tabptr->idq   == 0)) {
       iraferr();
       sprintf  (ErrMsg, "Column not found in input table.\n");
       e_error  (ErrMsg);
       c_tbtclo (tabptr->tp);
       return   (NULL);
   }

   return (tabptr);
}

/***************************************************************************
 *
 * allocPhd
 * Routine to allocate the primary header structure and extract the 
 * necessary keyword values
 *
****************************************************************************/

PhdInfo *
allocPhd (char *hdTable) {

   PhdInfo *phdptr = NULL;

   /* Local function prototype */
   void fillKeyword (PhdInfo *, char *, char *);

   /* Allocate the pointer to the primary header structure */
   phdptr = (PhdInfo *) calloc (1, sizeof(PhdInfo));
   if (phdptr == NULL) {
      sprintf (ErrMsg, "No memory to allocate pointer to primary header.\n");
      e_error (ErrMsg);
      return (NULL);
   }

   /* Open the table */
   phdptr->hd = c_tbtopn (hdTable, IRAF_READ_ONLY, ZERO);
   if (c_iraferr()) {
       iraferr();
       sprintf (ErrMsg, "Table header - %s - could not be opened.\n", hdTable);
       e_error (ErrMsg);
       return (NULL);
   }
 
   /* Obtain the necessary keyword values */
   fillKeyword (phdptr, "INSTRUME", phdptr->instrume);
   fillKeyword (phdptr, "ROOTNAME", phdptr->rootname);
   fillKeyword (phdptr, "DETECTOR", phdptr->detector);
   fillKeyword (phdptr, "CENWAVE",  phdptr->cenwave);
   fillKeyword (phdptr, "OPT_ELEM", phdptr->opt_elem);
   fillKeyword (phdptr, "APERTURE", phdptr->aperture);

   return (phdptr);
}

/***************************************************************************
 *
 * fillKeyword
 * Obtain specified keywords from the header.  If the keyword is missing,
 * just warn the user.
 *
****************************************************************************/

void
fillKeyword (PhdInfo *phdptr, char *keyword, char *keyvalue) {

   char errorString[] = "Keyword %s could not be obtained from the primary header.\n";

   c_tbhgtt (phdptr->hd, keyword, keyvalue, MAXCHKEY);
   if (c_iraferr()) {
      iraferr();
      sprintf (ErrMsg, errorString, keyword);
      e_warn  (ErrMsg);
      strcpy  (keyvalue, "\0");
   }

   return;
}

/***************************************************************************
 *
 * allocRow
 * Routine to allocate the row pointers for all requested rows, allocate
 * the pointers for the selected columns, and read in the data from the
 * selected columns.
 *
****************************************************************************/

int
allocRow (TblDesc *tabptr, char *rowSelect, RowContents *rowptr[], int row[]) {

   IRAFPointer pcode;   /* Pseudocode                  */
   int nrow;            /* Selected row counter        */
   int irow;            /* Loop index                  */
   int nwave,           /* Size of arrays              */
       ngross,
       nback,
       nnet,
       nflux,
       nerror,
       ndq;

   nrow  = 0;
   pcode = c_trsopen (tabptr->tp, rowSelect);
   for (irow = 1; irow <= tabptr->nrows; irow++) {
        if (c_trseval (tabptr->tp, irow, pcode)) {

            /* Allocate memory for a row structure */
            rowptr[nrow] = (RowContents *) calloc (1, sizeof(RowContents));
            if (rowptr[nrow] == NULL) {
               sprintf (ErrMsg, "Not enough memory to allocate row pointer.\n");
               e_error (ErrMsg);
               return  (-1);
            }

            c_tbegts (tabptr->tp, tabptr->isporder, irow, &rowptr[nrow]->sporder);
            c_tbegts (tabptr->tp, tabptr->inelem, irow, &rowptr[nrow]->nelem);

            /* Allocate data arrays */
            rowptr[nrow]->wave  =  
                   (double *) calloc (rowptr[nrow]->nelem, sizeof(double));
            rowptr[nrow]->gross = 
                   (float *)  calloc (rowptr[nrow]->nelem, sizeof(float));
            rowptr[nrow]->back  = 
                   (float *)  calloc (rowptr[nrow]->nelem, sizeof(float));
            rowptr[nrow]->net   = 
                   (float *)  calloc (rowptr[nrow]->nelem, sizeof(float));
            rowptr[nrow]->flux  = 
                   (float *)  calloc (rowptr[nrow]->nelem, sizeof(float));
            rowptr[nrow]->error = 
                   (float *)  calloc (rowptr[nrow]->nelem, sizeof(float));
            rowptr[nrow]->dq    = 
                   (short *)  calloc (rowptr[nrow]->nelem, sizeof(short));

            if (rowptr[nrow]->wave == NULL || rowptr[nrow]->gross == NULL ||
                rowptr[nrow]->back == NULL || rowptr[nrow]->net   == NULL ||
                rowptr[nrow]->flux == NULL || rowptr[nrow]->error == NULL ||
                rowptr[nrow]->dq   == NULL) {
                sprintf (ErrMsg, "Not enough memory to allocate array pointers.\n");
                e_error (ErrMsg);
                return  (-1);
            }

            /* Read in the arrays */
            nwave  = c_tbagtd (tabptr->tp, tabptr->iwavelength, irow, 
                               rowptr[nrow]->wave, 1, rowptr[nrow]->nelem);
            ngross = c_tbagtr (tabptr->tp, tabptr->igross, irow,
                               rowptr[nrow]->gross, 1, rowptr[nrow]->nelem);
            nback  = c_tbagtr (tabptr->tp, tabptr->ibackground, irow,
                               rowptr[nrow]->back, 1, rowptr[nrow]->nelem);
            nnet   = c_tbagtr (tabptr->tp, tabptr->inet, irow,
                               rowptr[nrow]->net, 1, rowptr[nrow]->nelem);
            nflux  = c_tbagtr (tabptr->tp, tabptr->iflux, irow,
                               rowptr[nrow]->flux, 1, rowptr[nrow]->nelem);
            nerror = c_tbagtr (tabptr->tp, tabptr->ierror, irow,
                               rowptr[nrow]->error, 1, rowptr[nrow]->nelem);
            ndq    = c_tbagts (tabptr->tp, tabptr->idq, irow,
                               rowptr[nrow]->dq, 1, rowptr[nrow]->nelem);

            if ((nwave  != rowptr[nrow]->nelem) || 
                (ngross != rowptr[nrow]->nelem) ||
                (nback  != rowptr[nrow]->nelem) || 
                (nnet   != rowptr[nrow]->nelem) ||
                (nflux  != rowptr[nrow]->nelem) ||
                (nerror != rowptr[nrow]->nelem) ||
                (ndq    != rowptr[nrow]->nelem)) {
               sprintf (ErrMsg, "Insufficent number of elements read from table.\n");
               e_error (ErrMsg);
               return  (-1);
            }
           
            /* Update the selected row array and number of selected rows */
            *(row + nrow) = irow;
            nrow++;
        }
   }

   c_trsclose (pcode);

   return (nrow);
}
