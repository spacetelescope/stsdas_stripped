#include <stdio.h>
#include <time.h>
#include <float.h>
#include <math.h>

#include <c_iraf.h>
#include <xtables.h>

#include "echscript.h"

#define   NCOLS 6
 
/*****************************************************************************
 *
 * File:     igisetup.c
 * Purpose:  Subroutines for the e_echscript.c main driver.  These routines
 *           perform the setup for the output IGI scripts.
 * Routines: igiSingle, igiMultiple, igiMultLimits, igiPanel, igiDiag
 *
 * Author:  Michele D. De La Pena
 * Date:    28 April 1997
 * Mods:    26 November 1997 - MDD: Made y-axis labels in ColInfo structure
 *             singular, wminMult/wmaxMult tested against IRAF_INDEFD defined
 *             in xtables.h, added limit checking on input wavelengths, added
 *             limits for "single" plot mode, added annotation to output
 *             scripts, added diagnostic plot mode.
 *          13 March 1998 - MDD: Added a check to make sure there are
 *             data quality flags to plot in igiDiag.
 *          04 June 1998 - MDD: Added view only capability to all modes.
 * Mods:    04 December 1998 - MDD: File name changed and included header file
 *             e_echscript.h -> echscript.h
 * Mods:    23 July 1999 - MDD: Cleaned up use of CVOS include files.
 *
******************************************************************************/
 
typedef struct 
{
   char *colname;
   char *colunits;
} ColInfo;

ColInfo tblCol[] = 
{
   {"GROSS",      "(count s\\u-\\u1)"},
   {"BACKGROUND", "(count s\\u-\\u1 \\ pixel\\u-\\u1)"},
   {"NET",        "(count s\\u-\\u1)"},
   {"FLUX",       "(erg cm\\u-\\u2 \\ s\\u-\\u1 \\gV\\u-\\u1)"},
   {"ERROR",      "(erg cm\\u-\\u2 \\ s\\u-\\u1 \\gV\\u-\\u1)"},
   {"DQ",         " "}
};


/*****************************************************************************
 *
 * igiSingle
 * Routine to generate the IGI script for plotting the specified echelle
 * orders, one spectral order per plot and one plot per page (landscape). 
 * 01 December 1997 - MDD: Modified to accept input min/max wavelengths,
 *                    and print wavelengths as floating-point values.
 * 29 January  1998 - MDD: If requested orders are outside the wavelength
 *                    regime specified by the user, issue a warning and do 
 *                    *NOT* plot the orders.  Compute y-range of plot only for
 *                    fluxes within requested wavelength regime.
 * 04 June 1998     - MDD: Added viewonly capability.
 *
******************************************************************************/

int
igiSingle (char *outScript, char *inTable, PhdInfo *ptr, char *fluxCol, 
           char *title, RowContents *rowptr[], int *row, int nrow,
           double in_wmin, double in_wmax, Bool viewonly) {

   double *dflux = NULL;        /* Type double of intensity array         */
   int    i, j;                 /* Loop index                             */
   char   sptitle[SZ_NAME];     /* Generic plot title with spectral order */
   char   gtitle[SZ_NAME];      /* Generic plot title                     */
   char   stime[SZ_NAME];       /* Date/Time string                       */
   char   fluxName[SZ_NAME];    /* Flux column name                       */
   char   fluxUnits[SZ_NAME];   /* Flux column units                      */
   double fmax = DBL_MIN;       /* Flux limits for plot                   */
   double fmin = DBL_MAX;       /* Flux limits for plot                   */
   double tmpmin = DBL_MAX;     /* Temporary min wave for all orders      */
   double tmpmax = DBL_MIN;     /* Temporary max wave for all orders      */
   double wmax = DBL_MIN;       /* Wavelength limits for plot             */
   double wmin = DBL_MAX;       /* Wavelength limits for plot             */
   int    wmindex;              /* Wavelength index for minimum limit     */
   int    wmaxdex;              /* Wavelength index for maximum limit     */
   short  wval;                 /* Number of wavelengths within limits    */
   char   ylabel[SZ_NAME];      /* Constructed y-axis label               */
   time_t date = time(NULL);    /* Date/Time                              */
   
   FILE   *fp = NULL;           /* Output file pointer                    */

   /* Allocate the file pointer for the output script */
   fp = fopen(outScript, "wt");
   if (fp == NULL) {
      sprintf (ErrMsg, "Problem opening output file - %s.\n", outScript);
      e_error (ErrMsg);
      return  (-1);
   }

   /* Initialize strings */
   sptitle[0]   = '\0';
   gtitle[0]    = '\0';
   fluxName[0]  = '\0';
   fluxUnits[0] = '\0';
   stime[0]     = '\0';
   ylabel[0]    = '\0';

   /* Setup up the Date/Time */
   sprintf (stime, "%s", ctime(&date));

   /* If necessary, construct default general title */
   if (strcmp(title,"") == 0) 
       sprintf (gtitle, "title %s  %s  %s  %s  %s  %s  ", ptr->instrume,
                ptr->rootname, ptr->detector, ptr->cenwave, ptr->opt_elem,
                ptr->aperture);
   else
       sprintf (sptitle, "title %s\n", title);

   /* Determine the intensity column to plot */
   for (i = 0; i < NCOLS; i++) {
        if (strncmp(fluxCol, tblCol[i].colname, 3) == 0) {
            strcpy (fluxName, tblCol[i].colname);
            strcpy (fluxUnits, tblCol[i].colunits);
            break;
        }
   }
  
   /* Set up the ylabel from the chosen intensity value */
   sprintf (ylabel, "label %s %s\n", fluxName, fluxUnits);

   /* Define the characters */
   fputs ("### IGI script for SINGLE plot mode.\n", fp);
   fputs ("\n", fp);
   fputs ("### Use the igi software fonts.\n", fp);
   fputs ("fontset igi\n", fp);
   fputs ("\n", fp);

   /* Loop over the chosen spectral orders */
   for (i = 0; i < nrow; i++) {

      /* Determine the wavelength limits for each order based on the data */
      tmpmin = DBL_MAX;
      tmpmax = DBL_MIN;
      rangeFind (rowptr[i]->wave, rowptr[i]->nelem, &tmpmin, &tmpmax);

      /* Check the user provided wavelength limits.  If set, make sure that *
       * the wavelength bounds are reasonable.  The check for == 0.0 is for *
       * backwards compatibility with v1.0.                                 */
      if ((in_wmin == IRAF_INDEFD) || (in_wmin == 0.0))  
           wmin = tmpmin;
      else
           wmin = in_wmin;
      if ((in_wmax == IRAF_INDEFD) || (in_wmax == 0.0))  
           wmax = tmpmax;
      else
           wmax = in_wmax;

      if ((wmin > tmpmax) || (wmax < tmpmin)) {
           sprintf (ErrMsg, 
              "Data outside bounds of the specified wavelengths.  Order: %d.",
               rowptr[i]->sporder);
           e_warn (ErrMsg);
           sprintf (ErrMsg, "Data from this order will not be plotted.\n");
           e_warn (ErrMsg);
           continue;
      }
 
      if (wmin > wmax) {
          sprintf (ErrMsg, "Error in specification of wavelengths.  Order: %d.",
                            rowptr[i]->sporder);
          e_warn (ErrMsg);
          sprintf (ErrMsg, "Using computed wavelengths to generate graphics.\n");
          e_warn (ErrMsg);
          wmin = tmpmin;
          wmax = tmpmax;
      }

      /* Determine the indices associated with wmin & wmax limits */
      wmindex = 0;
      wmaxdex = 0;
      for (j = 0; j < rowptr[i]->nelem; j++) {
           if (*(rowptr[i]->wave + j) < wmin) 
               wmindex = j;
           if (*(rowptr[i]->wave + j) < wmax) 
               wmaxdex = j;
      }
      wval = wmaxdex - wmindex + 1;

      /* Allocate temporary array, type double, for intensity values */
      dflux = (double *) calloc (wval, sizeof(double));
      if (dflux == NULL) {
          sprintf (ErrMsg, "No memory to allocate temporary pointer.\n");
          e_error (ErrMsg);
          return  (-1);
      }

      /* Begin general plot set up */
      fputs ("### General plot set up and print the time.\n", fp);
      fputs ("erase\n", fp);

      /* Bottom label */
      fputs   ("justify 5; expand 0.5; vmove 0.8 0.01\n", fp);
      fprintf (fp, "label %s", stime);
      fputs   ("expand 0.8\n", fp);
      fputs   ("\n", fp);

      /* Set up the plot */
      fputs   ("### Define the input file and define the data columns.\n", fp);
      fprintf (fp, "data %s\n", inTable);
      fprintf (fp, "xcolumn WAVELENGTH %d;", *(row + i));
      fprintf (fp, " ycolumn %s %d\n", fluxName, *(row + i));
      fputs   ("\n", fp);

      fputs ("### Define the plot location.\n", fp);
      fputs ("location 0.15 0.90 0.15 0.90\n", fp);
      fputs ("\n", fp);

      /* Determine the range of the flux data */
      fmin = DBL_MAX;
      fmax = DBL_MIN;
      switch (*fluxCol) {
        case 'G': 
           for (j = wmindex; j <= wmaxdex; j++) 
                *(dflux + j - wmindex) = (double)(*(rowptr[i]->gross + j));
           break;
        case 'B': 
           for (j = wmindex; j <= wmaxdex; j++) 
                *(dflux + j - wmindex) = (double)(*(rowptr[i]->back + j));
           break;
        case 'N':
           for (j = wmindex; j <= wmaxdex; j++) 
                *(dflux + j - wmindex) = (double)(*(rowptr[i]->net + j));
           break;
        case 'F':
           for (j = wmindex; j <= wmaxdex; j++) 
                *(dflux + j - wmindex) = (double)(*(rowptr[i]->flux + j));
           break;
        case 'E':
           for (j = wmindex; j <= wmaxdex; j++) 
                *(dflux + j - wmindex) = (double)(*(rowptr[i]->error + j));
           break;
        case 'D':
           for (j = wmindex; j <= wmaxdex; j++) 
                *(dflux + j - wmindex) = (double)(*(rowptr[i]->dq + j));
           break;
        default:
           sprintf (ErrMsg, "Selected flux column - %s - is invalid.\n",
                    fluxCol);
           e_error (ErrMsg);
           return  (-1);
           break;
      }

      rangeFind (dflux, wval, &fmin, &fmax);

      fputs   ("### Set the plot limits and draw the box.\n", fp);
      fprintf (fp, "limits %.4f %.4f %.4g %.4g; margin; box\n", 
               wmin, wmax, fmin, fmax);
      fputs   ("\n", fp);

      fputs ("### Label the axes.\n", fp);
      fputs ("xlabel Wavelength (\\gV)\n", fp);
      fputs ("angle 90\n", fp);
      fputs ("vmove 0.06 0.5; justify 5\n", fp);
      fputs (ylabel, fp);
      fputs ("angle 0\n", fp);
      fputs ("\n", fp);

      fputs ("### Draw the curve.\n", fp);
      fputs ("connect\n", fp);
      fputs ("\n", fp);

      /* Add the spectral order number to the general plot title */
      if (strcmp(title,"") == 0)
          sprintf (sptitle, "%s%d\n", gtitle, rowptr[i]->sporder);

      fputs ("### Print the plot title.\n", fp);
      fputs ("expand 0.9\n", fp);
      fputs (sptitle, fp);
      fputs ("\n", fp);

      /* Bring up the cursor for view only mode */
      if (nrow > 1 && (i != (nrow-1)) && viewonly) {
          fputs ("### Cursor Mode.\n", fp);
          fputs ("curses\n\n", fp);
      }

      /* Clean up memory */
      free (dflux);
   }

   /* Close the output file and notify the user if appropriate */
   fclose (fp);
 
   if (!viewonly) {
       sprintf   (ErrMsg, "The output file - %s - has been written.\n", outScript);
       e_message (ErrMsg);
   }
 
   return (0);
}

/*****************************************************************************
 *
 * igiMultiple
 * Routine to generate the IGI script for plotting the specified echelle
 * orders, all selected orders in one plot on one page (landscape).
 * 05 December 1997 - MDD: wavelengths are floating-point values.
 * 04 June 1998     - MDD: Added viewonly capability.
 *
******************************************************************************/

int
igiMultiple (char *outScript, char *inTable, PhdInfo *ptr, char *fluxCol, 
             char *title, RowContents *rowptr[], int *row, int nrow, 
             double wmin, double wmax, Bool viewonly) {

   time_t date = time(NULL);    /* Date/Time                              */
   char   fluxName[SZ_NAME];    /* Flux column name                       */
   char   fluxUnits[SZ_NAME];   /* Flux column units                      */
   double fmax = DBL_MIN;       /* Flux limits for plot                   */
   double fmin = DBL_MAX;       /* Flux limits for plot                   */
   char   gtitle[SZ_NAME];      /* Generic plot title                     */
   int    j;                    /* Loop index                             */
   char   stime[SZ_NAME];       /* Date/Time string                       */
   int    status = 0;           /* Return status                          */
   char   ylabel[SZ_NAME];      /* Constructed y-axis label               */

   FILE   *fp = NULL;           /* Output file pointer                    */

   /* Allocate the file pointer for the output script */
   fp = fopen(outScript, "wt");
   if (fp == NULL) {
      sprintf (ErrMsg, "Problem opening output file - %s.\n", outScript);
      e_error (ErrMsg);
      return  (-1);
   }

   /* Initialize title */
   gtitle[0]    = '\0';
   fluxName[0]  = '\0';
   fluxUnits[0] = '\0';
   stime[0]     = '\0';
   ylabel[0]    = '\0';

   sprintf (stime, "%s", ctime(&date));

   /* If necessary, construct default general title */
   if (strcmp(title,"") == 0) 
       sprintf (gtitle, "title %s  %s  %s  %s  %s  %s  ", ptr->instrume,
                ptr->rootname, ptr->detector, ptr->cenwave, ptr->opt_elem,
                ptr->aperture);
   else
       sprintf (gtitle, "title %s\n", title);

   /* Determine the intensity column to plot */
   for (j = 0; j < NCOLS; j++) {
        if (strncmp(fluxCol, tblCol[j].colname, 3) == 0) {
            strcpy (fluxName, tblCol[j].colname);
            strcpy (fluxUnits, tblCol[j].colunits);
            break;
        }
   }
   
   /* Set up the ylabel from the chosen intensity value */
   sprintf (ylabel, "label %s %s\n", fluxName, fluxUnits);
   
   /* Define the characters */
   fputs ("### IGI script for MULTIPLE plot mode.\n", fp);
   fputs ("\n", fp);
   fputs ("### General plot set up and print the time.\n", fp);
   fputs ("fontset igi\n",fp);
   fputs ("erase\n", fp);

   /* Bottom label */
   fputs   ("justify 5; expand 0.5; vmove 0.8 0.01\n", fp);
   fprintf (fp, "label %s", stime);
   fputs   ("expand 0.8\n", fp);
   fputs   ("\n", fp);

   /* Identify the input data source */
   fputs   ("### Define the input file and set the plot location.\n", fp);
   fprintf (fp, "data %s\n", inTable);
   fputs   ("location 0.15 0.90 0.15 0.90\n", fp);
   fputs   ("\n", fp);

   /* Determine the plot wavelength and intensity limits */
   status = igiMultLimits (fluxCol, rowptr, nrow, &wmin, &wmax, 
                           &fmin, &fmax);
   if (status < 0) {
       sprintf (ErrMsg, "Problem determining limits for MULTIPLE option.\n");
       e_error (ErrMsg);
       return  (-1);
   }

   /* Loop over the chosen spectral orders */
   for (j = 0; j < nrow; j++) {

      fputs   ("### Define the data columns.\n", fp);
      fprintf (fp, "xcolumn WAVELENGTH %d;", *(row + j));
      fprintf (fp, " ycolumn %s %d\n", fluxName, *(row + j));
      fputs   ("\n", fp);

      if (j == 0) { 
         fputs   ("### Set the plot limits and draw the box.\n", fp);
         fprintf (fp, "limits %.4f %.4f %.4g %.4g; margin; box\n", 
                  wmin, wmax, fmin, fmax);
         fputs   ("\n", fp);

         fputs ("### Label the axes.\n", fp);
         fputs ("xlabel Wavelength (\\gV)\n", fp); 
         fputs ("angle 90\n", fp); 
         fputs ("vmove 0.06 0.5; justify 5\n", fp);
         fputs (ylabel, fp);
         fputs ("angle 0\n", fp);
         fputs ("\n", fp);
      }

      /* Plot the order */
      fputs ("### Draw the curve.\n", fp);
      fputs ("connect\n", fp);
      fputs ("\n", fp);
   }

   /* Plot title */
   fputs ("### Print the plot title.\n", fp);
   fputs ("expand 0.9\n", fp);
   fputs (gtitle, fp);
   fputs ("\n", fp);

   /* Close the output file and notify the user if appropriate */
   fclose (fp);
 
   if (!viewonly) {
       sprintf   (ErrMsg, "The output file - %s - has been written.\n", outScript);
       e_message (ErrMsg);
   }

   return (0);
}

/*****************************************************************************
 *
 * igiMultLimits
 * Routine to determine the wavelength and flux limits for plotting multiple
 * orders on one plot.
 * 01 December 1997 - MDD: Added checking on input wavelength limits.
 * 02 February 1998 - MDD: Y-range determined only over requested wavelength
 *                         regime.
 *
******************************************************************************/

int
igiMultLimits (char *fluxCol, RowContents *rowptr[], int nrow, 
               double *wminMult, double *wmaxMult, double *fminMult, 
               double *fmaxMult) {

   double dummy  = 0.0;                /* Dummy value                        */
   double fmin   = DBL_MAX;            /* Minimum intensity in wave range    */
   double fmax   = DBL_MIN;            /* Maximum intensity in wave range    */
   int    i, j;                        /* Generic loop indices               */
   double tmpmin = DBL_MAX;            /* Temporary min wave for all orders  */
   double tmpmax = DBL_MIN;            /* Temporary max wave for all orders  */
   double *wmin  = NULL;               /* Array of minimum wavelengths       */
   double *wmax  = NULL;               /* Array of maximum wavelengths       */

   /* Allocate arrays that hold the min/max wavelength and intensity */
   wmin = (double *) calloc (nrow, sizeof(double));
   wmax = (double *) calloc (nrow, sizeof(double));
   if ((wmin == NULL) || (wmax == NULL)) {
        sprintf (ErrMsg, "No memory to allocate min/max pointers.\n");
        e_error (ErrMsg);
        return  (-1);
   }

   /* Initialize variables */
   for (i = 0; i < nrow; i++) {
        wmin[i] = DBL_MAX;
        wmax[i] = DBL_MIN;
   }

   /* Obtain the min/max wavelengths of the selected orders */
   for (i = 0; i < nrow; i++) 
        rangeFind (rowptr[i]->wave, rowptr[i]->nelem, &wmin[i], &wmax[i]);

   /* Determine the wavelength extremes for the plot based on the data */
   rangeFind (wmin, (short) nrow, &tmpmin, &dummy);
   rangeFind (wmax, (short) nrow, &dummy, &tmpmax);

   /* Check the user provided wavelength limits.  If set, make sure that *
    * the wavelength bounds are reasonable.  The check for == 0.0 is for *
    * backwards compatibility with v1.0.                                 */
   if ((*wminMult == IRAF_INDEFD) || (*wminMult == 0.0))  
       *wminMult = tmpmin;
   if ((*wmaxMult == IRAF_INDEFD) || (*wmaxMult == 0.0))  
       *wmaxMult = tmpmax;

   if ((*wminMult > tmpmax) || (*wmaxMult < tmpmin)) {
        sprintf (ErrMsg, "Data outside bounds of the specified wavelengths.");
        e_warn (ErrMsg);
        sprintf (ErrMsg, "Using computed wavelengths to generate graphics.\n");
        e_warn (ErrMsg);
        *wminMult = tmpmin;
        *wmaxMult = tmpmax;
   }
 
   if (*wminMult > *wmaxMult) {
       sprintf (ErrMsg, "Error in specification of wavelengths.");
       e_warn (ErrMsg);
       sprintf (ErrMsg, "Using computed wavelengths to generate graphics.\n");
       e_warn (ErrMsg);
       *wminMult = tmpmin;
       *wmaxMult = tmpmax;
   }
  
   /* Determine the flux extremes based upon the wavelength regime *
    * actually included in the plot.                               */
   for (i = 0; i < nrow; i++) {
        if ((*wminMult < wmax[i]) && (*wmaxMult > wmin[i])) {

           switch (*fluxCol) {
             case 'G': 
                for (j = 0; j < (rowptr[i]->nelem) - 1; j++) {
                     if ((*wminMult <= *(rowptr[i]->wave + j)) &&
                         (*wmaxMult >= *(rowptr[i]->wave + j))) {
                          if (*(rowptr[i]->gross + j) < fmin)
                                fmin = *(rowptr[i]->gross + j);
                          if (*(rowptr[i]->gross + j) > fmax)
                                fmax = *(rowptr[i]->gross + j);
                     }
                }
                break;
             case 'B': 
                for (j = 0; j < (rowptr[i]->nelem) - 1; j++) {
                     if ((*wminMult <= *(rowptr[i]->wave + j)) &&
                         (*wmaxMult >= *(rowptr[i]->wave + j))) {
                          if (*(rowptr[i]->back + j) < fmin)
                                fmin = *(rowptr[i]->back + j);
                          if (*(rowptr[i]->back + j) > fmax)
                                fmax = *(rowptr[i]->back + j);
                     }
                }
                break;
             case 'N':
                for (j = 0; j < (rowptr[i]->nelem) - 1; j++) {
                     if ((*wminMult <= *(rowptr[i]->wave + j)) &&
                         (*wmaxMult >= *(rowptr[i]->wave + j))) {
                          if (*(rowptr[i]->net + j) < fmin)
                                fmin = *(rowptr[i]->net + j);
                          if (*(rowptr[i]->net + j) > fmax)
                                fmax = *(rowptr[i]->net + j);
                     }
                }
                break;
             case 'F':
                for (j = 0; j < (rowptr[i]->nelem) - 1; j++) {
                     if ((*wminMult <= *(rowptr[i]->wave + j)) &&
                         (*wmaxMult >= *(rowptr[i]->wave + j))) {
                          if (*(rowptr[i]->flux + j) < fmin)
                                fmin = *(rowptr[i]->flux + j);
                          if (*(rowptr[i]->flux + j) > fmax)
                                fmax = *(rowptr[i]->flux + j);
                     }
                }
                break;
             case 'E':
                for (j = 0; j < (rowptr[i]->nelem) - 1; j++) {
                     if ((*wminMult <= *(rowptr[i]->wave + j)) &&
                         (*wmaxMult >= *(rowptr[i]->wave + j))) {
                          if (*(rowptr[i]->error + j) < fmin)
                                fmin = *(rowptr[i]->error + j);
                          if (*(rowptr[i]->error + j) > fmax)
                                fmax = *(rowptr[i]->error + j);
                     }
                }
                break;
             case 'D':
                for (j = 0; j < (rowptr[i]->nelem) - 1; j++) {
                     if ((*wminMult <= *(rowptr[i]->wave + j)) &&
                         (*wmaxMult >= *(rowptr[i]->wave + j))) {
                          if (*(rowptr[i]->dq + j) < fmin)
                                fmin = *(rowptr[i]->dq + j);
                          if (*(rowptr[i]->dq + j) > fmax)
                                fmax = *(rowptr[i]->dq + j);
                     }
                }
                break;
             default:
                sprintf (ErrMsg, "Selected flux column - %s - is invalid.\n",
                         fluxCol);
                e_error (ErrMsg);
                return  (-1);
                break;
           }
      }
   }
   *fminMult = fmin;
   *fmaxMult = fmax;
  
   /* Clear memory */
   free (wmin);
   free (wmax);

   return (0);
}

/*****************************************************************************
 *
 * igiPanel
 * Routine to generate the IGI script for plotting the specified echelle
 * orders, one spectral order per plot, four plots to a page (portrait).
 *
******************************************************************************/

int
igiPanel (char *outScript, char *inTable, PhdInfo *ptr, char *fluxCol, 
          char *title, RowContents *rowptr[], int *row, int nrow,
          Bool viewonly) {

   int  i, j;                      /* Loop indices                           */
   int  np;                        /* Panel counter                          */
   int  npanel = 4;                /* No. of graphic panels per page         */
   char gtitle[SZ_NAME];           /* Generic plot title                     */
   char sptitle[SZ_NAME];          /* Generic plot title with spectral order */
   char stime[SZ_NAME];            /* Date/Time string                       */
   char fluxName[SZ_NAME];         /* Flux column name                       */
   char fluxUnits[SZ_NAME];        /* Flux column units                      */
   char ylabel[SZ_NAME];           /* Constructed y-axis label               */
   time_t date = time(NULL);       /* Date/Time                              */

   FILE   *fp = NULL;              /* Output file pointer                    */

   /* Allocate the file pointer for the output script */
   fp = fopen(outScript, "wt");
   if (fp == NULL) {
      sprintf (ErrMsg, "Problem opening output file - %s.\n", outScript);
      e_error (ErrMsg);
      return  (-1);
   }
   
   /* Initialize title */
   gtitle[0]    = '\0';
   sptitle[0]   = '\0';
   fluxName[0]  = '\0';
   fluxUnits[0] = '\0';
   stime[0]     = '\0';
   ylabel[0]    = '\0';

   sprintf (stime, "%s\n", ctime(&date));

   /* If necessary, construct default general title */
   if (strcmp(title,"") == 0) 
       sprintf (gtitle, "title %s  %s  %s  %s  %s  %s  ", ptr->instrume,
                ptr->rootname, ptr->detector, ptr->cenwave, ptr->opt_elem,
                ptr->aperture);
   else
       sprintf (sptitle, "title %s\n", title);

   /* Determine the intensity column to plot */
   for (i = 0; i < NCOLS; i++) {
        if (strncmp(fluxCol, tblCol[i].colname, 3) == 0) {
            strcpy (fluxName, tblCol[i].colname);
            strcpy (fluxUnits, tblCol[i].colunits);
            break;
        }
   }
   
   /* Set up the ylabel from the chosen intensity value */
   sprintf (ylabel, "label %s\n", fluxName);

   /* Write comments in igi script. */
   fputs ("### IGI script for PANEL plot mode.\n", fp);
   fputs ("\n", fp);

   /* Define the characters */
   fputs ("### Set up the software font, the four plotting windows, and define input file.\n", fp);
   fputs ("fontset igi\n",fp);

   fprintf (fp, "window 1 %d\n", npanel);
   fprintf (fp, "data %s\n", inTable);
   fputs   ("\n", fp);

   /* Loop over the chosen spectral orders */
   for (j = 0; j < nrow; j++) {

      /* Determine which panel and erase if it is a new page */
      /* Force the first plot to be at the top of the page   */
      np = abs((j % npanel) + 1 - (npanel + 1));
      if (np == npanel) {

          /* Bring up the cursor for view only mode */
          if (j > 0 && viewonly) {
              fputs ("### Cursor Mode.\n", fp);
              fputs ("curses\n\n", fp);
          }

          fputs ("### Start a new plot page.\n", fp);
          fputs ("erase\n", fp);
          fputs ("\n", fp);
      }
      fputs   ("### Set up plotting for a specific window.\n", fp);
      fputs   ("expand 0.7\n", fp);
      fprintf (fp, "window %d\n",np);
      fputs   ("\n", fp);

      /* Set up the plot */
      fputs   ("### Define define the data columns.\n", fp);
      fprintf (fp, "xcolumn WAVELENGTH %d;", *(row +  j));
      fprintf (fp, " ycolumn %s %d\n", fluxName, *(row + j));
      fputs   ("\n", fp);

      fputs ("### Set the plot location, limits, and draw the box.\n", fp);
      fputs ("location 0.12 0.9 0.25 0.85\n", fp);
      fputs ("limits; margin; box\n", fp);
      fputs ("\n", fp);

      fputs ("### Label the x-axis.\n", fp);
      fputs ("xlabel Wavelength (\\gV)\n", fp);
      fputs ("angle 90\n", fp);
      fputs ("\n", fp);

      fputs ("### Draw the curve.\n", fp);
      fputs ("connect\n", fp);
      fputs ("\n", fp);

      /* Add the spectral order number to the general plot title */
      if (strcmp(title,"") == 0)
          sprintf (sptitle, "%s%d\n", gtitle, rowptr[j]->sporder);

      fputs ("### Print the plot title.\n", fp);
      fputs ("expand 0.8\n", fp);
      fputs (sptitle, fp);
      fputs ("\n", fp);

      fputs ("### Label the y-axis.\n", fp);
      fputs ("vmove 0.04 0.5; justify 5\n", fp);
      fputs (ylabel, fp);
      fputs ("angle 0\n", fp);
      fputs ("\n", fp);
   }

   /* Close the output file and notify the user if appropriate */
   fclose (fp);
 
   if (!viewonly) {
       sprintf   (ErrMsg, "The output file - %s - has been written.\n", outScript);
       e_message (ErrMsg);
   }
 
   return (0);
}

/*****************************************************************************
 *
 * igiDiag
 * Routine to generate the IGI script for plotting all data arrays of the
 * specified echelle orders, one order per page (portrait).
 * M.D. De La Pena - 13 March 1998: Added a check to make sure there are
 *    data quality flags to plot.
 * 04 June 1998 - MDD: Added viewonly capability.
 *
******************************************************************************/

int
igiDiag (char *outScript, char *inTable, PhdInfo *ptr, char *title, 
         RowContents *rowptr[], int *row, int nrow, Bool viewonly) {

   double *dflux = NULL;           /* Type double of intensity array       */
   int    endpt    = 0;            /* Memory offset of last array position */
   double fmax = DBL_MIN;          /* Flux limits for plot                 */
   double fmin = DBL_MAX;          /* Flux limits for plot                 */
   char   gtitle[SZ_NAME];         /* Generic title                        */
   int    i, j, k, l;              /* Loop indices                         */
   int    index   = 0;             /* Array index                          */
   char   sptitle[SZ_NAME];        /* Generic title with spectral order    */
   int    status  = 0;             /* Return status                        */
   float  ybot[NCOLS];             /* Bottom coordinates for each panel    */
   float  yincrement = 0.0;        /* Y-increment of plots                 */
   float  ylimits[]  = {0.1, 0.9}; /* Total y-size of entire plot          */
   float  yloc[NCOLS];             /* Midpoint coordinate for each panel   */
   float  ytop[NCOLS];             /* Top coordinates for each panel       */
   float  ytmp    = 0.0;           /* Temporary value                      */
   double wmax = DBL_MIN;          /* Wavelength limits for plot           */
   double wmin = DBL_MAX;          /* Wavelength limits for plot           */

   DataDQ *headptr  = NULL;        /* Point to the head of the linked list */
   FILE   *fp = NULL;              /* Output file pointer                  */

   /* Allocate the file pointer for the output script */
   fp = fopen(outScript, "wt");
   if (fp == NULL) {
      sprintf (ErrMsg, "Problem opening output file - %s.\n", outScript);
      e_error (ErrMsg);
      return  (-1);
   }
   
   /* Initialize title */
   gtitle[0]    = '\0';
   sptitle[0]   = '\0';

   /* Compute the locations of each panel */
   yincrement = (ylimits[1] - ylimits[0]) / (float) NCOLS;
   ybot[0]    = ylimits[0];
   ytop[0]    = ylimits[0] + yincrement;
   ytmp       = (ytop[0] - ybot[0]) / 2.0;
   yloc[0]    = ybot[0] + ytmp;
   for (i = 1; i < NCOLS; i++) {
        ybot[i] = ytop[i - 1];
        ytop[i] = ybot[i] + yincrement;
        yloc[i] = yloc[i - 1] + yincrement;
   }

   /* If necessary, construct default general title */
   if (strcmp(title,"") == 0) 
       sprintf (gtitle, "title %s  %s  %s  %s  %s  %s  ", ptr->instrume,
                ptr->rootname, ptr->detector, ptr->cenwave, ptr->opt_elem,
                ptr->aperture);
   else
       sprintf (sptitle, "title %s\n", title);

   /* Define the characters */
   fputs ("### IGI script for DIAGNOSTIC plot mode.\n", fp);
   fputs ("\n", fp);

   fputs ("### General plot set up.\n", fp);
   fputs ("fontset igi\n",fp);
   fputs ("expand 0.7\n", fp);
   fputs ("justify 5\n", fp);
   fputs ("angle 0\n", fp);

   /* Allocate temporary array, type double, for intensity values */
   dflux = (double *) calloc (rowptr[0]->nelem, sizeof(double));
   if (dflux == NULL) {
       sprintf (ErrMsg, "No memory to allocate temporary pointer.\n");
       e_error (ErrMsg);
       return  (-1);
   }

   /* Loop over the chosen spectral orders */
   for (i = 0; i < nrow; i++) {
        fputs ("erase\n", fp);
        fputs ("\n", fp);

        /* Loop over the data arrays for one order */
        for (j = 0; j < NCOLS; j++) {
             index = NCOLS - j - 1;

             /* Set the physical location of the plot starting at the bottom */
             fputs   ("### Define the plot location.\n", fp);
             fprintf (fp, "location 0.17 0.95 %.2f %.2f\n", ybot[j], ytop[j]);
             fputs   ("\n", fp);

             /* If the DQ data, then determine bit settings */
             if (strcmp(tblCol[index].colname, "DQ") == 0) {

                 /* Decompose the data quality flags */
                 headptr = CreateList (rowptr[i]->dq, rowptr[i]->wave, 
                                       rowptr[i]->nelem, headptr);
                 wmin  = (*(rowptr[i]->wave));
                 endpt = (rowptr[i]->nelem) - 1;
                 wmax  = (*(rowptr[i]->wave + endpt));

                 fputs   ("### Set DQ plot limits, draw a grid and the box.\n", fp);
                 fprintf (fp, "limits %.4f %.4f 0 15\n", wmin, wmax);
                 fputs   ("ticksize 0 0 1 2\n", fp);
                 fputs   ("margin; box\n", fp);
                 fputs   ("ltype dotted; grid\n", fp);
                 fputs   ("ticksize 0 0 0 0\n", fp);
                 fputs   ("ltype 0\n", fp);
                 fputs   ("\n", fp);

                 fputs ("### Label the x-axis.\n", fp);
                 fputs ("xlabel Wavelength (\\gV)\n\n", fp);

                 /* If any data quality flags, plot them. */
                 if (headptr != NULL) {
                     fputs ("### Move and draw the DQ bits.\n", fp);
                     fputs ("expand 0.2\n", fp);
                     fputs ("ptype 4 3\n", fp);

                     /* Print the contents of DQ linked list */
                     PrintList (fp, headptr);

                     fputs ("expand 0.7\n", fp);
                     fputs ("\n", fp);

                     /* Delete the DQ linked list for this order */
                     DeleteList (headptr);
                     headptr = NULL;
                 }
             }
             else {

                 /* Set up the data */
                 fputs   ("### Define the input file and data columns.\n", fp);
                 fprintf (fp, "data %s\n", inTable);
                 fprintf (fp, "xcolumn WAVELENGTH %d;", *(row +  i));
                 fprintf (fp, " ycolumn %s %d\n", tblCol[index].colname, 
                               *(row + i));
                 fputs   ("\n", fp);

                 /* Determine the yrange */
                 switch (tblCol[index].colname[0]) {
                   case 'G': 
                      for (l = 0; l < (rowptr[i]->nelem) - 1; l++) 
                           *(dflux + l) = (double)(*(rowptr[i]->gross + l));
                      break;
                   case 'B': 
                      for (l = 0; l < (rowptr[i]->nelem) - 1; l++) 
                           *(dflux + l) = (double)(*(rowptr[i]->back + l));
                      break;
                   case 'N':
                      for (l = 0; l < (rowptr[i]->nelem) - 1; l++) 
                           *(dflux + l) = (double)(*(rowptr[i]->net + l));
                      break;
                   case 'F':
                      for (l = 0; l < (rowptr[i]->nelem) - 1; l++) 
                           *(dflux + l) = (double)(*(rowptr[i]->flux + l));
                      break;
                   case 'E':
                      for (l = 0; l < (rowptr[i]->nelem) - 1; l++) 
                           *(dflux + l) = (double)(*(rowptr[i]->error + l));
                      break;
                   default:
                      sprintf (ErrMsg, "Flux column - %s - is invalid.\n",
                               tblCol[index].colname);
                      e_error (ErrMsg);
                      return  (-1);
                      break;
                 }

                 fmin = DBL_MAX;
                 fmax = DBL_MIN;
                 rangeFind (dflux, rowptr[i]->nelem, &fmin, &fmax);

                 fprintf (fp, "### Set the plot limits and draw the %s box.\n",
                          tblCol[index].colname);
                 fprintf (fp, "limits %.4f %.4f %.4g %.4g\n",
                               wmin, wmax, fmin, fmax);
                 fputs   ("margin; box 0 2\n", fp);
                 fputs   ("\n", fp);

                 fputs ("### Draw the curve.\n", fp);
                 fputs ("connect\n", fp);
                 fputs ("\n", fp);
             }
      }

      /* Add the spectral order number to the general plot title */
      if (strcmp(title,"") == 0)
          sprintf (sptitle, "%s%d\n", gtitle, rowptr[i]->sporder);

      fputs ("### Print the plot title.\n", fp);
      fputs ("expand 0.9\n", fp);
      fputs (sptitle, fp);
      fputs ("\n", fp);

      /* Apply the appropriate y-labels */
      fputs ("### Print the y-axis labels.\n", fp);
      fputs ("expand 0.7\n", fp);
      fputs ("location 0.1 0.9 0.1 1.0\n", fp);
      fputs ("angle 90\n", fp);
      for (j = 0; j < NCOLS; j++) {
           index = NCOLS - j - 1;
           fprintf (fp, "vmove 0.07 %.2f\n", yloc[j]);
           if (strcmp(tblCol[index].colname, "BACKGROUND") == 0)
               fputs ("label BACK\n", fp);
           else
               fprintf (fp, "label %s\n", tblCol[index].colname);

           if (strcmp(tblCol[index].colname, "DQ") == 0) {
               fprintf (fp, "vmove 0.09 %.2f\n", yloc[j]);
               fputs ("label (bit)\n", fp);
           }
      }

      /* Bring up the cursor for view only mode */
      if (nrow > 1 && (i != (nrow-1)) && viewonly) {
          fputs ("\n### Cursor Mode.\n", fp);
          fputs ("curses\n\n", fp);
      }
   }

   /* Close the output file and notify the user if appropriate */
   fclose (fp);
 
   if (!viewonly) {
       sprintf   (ErrMsg, "The output file - %s - has been written.\n", outScript);
       e_message (ErrMsg);
   }

   /* Clear memory */
   free (dflux);

   return (0);
}
