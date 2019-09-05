#ifndef E_ECHSCRIPT_H
#define E_ECHSCRIPT_H

/* Define the structure for the table descriptor */
typedef struct {
   IRAFPointer tp;              /* Pointer to the table  */
   int         nrows;           /* No. rows in table     */
   int         array_size;      /* No. of items in array */
   IRAFPointer isporder;        /* Spectral order        */
   IRAFPointer inelem;          /* No. elements in row   */
   IRAFPointer iwavelength;     /* Wavelengths           */
   IRAFPointer igross;          /* Gross flux            */
   IRAFPointer ibackground;     /* Background flux       */
   IRAFPointer inet;            /* Net flux              */
   IRAFPointer iflux;           /* Calibrated net flux   */
   IRAFPointer ierror;          /* Calibrated error      */
   IRAFPointer idq;             /* Data quality          */
} TblDesc;

/* Define the structure for the primary header information of the table */
typedef struct {
   IRAFPointer hd;              /* Pointer to primary header */
   char instrume[20];
   char rootname[20];
   char detector[20];
   char cenwave[20];
   char opt_elem[20];
   char aperture[20];
} PhdInfo;

/* Define the structure for the row contents of the table */
typedef struct {
   short  sporder;
   short  nelem;
   double *wave;
   float  *gross;
   float  *back;
   float  *net;
   float  *flux;
   float  *error;
   short  *dq;
} RowContents;

/* Define the node contents for the linked list */
struct datadq
{
  double waveDQ;
  short  flagsDQ;
  struct datadq *next;
};
typedef struct datadq DataDQ;


/*** Declare the function prototypes ***/

/***** Functions found in igisetup.c                             *****/
/* igiSingle   - single spectral order per plot (landscape)            */
/* igiMultiple - multiple spectral orders in a single plot (landscape) */
/* igiMultLimits - determine the wavelengths and intensity plot limits */
/* igiPanel    - multiple spectral orders, each in a separate plot,    */
/*               four panels to a page (portrait)                      */
/* igiDiag     - single spectral order per page (portrait) -- all data */
/*               arrays are plotted                                    */

int igiSingle     (char *, char *, PhdInfo *, char *, char *, RowContents *[], 
                   int *, int, double, double, Bool);
int igiMultiple   (char *, char *, PhdInfo *, char *, char *, RowContents *[], 
                   int *, int, double, double, Bool);
int igiMultLimits (char *, RowContents *[], int, double *, double *, double *,
                   double *);
int igiPanel      (char *, char *, PhdInfo *, char *, char *, RowContents *[], 
                   int *, int, Bool);
int igiDiag       (char *, char *, PhdInfo *, char *, RowContents *[], 
                   int *, int, Bool);

/***** Functions for the DQ linked list                            *****/
/* CreateNode - allocate a pointer to a new empty DQ node              */
/* CreateList - generate the linked list of wavelength/flag values     */
/* DecodeDQ   - decode each DQ flag into the component bit settings    */
/* AddToFront - add each new wavelength/DQ flag to the front of the LL */
/* DeleteList - walk the list and delete each node                     */
/* PrintList  - walf the list and print the wavelength and DQ value    */

DataDQ * CreateNode (void); 
DataDQ * CreateList (short *, double *, int, DataDQ *);
int      DecodeDQ   (int , int *);
DataDQ * AddToFront (DataDQ *, DataDQ *);
void     DeleteList (DataDQ *);
void     PrintList  (FILE *, DataDQ *);

/* Functions found in errors.c */
void	  e_warn      (char *);
void	  e_error     (char *);
void	  e_message   (char *);

/*** End declaration of the function prototypes ***/

/* Definitions */
# define	SZ_NAME		256	/* File name maximum size */
extern	char	ErrMsg[81];		/* Error message text     */

#endif
