/* trxyeq.h */

# define SZ_CBUF       65	/* buffer for e.g. filter */
# define SZ_FNAME     255

/* Distortion information, used for imaging mode only, read from _idc.
   The members here are all in reference pixels.
*/

# define MAX_ORDER   5			/* maximum degree of polynomial */
# define DCM_SIZE  (MAX_ORDER + 1)	/* distortion coefficient matrix size */
# define MAX_NCOEFF (DCM_SIZE * DCM_SIZE)	/* number of coefficients */
# define WHICH_COEFF(i,j) (i*DCM_SIZE + j)

typedef struct {
	int allocated;		/* true if memory has been allocated */
	int forward;		/* true if direction is "FORWARD" */
	int norder;		/* order of the polynomial fit */
	double scale;		/* arcseconds per reference pixel */
	double xref, yref;	/* zero points in input (zero indexed) */
	double offset[2];	/* so distortion is zero at crpix[12] */
	double *xcoeff, *ycoeff;	/* coefficients for X and Y */
} DistInfo;

void geocorr (IRAFPointer, DistInfo *, double [], double [], double, double,
		double *, double *, double *, double *);
void geoOffset (IRAFPointer, DistInfo *, double [], double []);
int getIDC (char *, char *, char *, char *, DistInfo *);
void trim_blanks (char *);
int getPrimaryInfo (char *, char *, char *, char *, char *);
void getLTInfo (IRAFPointer, double [], double []);
void freeDist (DistInfo *);
int checkError (void);
double getRA (char **);
double getDec (char **);
