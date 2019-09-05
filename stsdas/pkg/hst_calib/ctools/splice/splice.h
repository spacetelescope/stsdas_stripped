# define STIS_LINE  1023
# define STIS_CNAME   79

# define EXPLICIT_WL   0	/* wavelengths specified in an input table */
# define MIN_D_WL      1	/* fine spacing of output wavelengths */
# define MAX_D_WL      2	/* coarse spacing */

# define NELEM_INDEX   0
# define WL_INDEX      1
# define FLUX_INDEX    2
# define ERR_INDEX     3
# define DQ_INDEX      4
# define WGT_INDEX     5
# define SW_INDEX      6
# define NCOLS         7		/* number of column names */

/* The array of wavelengths contains nelem+1 elements.  The values are the
   wavelengths at the start of each bin, except that the nelem+1 value is
   the wavelength at the end of the last bin.
*/
typedef struct {
	int nelem;		/* number of elements in the following arrays */
	double *wl;		/* wavelengths at start of bin */
	double *flux;		/* flux within bins */
	double *error;		/* errors */
	short *dq;		/* data quality flags */
	double *weight;		/* weights */
	double scalar_weight;	/* e.g. exposure time */
} Spectrum;

typedef struct {
	int nspec;		/* current number of spectra */
	int max_spec;		/* allocated size of array */
	Spectrum **spec;	/* array of spectra */
} SpecArray;
