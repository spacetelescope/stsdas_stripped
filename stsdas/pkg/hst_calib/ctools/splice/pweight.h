# define STIS_CBUF      25
# define STIS_FNAME    129

# define ARRAY_SIZE_ERROR         4
# define OUT_OF_MEMORY          111
# define OPEN_FAILED            114
# define TABLE_ERROR            141
# define COLUMN_NOT_FOUND       142
# define GENERIC_ERROR_CODE    1111

# define OMIT      0		/* codes for HELCORR calibration switch */
# define COMPLETE  2

# define UNKNOWN   (-1)		/* value for extrsize if it wasn't specified */

# define AREA_HST 45238.93416	/* cm**2 */

typedef struct {
	int verbose;			/* print additional info? */

	/* info about input image */
	char aperture[STIS_CBUF+1];	/* aperture name */
	char opt_elem[STIS_CBUF+1];	/* name of grating or mirror */
	int helcorr;		/* calibration flag (heliocentric correction) */
	int cenwave;			/* central wavelength */
	double exptime;			/* exposure time in seconds */
	double ra_targ, dec_targ;	/* target coordinates */
	double time_of_exp;		/* time (MJD) of middle of exposure */
	double hfactor;			/* heliocentric correction factor */
	int sporder;			/* value from first row or header */
	int extrsize;			/* height of extraction box */

	int nrows;			/* number of rows in input table */

	/* calibration tables */
	char phottab[STIS_FNAME+1];	/* photometric throughput table */
	char pctab[STIS_FNAME+1];	/* photometric correction table */
	char apertab[STIS_FNAME+1];	/* aperture throughput table */
} WgtInfo;

/* This contains the throughput curve (the _pht table), the photometric
   correction curve (the _pct table, resampled to the same wavelengths
   as the _pht), and the aperture throughput curve (the _apt table).
*/

typedef struct {

	/* The first three are from the phottab (_pht). */
	int p_nelem;		/* size of arrays */
	double *p_wl;		/* array of wavelengths from _pht */
	double *p_thru;		/* array of throughputs (QE) */

	/* these are from the pctab (_pct) */
	int pct_nelem;		/* size of arrays */
	double *pct_wl;		/* wavelengths for pct_ratio */
	double *pct_ratio;	/* pct_h / pct_inf */

	/* these three are from the apertab (_apt) */
	int f_nelem;		/* size of arrays */
	double *f_wl;		/* array of wavelengths from _apt */
	double *f_thru;		/* array of filter throughputs */
} PhotInfo;
