/* CALNIC.H: Main include file for CALNICA and CALNICB tasks.
** Defines things that are common to both tasks.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Oct.  1996	Upgraded for Build 2 definitions
** H.Bushouse	27-Jan-1997	Added "dummy" to RefFile structure
** H.Bushouse	23-Oct-1997	Increased MsgText size from SZ_LIN to 2*SZ_LIN
**				(Version 2.2)
** H.Bushouse	19-Mar-1998	Added ZEROSIG dq flag value and changed
**				BAD_BACK dq macro to GROT (Version 2.2)
*/

# define	SZ_NAME		128	/* String length for file names */
# define	SZ_LIN		80	/* String length for header lines */
# define	SZ_KW		8	/* String length for keyword names */
# define	SZ_STRKWVAL	68	/* String length for keyword values */

# define	SZ_NICIMG	256	/* Size of a NICMOS image */
# define	MAX_MAREADS	26	/* Max number of MultiAccum reads */

/* Data Quality Flag values */
# define	REED_SOL	1	/* Reed-Solomon decoding error */
# define	BAD_LIN		2	/* Poor linearity correction */
# define	BAD_DARK	4	/* Poor dark correction */
# define	BAD_FLAT	8	/* Poor flat field correction */
# define	GROT		16	/* Pixel affected by "grot" */
# define	DEFECTIVE	32	/* Hot or cold pixel */
# define	SATURATED	64	/* Saturated pixel */
# define	MISSING		128	/* Missing data (telemetry dropout) */
# define	BADPIX		256	/* Bad pixel set during calibration */
# define	CR_HIT		512	/* Cosmic Ray hit */
# define	SOURCE		1024	/* Pixel contains source */
# define	ZEROSIG		2048	/* Zero read signal correction */

/* Error variables */
extern	int	status;			/* status value */
extern	char	MsgText[2*SZ_LIN+1];	/* message string */

/* Error functions */
void	errchk    ();			/* HSTIO error check */
void	n_kwerr   (char *, char *);	/* keyword error */
void	n_openerr (char *);		/* error opening file */
void	n_readerr (char *);		/* error reading file */
void	n_filerr  (char *);		/* generic file error */
void	n_message (char *);		/* write normal message */
void	n_warn    (char *);		/* write warning message */
void	n_error   (char *);		/* write error message */

/* Calibration switch values */
enum CalSwitch_ {BLANK, OMIT, PERFORM, SKIP, OMITTED, PERFORMED, SKIPPED};
typedef enum CalSwitch_ CalSwitch;

/* Observing mode values */
enum ObsModes_ {ACCUM, MULTIACCUM, RAMP, BRIGHTOBJ, ACQ};
typedef enum ObsModes_ ObsModes;

/* Reference file information */
typedef struct {
	char name[SZ_NAME+1];		/* Reference file name */
	char pedigree[SZ_STRKWVAL+1];	/* Ref file pedigree string */
	char descrip[SZ_STRKWVAL+1];	/* Ref file descrip string */
	Bool dummy;			/* Is ref file dummy? */
} RefFile;

/* Calibration step information */
typedef struct {
	CalSwitch corr;			/* calibration switch value */
	CalSwitch done;			/* calibration indicator value */
	char swname[SZ_KW+1];		/* calibration switch keyword name */
	char indname[SZ_KW+1];		/* calibration indicator keyword name */
	char pdname[SZ_KW+1];		/* calibration pedigree keyword name */
	RefFile ref;			/* calibration ref file structure */
} CalStep;

