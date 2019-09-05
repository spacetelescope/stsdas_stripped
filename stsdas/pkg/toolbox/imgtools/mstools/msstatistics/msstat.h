# include <c_iraf.h>

# define	VERSION		"1.7"

# define	SZ_EXT		5	/* File name extension maximum size */
# define	SZ_OUTLINE	320	/* Output line maximum size         */
# define	SZ_STR		50	/* Work string size                 */
# define	HLIM		32768   /* Max number of bins in histogram  */
# define	SZ_GNIC		5       /* NICMOS group size                */
# define	SZ_GSTIS	3       /* STIS group size                  */
# define	SZ_GACS		3       /* ACS group size                   */
# define	TYPE_STAT	0	/* Dictionary and string parsing    */
# define	TYPE_HDU	1 	/* directives                       */
# define	TYPE_CLHDU	2
# define	OUT_PSET	"egstp" /* Output pset name                 */
# define	E_INDEF		0.0F	/* Not the best choice. See e_report*/


/* NICMOS data quality bits */
# define	N_RS		  1    /* Reed-Solomon                      */
# define	N_PLIN		  2    /* Poor linearity correction         */
# define	N_PDARK		  4    /* Poor dark correction              */
# define	N_PFF		  8    /* Poor flat-field correction        */
# define	N_PBCK		  16   /* Grot                              */
# define	N_DEFECT	  32   /* Defective pixel                   */
# define	N_SATUR		  64   /* Saturated pixel                   */
# define	N_TELEM		  128  /* Telemetry problem                 */
# define	N_BADPIX	  256  /* Bad pixel determ. by calibration  */
# define	N_CRAY		  512  /* Cosmic ray                        */
# define	N_SOURCE	  1024 /* Source                            */
# define	N_ZREAD		  2048 /* Zero-read signal correction       */
# define	N_USER1		  4096 /* User 1                            */
# define	N_USER2		  8192 /* User 2                            */

/* STIS data quality bits */
# define	S_RS		  1    /* Reed-Solomon                      */
# define	S_FILL		  2    /* Data replace by fill value        */
# define	S_BADDET	  4    /* Bad detector pixel                */
# define	S_MASK		  8    /* Data masked or beyond aperture    */
# define	S_LDARK		 16    /* Large dark rate                   */
# define	S_LFBLEM	 32    /* Large flat field blemish          */
# define	S_RES1		 64    /* Reserved                          */
# define	S_RES2		128    /* Reserved                          */
# define	S_SAT		256    /* Saturated                         */
# define	S_BADREF	512    /* Bad pixel in reference file       */
# define	S_SFBLEM       1024    /* Small flat field blemish          */
# define	S_EXTBACK      2048    /* Uncertain background in extraction*/
# define	S_EXTBAD       4096    /* Bad data in extraction            */
# define	S_COMBREJ      8192    /* Rejected during image combination */
# define	S_RES3        16384    /* Reserved                          */

/* COS data quality bits */
# define	C_RS		  1    /* Reed-Solomon                      */
# define	C_BRUSH		  2    /* brush mark > TBD percent          */
# define	C_GRID  	  4    /* grid shadow mark > TBD percent    */
# define	C_NEAREDGE	  8    /* spectrum near an edge of the detector    */
# define	C_DEAD 		 16    /* dead spot                         */
# define	C_HOT   	 32    /* hot spot                          */
# define	C_BURST		 64    /* count rate implies a burst (FUV only)    */
# define	C_OUTBOUNDS	128    /* pixel is outside the subarray     */
# define	C_DATAFILL	256    /* data fill due to telemetry drop-out      */
# define	C_PHLOW 	512    /* pulse height is below cutoff      */
# define	C_PHHIGH       1024    /* pulse height is above cutoff         */
# define	C_BADTIME      2048    /* time is inside a bad time interval   */
# define	C_BADWAVE      4096    /* wavelength is below MIN_WAVELENGTH   */
# define	C_RES1         8192    /* Reserved                             */
# define	C_RES2        16384    /* Reserved                             */

/* ACS data quality bits (for now, these are copied from STIS )*/
# define	A_RS		  1    /* Reed-Solomon                      */
# define	A_FILL		  2    /* Data replace by fill value        */
# define	A_BADDET	  4    /* Bad detector pixel                */
# define	A_MASK		  8    /* Data masked or beyond aperture    */
# define	A_HOTPIX	 16    /* Hot pixel                         */
# define	A_CTETAIL	 32    /* CTE tail                          */
# define	A_WARMPIX	 64    /* Warm pixel                        */
# define	A_BADBIAS	128    /* Bad bias pixel                    */
# define	A_SATPIX	256    /* Saturated pixel                   */
# define	A_BADFLAT	512    /* Bad pixel in flatfield file       */
# define	A_TRAP         1024    /* Charge trap                       */
# define	A_ATODSAT      2048    /* a-to-d saturated pixel            */
# define	A_MDCR         4096    /* Multidrizzle CR                   */
# define	A_CALCR        8192    /* Calacs CR                         */
# define	A_RES3        16384    /* Reserved                          */

/* WFC3 data quality bits */
# define	W3_RS		  1    /* Reed-Solomon                      */
# define	W3_FILL		  2    /* Data replaced by fill value       */
# define	W3_BADDET	  4    /* Bad detector pixel                */
# define	W3_BADZERO	  8    /* Bad in IR zero read               */
# define	W3_HOTPIX	 16    /* Hot pixel                         */
# define	W3_CTETAIL	 32    /* CTE tail (UVIS)                   */
# define	W3_UNSTABLE	 32    /* Unstable (IR)                     */
# define	W3_WARMPIX	 64    /* Warm pixel                        */
# define	W3_BADBIAS 	128    /* Bad bias level                    */
# define	W3_SATPIX	256    /* full-well or a-to-d Saturated     */
# define	W3_BADFLAT	512    /* Bad flatfield value               */
# define	W3_TRAP        1024    /* Charge trap (UVIS)                */
# define	W3_ATODSAT     2048    /* a-to-d saturated pixel (UVIS)     */
# define	W3_ZEROSIG     2048    /* zero read signal (IR)             */
# define	W3_MDCR        4096    /* MultiDrizzle CR                   */
# define	W3_CALCR       8192    /* Calwf3 CR                         */
# define	W3_CROSSTALK  16384    /* Crosstalk/Ghost                   */

/* WFPC data quality bits */
# define	W_RS		  1    /* Reed-Solomon                      */
# define	W_CAL		  2    /* Calibration file defect           */
# define	W_CAMERA	  4    /* Permanent camera defect           */
# define	W_SATUR		  8    /* Saturated pixel                   */
# define	W_MISSING	  16   /* Missing data                      */
# define	W_BADPIX	  32   /* Generic bad pixel                 */
# define	W_OVERLAP	  64   /* Image overlap pixel (WFPC2 only)  */
# define	W_CRAY		  128  /* Cosmic ray                        */
# define	W_TRAP		  256  /* Trap column pixel (WFPC2 only)    */
# define	W_UHOT		  512  /* "Unfixable" hot pixel             */
# define	W_FHOT		  1024 /* "Fixed" hot pixel                 */

/* These definition are used by routine e_strdic and must match the 
statType_ and HDUType_ enum types defined below. Routine e_strExtract
mandates that dictionary strings *must* be terminated by the separator 
character. */
# define STAT_TYPES "|doall|npix|min|max|sum|mean|midpt|mode|stddev|skew|kurt|wmean|wvar|"
# define MAX_STATS 13
# define HDU_TYPES "|primary|science|dqf|error|time|sample|"
# define MAX_HDUS  6

# define	MAX_FTYPE	9	/* Maximum # of supported file types */
# define	SZ_FNAME	256

/* This definition must be put in here since IRAF's libc has *no* fabs */
#ifdef __STDC__
#define __P(p)  p
#else
#define __P(p)  ()
#endif
extern double fabs __P((double));

/* HDU types. Primary header does not contain pixel data. */
enum HDUType_ { PRIMARY=-1, SCIENCE, DQ, ERROR, TIME, SAMPLE };
typedef enum HDUType_ HDUType;

/* File types. */
enum fileType_ { OIF, GEIS, NICMOS, STIS, FITS, ACS, COS, WFC3UV, WFC3IR};
typedef enum fileType_ fileType;

/* Statistical quantities. First significant is NPIX.  */
enum statType_ {ALL=-1, NPIX, MIN, MAX, SUM, MEAN, MIDPT, MODE, STDDEV, 
                SKEW, KURT, WMEAN, WVAR};
typedef enum statType_ statType;

extern	char	MsgText[SZ_OUTLINE];    /* message text      */

typedef struct {
	double                min;
	double                max;
	double                sum;  /* Accumulators */
	double               sum2;
	double               sum3;
	double               sum4;
	double              sumxw;
	double              sumww;
	double             stddev;
	long                 npix;
	long           *histogram;  /* Histogram */
	float               floor;
	float             ceiling;
	float            binWidth;
	float            invWidth;
	int                 nbins;
} Accumulators;

typedef struct {
	char      filename[SZ_FNAME]; /* Image name stripped of section/group  */
	char       dqfname[SZ_FNAME]; /* DQF name stripped of section/group    */
	char       section[SZ_FNAME]; /* Section                               */
	char    inputExten[SZ_EXT+1]; /* Extension in input file name          */
	char      dqfExten[SZ_EXT+1]; /* Extension in input DQ file name       */
	fileType               ftype; /* File type                             */
	int                groupSpec; /* Group in file name                    */
	int                    group; /* Current group                         */
	int                  ngroups; /* # of groups in file                   */
	Bool               allGroups; /* Do all groups in each image ?         */
	int               group_init; /* Group range list                      */
	int                group_end; /* Group range list                      */
	int               group_step; /* Group range list                      */
	IRAFPointer     im[MAX_HDUS]; /* IMIO pointers to each HDU             */
	IRAFPointer              msk; /* IMIO pointer to current image's mask  */
	IRAFPointer              dqf; /* IMIO pointer to current group's DQF   */
	IRAFPointer              err; /* IMIO pointer to current group's ERR   */
	Bool                   mskOK; /* Mask exists for this file ?           */
	Bool                   dqfOK; /* DQF exists for this file ?            */
	Bool                   errOK; /* Error exists for this file ?          */
	unsigned short gmask[MAX_FTYPE];/* Active mask bits for each ftype     */
	Bool             globalAccum; /* Accumulate across groups ?            */
	HDUType        hdu[MAX_HDUS]; /* HDUs the user wants to process        */
	int                     nhdu; /* # of HDUs the user wants to process   */
	HDUType               cl_hdu; /* HDU the user wants to output in CL    */
	statType    stats[MAX_STATS]; /* stats the user wants to compute       */
	int                   nstats; /* # of stats the user wants to compute  */
	Accumulators accum[MAX_HDUS]; /* One set of accumulators for each HDU  */
} Control; 


/* Function declarations */
void e_message    (char *);
void e_warn       (char *);
void e_warn2      (char *);
void e_error      (char *);
void e_IRAFerror  ();



