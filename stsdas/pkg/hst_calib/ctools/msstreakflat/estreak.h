# include <c_iraf.h>

# define	VERSION		"1.4 (12May98)"

# define	SZ_NAME		256	/* File name maximum size */
# define	SZ_OUTLINE	160	/* STDOUT line maximum size */
# define	TMPSUFF		"_tmp"	/* Temporary file name suffix */
# define	N_SPTSUFF	"_spt"	/* NICMOS support file name suffix */
# define	W_OUT		".r6h"	/* WFPC output name extension */
# define	W_DQOUT		".b6h"	/* WFPC DQF output name extension */
# define	W_SPTEXT	".shh"	/* WFPC support file name extension */
# define	MAX_ARRAYS	6	/* Maximum # of arrays in a group */
# define	SCIEXTNAME	"SCI"	/* Science EXTNAME */
# define	ERREXTNAME	"ERR"	/* Errors EXTNAME */
# define	DQFEXTNAME	"DQ"	/* DQF EXTNAME */
# define	INTEXTNAME	"TIME"	/* Integration time EXTNAME */
# define	SMPEXTNAME	"SAMP"	/* Number of samples EXTNAME */
# define	EXTVER		"1"	/* Default EXTVER. Must be string ! */
# define	NEXTVER		1	/* Numeric version of above */
# define	MAX_FILES	1000
# define	SHDR		0
# define	SSCI		1	/* Symbolic references. They do NOT */
# define	SDQF		2	/* relate to the physical ordering  */
# define	SERR		3	/* of extensions in FITS files.     */
# define	SINT		4
# define	SSMP		5

# define	NIC_XSIZE	256     /* Default image sizes       */
# define	NIC_YSIZE	256	/* and number of groups      */
# define	NIC_NGROUPS	1
# define	WFPC_XSIZE	800
# define	WFPC_YSIZE	800
# define	WFPC_NGROUPS	4
# define	WIND1		0.25	/* Window where to compute   */
# define	WIND2		0.75	/* average ratio in median   */

# define	WF1_CHIP1	224.09F /* Position angle offsets    */
# define	WF1_CHIP2	314.68F
# define	WF1_CHIP3	 45.00F
# define	WF1_CHIP4	134.89F
# define	PC_CHIP1	270.00F
# define	PC_CHIP2	  0.00F
# define	PC_CHIP3	 90.00F
# define	PC_CHIP4	180.00F
# define	WF2_CHIP1	224.70F
# define	WF2_CHIP2	314.22F
# define	WF2_CHIP3	 44.52F
# define	WF2_CHIP4	135.09F
# define	NIC_CAM1	315.327F
# define	NIC_CAM2	314.519F
# define	NIC_CAM3	314.861F

# define	MAX_ITER	20	/* Maximum # of iterations           */
# define	SZ_TIMESTAMP	35	/* Maximum size of time stamp string */
# define	SZ_HISTORY	160	/* Maximum size of HISTORY record    */
# define 	SZ_KEYWORD	20	/* Maximum length of keyword name    */
# define 	SZ_SUFFIX	5	/* Maximum length of name suffix     */
# define 	SZ_EXTENSION	6	/* Maximum length of name extension  */

/* NICMOS data quality bits */
# define	N_RS		  1    /* Reed-Solomon  */
# define	N_PLIN		  2    /* Poor linearity correction  */
# define	N_PDARK		  4    /* Poor dark correction  */
# define	N_PFF		  8    /* Poor flat-field correction  */
# define	N_GROT		  16   /* Grot */
# define	N_DEFECT	  32   /* Defective pixel  */
# define	N_SATUR		  64   /* Saturated pixel  */
# define	N_TELEM		  128  /* Telemetry problem  */
# define	N_BADPIX	  256  /* Bad pixel determ. by calibration  */
# define	N_CRAY		  512  /* Cosmic ray  */
# define	N_SOURCE	  1024 /* Source */
# define	N_ZREAD		  2048 /* Zero-read signal correction */
# define	N_USER1		  4096 /* User 1 */
# define	N_USER2		  8192 /* User 2 */

/* WFPC data quality bits */
# define	W_RS		  1    /* Reed-Solomon  */
# define	W_CAL		  2    /* Calibration file defect */
# define	W_CAMERA	  4    /* Permanent camera defect */
# define	W_SATUR		  8    /* Saturated pixel  */
# define	W_MISSING	  16   /* Missing data  */
# define	W_BADPIX	  32   /* Generic bad pixel */
# define	W_OVERLAP	  64   /* Image overlap pixel (WFPC2 only) */
# define	W_CRAY		  128  /* Cosmic ray  */
# define	W_TRAP		  256  /* Trap column pixel (WFPC2 only) */
# define	W_UHOT		  512  /* "Unfixable" hot pixel */
# define	W_FHOT		  1024 /* "Fixed" hot pixel */

# define	BADVAL		-100000.F
# define	NO_PIXVAL	-10000.F          /* no PIXVALUE keyword */
# define	MBYTE		(1024.F * 1024.F) /* One megabyte...     */
# define	SZ_STR		50                /* Size of work string */
# define	PI2		1.570796327       /* Pi/2                */
# define	DEG2RAD		0.01745329        /* deg to rad          */

/* This definition is used by routine g_strdic and must match the 
Instrument_ enum type defined below. It also must be consistent with
the keyword ordering in routine g_checkInput. */
# define	INSTRUMENTS	"|WFPC|WFPC2|NICMOS|STIS"

/* Macros for 2-D access of data arrays */
# define GPix(a,i,j)      (a).data[(j) *(a).nx  + (i)]
# define GPPix(a,i,j)     (a)->data[(j)*(a)->nx + (i)]

/* Instruments */
enum Instrument_ { WFPC=1, WFPC2=2, NICMOS=3, STIS=4 };
typedef enum Instrument_ Instrument;

/* Image access modes */
enum Acc_ { Ddisk = 1,  Memory = 0 };
typedef enum Acc_ AccMode;

/* This definition must be put in here since IRAF's libc has *no* fabs */
#ifdef __STDC__
#define __P(p)  p
#else
#define __P(p)  ()
#endif
extern double fabs __P((double));

extern	char	ErrText[SZ_OUTLINE];    /* error message text */

/* Basic data array */
typedef struct {
	int                 nx;	 /* Pixels / line                       */
	int                 ny;  /* # of lines                          */
	unsigned long  bufsize;  /* Full buffer size                    */
	float            *data;  /* Pixel data                          */
} floatArray;


/* I/O control */

typedef struct {
	char          *filename;  /* File name (raw)                       */
	AccMode      accessMode;  /* Access mode                           */
	float       streakAngle;  /* Streak angle from SHP                 */
	floatArray         *inp;  /* SCI input array when in memory mode   */
	floatArray         *tmp;  /* SCI temp array when in memory mode    */
} IOImage;

typedef struct {
	Instrument      instrument; /* Instrument flag                        */
	int                  group; /* Current group (WFPC only)              */
	int                ngroups; /* # of groups (0 - 3 for WFPC)           */
	float       angleOffset[4]; /* Streak angle offset (4 for WFPC)       */
	char        inputSuffix[5]; /* Suffix in input file names             */
	char         inputExten[6]; /* Extension in input file names          */
	char           dqfExten[6]; /* Extension in input DQ file names (WFPC)*/
	unsigned short        mask; /* Active mask bits.                      */
	int                 x_size; /* X axis size                            */
	int                 y_size; /* Y axis size                            */
	int            availMemory; /* Amount of available memory in Mb       */
	int                blkSize; /* # of image lines for median algorithm  */
	IOImage             *image; /* One entry per image                    */
	int                 nimage; /* # of images in *image vector           */
	int                current; /* Index of currently open image          */
	Bool              open_all; /* All arrays are open ?                  */
	floatArray            *sci; /* Current open inp/tmp when in memory    */
	IRAFPointer im[MAX_ARRAYS]; /* IMIO pointers to primary,sci,dq,err... */
} IOControl; 


/* Function declarations */
int  g_openImage  (IOControl *, int, Bool);
void g_closeImage (IOControl *);
void g_getBlock   (IOControl *, int, int, floatArray *);
void g_putImage   (IOControl *, floatArray *);
int  allocArray   (floatArray *, int , int);
void freeArray    (floatArray *);
void g_message    (char *);
void g_warn       (char *);
void g_warn2      (char *);
void g_error      (char *);



