# include <c_iraf.h>

# define	VERSION		"1.3 (07Feb97)"
# define	SZ_NAME		256	/* File name maximum size */
# define	SZ_HISTORY	160	/* HISTORY record maximum size */
# define	SZ_TIMESTAMP	35	/* Maximum size of time stamp string */
# define	SZ_OUTLINE	160	/* STDOUT line maximum size */
# define	SZ_KW		20	/* Maximum header keyword length */

# define	NIC_XSIZE	256	/* Expected NICMOS axis sizes. */
# define	NIC_YSIZE	256
# define	STI_XSIZE	1024	/* Expected STIS axis sizes. */
# define	STI_YSIZE	1024

# define	BAD_FLAG	32

/* This definition must be put in here since IRAF's libc has *no* fabs */
#ifdef __STDC__
#define __P(p)  p
#else
#define __P(p)  ()
#endif
extern double fabs __P((double));

/* Supported instruments. */
enum Instrument_ { NICMOS, STIS };
typedef enum Instrument_ Instrument;

extern	char	MsgText[SZ_OUTLINE];    /* Message text */

typedef struct {
	float       threshold; /* Threshold value in sigma units */
	float         badfrac; /* Bad pixel detection fraction */
	float           kclip; /* Threshold for sigma-clip algorithm */
	float       cleanfrac; /* Fraction of pixels to clean off from window*/
	float         *buffer; /* Stores pixels extracted from window */
	int            window; /* Window size where to compute local stats */
	int             nclip; /* # iterations in sigma-clip algorithm */
	int          *counter; /* Counter array */
} Counter;

typedef struct {
	Instrument   instrument; /* Instrument */
	int               group; /* Current group */
	int             ngroups; /* # of groups in file */
	int               xsize; /* Pixel array size */
	int               ysize;
	IRAFPointer          im; /* IMIO pointer to current science HDU */
} Image; 

/* Messaging functions. */
void b_message    (char *);
void b_warn       (char *);
void b_error      (char *);
void b_IRAFerror  ();



