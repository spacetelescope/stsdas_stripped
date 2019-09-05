# include <c_iraf.h>

# define	VERSION		"1.2 (04Mar97)"
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
# define	MBYTE		(1024L * 1024L)

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
	Instrument   instrument; /* Instrument                            */
	int               image; /* Current image pair                    */
	int             nimages; /* Number of input image pairs           */
	int               group; /* Current group                         */
	int             ngroups; /* Number of of groups in file           */
	int              extver; /* EXTVER in MULTIACCUM input files      */
	int               xsize; /* Pixel array size                      */
	int               ysize;
	int            blkStart; /* First image line of current block     */
	int              blkEnd; /* Lat image line of current block       */
	int             blkSize; /* Block size in pixels                  */
	IRAFPointer         im1; /* IMIO pointer to current file1         */
	IRAFPointer         im2; /* IMIO pointer to current file2         */
} Image; 

typedef struct {
	float             kclip; /* Threshold for sigma-clip algorithm    */
	float         cleanfrac; /* Fraction of pixels to clean off       */
	float          **buffer; /* Pixelwise buffer across image stack   */
	float           *rnoise; /* Readnoise array                       */
	int               nclip; /* Number of sigma-clip iterations       */
} Algorithm;


/* Messaging functions. */
void rn_message    (char *);
void rn_warn       (char *);
void rn_error      (char *);
void rn_IRAFerror  ();



