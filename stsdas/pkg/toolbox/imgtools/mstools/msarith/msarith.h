# include <hstio.h>

# define	MSARITH_VERSION "2.7 (2May07)"

# define	OPERATORS	"+-*/"
# define	OPERAND_FILE	0	/* This definitions must    */
# define	OPERAND_LONG	1	/* conform to the ones used */
# define	OPERAND_DOUBLE	2	/* in routine get_numeric   */

# define	SZ_NAME		256	/* File name maximum size */

/* DQF to be set in case of math error */
# define	BADPIX		256	/* NICMOS */
# define	DATALOST	2	/* STIS   */
# define	DATAFILL	256	/* COS   */

/* This definition must be put in here since IRAF's libc has *no* fabs */
#ifdef __STDC__
#define __P(p)  p
#else
#define __P(p)  ()
#endif
extern double fabs __P((double));

/* Supported instruments. */
enum Instrument_ { NICMOS, STIS, ACS, COS, WFC3IR, WFC3UV};
typedef enum Instrument_ Instrument;

extern	char	ErrText[81];		/* error message text */
extern double fabs __P((double));

/* Holds numeric constant and associated error */
typedef struct {
	double	value;
	double	error;
} mathConst;

/* Main data structure, used to partially conceal instrument dependencies. */
typedef struct {
	Instrument         instrument; /* Instrument type                   */
	SingleNicmosGroup  dsng;       /* NICMOS data structure             */
	SingleGroup        dsg;        /* STIS data structure               */
	SingleNicmosGroup  *sng;       /* Pointer to NICMOS data structure  */
	SingleGroup        *sg;        /* Pointer to STIS data structure    */
} GenericGroup;

/* Control structure for arithmetic routines */
typedef struct {
	char	  infile1[SZ_NAME+1];	/* name of input file 1              */
	char	  infile2[SZ_NAME+1];	/* name of input file 2 (if existent)*/ 
	char	  outfile[SZ_NAME+6];	/* output file name and extension    */
	char	  *oper;		/* pointer to arithmetic operator    */
	Bool      cr1, cr2;             /* count rate flag from file         */
	Bool	  isCRUser;		/* count rate flag set by user       */
	Bool	  isCountRate;		/* count rate flag set by program    */
	Bool	  isFile;		/* 2nd operand is a file ?           */
	Bool	  isInv;		/* inverse operation ?               */
	int	  divZero;		/* Number of divisions by zero       */
	int	  verbose;		/* Verbosity level                   */
	int 	  *list1;		/* input group list 1                */
	int 	  *list2;		/* input group list 2                */
	int 	  ngroup1;		/* first group list size             */
	int 	  ngroup2;		/* second group list size            */
	int 	  group1;		/* current group from list 1         */
	int 	  group2;		/* current group from list 2         */
	int 	  ogroup;		/* current output group number       */
	float	  divZero_replace;	/* Replaces division by zero result  */
	mathConst num_operand;		/* holds numeric (2nd) operand       */
} arithControl;

/* Error functions */
void	n_hstioerr ();
void	n_sqrterr  (float *, int);
void	n_filerr   (char *);
void	n_warn     (char *);
void	n_error    (char *);
void	n_message  (char *);

