/* CALNICA.H: Main include file for CALNICA. Defines things specific
** to CALNICA.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Oct.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	27-Jan-1997	Changed length of NicInfo.sampseq from 8 to
**				SZ_STRKWVAL+1 chars. Changed NlinData struct
**				elements coeff,error,nodes to type FloatHdrData
**				and dqual to ShortHdrData (Version 2.1)
** H.Bushouse	28-Jul-1997	Changed NicInfo.bunit and tdftrans from scalar
**				to vector of length MAX_MAREADS (Version 3.0)
** H.Bushouse	18-Aug-1997	Added zsci and zerr elements to NlinData struct
**				(Version 3.0)
** H.Bushouse	11-Sep-1997	Added crthresh and zsthresh to NicInfo
**				structure (Version 3.0)
** H.Bushouse	13-Feb-1998	Added sampzero to NicInfo structure; added
**				ZSIG CalStep structure (Version 3.2)
** H.Bushouse	01-Oct-1998	Added samp_rej to NicInfo structure (Vsn 3.3)
** H.Bushouse	15-Oct-1998	Added barthresh and BARS CalStep to NicInfo
**				structure (Version 3.3)
** H.Bushouse	03-Oct-2000	Removed RefDataLoaded from NicInfo structure
**				(Version 4.0)
** R. Jedrzeje  31-Jan-2002     Removed CalSteps BSEQ and PEDE from NicInfo
                                Added dyndarkfile member to NicInfo
** V. Laidler   May 2007        Major rewrite of n_cridcalc.c initiated by
**                              Mike Regan. 
** V. Laidler   12-Jun-2007     Bugfix, comments, & slight refactoring of
**                              n_cridcalc.c/fitsamps: tickets #65&66
** V. Laidler   14-Jun-2007     Bugfix & major refactoring of n_cridcalc.c/
**                              linfit: ticket#66. Argument sequence changed.
V. Laidler   22 Jun 07      Significant refactoring to n_cridcalc.c/fitsamps
V. Laidler   26 Jun 07      Temporarily added some debug print statements to
                                n_cridcalc.c/fitsamps.
V. Laidler    3-5 Jul 07      Correct logic problem in linfit for error calculations
                              for ndata==2. Ticket #80.
V. Laidler    9 Jul 07      Pulled RejFirstRead out of RejSpikes in n_cridcalc.c; now
                              fitsamps calls it directly. Ticket #67.
V. Laidler   17 Jul 07      Fixed dimensionality of arrays in n_cridcalc.c /
                              fitsamps; ticket #87.
V. Laidler   27 Jul 07      Put a catch into n_cridcalc.c/EstimateDarkAndGlow
                              for the case when there are no evenly spaced
                              intervals detected; ticket #68.
V. Laidler   27 Jul 07      Put a test into n_cridcalc.c/n_mcrid to avoid
                              calling {Get|Estimate}DarkAndGlow if no dark
                              correction is being performed: as per #68.
V. Laidler   17 Aug 07      Put a test into n_cridcalc.c/linfit to avoid
                              sqrt(negative number); as per #91.
V. Laidler    5 Sep 07      Properly handle cases in which all samples are 
                              marked bad in the DQ array; as per #105.
V. Laidler   13 Sep 07      Trap the case when max_CRs is exceeded; as per
                              #96.
V. Laidler   24 Sep 07      Fix bug in uncertainty calculations, as per #108
V. Laidler   25 Sep 07      Fixed typo in bug fix: still #108..
R. Jedrzejewski  26 Oct 07   Set pixels with max_CR CRs to a sensible value,
                             flag with new HIGH_CURVATURE dq flag (4096) and
                             unset the CR_HIT bit (4.2d21), ticket #96
R. Jedrzejewski  05 Nov 07   Better calculation of sensible value.  Print
                             less. (4.2d22).
R. Jedrzejewski  07 Nov 07   Check for temperature=0.0, and abort if so.
                             Check for _raw1.fits suffix.  Version 4.2d23
                             for ticket #129
R. Jedrzejewski  30 Nov 07   Change max_CRs in n_cridcalc.c from 4 to 3
                             Version 4.2d24 for ticket #129
$. Jedrzejewski  05 Mar 08   Do up-the-ramp processing of pixels that
                             don't have any DQ=0 reads.  Propagate DQ
                             flags to the _cal file.
                             Version 4.2
R. Jedrzejewski  18 Apr 08   Restore 4096 DQ flag to USER1.  Use 16384
                             for HIGH_CURVATURE
R. Jedrzejewski  08-Jul-08   Check for TFBCALC="COMPLETE", if so, use
                             TFBTEMP keyword instead of temperature from
                             _spt file.  Version 4.3.
R. Jedrzejewski  04-Aug-08   Added prefix to NicInfo structure.

M. Sosey		0ct-2008	added some new structure members to help with 
							implementing temp dependent flats, ampglow, 
                            linear darks and photometry scaling. They now look
                            for TFBLOW and TFBHIGH in the reference file
                            headers for the TFBtemp correspondence.

*/

# define	CALNICA_VERSION	"4.4.1"

/* Dark image interpolation type */
enum DarkTypes_ {MATCH, INTERP, EXTRAP, NEAREST};
typedef enum DarkTypes_ DarkTypes;

/* Data units */
enum DataUnits_ {COUNTS, COUNTRATE};
typedef enum DataUnits_ DataUnits;

/* Observation and calibration information */
typedef struct {
	char	   filename[SZ_NAME+1];	    /* input file name              */
    char       prefix[SZ_NAME+1];       /* prefix from input file name  */
    int	   group;		    /* current group                */
	int	   ngroups;		    /* total number of groups       */
	Bool	   NullData[MAX_MAREADS];   /* null data flag               */
	DarkTypes  DarkType;		    /* dark interpolation type      */
	int	   darkframe1;		    /* 1st dark ref frame used      */
	int	   darkframe2;		    /* 2nd dark ref frame used      */
	int	   ndarks;		    /* number of dark ref images    */
	double	   dtimes[MAX_MAREADS];     /* dark image exposure times    */
    char       darkmeth[SZ_STRKWVAL+1]; /* dark calculation method      */
    int        ndelta;                  /* #deltatimes in dark ref      */
    double     ddeltas[MAX_MAREADS];    /* dark image delta times       */
    double     temperature;             /* dark temperature             */
    double     tfbtemp;                 /* temp-from-bias value from the science data*/
    char       dyndarkfile[SZ_NAME+1];  /* name of dynamic dark file    */
    Bool       writedark;               /* do we write out dynamic dark?*/
	float ampScale; /*scale value for ampglow image*/
    float linScale; /*scale value for lindark image*/   
    char	flatmeth[SZ_STRKWVAL+1];	 /*flat calculation method, STATIC or TEMPERATURE-DEPENDENT*/
    int		flatext; 					/*the tdf extension of the flat to use, set to 1 default*/
    int	    totFlatImages; 				/*the total number of flat images available, default to 1*/
	char	   instr[SZ_STRKWVAL+1];    /* instrument name              */
	int	   camera;		    /* camera number                */
	ObsModes   obsmode;		    /* observation mode             */
	char	   filter[SZ_STRKWVAL+1];   /* filter name                  */
	int	   nread;		    /* number of reads (MIFS)       */
	int	   nsamp;		    /* number of samples (MACCUM)   */
	char	   sampseq[SZ_STRKWVAL+1];  /* sample sequence (MACCUM)     */
	char	   readout[SZ_STRKWVAL+1];  /* readout speed                */
	float	   adcgain;		    /* ADC gain setting             */
	double	   exptime[MAX_MAREADS];    /* group exposure times         */
        double     delttime[MAX_MAREADS];   /* group delta times            */
	double	   sampzero;		    /* sample zero exptime (MACCUM) */
	DataUnits  bunit[MAX_MAREADS];	    /* BUNIT value                  */
	int	   tdftrans[MAX_MAREADS];   /* number of TDF transitions    */
	float	   crthresh;		    /* CR rejection threshold       */
	float	   zsthresh;		    /* ZSIG signal threshold        */
	float	   barthresh;		    /* BAR detection threshold      */
	int	   samp_rej;		    /* Number of samples to reject  */
	CalStep	   ZSIG;		    /* ZSIG cal step structure      */
	CalStep	   ZOFF;		    /* ZOFF cal step structure      */
	CalStep	   MASK;		    /* MASK cal step structure      */
	CalStep	   BIAS;		    /* BIAS cal step structure      */
	CalStep	   NOIS;		    /* NOIS cal step structure      */
	CalStep	   DARK;		    /* DARK cal step structure      */
	CalStep	   NLIN;		    /* NLIN cal step structure      */
	CalStep    BARS;		    /* BARS cal step structure      */
	CalStep	   FLAT;		    /* FLAT cal step structure      */
	CalStep	   UNIT;		    /* UNIT cal step structure      */
	CalStep	   PHOT;		    /* PHOT cal step structure      */
	CalStep	   CRID;		    /* CRID cal step structure      */
	CalStep	   BACK;		    /* BACK cal step structure      */
	CalStep	   WARN;		    /* WARN cal step structure      */
} NicInfo;

/* Non-linearity reference data structure */
typedef struct {
	Hdr	     *globalhdr;
	FloatHdrData *coeff;
	FloatHdrData *error;
	ShortHdrData *dqual;
	FloatHdrData *nodes;
	FloatHdrData *zsci;
	FloatHdrData *zerr;
} NlinData;

/* Photometry parameters data structure */
typedef struct {
	char		mode[SZ_STRKWVAL+1];	/* PHOTMODE string */
	float		flam;			/* PHOTFLAM value  */
	float		fnu;			/* PHOTFNU  value  */
	float		zpt;			/* PHOTZPT  value  */
	float		plam;			/* PHOTPLAM value  */
	float		bw;	    		/* PHOTBW   value  */
    float       reft;            /* PHOTREFT value */
    float       f_c0;           /* PHOTF_C0 value */
    float       f_c1;           /* PHOTF_C1 value */
    float       ferr;           /* PHOTFERR value */
    float       tfblow;         /* TFB_LOW header keyword value */
    float       tfbhigh;        /* TFB_HIGH header keyword value */
    float       zpscale;        /* Computed temp-dep scale factor */
} PhotData;

