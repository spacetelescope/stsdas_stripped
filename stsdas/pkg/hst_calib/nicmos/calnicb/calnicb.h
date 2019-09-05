/* CALNICB.H: Main include file for CALNICB. Defines things specific
** to CALNICB.
**
** Revision history:
** H.Bushouse	April 1996	Build 1
** H.Bushouse	Oct 1996	Build 2: uses enum defs; modified WCS data
**				structure to use arrays
** H.Bushouse	30-Jan-1997	Changed asninfo.nexp to asninfo.niter; removed
**				patt_off, porient, dithsize, chopsize elements
**				from AsnInfo structure (Version 2.0)
** H.Bushouse	27-Feb-1997	Added docombine to AsnInfo structure
**				(Version 2.0)
** H.Bushouse	29-Apr-1997	Added obsTypes, imageTypes definitions; changed
**				MemberInfo.imgtype to mtype (Version 2.0)
** H.Bushouse	01-Dec-1997	Added subbkg, usemeanbkg, crthresh, and xcwin
**				to AsnInfo structure (Version 2.2)
** H.Bushouse	17-Mar-1998	Added readbkg and readoffs to AsnInfo struct
**				(Version 2.2)
** H.Bushouse	10-May-1999	Added newpattkeys to AsnInfo structure
**				(Version 2.3)
** H.Bushouse	04-Apr-2000	Added targname to NicInfo structure; added
**				POST_SAA_DARK to list of obsTypes.
**				(Version 2.4)
** R.Jedrzejew  25-Mar-2003     Added ability to process instrument-specific
**                              (non-NICMOS) patterns; changed n_setup.c.
**                              (Version 2.6)
**
**M. Sosey      06-Nov-2008  Updated header information per OPR 61187/55050
**				   so that the total exptime is updated and the expstart
**                 and expend keywords correspond to the entire association
**					that has been combined
**
** W.J. Hack    22-Jan-2009  Calculation of EXPEND/EXPTIME for product changed
**                 to exclude background exposures that may be present in the ASN
**                 (Version 2.6.1)
** W.J. Hack    8-Apr-2009  EXPEND/EXPSTART/EXPTIME keywords computed separately
**                 for ALL products to insure that background _mos files 
**                 have correct header values. (Version 2.6.2)
**
** W.J. Hack    13-Nov-2009 Logic in n_combniter was amended to handle 
**                 pattern positions without any members. (Version 2.6.3)
** 
*/

# define	CALNICB_VERSION	"2.6.3"

# define	BADVAL	-99999

/* Background pattern types */
enum patternTypes_ {NONE, DITH, CHOP, DITHCHOP};
typedef enum patternTypes_ patternTypes;

/* Background pattern names */
enum patternNames_ {SPIRALDITH, SQUAREWAVEDITH, XSTRIPDITH, YSTRIPDITH,
		    ONECHOP, TWOCHOP, FOURCHOP, EIGHTCHOP,
		    SPIRALDITHCHOP, XSTRIPDITHCHOP, YSTRIPDITHCHOP};
typedef enum patternNames_ patternNames;

/* Image types */
enum imageTypes_ {OBJ, SKY, OBJSKY};
typedef enum imageTypes_ imageTypes;

/* Observation type */
enum obsTypes_ {EXT, DARK, POST_SAA_DARK, IFLAT, EFLAT};
typedef enum obsTypes_ obsTypes;

/* Observation and calibration information */
typedef struct {
	char instr[SZ_STRKWVAL+1];
	int  camera;
	char filter[SZ_STRKWVAL+1];
	char imagetype[SZ_STRKWVAL+1];
	char targname[SZ_STRKWVAL+1];
	obsTypes obs_type;
	CalStep BACK;
    
} NicInfo;

typedef struct {
	float  crpix[2];
	double crval[2];
	double cd[2][2];
	char   ctype[2][SZ_STRKWVAL+1];
} WCS;

typedef struct {
	char name[SZ_NAME+1];
	char mtype[SZ_STRKWVAL+1];
	Bool status;
	WCS wcs;
	imageTypes type;
	int patpos;
	int mospos;
	float backest1;
	float backest2;
	float backest3;
	Bool  bkgimg;
	float bkg;
	float dx, dy;
	float xi, yi;
    int bkgid;
    int bkgprod;
    double expstart; /*keep start time for each product, in MJD*/
    double expend; /*keep end time for each product, in MJD*/
    float exptime; /*keep track of total exposure time for each product*/
} MemberInfo;

typedef struct {
	char asn_table[SZ_NAME+1];
	char asc_table[SZ_NAME+1];
	char id[10];
	char pattern[SZ_STRKWVAL+1];
	Bool newpattkeys;
	int numpos;
	int nummos;
	patternTypes patt_type;
	patternNames patt_name;
	int niter;
	int nobj;
	int nsky;
	int *posmems;
	int *mosmems;
	float meanbkg;
	int nmembers;
	int tmembers;
    int pmembers;   /* keep track of number of background products */
	MemberInfo *member;
	Bool docombine;
	Bool subbkg;
	Bool usemeanbkg;
	Bool readbkg;
	Bool readoffs;
	float crthresh;
	int xcwin;
    float exptime; /*keep track of total exposure time for asn*/
    double expstart; /*keep start time for whole asn, in MJD*/
    double expend; /*keep end time for whole asn, in MJD*/

} AsnInfo;

typedef struct {
	int nmembers;
	SingleNicmosGroup *member;
} AsnImages;

