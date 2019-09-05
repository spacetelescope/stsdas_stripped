/* Definition of some constants. */
# define FITS_LINE 81
# define SZ_FNAME 255
# define PLATESCALE 0.0508
# define COSTHETA 0.70711
# define SINTHETA 0.70711
# define SPIRALSIZE 9

/* Macro to round up  anumber ot its decimal part. */
# define NINT(x)  \
         ((x >= 0.) ? (int) (x*10 + 0.5)/10.0 : (int) (x*10 - 0.5)/10.0)

/* Macro to find the maximum of two values */
#define MAX(a, b) \
        ((a) >= (b) ? (a) : (b))

struct  imageinfo {
    char  visit[FITS_LINE];
    char  obsmode[FITS_LINE];
    char  obstype[FITS_LINE];
    char  rootname[FITS_LINE];
    char  aperture[FITS_LINE];
    char  acqtype[FITS_LINE];
    char  targname[FITS_LINE];
    char  tdateobs[FITS_LINE];
    char  ttimeobs[FITS_LINE];
    char  optelem[FITS_LINE];
    char  peakcent[FITS_LINE];
    char  search[FITS_LINE];   /* centmeth for ACQ, pksearch for ACQ/PEAK */
    char  domfgs[FITS_LINE];   /* dgestar from spt file for ta monitoring */
    char  subfgs[FITS_LINE];   /* sgestar from spt file for ta monitoring */
    char  ocstdfx[FITS_LINE];  /* take-data flag from spt file */
    int   proposid;
    int   box_step;            /* checkbox for ACQ, numstep for ACQ/PEAK */
    int   sizaxis1;
    int   sizaxis2;
    int   corner1;
    int   corner2;
    int   otaslwa1;
    int   otaslwa2;
    int   naxis1;
    int   naxis2;
    int   dwell[SPIRALSIZE][SPIRALSIZE];
    double  expnum;
    double  texptime;
    double  biaslev;
    double  counts1;   /* maxcntch for ACQ, flux in conf. image for ACQ/PEAK */
    double  counts2;   /* maxcntch, used only for ACQ */
    double  pedestal;  /* pedestal (baseline), used only for ACQ/PEAK */
    double  goodmax1;	/* goodmax in first ACQ/PEAK image */
    double  goodmax2;	/* goodmax in second ACQ image */
    double  goodmax3;	/* goodmax in third ACQ image */
    double  peakstep;
    double  coarse1;
    double  coarse2;
    double  fine1;
    double  fine2;
    double  refaper1;
    double  refaper2;
    double  a1coarse_pix;
    double  a2coarse_pix;
    double  a1coarse_arc;
    double  a2coarse_arc;
    double  V2coarse;
    double  V3coarse;
    double  a1fine_pix;
    double  a2fine_pix;
    double  a1fine_arc;
    double  a2fine_arc;
    double  V2fine;
    double  V3fine;
    double  a1total_pix;  /* for ACQ & ACQ/PEAK */
    double  a2total_pix;  /* for ACQ & ACQ/PEAK */
    double  a1total_arc;  /* for ACQ & ACQ/PEAK */
    double  a2total_arc;  /* for ACQ & ACQ/PEAK */
    double  V2total;      /* for ACQ & ACQ/PEAK */
    double  V3total;      /* for ACQ & ACQ/PEAK */
    double  targax1;
    double  targay1;
    double  targax4;
    double  targay4;
    double  apera1;
    double  apera2;
    double  aperlka1;
    double  aperlka2;
};

typedef struct imageinfo  imageinfo;
