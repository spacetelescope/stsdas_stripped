/* C program that prints header information for a list of STIS FITS  */
/* images.                                                           */
/* It makes use of the imio.h functions.                             */
/* Created by R. Katsanis, 12-September-1997.                        */
/* Phil Hodge, 15-October-1997:  include option to run as iraf task  */


# include <stdio.h>
# include <string.h>
# include <math.h>
# include <xclio.h>
# include <ximio.h>

# define SZ_FNAME 255
# define FITS_LINE 81

static int PrintOne (char *);
static void WriteError (int, char *);

# if defined (NATIVE_IRAF)
IRAFTASK(infostis) {
# else
int main(int argc, char **argv) {
# endif

  char         input[SZ_FNAME];		/* list of input files */
  IRAFPointer  imlist;
  char         infile[SZ_FNAME];	/* one input image name */
  int          nfiles;			/* total number of input files */
  int          j;			/* loop index for file number */
  int          status;

  c_irafinit (argc,argv);

# if defined (NATIVE_IRAF)
	c_clgstr ("input", input, SZ_FNAME);
	imlist = c_imtopen (input);
	nfiles = c_imtlen (imlist);
	if (nfiles < 1) {
	    printf ("Null input, or no file matches input template.\n");
	    return;
	}
# else
  if (argc < 2) {
            printf ("syntax:  infostis file1 file2 ...\n");
            exit (1);
        }
	nfiles = argc - 1;
# endif

  /* Loop over all input files. */
  for (j=0; j<nfiles; j++) {

       /* Build the string "infile.fits[0]" for the next input file. */
# if defined (NATIVE_IRAF)
	if (c_imtgetim (imlist, infile, SZ_FNAME) < 1) {
	    printf ("Error parsing input string `%s'\n", input);
	    return;
	}
# else
       argv++;
       strcpy (infile, *argv);
# endif

       strcat (infile, "[0]");

       if (status = PrintOne (infile)) {
	    WriteError (status, infile);
	    continue;
       }
  }

# if defined (NATIVE_IRAF)
	c_imtclose (imlist);
# else
	exit (0);
# endif
}

static int PrintOne (char *infile) {

       IRAFPointer  im;
       int          i;
       char    rootname[FITS_LINE] = "N/A",
                 propid[FITS_LINE] = "N/A",
                linenum[FITS_LINE] = "N/A",
               targname[FITS_LINE] = "N/A",
               detector[FITS_LINE] = "N/A",
                obstype[FITS_LINE] = "N/A",
                obsmode[FITS_LINE] = "N/A",
                 sclamp[FITS_LINE] = "N/A",
               aperture[FITS_LINE] = "N/A",
                 filter[FITS_LINE] = "N/A",
               opt_elem[FITS_LINE] = "N/A",
                 ccdamp[FITS_LINE] = "N/A";
       double       ra_targ = 0., dec_targ = 0., equinox = 2000., exptime = 0.;
       int          bin1 = -999, bin2 = -999, nextend = 0, imsets = 0;
       int          cenwave = 0, ccdgain = 0, crsplit = 0;
       Bool         subarray=False;

       /* Internal constants */

       double       num, num1, hh, mm, ss, dec, dd, dm, ds;
       int          spectra, ccd;    /* to use as booleans */

       int status = 0;

       /* Initialize boolean variables.  */
       spectra = 0;
       ccd = 0;


       /* Open input file */
       im = c_immap (infile, IRAF_READ_ONLY, 0);
       if (status = c_iraferr())
	    return (status);


       /* Read primary header keywords */

       if (c_imaccf (im, "rootname") != 0)
	   c_imgstr (im, "rootname", rootname, FITS_LINE-1);
       if (c_imaccf (im, "proposid") != 0)
	   c_imgstr (im, "proposid", propid, FITS_LINE-1);
       if (c_imaccf (im, "linenum") != 0)
	   c_imgstr (im, "linenum", linenum, FITS_LINE-1);
       if (c_imaccf (im, "targname") != 0)
	   c_imgstr (im, "targname", targname, FITS_LINE-1);
       if (c_imaccf (im, "detector") != 0)
	   c_imgstr (im, "detector", detector, FITS_LINE-1);
       if (c_imaccf (im, "obstype") != 0)
	   c_imgstr (im, "obstype", obstype, FITS_LINE-1);
       if (c_imaccf (im, "obsmode") != 0)
	   c_imgstr (im, "obsmode", obsmode, FITS_LINE-1);
       if (c_imaccf (im, "sclamp") != 0)
	   c_imgstr (im, "sclamp", sclamp, FITS_LINE-1);
       if (c_imaccf (im, "aperture") != 0)
	   c_imgstr (im, "aperture", aperture, FITS_LINE-1);
       if (c_imaccf (im, "filter") != 0)
	   c_imgstr (im, "filter", filter, FITS_LINE-1);
       if (c_imaccf (im, "opt_elem") != 0)
	   c_imgstr (im, "opt_elem", opt_elem, FITS_LINE-1);

       if (c_imaccf (im, "ra_targ") != 0)
	   ra_targ  = c_imgetd (im, "ra_targ");
       if (c_imaccf (im, "dec_targ") != 0)
	   dec_targ = c_imgetd (im, "dec_targ");
       if (c_imaccf (im, "equinox") != 0)
	   equinox  = c_imgetd (im, "equinox");
       if (c_imaccf (im, "binaxis1") != 0)
	   bin1     = c_imgeti (im, "binaxis1");
       if (c_imaccf (im, "binaxis2") != 0)
	   bin2     = c_imgeti (im, "binaxis2");
       if (c_imaccf (im, "texptime") != 0)
	   exptime  = c_imgetd (im, "texptime");
       if (c_imaccf (im, "nextend") != 0)
	   nextend  = c_imgeti (im, "nextend");

       if (c_imaccf (im, "subarray") != 0)
	   subarray = c_imgetb (im, "SUBARRAY");
	
       /* Finds out observation type and detector. */

       if ( strcmp(obstype,"SPECTROSCOPIC") == 0 )
	    spectra = 1;
       if ( strcmp(detector,"CCD") == 0 )
	    ccd = 1;


       /* If spectra asks for central wavelength. */

       if ( spectra && c_imaccf (im, "cenwave") != 0 )
	    cenwave = c_imgeti (im, "cenwave");


       /* If CCD ask for CCD keywords. */

       if ( ccd ) {
	   if (c_imaccf (im, "ccdamp") != 0)
		c_imgstr (im, "ccdamp", ccdamp, FITS_LINE-1);
	   if (c_imaccf (im, "ccdgain") != 0)
		ccdgain = c_imgeti (im, "ccdgain");
	   if (c_imaccf (im, "crsplit") != 0)
		crsplit = c_imgeti (im, "crsplit");
       }


       /* Close input file. */
       c_imunmap (im);


       /* Perform calculations with parameters. */

       /* Calculates number of imsets. */
       if ( nextend > 0 )
	    imsets = nextend / 3;
       else
	    imsets = 0;


       /*  Convert RA to hh:mm:ss  */

       num1 = ra_targ / 15.0;
       num = modf(num1, &hh);
       num = (num1 - hh) * 60.0;
       ss = modf (num, &mm);
       ss = (num - mm) * 60.0;


       /*  Convert declination decimal to dd:mm:ss  */

       if ( dec_targ < 0 )
	    dec = dec_targ * -1;
       else
	    dec = dec_targ;

       num = modf (dec,&dd);
       num = (dec - dd) * 60.0;
       ds  = modf (num,&dm);
       ds = (num - dm) * 60.0;


       /*  Print formatted output.  */

       for (i=0; i<80; i+=1)
            printf ("-");
       printf ("\n");

       printf ("                                   S T I S\n");

       for (i=0; i<80; i+=1)
            printf ("-");
       printf ("\n\n");

       printf ("%18s %-18s %16s %-25s\n", "Rootname:", rootname,
	       "Detector:", detector);

       printf ("%18s %-18s %16s %-25s\n", "Proposal ID:", propid,
               "Obs Type:", obstype);

       printf ("%18s %-18s %16s %-25s\n", "Exposure ID:", linenum,
               "Obs Mode:", obsmode);

       printf ("%54s %s\n", "Lamp:", sclamp);

       printf ("%18s %-18s %16s %-25s\n", "Target Name:", targname,
               "Aperture:", aperture);

       printf ("%19s %02.0f%s%02.0f%s%04.1f %23s %s\n",
               "Right Ascension: ", hh, ":", mm, ":", ss, "Filter:", filter);

       if ( dec_targ >= 0 )
            printf ("%18s %s%02.0f%s%02.0f%s%04.1f %23s %s\n", "Declination:",
		    "+", dd, ":", dm, ":", ds, "Opt Element:", opt_elem);
       else  printf ("%18s %s%02.0f%s%02.0f%s%04.1f %23s %s\n", "Declination:",
		      "-", dd,":", dm, ":", ds, "Opt Element:", opt_elem);

       if ( spectra )
            printf ("%18s %-18.1f %16s %-25d\n", "Equinox:", equinox,
                    "Central Wave:", cenwave);
       else if ( ccd )
                 printf ("%18s %-18.1f %16s %s\n", "Equinox:",
                         equinox, "CCD amp:", ccdamp);
       else printf ("%18s %-18.1f\n", "Equinox:", equinox);

       if ( ccd && spectra )
            printf ("%54s %s", "CCD amp:", ccdamp);
       else if ( ccd )
                 printf ("%54s %d", "Gain:", ccdgain);

       printf ("\n");
       if ( ccd && spectra )
            printf ("%18s %-18d %16s %-25d", "Axis 1 binning:",
                    bin1, "Gain:", ccdgain);
       else if ( ccd )
                 printf ("%18s %-18d %16s %-25d", "Axis 1 binning:",
                         bin1, "CR-split:", crsplit);
       else printf ("%18s %-18d","Axis 1 binning:", bin1);


       printf ("\n");
       if ( ccd && spectra )
            printf ("%18s %-18d %16s %-25d", "Axis 2 binning:",
                    bin2, "CR-split:", crsplit);
       else  printf ("%18s %-18d", "Axis 2 binning:", bin2);

       printf ("\n");
       if ( subarray )
            printf ("%18s %s\n\n", "Subarray:", "yes");
       else
	    printf ("%18s %s\n\n", "Subarray:", "no");

       printf ("%18s %-.1f %s\n", "Total Exp. Time:", exptime, "sec");
       printf ("%18s %-18d\n\n", "Number of imsets:", imsets);

       return (0);
}

static void WriteError (int errcode, char *infile) {

	if (errcode == 827)
	    printf ("Can't open %s; skipping ...\n", infile);
	else
	    printf ("ERROR    (IRAF) %d:  %s\n", c_iraferr(), c_iraferrmsg());
}
