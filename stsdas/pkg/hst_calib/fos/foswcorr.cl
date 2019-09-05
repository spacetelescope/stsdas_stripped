################################################################################
#
# IRAF script to apply a zero-point correction to the FOS blue wavelength 
# scale (i.e., c0h/c0d file) for GRNDMODE = SPECTROSCOPY or RAPID-READOUT data 
# only.  Also, this correction is not applicable to PRISM data, as the 
# wavelength scale is non-linear.  The algorithm coded in this script is 
# based upon the CAL/FOS Instrument Science Report 149.
#
# This script will copy the original ipppssoot.c0h/c0d files to 
# ipppssoot_orig.c0h/c0d to preserve the original data.  A temporary working 
# image is written to the tmp$ area and is deleted upon the successful 
# completion of this script.  The ipppssoot.c0h/c0d files are overwritten with 
# the corrected wavelength values.
#
# USAGE: In order to use this task, it must first be defined for the IRAF
#        system.  The following lines define the task and then invoke the task.
#        cl> task foswcorr = foswcorr.cl
#        cl> foswcorr ipppssoot.c0h
#
# Version    Author                      Date
# ------------------------------------------------------
#   1.0      M.D. De La Pena, SSG/STScI   18 August 1998
#
################################################################################

procedure foswcorr (input)

file   input       {"",           prompt = "Input FOS c0h file to be updated"}
string version     {"Version 1.0, Oct98", prompt = "Installation date"}
string mode        {"al",         prompt = ""}

begin

    # Declarations.

    # Variables to hold input parameters used locally.
    string infile             # Input file name.
    string outfile            # Output file name.

    # Local variables.
    int    a                  # Dummy variables.
    int    dmin               # Minimum pixel value for first non-zero wave.
    int    imjdexpo           # int (mjdexpo).
    int    nchnls             # Number of channels.
    int    npts               # Number of data points in spectrum.
    int    nxsteps            # Number of X steps.
    int    overscan           # Number of overscan steps.
    real   a0, a1             # Wavelength zero-point drift parameters.
    real   bgrating           # Grating specific pixel offset.
    real   mjdexpo            # MJD of the observation.
    real   pixshift           # Computed shift in pixels.
    real   slope              # Slope of original wavelength array.
    real   wavshift           # Computed shift in angstroms.
    string detector           # Detector: BLUE.
    string grating            # Grating.
    string instrument         # Instrument: FOS.
    string msg                # Message string.
    string softmode           # Software mode: SPECTROSCOPY or RAPID-READOUT.
    string workroot           # Root of temporary working image.

    # Reassign the input parameter value.
    infile = input

    # Check that the necessary tasks/packages are loaded.
    msg = ""
    if (!deftask("fparse"))      msg = msg // " stsdas.toolbox.tools"
    if (!deftask("gcopy"))       msg = msg // " stsdas.toolbox.imgtools"
    if (!deftask("access"))      msg = msg // " language"
    if (!deftask("imgets"))      msg = msg // " images.imutil"
    if (!deftask("tdump"))       msg = msg // " tables"
    if (strlen(msg) > 0) {
	printf ("Please load packages: %s\n", msg)
	bye
    }

    # Acquire the instrument, detector, grating, software mode.
    imgets (infile, "INSTRUME")
    instrument = imgets.value
    imgets (infile, "DETECTOR")
    detector   = imgets.value
    imgets (infile, "FGWA_ID")
    grating    = imgets.value
    imgets (infile, "GRNDMODE")
    softmode   = imgets.value

    # Verify that the appropriate data is about to be processed.  This
    # script only applies to the FOS , blue-side, spectroscopy or 
    # rapid-readout data.  PRISM data is not appropriate.
    if (instrument != "FOS")
        error (1, "This correction is only applicable to FOS.")
    if (detector != "BLUE")
        error (1, "This correction is only applicable to DETECTOR = BLUE.")
    if ((softmode != "SPECTROSCOPY") && (softmode != "RAPID-READOUT"))
        error (1, "This correction is only applicable to GRNDMODE = " //
                  "SPECTROSCOPY or RAPID-READOUT.")
    if (grating == "PRI")
        error (1, "This correction is not appropriate for PRISM data.")
   
    # Generate the file name used to preserve the original c0h file.
    fparse (input   = infile,
            verbose = no)
    outfile = fparse.root // "_orig" // fparse.extension
    if (access(outfile))
        error (1, "Output save file - " // outfile // " - already exists.")

    # Call 'gcopy' to copy the original c0h file.
    gcopy (input   = infile, 
           output  = outfile, 
           groups  = "ALL",
           i2toi4  = no,
           verbose = no)

    # Acquire the keywords necessary to compute the number of points
    # in the spectrum.
    imgets (infile, "NXSTEPS")
    nxsteps  = int(imgets.value)
    imgets (infile, "NCHNLS")
    nchnls   = int(imgets.value)
    imgets (infile, "OVERSCAN")
    overscan = int(imgets.value)
    npts = (nchnls + (overscan - 1)) * nxsteps
    
    # Acquire the FPKTTIME keyword which will define the MJD of the exposure.
    imgets (infile, "FPKTTIME")
    mjdexpo = real(imgets.value)

    # Determine the bgrating offset.
    if (grating == "H13")
        bgrating = -0.817
    else if (grating == "H19")
        bgrating = 0.587
    else if (grating == "H27")
        bgrating = 0.081
    else if (grating == "H40")
        bgrating = -0.440
    else if (grating == "H57")
        bgrating = 0.080
    else if (grating == "L65")
        bgrating = -0.579
    else if (grating == "L15")
        bgrating = 0.0
    else
        error (1, "Invalid FGWA_ID value.")

    # Determine the a0 and a1 values depending upon the MJD epoch.
    imjdexpo = int(mjdexpo)
    if ((imjdexpo > 48100) && (imjdexpo < 49083)) {
       a0 = 106.57
       a1 = -0.00221
    }
    else if ((imjdexpo >= 49083) && (imjdexpo <= 49335)) {
       a0 = -115.44
       a1 = 0.00231
    }
    else if (imjdexpo > 49335) {
       a0 = 60.14
       a1 = -0.00126
    }
    else
       error (1, "Invalid FPKTTIME value.")

    # Create a temporary image name for working space.
    workroot = mktemp ("tmp$FOSBwork")

    # Generate the shifted wavelengths.
    imexpr (expr    = "shift(a,1)",
            output  = workroot // ".hhh",
            a       = infile // "[1]")

    # Compute the difference between each wavelength and its neighbor.
    imarith (operand1 = infile // "[1]",
             op       = "-",
             operand2 = workroot // ".hhh",
             result   = workroot // ".hhh",
             title    = "",
             divzero  = 0.0,
             hparams  = "",
             pixtype  = "",
             calctype = "",
             verbose  = no,
             noact    = no)

    # Determine the first pixel which contains a non-zero wavelength
    # value.  L15 and L65 c0h files can contain contiguous zero 
    # wavelength values at the beginning of the array.
    pixlocate (input   = infile // "[1]",
               lower   = 1.0,
               upper   = INDEF,
               maxvals = 10000,
               border  = 0,
               outside = no) | tdump (table  = "STDIN",
                                      cdfile = "",
                                      pfile  = "",
                                      dataf  = "STDOUT",
                                      col    = "c1",
                                      rows   = "1",
                                      pwidth = 80) | scan (dmin)

    # Compute the statistics on the wavelength differences.  Interested
    # in the mean differences from pixels (dmin+1) through npts.
    gstatistics (images  = workroot // ".hhh[" // dmin+1 // ":" // npts // "]",
                 masks   = "",
                 groups  = "1",
                 g_accum = no,
                 fields  = "doall",
                 lower   = INDEF,
                 upper   = INDEF,
                 gstpar  = "")

    slope = gstpar.mean

    # Compute the wavelength shift in pixels.
    pixshift = a0 + a1 * mjdexpo + bgrating

    # Compute the wavelength shift in angstroms.
    wavshift = pixshift * slope
 
    # Compute the new wavelength array.
    imcalc (input  = infile,
            output = infile,
            equals = "if im1 .gt. 0.0 then im1 + " // wavshift // " else 0.0",
            pixtype = "real",
            nullval = 0.0,
            verbose = no)

    # Clean up the temporary work space.
    imdelete (images   = workroot // "*.hhh", 
              go_ahead = yes, 
              verify   = no,
              default_acti = yes, 
              >& "dev$null")

end
