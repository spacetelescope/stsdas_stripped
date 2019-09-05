procedure normspflat (inflat,outflat)

file   inflat 	 {"", prompt="Name of input fringe flat"}
file   outflat   {"", prompt="Name of output, normalized fringe flat"}
bool   do_cal    {yes,prompt="Perform bias and dark subtraction and CR rejection?"}
file   biasfile  {"", prompt="Name of superbias image"}
file   darkfile  {"", prompt="Name of superdark image"}
file   pixelflat {"", prompt="Name of pixel-to-pixel flat?"}
file   wavecal	 {"", prompt="Name of wavecal file [ONLY FOR G750M SPECTRA]?"}
string Version   {"14Oct04", prompt="Date of Installation"}


begin

    int i, j, n, min_pix, cenwave, maxrow, startrow, lastrow, startcol, endcol
    int nimset, nextend, nrptexp, not_ready, highorder, loworder
    int fcol, lcol, ncol, fline, lline, numcols, numlines, bincols, binlines
    real scalefac, maxval, checkval
    string str1, str2, str3, str4, op, op1file, opfile 
    string c_inflat, c_outflat, c_wavecal, c_pixelflat, c_darkfile
    string theinflat, theinbase, theoutflat, thereffile, thedark
    string min_pix_sect, suffix, thebias, c_biasfile
    real mn, sig, med, mod, min, max, ll, ul, fivesig, m_fivesig, p_fivesig
    string img, aperture, opt_elem, thewavecal, thepixelflat
    file tmp_insert

    print ("NORMSPFLAT Version ",(Version), " - Normalizing Fringe Flat")

    set imtype=fits
    not_ready = 0
    
    #*********************************************************************
    # Check if necessary IRAF/STSDAS packages are loaded
    #
    if (! defpac ("stsdas")) {
	print("Please load 'stsdas' package before running this task")
	not_ready = 1
        }
    if (! defpac ("images")) {
	print("Please load 'images' package before running this task")
	not_ready = 1
        }
    if (! defpac ("imfit")) {
	print("Please load 'imfit' package before running this task")
	not_ready = 1
        }
    if (! defpac ("imgtools")) {
	print("Please load 'imgtools' package before running this task")
	not_ready = 1
        }
    if (! defpac ("ttools")) {
	print("Please load 'ttools' package before running this task")
	not_ready = 1
        }
    if (! defpac ("mstools")) {
	print("Please load 'mstools' package before running this task")
	not_ready = 1
        }

    if (not_ready == 1){
	return
        }

    #*********************************************************************
    # Avoid multiple queries of input parameters.... %^$#*%^$ IRAF CL
    #
    c_inflat    = inflat
    c_pixelflat = pixelflat
    c_outflat   = outflat
    c_biasfile  = biasfile
    c_darkfile  = darkfile
    c_wavecal   = wavecal
    #
    #*********************************************************************
    # Strip ".fits" file extensions to prevent confusion
    #
    i = strlen (c_inflat)
    if (substr (c_inflat, i-4, i) == ".fits") {
        theinflat = substr (c_inflat, 1, i-5)}
    else {
        theinflat = c_inflat}
    #
    i = strlen (theinflat)
    if (substr (theinflat, i-3, i) == "_raw") {
        theinbase = substr (theinflat, 1, i-4)}
    else if (substr (theinflat, i-3, i) == "_crj") {
        theinbase = substr (theinflat, 1, i-4)}
    else if (substr (theinflat, i-3, i) == "_sx2") {
	theinbase = substr (theinflat, 1, i-4)}
    else if (substr (theinflat, i-3, i) == "_x2d") {
	theinbase = substr (theinflat, 1, i-4)}
    else if (substr (theinflat, i-4, i) == "_clff") {
	theinbase = substr (theinflat, 1, i-5)}
    else {
        theinbase = theinflat}
    #
    i = strlen (c_wavecal)
    if (substr (c_wavecal, i-4, i) == ".fits") {
	thewavecal = substr (c_wavecal, 1, i-5)}
    else {
 	thewavecal = c_wavecal}
    #
    i = strlen (c_outflat)
    if (substr (c_outflat, i-4, i) == ".fits") {
	theoutflat = substr (c_outflat, 1, i-5)}
    else {
 	theoutflat = c_outflat}
    #
    if (access ("calstis.logfile")){
        del ("calstis.logfile", verify-)}
    if (access (theoutflat//".fits")){
        del (theoutflat//".fits", verify-)}
    #
    #*********************************************************************
    # Get header information from input file (NEXTEND,NRPTEXP,OPT_ELEM)
    #
    if (access ("op1file")){
        del ("op1file", verify-)}
    if (access ("strfile1")){
        del ("strfile1", verify-)}
    if (access ("strfile2")){
        del ("strfile2", verify-)}
    if (access ("strfile3")){
        del ("strfile3", verify-)}
    #
    imgets (theinflat//".fits[0]","nextend")
    nextend = int(imgets.value)
    nimset = nint(nextend/3)
    imgets (theinflat//".fits[0]","nrptexp")
    nrptexp = int(imgets.value)
    imgets (theinflat//".fits[0]","opt_elem")
    opt_elem = imgets.value
    #
    #*********************************************************************
    # If "do_cal = yes", perform basic calstis reduction first (overscan
    # correction, cosmic ray rejection, bias level subtraction, and dark 
    # correction). In case of G750M spectra, do wavelength calibration and 
    # geometric correction as well. 
    #
    if (do_cal) {
        #*****************************************************************
        # If darkfile = "" then use darkfile in main header of
        # input flat field
        # 
        imgets (theinflat//".fits[0]", "DARKFILE", > "dev$null")
        i = strlen(c_darkfile)
        if (i == 0 || substr(darkfile, 1, 1) == " ") {
 	    c_darkfile = imgets.value}
        #
        i = strlen(c_darkfile)
        if (substr(c_darkfile, i-4, i) == ".fits") {
            thedark = substr(c_darkfile, 1, i-5)}
        else {
            thedark = c_darkfile}
        #
        #*****************************************************************
        # If biasfile = "" then use biasfile in main header of
        # input flat field
        # 
        imgets (theinflat//".fits[0]", "BIASFILE", > "dev$null") 
        i = strlen(c_biasfile)
        if (i == 0 || substr(biasfile, 1, 1) == " ") {  
            c_biasfile = imgets.value}
        #
        i = strlen(c_biasfile)
        if (substr(c_biasfile, i-4, i) == ".fits") {
            thebias = substr(c_biasfile, 1, i-5)}
        else {
            thebias = c_biasfile}
        #
        #*****************************************************************
        # If pixelflat = "" then use pixelflat in main header of
        # input flat field
        #
        imgets (theinflat//".fits[0]", "PFLTFILE", > "dev$null")
        i = strlen(c_pixelflat)
        if (i == 0 || substr(pixelflat, 1, 1) == " ") {
            c_pixelflat = imgets.value}
        i = strlen(c_pixelflat)
        if (substr(c_pixelflat, i-4, i) == ".fits") {
            thepixelflat = substr(c_pixelflat, 1, i-5)}
        else {
            thepixelflat = c_pixelflat}
        #
        if (nextend <= 3) {
            print ("Sorry, your input image seems to have \
            only 1 imset.")
            print ("This task can only handle images with the  \
            NEXTEND keyword being equal to 3*N (N > 1).")
            delete ("op1file,strfile*", verify-, > "dev$null")
            return
            }
        print(" ")
        if (nrptexp != nimset) {
            print("Keyword NRPTEXP: ",nrptexp," while nr. of imsets = ",nimset)
            hedit (theinflat//".fits[0]", "NRPTEXP", nimset, verify-, \
                   delete=no, show-, update+)
            hedit (theinflat//".fits[0]", "CRSPLIT", "1", verify-, show-, \
                   delete=no, update+)
            print (">>>> Updated keyword NRPTEXP to ",nimset)
            print ("    (and put keyword CRSPLIT at 1)")
            }
        else {
            print("Keyword NRPTEXP: ",nrptexp," while nr. of imsets = ", \
                  nimset, " -> OK")
            }
        print(" ")
        hedit (theinflat//".fits[0]", \
               "DQICORR,BLEVCORR,BIASCORR,CRCORR,DARKCORR,FLATCORR", "PERFORM",
               add=no, addonly=no, delete=no, verify-, show-, update+)
        hedit (theinflat//".fits[0]", "BIASFILE", \
               thebias//".fits", delete=no, verify-, show-, update+) 
        hedit (theinflat//".fits[0]", "DARKFILE", \
               thedark//".fits", delete=no, verify-, show-, update+) 
        hedit (theinflat//".fits[0]", "PFLTFILE", \
               thepixelflat//".fits", delete=no, verify-, show-, update+) 
        #
        i = strlen (opt_elem)
        if (substr (opt_elem, i-4, i) == "G750M"){
            #
            # Perform wavelength calibration and geometric correction 
            # for G750M flat
            #
            hedit (theinflat//".fits[0]", \
                   "WAVECORR,HELCORR,X2DCORR", "PERFORM", add=no, addonly=no,
                   delete=no, verify-, show-, update+)
            print ("Running calstis on input G750M fringe flat ...")
            print ("   >>> Log file of calstis procedure will be ", \
                   "calstis.logfile")
            calstis (theinflat//".fits", wavecal=thewavecal//".fits", \
                     outroot="", savetmp-, verbose-, > "calstis.logfile") }
        else {
            hedit (theinflat//".fits[0]", \
                   "WAVECORR,HELCORR,X2DCORR", "OMIT", add=no, addonly=no,
                   delete=no, verify-, show-, update+) 
            print ("Running calstis on input G750L fringe flat ...")
            print ("   >>> Log file of calstis procedure will be ", \
                   "calstis.logfile")
            calstis (theinflat//".fits", savetmp-, verbose-, \
                     outroot="", > "calstis.logfile")
            }
        }
    #
    #*********************************************************************
    # Create output image (unity) with the right header format; 
    # Also, if the input fringe flat is a G750L one, correct it for 
    # pixel-to-pixel variations by dividing by a well-exposed 50CCD
    # pipeline flat prior to any scaling etc. Note that in that case,
    # the GO will have to retrieve the 50CCD flat (or e.g. a G430M flat)
    #
    i = strlen (opt_elem)
    if (do_cal) {
        if (substr (opt_elem, i-4, i) == "G750M"){
            msarith (theinbase//"_sx2.fits", "/", theinbase//"_sx2.fits", \
                     theoutflat//"_tmp.fits",  verbose=0)}
        else {
            if (access (theinbase//"_clff.fits")){
                del (theinbase//"_clff.fits", verify-)}
            msarith (theinbase//"_crj.fits", "/", theinbase//"_crj.fits", \
                     theoutflat//"_tmp.fits",  verbose=0)
            # "thepixelflat.fits" will have to be copied onto a "normal"
            # STIS file first for the next msarith operation to work. 
            #  msarith (theinbase//"_crj.fits", "/", thepixelflat//".fits", \ 
            #  theinbase//"_clff.fits", verbose=0)
            copy (theinbase//"_crj.fits", theinbase//"_clff.fits", verbose-)
            }
        }
    else {
        if (substr (opt_elem, i-4, i) == "G750M"){
        msarith (theinflat//".fits", "/", theinflat//".fits", \
                 theoutflat//"_tmp.fits",  verbose=0)}
        else{
            if (access (theinbase//"_clff.fits")){
                del (theinbase//"_clff.fits", verify-)
                }
            print ("---------------------------------------------------", \
                   "----------------------")
            print ("IT IS ASSUMED THAT PIXEL-TO-PIXEL FLATFIELDING HAS ", \
                   "ALREADY BEEN PERFORMED")
            print ("---------------------------------------------------", \
                   "----------------------")
            msarith (theinflat//".fits", "/", theinflat//".fits", \
                     theoutflat//"_tmp.fits",  verbose=0)
            # "thepixelflat.fits" will have to be copied onto a "normal"
            # STIS file first for the next msarith operation to work. 
            #    msarith (theinflat//".fits", "/", thepixelflat//".fits", \ 
            #      theinbase//"_clff.fits", verbose=0) 
            copy (theinflat//".fits", theinbase//"_clff.fits", verbose-)
            }
        }
    #
    #*************************************************************************
    # Do a line-by-line cubic spline fit to the fringe flat to remove the lamp 
    # function. The range of lines to correct is set according to the APERTURE
    # name in the main header. 
    # The actual spline fitting procedure is dependent on the grating and
    # central wavelength of the observation: In case of a G750M spectrum with
    # central  wavelength below 10000 A, a spline fit with only one spline
    # piece is made; for a G750M spectrum with central wavelength 10363 A, a
    # spline fit with 7 pieces is made; for a G750L flat at 7751 A, the
    # normalization is done by a combination of 2 cubic spline fits to the
    # lamp function: one fit using 10 spline pieces, and one fit using 60
    # spline pieces. 
    # Otherwise (i.e., G750L grating at 8975 A), a single fit is made with 20 
    # spline pieces. 
    #
    if (access ("opfile")){
        del ("opfile", verify-)}
    if (access ("strfile")){
        del ("strfile", verify-)}
    if (access ("strfile2")){
        del ("strfile2", verify-)}

    i = strlen (opt_elem)
    if (substr (opt_elem, i-4, i) == "G750L"){
        print ((theinbase)//"_clff.fits", >> "opfile")
        imgets (theinbase//"_clff.fits[sci,1]", "naxis1")
        numcols = int(imgets.value)
        imgets (theinbase//"_clff.fits[sci,1]", "naxis2")
        numlines = int(imgets.value)
        imgets (theinbase//"_clff.fits[sci,1]", "binaxis1")
        bincols = int(imgets.value)
        imgets (theinbase//"_clff.fits[sci,1]", "binaxis2")
        binlines = int(imgets.value)
        }
    else {
        if (access (theinbase//"_sx2.fits")){
        print ((theinbase)//"_sx2.fits", >> "opfile")
        imgets (theinbase//"_sx2.fits[sci,1]", "naxis1")
        numcols = int(imgets.value)
        imgets (theinbase//"_sx2.fits[sci,1]", "naxis2")
        numlines = int(imgets.value)
        imgets (theinbase//"_sx2.fits[sci,1]", "binaxis1")
        bincols = int(imgets.value)
        imgets (theinbase//"_sx2.fits[sci,1]", "binaxis2")
        binlines = int(imgets.value)
        }
    else if (access (theinbase//"_x2d.fits")){
        print ((theinbase)//"_x2d.fits", >> "opfile")
        imgets (theinbase//"_x2d.fits[sci,1]", "naxis1")
        numcols = int(imgets.value)
        imgets (theinbase//"_x2d.fits[sci,1]", "naxis2")
        numlines = int(imgets.value)
        imgets (theinbase//"_x2d.fits[sci,1]", "binaxis1")
        bincols = int(imgets.value)
        imgets (theinbase//"_x2d.fits[sci,1]", "binaxis2")
        binlines = int(imgets.value)
        }
    else {
        print ("Sorry, your calstis'ed G750M flat has neither ", \
               "_sx2 nor _x2d extension.")
        print ("What happened?")
        return
        }
    }
    #
    list = "opfile"
    while (fscan (list,img) != EOF){
        keypar (img//"[0]","CENWAVE")
        cenwave = int(keypar.value)
        keypar (img//"[0]","APERTURE")
        aperture = keypar.value
        print ("                        Aperture: ", aperture)
        print ("              Central wavelength: ", cenwave)
        print ("Number of columns in fringe flat: ", numcols)
        print ("   Number of rows in fringe flat: ", numlines)
        }
    del ("opfile", verify-)
    #------------------------------------------------------------------------
    # Get row with max. counts in the short-slit fringe flat
    #
    #
    i = strlen (aperture)
    if (substr (aperture, 1, 3) != "52X"){
        fcol = nint(0.2*numcols)
        lcol = nint(0.8*numcols)
        ncol = lcol-fcol+1
        fline = nint(0.45*numlines)+1
        lline = nint(0.55*numlines)
        #
        if (access ("temp_slitprof.fits")){
            del ("temp_slitprof.fits", verify-)
            }
        blkavg (img//"[sci]["//fcol//":"//lcol//",*]", \
                "temp_slitprof.fits", ncol, 1, option="average")
        # blkavg (img//"[sci][200:800,*]", "temp_slitprof.fits", 601, 1)
        #
        ## Create ascii file with the pixels containing image values > 1000 ADU
        # Create ascii file with the pixels containing image values > 
        #						     1/5 of max. ADU
        #
        imstat ("temp_slitprof.fits[1,"//fline//":"//lline//"]", lower=INDEF, \
                upper=INDEF, fields="max", binwidth=0.1, format-) | scan(maxval)
        checkval = maxval/5.
        if (access ("temp_slitprof.dat")){
            del ("temp_slitprof.dat", verify-)}
        #pixlocate ("temp_slitprof.fits[1,"//fline//":"//lline//"]",lower=1000,
        #        upper=INDEF, maxvals=1000, border=0, > "temp_slitprof.dat")
        pixlocate ("temp_slitprof.fits[1,"//fline//":"//lline//"]", \
                   lower=checkval, upper=INDEF, maxvals=5000, border=0, \
                   > "temp_slitprof.dat")
        #
        # Convert to STSDAS table 
        #
        if (access ("temp_slitprof.tab")){
            del ("temp_slitprof.tab", verify-)}
        if (access ("temp_slitprof.cd")){
            del ("temp_slitprof.cd", verify-)}
        print ("# format for slit profile table", > "temp_slitprof.cd")
        print ("AXIS2    I      i5              Pixels", >> "temp_slitprof.cd")
        print ("DATA     R      f15.6           Counts", >> "temp_slitprof.cd")
        tcreate ("temp_slitprof.tab", "temp_slitprof.cd", \
                 "temp_slitprof.dat", uparfile="", nskip=0, nlines=0, \
                 nrows=0, hist=yes, extrapar=5, tbltype="default", extracol=0)
        tcalc ("temp_slitprof.tab", "data", \
               "if data < 0.0 then data*(-1.) else data", datatype="real")
        tsort ("temp_slitprof.tab", "data", ascend=no, casesens=no)
        tprint ("temp_slitprof.tab", prdata+, pwidth=80, plength=0, showrow-, \
                orig_row+, showhdr-, showunit-, columns="axis2", rows="1", \
                option="plain", prparam-, align+, sp_col="", \
                lgroup=0) | scan(maxrow)
        maxrow=maxrow+fline-1
        del ("temp_slitprof.dat,temp_slitprof.cd,temp_slitprof.tab", verify-)
        del ("temp_slitprof.fits", verify-)
        }
    #------------------------------------------------------------------------
    #
    #------------------------------------------------------------------------
    # Set rows to be fit according to aperture name
    #
    i = strlen (aperture)
    j = strlen (opt_elem)
    if (substr (aperture, i-7, i) == "0.3X0.09"){
        startrow=maxrow-nint(4./binlines+0.25)
        lastrow=maxrow+nint(3./binlines+0.25) 
        }
    else if (substr (aperture, i-7, i) == "0.2X0.06"){
        startrow=maxrow-nint(3./binlines+0.25)
        lastrow=maxrow+nint(2./binlines+0.25) }
    else if (substr (aperture, 1, 3) != "52X"){
        print ("ERROR: not able to understand APERTURE keyword (", \
               aperture, ")") 
        return }
    else if (substr (opt_elem, j-4, j) == "G750M"){
	startrow=nint(92./binlines)+1
	lastrow=startrow+nint(1024./binlines)-1 }
    else { 
        startrow=1
        lastrow=numlines }
    #------------------------------------------------------------------------
    #
    #------------------------------------------------------------------------
    # Determine fitting procedure according to grating and central wavelength
    #
    if (substr (opt_elem, j-4, j) == "G750M") {
        if (access ("norm_inflat.fits")){
            del ("norm_inflat.fits", verify-)}

        if (cenwave < 9800) {
            fit1d (img//"[sci]", "norm_inflat.fits", type="ratio", axis=1, \
                   interactive=no,naverage=2, function="spline3",order=1, \
                   low_reject=5.0,high_reject=5., niterate=2, \
                   sample="86:1109", grow=0.)
            }
        else if (cenwave == 9851) {
            fit1d (img//"[sci]", "norm_inflat.fits", type="ratio", axis=1, \
                   interactive=no,naverage=2, function="spline1",order=2, \
                   low_reject=5.0,high_reject=5., niterate=2, \
                   sample="86:1109", grow=0.)
            }
        else {
            fit1d (img//"[sci]", "norm_inflat.fits", type="ratio", axis=1, \
                   interactive=no,naverage=2, function="spline3",order=2, \
                   low_reject=5.0,high_reject=5., niterate=2, \
                   sample="86:1109", grow=0.)
            }

        imcopy ("norm_inflat.fits[*,"//startrow//":"//lastrow//"]", \
                theoutflat//"_tmp.fits[sci][*,"//startrow//":"//lastrow//"]", \
                verb-)

        del ("norm_inflat.fits", verify-) 
        }
     else if (cenwave == 7751) {
         if (access ("norm_inflat_a.fits")){
         del ("norm_inflat_a.fits", verify-)}
     if (access ("norm_inflat_b.fits")){
         del ("norm_inflat_b.fits", verify-)}
     if (access ("resp_inflat_a.fits")){
         del ("resp_inflat_a.fits", verify-)}
     if (access ("resp_inflat_b.fits")){
         del ("resp_inflat_b.fits", verify-)}
     if (access ("diff_resp_a_b.fits")){
         del ("diff_resp_a_b.fits", verify-)}
     if (access ("resp_inflat_abyb.fits")){
         del ("resp_inflat_abyb.fits", verify-)}

     if (bincols == 1) {
         startcol  = 591
         endcol    = 640
         highorder = 60
         loworder  = 12
         }
     else if (bincols == 2){
         startcol  = 295
         endcol    = 320
         highorder = 50
         loworder  = 12
         }
     else if (bincols == 4){
         startcol  = 145
         endcol    = 160
         highorder = 50
         loworder  = 12
         }

     fit1d (img//"[sci]", "norm_inflat_a.fits", type="ratio", axis=1, \
            interactive=no,naverage=2, \
            function="spline3",order=highorder,low_reject=5.0,high_reject=5., \
            niterate=2, sample="*", grow=0.)
     imarith (img//"[sci]", "/", "norm_inflat_a.fits", "resp_inflat_a.fits", \
              verbose-, noact-)
     fit1d (img//"[sci]", "norm_inflat_b.fits", type="ratio", axis=1, \
            interactive=no,naverage=2, \
            function="spline3",order=loworder,low_reject=5.0,high_reject=5., \
            niterate=2, sample="*", grow=0.)
     imarith (img//"[sci]", "/", "norm_inflat_b.fits", "resp_inflat_b.fits", \
              verbose-, noact-)
   
     hedit ("resp_inflat_?.fits", "nextend", "", add-, delete+, verify-, \
            show-, update+, > "dev$null")
     imcalc ("resp_inflat_a.fits,resp_inflat_b.fits", "diff_resp_a_b.fits", \
             "if im1 < im2 then im2-im1 else im1-im2", verb-)
     imarith ("resp_inflat_a.fits", "/", "resp_inflat_b.fits", \
              "resp_inflat_abyb.fits", verbose-, noact-)

     for (i=startrow; i <= lastrow; i += 1){
 
         tmp_insert = mktemp("tmp$ff")
         minmax ("diff_resp_a_b.fits["//startcol//":"//endcol//","//i//"]", \
                 force-, update-, verbose-)
         min_pix_sect = str(minmax.minpix)
         n = strlen (min_pix_sect)
         # get rid of square brackets in min_pix_sect and add "startcol-1"
         # (eg 590)
         min_pix = int(substr (min_pix_sect, 2, n-1)) + startcol - 1
         imstat ("resp_inflat_abyb.fits["//min_pix//","//i//"]",\
                 fields="mean", format-) | scan(scalefac)
         print ("Line: ", i, "-- Min. difference at col: ", min_pix, \ 
                " Difference: Factor ", scalefac)
   
         imcalc ("norm_inflat_a.fits[*,"//i//"],norm_inflat_b.fits[*,"//i//"]",\
                 tmp_insert, \
                 "if x .gt. "//min_pix//" then (im2*"//scalefac//") else im1", verb-)
         
         imcopy (tmp_insert//"[*]", theoutflat//"_tmp.fits[sci][*,"//i//"]", \
                 verb-)
         del (tmp_insert//"*", verify-)
         }
     }
     else {
         if (access ("norm_inflat.fits")){
         del ("norm_inflat.fits", verify-)}

     fit1d (img//"[sci]", "norm_inflat.fits", type="ratio", axis=1, \
            interactive=no,naverage=2, \
            function="spline3",order=20,low_reject=3.0,high_reject=3., \
            niterate=2, sample="*", grow=0.)

     imcopy ("norm_inflat.fits[*,"//startrow//":"//lastrow//"]", \
             theoutflat//"_tmp.fits[sci][*,"//startrow//":"//lastrow//"]", verb-)
     }
     #
     #*********************************************************************
     #
     if (access ("norm_inflat_a.fits")){
         del ("norm_inflat_a.fits", verify-)}
     if (access ("norm_inflat_b.fits")){
         del ("norm_inflat_b.fits", verify-)}
     if (access ("resp_inflat_a.fits")){
         del ("resp_inflat_a.fits", verify-)}
     if (access ("resp_inflat_b.fits")){
         del ("resp_inflat_b.fits", verify-)}
     if (access ("diff_resp_a_b.fits")){
         del ("diff_resp_a_b.fits", verify-)}
     if (access ("resp_inflat_abyb.fits")){
         del ("resp_inflat_abyb.fits", verify-)}

     if (access (theinbase//"_clff.fits")) {
         del (theinbase//"_clff.fits", verify-)
         }
     rename (theoutflat//"_tmp.fits", theoutflat//".fits")
     print (" ")
     print ("Output fringe flat: ", theoutflat, ".fits")
     #
     # DONE!
     # 
end
