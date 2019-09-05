procedure prepspec (inspec)

file 	inspec 	  {"", prompt="Name of input `raw' science spectrum"}
file 	outroot   {"", prompt="Root for output file name"}
file 	darkfile  {"", prompt="Name of superdark image"}
file 	pixelflat {"", prompt="Name of pixel-to-pixel flat?"}
string  initgues  {"min", prompt="Initial value estimate for ocrreject (min|med)"}
string  Version   {"14Oct04",prompt="Date of Installation"}


begin

    int i, j, cenwave, nlines
    int nimset, nextend, nrptexp, inextend, fnextend, not_ready 
    string str1, str2, str3, str4, op, op1file, opfile, op2file, s1, s2, s3
    string c_inspec, c_outspec, c_inflat, c_pixelflat, c_darkfile, c_outroot
    string theinflat, theinbase, theinspec, theoutspec, thedark
    string theoutroot, img, aperture, opt_elem, thewavecal, thepixelflat
    #
    print ("PREPSPEC Version ",(Version), \ 
           " - Preparing STIS G750 (L/M) spectra for de-fringing")
    #
    set imtype=fits
    not_ready = 0
    #*********************************************************************
    # Check if necessary IRAF/STSDAS packages are loaded
    #
    if (! defpac ("stsdas")) {
	print("Please load 'stsdas' package before running this task")
	not_ready = 1
        }
    if (! defpac ("imgtools")) {
	print("Please load 'imgtools' package before running this task")
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
    # Avoid multiple queries of input parameters....
    #
    c_inspec    = inspec
    c_outroot   = outroot
    c_darkfile  = darkfile
    c_pixelflat = pixelflat
    #
    #*********************************************************************
    # Strip ".fits" file extensions to prevent confusion
    #
    i = strlen (c_inspec)
    if (substr (c_inspec, i-4, i) == ".fits") {
        theinspec = substr (c_inspec, 1, i-5)}
    else {
 	theinspec = c_inspec}
    #
    i = strlen (theinspec)
    if (substr (theinspec, i-3, i) == "_raw") {
        theinbase = substr (theinspec, 1, i-4)}
    else if (substr (theinspec, i-3, i) == "_crj") {
	print ("You gotta be kidding! ", \
               "I'm sure you don't want to process a _crj file... Bye!")
	return
        #	theinbase = substr (theinspec, 1, i-4)
	}
    else if (substr (theinspec, i-3, i) == "_sx2") {
	print ("You gotta be kidding! ", \
               "I'm sure you don't want to process a _sx2 file... Bye!")
	return
        #	theinbase = substr (theinspec, 1, i-4)
	}
    else {
 	theinbase = theinspec}
    #
    i = strlen (c_outroot)
    if (i >= 5 && substr (c_outroot, i-4, i) == ".fits") {
        theoutroot = substr (c_outroot, 1, i-5)}
    else if (i == 0 || substr(c_outroot, 1, 1) == " ") {
	theoutroot = theinbase}
    else {
        theoutroot = c_outroot}
    #
    #
    #*****************************************************************
    # If pixelflat = "" then use pixelflat in main header of
    # input flat field
    #
    imgets (theinspec//".fits[0]", "PFLTFILE", > "dev$null")
    i = strlen(pixelflat)
    if (i == 0 || substr(pixelflat, 1, 1) == " ") {
        c_pixelflat = imgets.value}
    i = strlen (c_pixelflat)
    if (substr (c_pixelflat, i-4, i) == ".fits") {
        thepixelflat = substr (c_pixelflat, 1, i-5)}
    else {
 	thepixelflat = c_pixelflat}
    #
    #********************************************************************
    # If darkfile = "" then use darkfile in main header of input spectrum
    # 
    imgets (theinspec//".fits[0]", "DARKFILE", > "dev$null") 
    i = strlen (darkfile)
    if (i == 0 || substr(darkfile, 1, 1) == " ") {
 	c_darkfile = imgets.value}
    i = strlen (c_darkfile)
    if (substr (c_darkfile, i-4, i) == ".fits") {
        thedark = substr (c_darkfile, 1, i-5)}
    else {
 	thedark = c_darkfile}
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
    #*********************************************************************
    # Perform basic calstis reduction first (overscan
    # correction, cosmic ray rejection, bias level subtraction, and dark 
    # correction). In case of G750M spectra, do wavelength calibration and 
    # geometric correction as well. 
    #
    if (access ("prepspec.log")){
        del ("prepspec.log", verify-)}
    #******************************************************************
    # Get header information from input file (NEXTEND,NRPTEXP,OPT_ELEM)
    #
    imgets (theinspec//".fits[0]","nextend")
    nextend = int(imgets.value)
    nimset = nextend/3
    imgets (theinspec//".fits[0]","nrptexp")
    nrptexp = int(imgets.value)
    imgets (theinspec//".fits[0]","opt_elem")
    opt_elem = imgets.value
    #
    if (nextend <= 3) {
  	print ("Sorry, your input image seems to have \
        only 1 imset.")
  	print ("This task can only handle images with the  \
        NEXTEND keyword being equal to 3*N (N > 1).")
  	delete ("op1file,strfile*", verify-)
  	bye
        }
    print(" ")
    if (nrptexp != nimset) {
        print("Keyword NRPTEXP: ",nrptexp," while nr. of imsets = ",nimset)
  	hedit (theinspec//".fits[0]", "NRPTEXP", nimset, verify-, \
               show-, update+)
  	hedit (theinspec//".fits[0]", "CRSPLIT", "1", verify-, show-, \
               update+)
  	print (">>>> Updated keyword NRPTEXP to ",nimset)
  	print ("    (and put keyword CRSPLIT at 1)")
        }
    else {
        print("Keyword NRPTEXP: ",nrptexp," while nr. of imsets = ",nimset, \
              " -> OK")
        }
    print(" ")
    hedit (theinspec//".fits[0]", \
           "DQICORR,BLEVCORR,BIASCORR,CRCORR,DARKCORR,FLATCORR", \
           "PERFORM", verify-, show-, update+)
    hedit (theinspec//".fits[0]", "DARKFILE", \
           thedark//".fits", verify-, show-, update+) 
    hedit (theinspec//".fits[0]", "PFLTFILE", \
           thepixelflat//".fits", verify-, show-, update+) 
    #
    #
    # Overscan subtraction
    #
    if (access ("blevcorr.log")) {
        del ("blevcorr.log", verify-)}
    if (access ("basic2d.log")) {
        del ("basic2d.log", verify-)}
    if (access ("ocrreject.log")) {
 	del ("ocrreject.log", verify-)}
    if (access (theinbase//"_blev.fits")) {
 	del (theinbase//"_blev.fits", verify-)}
    if (access (theinbase//"_crj_tmp.fits")) {
 	del (theinbase//"_crj_tmp.fits", verify-)}
    if (access (theoutroot//"_crj.fits")) {
 	del (theoutroot//"_crj.fits", verify-)}

    print ("Subtracting overscan levels ...")
    basic2d (theinspec//".fits", theinbase//"_blev.fits", \
             outblev="", dqicorr="perform", atodcorr="omit", \
             blevcorr="perform", doppcorr="omit", lorscorr="omit", \
             glincorr="omit", lflgcorr="omit", biascorr="omit", \
             darkcorr="omit", flatcorr="omit", shadcorr="omit", \
             photcorr="omit", statflag=no, verb-, > "blevcorr.log")

    hedit (theinspec//".fits[0]", "BIASCORR", "PERFORM", \
           verify-, show-, update+)
    hedit (theinbase//"_blev.fits[0]", "BIASCORR", "PERFORM", \
           verify-, show-, update+)
    #
    # Cosmic ray rejection
    #
    print ("Rejecting cosmic rays ...")
    ocrreject (theinbase//"_blev.fits", theinbase//"_crj_tmp.fits", verb+, \
               all=yes, crrejtab="", scalense="", initgues=initgues, \
               skysub="", crsigmas="", crradius=INDEF, crthres=INDEF, \
               badinpdq=INDEF, crmask="", > "ocrreject.log")
    hedit (theinbase//"_crj_tmp.fits[0]", "FILENAME", \
           theinbase//"_crj_tmp.fits", add-, delete-, verify-, show-, update+)
    #
    # Get rid of intermediate files created by basic2d
    #
    del (theinbase//"_blev.fits",verify-)
    #
    # Bias level correction
    #
    print ("Subtracting bias and dark, and dividing by pixel-to-pixel flat ...")
    basic2d (theinbase//"_crj_tmp.fits", theoutroot//"_crj.fits", \
             outblev="", dqicorr="omit", atodcorr="omit", \
             blevcorr="omit", doppcorr="omit", lorscorr="omit", \
             glincorr="omit", lflgcorr="omit", biascorr="perform", \
             darkcorr="perform", flatcorr="perform", shadcorr="omit", \
             photcorr="perform", statflag=yes, verb-, > "basic2d.log")
    #
    # Get rid of intermediate files created by ocrreject
    #
    del (theinbase//"_crj_tmp.fits",verify-)
    # 
    i = strlen (opt_elem)
    if (substr (opt_elem, i-4, i) == "G750M"){
        #
        # Perform wavelength calibration and geometric correction 
        # for G750M flat
        #
        if (access ("x2d.log")) {
            del ("x2d.log", verify-)}
        hedit (theoutroot//"_crj.fits[0]", \
               "WAVECORR,HELCORR,X2DCORR", "PERFORM", verify-, \
               show-, update+)
        hedit (theoutroot//"_crj.fits[0]", \
               "X1DCORR", "OMIT", verify-, show-, update+) 
        #   print ("Running calstis on input G750M spectrum ...")
        #   calstis (theinspec//".fits", \
        #     outroot=theoutroot, savetmp-, verbose-, > "prepspec.log") 
        #     wavecal=thewavecal//".fits", \
        print ("Running x2d on input G750M spectrum ...")
        x2d (theoutroot//"_crj.fits", output=theoutroot//"_sx2.fits", 
             helcorr="perform", fluxcorr="perform", statflag+, \
             verbose-, > "x2d.log")
        #     wavecal=thewavecal//".fits", \
        print ("Calibrated spectrum: file ", theoutroot, "_sx2.fits")
        }
    else {
        hedit (theinspec//".fits[0]", \
               "WAVECORR,HELCORR,X1DCORR,X2DCORR", "OMIT", \
               verify-, show-, update+) 
        #     print ("Running calstis on input G750L spectrum ...")
        #     calstis (theinspec//".fits", savetmp-, verbose-, \
        #       outroot=theoutroot, > "prepspec.log")
        print ("Calibrated spectrum: file ", theoutroot, "_crj.fits")
        }
    # 
    if (access (theinbase//"_flt.fits")){
        del (theinbase//"_flt.fits", verify-)}
    #
    # DONE!
    # 
end
