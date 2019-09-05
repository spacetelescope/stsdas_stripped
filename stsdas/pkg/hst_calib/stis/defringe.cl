procedure defringe (inspec,inflat,outspec)

file 	inspec 	  {"", prompt="Name of input science spectrum"}
file 	inflat 	  {"", prompt="Name of input [final] fringe flat"}
file 	outspec	  {"", prompt="Name of output science spectrum"}
bool    do_cal    {no, prompt="Perform calstis processing before defringing?"}
file 	darkfile  {"", prompt="Name of superdark image"}
file 	pixelflat {"", prompt="Name of pixel-to-pixel flat?"}
bool	do_defringe {yes, prompt="Correct spectrum for fringing?"}
string  Version   {"14Oct04", prompt="Date of Installation"}


begin

    bool   do_flat, do_calib
    int i, j, cenwave, nlines
    int nimset, nextend, nrptexp, inextend, fnextend, not_ready 
    string flatcorr, x2dcorr
    string str1, str2, str3, str4, op, op1file, s1
    string c_inspec, c_outspec, c_inflat, c_pixelflat, c_darkfile
    string theinflat, theinbase, theinspec, theoutspec, thedark
    string img, aperture, opt_elem, thewavecal, thepixelflat
    #
    print ("DEFRINGE Version ",(Version), \ 
           " - Defringing STIS G750L or G750M spectra")
    #
    do_flat    = yes
    do_calib   = yes
    set imtype = fits
    not_ready  = 0
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
    c_inflat    = inflat
    c_outspec   = outspec
    c_pixelflat = pixelflat
    c_darkfile  = darkfile
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
    i = strlen (c_inflat)
    if (substr (c_inflat, i-4, i) == ".fits") {
	theinflat = substr (c_inflat, 1, i-5)}
    else {
 	theinflat = c_inflat}
    #
    i = strlen (theinspec)
    if (substr (theinspec, i-3, i) == "_raw") {
	theinbase = substr (theinspec, 1, i-4)}
    else if (substr (theinspec, i-3, i) == "_crj") {
	theinbase = substr (theinspec, 1, i-4)}
    else if (substr (theinspec, i-3, i) == "_sx2") {
	theinbase = substr (theinspec, 1, i-4)}
    else {
 	theinbase = theinspec}
    #
    #
    #*****************************************************************
    # If pixelflat = "" then use pixelflat in main header of
    # input flat field
    #
    imgets (theinspec//".fits[0]", "PFLTFILE", > "dev$null")
    i = strlen(c_pixelflat)
    if (i == 0 || substr(pixelflat, 1, 1) == " ") {
        c_pixelflat = imgets.value}
    i = strlen (c_pixelflat)
    if (substr (c_pixelflat, i-4, i) == ".fits") {
        thepixelflat = substr (c_pixelflat, 1, i-5)}
    else {
 	thepixelflat = c_pixelflat}
    #
    i = strlen (c_outspec)
    if (substr (c_outspec, i-4, i) == ".fits") {
	theoutspec = substr (c_outspec, 1, i-5)}
    else {
 	theoutspec = c_outspec}
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
    # If "do_cal = yes", perform basic calstis reduction first (overscan
    # correction, cosmic ray rejection, bias level subtraction, and dark 
    # correction). In case of G750M spectra, do wavelength calibration and 
    # geometric correction as well. 
    #
    if (do_cal) {
        #*****************************************************************
        # If darkfile = "" then use darkfile in main header of
        # input spectrum
        # 
        imgets (theinspec//".fits[0]", "DARKFILE", > "dev$null") 
        i = strlen (darkfile)
        if (i == 0 || substr (darkfile, 1, 1) == " ") {
            c_darkfile = imgets.value}
        i = strlen (c_darkfile)
        if (substr (c_darkfile, i-4, i) == ".fits") {
            thedark = substr (c_darkfile, 1, i-5)}
        else {
            thedark = c_darkfile}
        #
        if (access ("spec.calstis.log")){
            del ("spec.calstis.log", verify-)}
        #******************************************************************
        # Get header information from input file (NEXTEND,NRPTEXP,OPT_ELEM)
        #
        print ((theinspec)//".fits", >> "op1file")
        #
        list = "op1file"
        while (fscan (list,s1) != EOF){
	keypar (s1//"[0]","nextend")
	nextend = int(keypar.value)
	keypar (s1//"[0]","nrptexp")
	nrptexp = int(keypar.value)
	keypar (s1//"[0]","opt_elem")
	opt_elem = keypar.value
	nimset = nextend/3
        }
        del ("op*file", verify-)
        #********************************************************************
        # If FLATCORR = "COMPLETE" then inform user and skip calstis step
        #
        imgets (theinspec//".fits[0]", "FLATCORR", > "dev$null")
        flatcorr = imgets.value
        i = strlen (flatcorr)
        if (substr (flatcorr, i-7, i) == "COMPLETE"){
            if (substr (opt_elem, 1, 5) == "G750M"){
                imgets (theinspec//".fits[0]", "X2DCORR", > "dev$null")
                x2dcorr = imgets.value
                if (substr (x2dcorr, 1, 8) == "COMPLETE"){
                    do_calib = no
                    }
                }
                if (do_calib) {
                    print (" ")
                    print ("FLATCORR ALREADY DONE -> WILL ONLY PERFORM STEPS AFTER ", \
                           "THAT, IF ANY.")
                    print (" ")
                    do_flat = no
                    }
                }
            #
            if (do_calib && do_flat) {
                if (nextend <= 3) {
                print ("Sorry, your input image seems to have only 1 imset.")
                print ("This task can only handle images with the ", \
                       "NEXTEND keyword being equal to 3*N (N > 1).")
                delete ("op1file,strfile*", verify-)
                bye
                }
            print(" ")
            if (nrptexp != nimset) {
                print("Keyword NRPTEXP: ",nrptexp," while nr. of imsets = ", \
                      nimset)
                hedit (s1//"[0]", "NRPTEXP", nimset, verify-, show-, update+)
                hedit (s1//"[0]", "CRSPLIT", "1", verify-, show-, update+)
                print (">>>> Updated keyword NRPTEXP to ",nimset)
                print ("    (and put keyword CRSPLIT at 1)")
                }
            else {
                print("Keyword NRPTEXP: ",nrptexp," while nr. of imsets = ", \
                      nimset, " -> OK")
                }
            print(" ")
            hedit (theinspec//".fits[0]", \
                   "DQICORR,BLEVCORR,BIASCORR,CRCORR,DARKCORR,FLATCORR", \
                   "PERFORM", verify-, show-, update+)
            hedit (theinspec//".fits[0]", "DARKFILE", \
                   thedark//".fits", verify-, show-, update+) 
            hedit (theinspec//".fits[0]", "PFLTFILE", \
                   thepixelflat//".fits", verify-, show-, update+)
            }
        #
        if (do_calib) {
            print ("Running calstis on input ", opt_elem, " spectrum ...")
            i = strlen (opt_elem)
            if (substr (opt_elem, i-4, i) == "G750M"){
            #
            # Perform wavelength calibration and geometric correction 
            # for G750M flat
            #
            hedit (theinspec//".fits[0]", "WAVECORR,HELCORR,X2DCORR", \
                   "PERFORM", verify-, show-, update+) 
            hedit (theinspec//".fits[0]", \
                   "X1DCORR", "OMIT", verify-, show-, update+) 
            calstis (theinspec//".fits", \
                     outroot="", savetmp-, verbose-, > "spec.calstis.log") 
            #  wavecal=thewavecal//".fits", \
            print ("Calibrated spectrum: file ", theinbase, "_sx2.fits")
            }
        else {
            hedit (theinspec//".fits[0]", \
                   "WAVECORR,HELCORR,X1DCORR,X2DCORR", "OMIT", verify-, \
                   show-, update+) 
            calstis (theinspec//".fits", savetmp-, verbose-, \
                     outroot="", > "spec.calstis.log") 
            print ("Cosmic-ray-corrected spectrum: file ", \
                   theinbase, "_crj.fits")
            }
        #
        if (access (theinbase//"_flt.fits")){
            del (theinbase//"_flt.fits", verify-)}
        }
    }
    #
    #*********************************************************************
    # If do_defringe = "yes", divide pipeline-calibrated spectrum by the 
    # fringe flat and prepare format for output spectrum
    #
    if (!do_defringe) {
        print (" ")
        bye
        }
    #
    if (access (theoutspec//".fits")){
        del (theoutspec//".fits", verify-)}
    if (access (theoutspec//"_tmp.fits")){
        del (theoutspec//"_tmp.fits", verify-)}
    #
    print ("Dividing by fringe flat ...")
    #
    if (access ("catfitsresult")){
        del ("catfitsresult", verify-)}
    catfits (theinflat//".fits", >> "catfitsresult")
    count ("catfitsresult") | scan (nlines)
    if (nlines <= 3) {
	fnextend = 1}
    else {
	fnextend = 3}
    if (access ("catfitsresult")){
        del ("catfitsresult", verify-)}
    #
    if (do_cal) {
        inextend = 3
        i = strlen (opt_elem)
        if (substr (opt_elem, i-4, i) == "G750M"){
	    if (fnextend == 3) {
                msarith (theinbase//"_sx2.fits", "/", theinflat//".fits", \
                         theoutspec//"_tmp.fits",  verbose=0) 
                }
            else {
                imarith (theinbase//"_sx2.fits[sci,1]", "/", \
                         theinflat//".fits", theoutspec//"_tmp.fits",verbose-) 
                copy (theinbase//"_sx2.fits", theoutspec//".fits",  verbose-) 
                }
            }
        else {
            if (fnextend == 3) {
                msarith (theinbase//"_crj.fits", "/", theinflat//".fits", \
                         theoutspec//"_tmp.fits",  verbose=0) 
                }
            else {
                imarith (theinbase//"_crj.fits[sci,1]", "/", \
                         theinflat//".fits", theoutspec//"_tmp.fits", verbose-)
                copy (theinbase//"_crj.fits", theoutspec//".fits",  verbose-) 
                }
            }
        }
    else {
        #
        # I.E., IF INPUT SPECTRUM WAS ALREADY RUN THROUGH CALSTIS
        #
        catfits (theinspec//".fits", >> "catfitsresult")
        count ("catfitsresult") | scan (nlines)
        if (nlines <= 3) {
            inextend = 1}
        else {
            inextend = 3}
        del ("catfitsresult", verify-)
        #
        print ("---------------------------------------------------", \
               "----------------------")
        print ("IT IS ASSUMED THAT PIXEL-TO-PIXEL FLATFIELDING OF ", c_inspec) 
        print ("HAS ALREADY BEEN PERFORMED")
        if (inextend >= 3) {
            imgets (theinspec//".fits[0]","opt_elem", > "dev$null")
            opt_elem = imgets.value
            #
            # IF INPUT SPECTRUM MULTI-IMSET
            #
            i = strlen (opt_elem)
            if (substr (opt_elem, i-4, i) == "G750M"){
                print ("(AND S-DISTORTION CORRECTION AS WELL) ")
                print ("---------------------------------------------------", \
                       "----------------------")
                if (fnextend == 3) {
                    msarith (theinspec//".fits", "/", theinflat//".fits", \
                             theoutspec//"_tmp.fits",  verbose=0) 
                    }
                else {
                    imarith (theinspec//".fits[sci,1]", "/", \
                             theinflat//".fits", \
                             theoutspec//"_tmp.fits",  verbose-)
                    copy (theinspec//".fits", theoutspec//".fits",  verbose-) 
                    }
                }
            else {
                print ("---------------------------------------------------", \
                       "----------------------")
                if (fnextend == 3) {
                    msarith (theinspec//".fits", "/", theinflat//".fits", \
                             theoutspec//"_tmp.fits",  verbose=0) 
                    }
                else {
                    imarith (theinspec//".fits[sci,1]", "/", \
                             theinflat//".fits", \
                             theoutspec//"_tmp.fits",  verbose-)
                    copy (theinspec//".fits", theoutspec//".fits",  verbose-) 
                    }
                }
            }
        else {
            #
            # IF INPUT SPECTRUM SINGLE-IMSET
            #
            imgets (theinspec//".fits","opt_elem", > "dev$null")
            opt_elem = imgets.value
            i = strlen (opt_elem)
            if (substr (opt_elem, i-4, i) == "G750M"){
                print ("(AND S-DISTORTION CORRECTION AS WELL) ")
                print ("---------------------------------------------------", \
                       "----------------------")
                if (fnextend == 3) {
                    imarith (theinspec//".fits", "/", \
                             theinflat//".fits[sci]", \
                             theoutspec//"_tmp.fits",  verbose-) 
                    }
                else {
                    imarith (theinspec//".fits", "/", theinflat//".fits", \
                             theoutspec//"_tmp.fits",  verbose-)
                    }
                }
            else {
                print ("---------------------------------------------------", \
                       "----------------------")
                if (fnextend == 3) {
                    imarith (theinspec//".fits", "/", \
                             theinflat//".fits[sci]", \
                             theoutspec//"_tmp.fits",  verbose-) 
                    }
                else {
                    imarith (theinspec//".fits", "/", theinflat//".fits", \
                             theoutspec//"_tmp.fits",  verbose-)
                    }
                }
            print (" ")
            copy (theinspec//".fits", theoutspec//".fits",  verbose-) 
            }
        }
    #
    #**********************************************************************
    # Put de-fringed spectra back into science extension of (copied) input 
    # spectrum. 
    #
    if (inextend >= 3 && fnextend == 3) {
        if (access (theoutspec//".fits")){
            del (theoutspec//".fits", verify-)}
        rename (theoutspec//"_tmp.fits", theoutspec//".fits", field="all")
        }
    else if (inextend >= 3 && fnextend != 3) {
        imcopy (theoutspec//"_tmp.fits", theoutspec//".fits[sci,1][*,*]", \
                verbose-)
        del (theoutspec//"_tmp.fits", ver-)
        }
    else {
        imcopy (theoutspec//"_tmp.fits", theoutspec//".fits[*,*]", verbose-)
        del (theoutspec//"_tmp.fits", ver-)
        }

    print (" ")
    #
    # DONE!
    # 
end
