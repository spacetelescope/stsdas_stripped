procedure mkfringeflat (inspec, inflat, outflat)

file 	inspec 	 {"", prompt="Name of input science spectrum"}
file 	inflat 	 {"", prompt="Name of input normalized fringe flat"}
file 	outflat	 {"", prompt="Name of output [final] fringe flat"}
bool	do_shift   {yes, \
	prompt="Shift flat to match fringes in spectrum?"}
real	beg_shift  {-0.5, prompt="Starting shift for shift determination"}
real	end_shift  {0.5, prompt="End shift for shift determination"}
real	shift_step {0.1, prompt="Step through range of shifts"}
bool	do_scale   {yes, \
	prompt="Scale fringe amplitude to match science spectrum?"}
real	beg_scale  {0.8, prompt="Lower limit of range of scaling factors"}
real	end_scale  {1.2, prompt="Upper limit of range of scaling factors"}
real 	scale_step {0.04, prompt="Step through range of scaling factors"}
int	extrloc    {INDEF, \
	prompt="Central line (row) to be extracted from 2-D spectrum"}
int	extrsize   {INDEF, \
	prompt="Number of lines to be extracted from 2-D spectrum"}
string	opti_spreg {"INDEF",\
	prompt="Spectral range (first:last) to normalize spectrum"}
string	rmsregion  {"INDEF",\
	prompt="Spectral range (first:last) to measure RMS's"}
struct *l1	   {prompt="Internal use only"}
string  Version    {"2011 Apr 20", prompt="Date of Installation"}


begin

    int i, min_row, nrows, f_row, l_row, f_shift, l_shift, step, nshift
    int expo, numcols, numlines, fcol, lcol, ncol, fline, lline, maxrow
    int ffrow, flrow, numextrows, bincols, binlines, nlines, fnextend
    int  ilow, iupp, istep, lastrowm1, lastrowm2, sax0, sax1, say0, say1
    int  fltbincols, fltbinlines, mnrow, mxrow
    int flt_ltv2	# ltv2 for inflat
    real scalefac, xsh, mean, stddev, median, w_shift_av, w_scale_av
    real weight_av, theshift, min_shift, thescale, min_scale
    real low_lim, coef1, coef2, ltv1, ltv2, centera2, sizaxis2
    string str1, str2, str3, str4, op, op1file, opfile, op2file, s1, s2, s3
    string theinbase, theinspec, theinflat, theoutflat, suba
    string c_inspec, c_inflat, c_outflat, shifted
    string flt, str, totoptreg, slitreg, aperture, opt_elem, apername
    bool noexten
    #
    # THE NEXT DECLARATIONS ARE FOR THE ITERATIVE STATISTICS STEP
    #
    real mn, sig, med, mod, min, max, ll, ul, fivesig, m_fivesig, p_fivesig
    real basemn, basemed, basesig, thesig
    int nx, npx, m, numiter, not_ready

    print ("MKFRINGEFLAT Version ",(Version), \ 
	   " - Matching fringes in flat to those in spectrum")

    set imtype=fits
    delete.go_ahead=yes
    delete.subfiles=yes
    not_ready = 0

#*********************************************************************
# Check if necessary IRAF/STSDAS packages are loaded

    if (! defpac ("stsdas")) {
	print("Please load 'stsdas', 'analysis' and 'fitting' packages ", \
	      "before running this task")
	not_ready = 1
    }
    else if (! defpac ("analysis")) {
	print("Please load 'analysis' and 'fitting' packages before ", \ 
	      "running this task")
	not_ready = 1
    }
    else if (! defpac ("fitting")) {
	print("Please load 'fitting' package before running this task")
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
    if (! defpac ("twodspec")) {
	print("Please load 'twodspec' and 'longslit' packages ", \ 
	"before running this task")
	not_ready = 1
    }
    else if (! defpac ("longslit")) {
	print("Please load 'longslit' package before running this task")
	not_ready = 1
    }
   
    if (not_ready == 1){
	bye
    }
    #*********************************************************************
    # Avoid multiple queries of input parameters....
    #
    c_inspec  = inspec
    c_inflat  = inflat
    c_outflat = outflat
    #*********************************************************************
    # Strip ".fits" file extensions to prevent confusion
    #
    i = strlen (c_inspec)
    if (substr (c_inspec, i-4, i) == ".fits") {
        theinspec = substr (c_inspec, 1, i-5)}
    else {
        theinspec = c_inspec}

    i = strlen (c_inflat)
    if (substr (c_inflat, i-4, i) == ".fits") 
	theinflat = substr (c_inflat, 1, i-5)
    else 
 	theinflat = c_inflat

    i = strlen (c_outflat)
    if (substr (c_outflat, i-4, i) == ".fits") 
	theoutflat = substr (c_outflat, 1, i-5)
    else 
 	theoutflat = c_outflat

    i = strlen (theinspec)
    if (substr (theinspec, i-3, i) == "_crj") 
	theinbase = substr (theinspec, 1, i-4)
    else if (substr (theinspec, i-3, i) == "_sx2") 
	theinbase = substr (theinspec, 1, i-4)
    else 
 	theinbase = theinspec

    if (access (theoutflat//".fits")){
	del (theoutflat//".fits", verify-)
    }
    if (!do_shift && !do_scale) { 
	print (" ")
	print ("Why on Earth issue this task ", \
	       "with switches `do_shift=no' and `do_scale=no'?")
	print ("Sorry, I refuse to work with that input...")
	return
    }
    #-----------------------------------------------------------------------
    # First get all necessary info from headers
    # 
    if (access ("shiftdata.dat")){
	del ("shiftdata.dat", verify-)
    }
    if (access ("op1file")){
 	del ("op1file", verify-)
    }
    if (access ("op2file")){
 	del ("op2file", verify-)
    }
    if (access ("strfile1")){
 	del ("strfile1", verify-)
    }
    #
    # Checking how many extensions are in the flat field fits file
    #
    if (access ("catfitsresult")){
        del ("catfitsresult", verify-)}
    catfits (theinflat//".fits", >> "catfitsresult")
    count ("catfitsresult") | scan (nlines)
    if (nlines <= 3) {
        fnextend = 1
    }
    else {
        fnextend = 3
    }
    if (access ("catfitsresult")){
        del ("catfitsresult", verify-)}
    #
    imgets (theinflat//".fits[0]", "SHIFTED", > "dev$null")
    shifted = imgets.value
    if (substr (shifted, 1, 2) == "YE"){
        print (" ")
        print (" NOTE: Input flat was already shifted")
        print ((theinflat)//".fits", >> "op1file")
    }
    else {
        if (fnextend == 3){
            print ((theinflat)//".fits[sci,1]", >> "op1file")
	}
        else if (fnextend == 1){
            print ((theinflat)//".fits", >> "op1file")
	}
        else {
            print ("Sorry, your input flat field doesn't seem to have ", \
                   "either three or one image extensions. Bye bye....")
            return
	}
    }
    print (" ")
    list = "op1file"
    print ((theinspec)//".fits", >> "op2file")
    l1 = "op2file"
    while (fscan (list,flt) != EOF && fscan (l1,str) != EOF) { }
    del ("op*file", verify-)
    imgets (str//"[sci]", "naxis1")
    numcols  = int(imgets.value)
    imgets (str//"[sci]", "naxis2")
    numlines = int(imgets.value)
    imgets (str//"[sci]", "binaxis1")
    bincols  = int(imgets.value)
    imgets (str//"[sci]", "binaxis2")
    binlines = int(imgets.value)
    imgets (str//"[sci]", "OPT_ELEM")
    opt_elem = imgets.value
    imgets (str//"[sci]", "APERTURE")
    aperture = imgets.value

    imgets (flt, "binaxis1")
    fltbincols  = int(imgets.value)
    imgets (flt, "binaxis2")
    fltbinlines = int(imgets.value)

    if (opt_elem == "G750M") {
        imgets (str//"[sci]", "LTV1")
        ltv1     = real(imgets.value)
	# get LTV2 for the input spectrum
        imgets (str//"[sci]", "CENTERA2")
        centera2 = real(imgets.value)
        imgets (str//"[sci]", "SIZAXIS2")
        sizaxis2 = real(imgets.value)
        ltv2 = 1 - centera2 + sizaxis2/2
	# get LTV2 for the input fringe flat
        imgets (flt, "CENTERA2")
        centera2 = real(imgets.value)
        imgets (flt, "SIZAXIS2")
        sizaxis2 = real(imgets.value)
	flt_ltv2 = 1 - centera2 + sizaxis2/2
    }
    else {
        imgets (str//"[sci]", "LTV1")
        ltv1     = real(imgets.value)
        imgets (str//"[sci]", "LTV2")
        ltv2     = real(imgets.value)
        imgets (flt, "LTV2")
	flt_ltv2 = real(imgets.value)
    }
    sax0 = nint(-ltv1) + 1
    sax1 = sax0 + numcols - 1
    say0 = nint(flt_ltv2 - ltv2) + 1
    say1 = say0 + numlines - 1
    suba = "[" // sax0 // ":" // sax1 // "," // say0 // ":" // say1 // "]"
    #
    #----------------------------------------------------------------------
    # Set number of rows to be extracted according to aperture name
    # if "extrsize" is "INDEF"
    #
    if (extrsize == INDEF) {
        i = strlen (aperture)
        if (substr (aperture, i-7, i) == "0.3X0.09"){
            numextrows = nint((9./binlines)+0.25)}
        else if (substr (aperture, i-7, i) == "0.2X0.06"){
            numextrows = nint((7./binlines)+0.25)}
        else if (substr (aperture, 1, 3) != "52X"){
            print ("ERROR: not able to understand APERTURE keyword (", \
                   aperture, ")") 
            return 
        }
        else {
            numextrows = 11/binlines}
        apername =  " [Aperture: "//aperture//"]"
    }
    else {
        numextrows = extrsize
        apername = ""
    }
    #
    #-----------------------------------------------------------------------
    # If default extraction, get row with max. counts in the target spectrum 
    # 
    if (extrloc == INDEF) {
        fcol = nint(0.2*numcols)
        lcol = nint(0.8*numcols)
        ncol = lcol-fcol+1
        fline = nint(0.4*numlines**2/1024.)+1
        lline = numlines-fline-1
        #
        if (access ("temp_slitprof.fits")){
            del ("temp_slitprof.fits", verify-)
   	}
        blkavg (str//"[sci]["//fcol//":"//lcol//",*]", \
                "temp_slitprof.fits", ncol, 1, option="average")
        #
        # Create ascii file with the pixels containing image values > 3 sigma
        # above the median (supposedly the target)
        #
        if (access ("temp_slitprof.dat")){
            del ("temp_slitprof.dat", verify-)}
        if (access ("temp_slitprof2.dat")){
            del ("temp_slitprof2.dat", verify-)}
        imstat ("temp_slitprof.fits[1,"//fline//":"//lline//"]", \
                fields="mean,midpt,stddev", lower=INDEF, upper=INDEF, \
                binwidth=0.1, form-) | scan(mn,med,sig)
        low_lim = med+3*sig
        pixlocate ("temp_slitprof.fits[1,"//fline//":"//lline//"]", \
                   lower=low_lim, upper=INDEF, maxvals=1024, border=0, \
                   outside-, >> "temp_slitprof.dat")
        #
        # Convert to STSDAS table 
        #
        if (access ("parabola.tab")){
            del ("parabola.tab", verify-)}
        if (access ("temp_slitprof.tab")){
            del ("temp_slitprof.tab", verify-)}
        if (access ("temp_slitprof.cd")){
            del ("temp_slitprof.cd", verify-)}
        print ("# format for slit profile table", >> "temp_slitprof.cd")
        print ("AXIS2    I      i5              Pixels", >> "temp_slitprof.cd")
        print ("DATA     R      f15.6           Counts", >> "temp_slitprof.cd")
        tcreate ("temp_slitprof.tab", "temp_slitprof.cd", \
                 "temp_slitprof.dat", uparfile="", nskip=0, nlines=0, \
                 nrows=0, extrapar=5, tbltype="default", extracol=0)
        tcalc ("temp_slitprof.tab", "data", \ 
               "if data < 0.0 then data*(-1.) else data", datatype="real")
        tsort ("temp_slitprof.tab", "data", ascend=no)
        tprint ("temp_slitprof.tab", showrow-, showhdr-, showunit-, \
                columns="axis2", rows="1", prparam-, prdata+, \
                option="plain") | scan(maxrow)
        #
        # Fit parabola to "maxrow" and 4 surrounding rows and output 
        # peak row
        #
        ffrow = maxrow-2+fline-1		# "+fline-1" added - 11/05/98
        flrow = maxrow+2+fline-1		# "+fline-1" added - 11/05/98
        pixlocate ("temp_slitprof.fits[1,"//ffrow//":"//flrow//"]", \
                   lower=INDEF, upper=INDEF, maxvals=1024, border=0, \
                   outside-, > "temp_slitprof2.dat")
        gfit1d ("temp_slitprof2.dat", "parabola", function="chebyshev", \
                order=3, xmin=INDEF, xmax=INDEF, ps+, interactive-, \
                > "dev$null")
        # Remember that gfit1d calls the zeroth order coefficient "coeff1"...
        tprint ("parabola.tab", showrow-, showhdr-, showunit-, \
                columns="coeff2,coeff3", rows="1", prparam-, prdata+, \
                option="plain") | scan(coef1,coef2)	# updated on 11/05/98
        maxrow = nint((coef1/coef2)*-0.5)
        maxrow = maxrow+ffrow-1		# "+fline-1" removed - 11/05/98
        del ("temp_slitprof*.dat,temp_slitprof.cd,temp_slitprof.tab", \
             verify-)
        del ("parabola.tab,temp_slitprof.fits", verify-)
        print ("         WARNING: USING DEFAULT EXTRACTION SETTING")
        print("(only appropriate for point sources in the center of the slit)")
        }
    #
    # Else take extraction center row from user input 
    #
    else if (numextrows/2+1 <= extrloc && extrloc <= numlines-numextrows/2) {
        maxrow = extrloc
        }
    else {
        mnrow = numextrows/2+1
        mxrow = numlines - numextrows / 2
        print("Sorry, the extrloc is not in the range: ", mnrow, ":", mxrow)
        return
    }
    print (" Extraction center: row ", maxrow)
    print ("   Extraction size: ", numextrows, "pixels", apername)
    #
    #----------------------------------------------------------------------
    # Determine actual row numbers to be extracted
    #
    if (access ("op1file")){
   	del ("op1file", verify-)
    }
    if (access ("opfile")){
   	del ("opfile", verify-)
    }
    fline = maxrow-nint((numextrows-0.49999)/2.)
    lline = maxrow+nint((numextrows-0.49999)/2.)
    print (str(fline), ":", str(lline), >> "op1file")
    l1 = "op1file"
    while (fscan(l1,slitreg) != EOF){ }
    #
    #----------------------------------------------------------------------
    # Determine spectral region to be normalized according to grating: 
    # put appropriate defaults for "INDEF" (default) setting
    #
    # Following line works with IRAF and PyRAF
    if (strlen(opti_spreg) == INDEF || substr ( opti_spreg, 1, 5) == "INDEF") {
	j = strlen (opt_elem)
	if (substr (opt_elem, j-4, j) == "G750M"){
	    opti_spreg = int(83/bincols) // ":" // int(1106/bincols)
	}
	else {
	    opti_spreg = int(5/bincols) // ":" // int(1020/bincols)
	}
    }
    if (strlen(rmsregion) == INDEF || substr(rmsregion, 1, 5) == "INDEF") {
	rmsregion = int(725/bincols) // ":" // int(900/bincols)
    }
    print (opti_spreg, ",", slitreg, >> "opfile")
    l1 = "opfile"
    while (fscan(l1,totoptreg) != EOF){
	print ("Range to be normalized: [", totoptreg, "]")
    }
    del ("op*file", verify-)
    #
    #***********************************************************************
    # Make fringe flat-corrected star spectra with shifted fringe flats, 
    # get de-fringing statistics and print out RMSs. 
    #
    if (do_shift) {
        print (" ")
        print (" DETERMINING BEST SHIFT FOR FRINGE FLAT")
        print (" ")
        expo = nint(log10(shift_step)-0.49999999)
        # print ("Exponent for step: ", expo)
        f_shift = nint(beg_shift/(10.0**(expo)))
        l_shift = nint(end_shift/(10.0**(expo)))
        step = nint(shift_step/(10.0**(expo)))
        # print ("Integer start in loop: ", f_shift)
        # print ("Integer   end in loop: ", l_shift)
        # print ("Integer  step in loop: ", step)
        #
        if (access ("shiftdata.dat")){
  	    del ("shiftdata.dat", verify-)
  	}

        for (i=f_shift; i <= l_shift; i += step){

            nshift = (i-f_shift)/step+1
            xsh = beg_shift+(nshift-1)*shift_step

            if (access ("star_cont_sh"//nshift//".fits")){
                del ("star_cont_sh"//nshift//".fits", verify-)}
            if (access ("norm_star_cont_sh"//nshift//".fits")){
                del ("norm_star_cont_sh"//nshift//".fits", verify-)}
            if (access ("norm_star_cont_sh"//nshift//"_1d.fits")){
                del ("norm_star_cont_sh"//nshift//"_1d.fits", verify-)}
            if (access (theinflat//"_sh"//nshift//".fits")){
                del (theinflat//"_sh"//nshift//".fits", verify-)}
            if (access (theinflat//"_bn"//".fits")){
                del (theinflat//"_bn"//".fits", verify-)}

            blkavg(flt, theinflat//"_bn"//".fits", \
                   bincols/fltbincols, binlines/fltbinlines)
            imshift (theinflat//"_bn"//".fits"//suba, \
                     theinflat//"_sh"//nshift//".fits", \
                     xshift=xsh, yshift=0.0, interp_type="linear", \
                     boundary_typ="nearest", constant=0.)
            del (theinflat//"_bn"//".fits", verify-)

            imarith (str//"[sci,1]", "/", theinflat//"_sh"//nshift//".fits", \
                     "star_cont_sh"//nshift//".fits", verbose-, noact-)
            del (theinflat//"_sh"//nshift//".fits")
            j = strlen (opt_elem)
            if (substr (opt_elem, j-4, j) == "G750M"){
                response ("star_cont_sh"//nshift//".fits["//totoptreg//"]", \
                          "star_cont_sh"//nshift//".fits["//totoptreg//"]", \
                          "norm_star_cont_sh"//nshift//".fits", \
                          interactive=no, threshold=1.0E-20, naverage=2, \
                          function="spline3", order=1, low_reject=3.0, \
                          high_reject=3., niterate=2, sample="*", grow=0.)
            }
            else if (substr (opt_elem, j-4, j) == "G750L"){
                response ("star_cont_sh"//nshift//".fits["//totoptreg//"]", \
                          "star_cont_sh"//nshift//".fits["//totoptreg//"]", \
                          "norm_star_cont_sh"//nshift//".fits", \
                          interactive=no, threshold=1.0E-20, naverage=2, \
                          function="spline3", order=15, low_reject=3.0, \
                          high_reject=3., niterate=2, sample="*", grow=0.)
            }
            else {
                print ("Sorry, this task is only meant for G750L or ", \
                       "G750M spectra")
            }
            del ("star_cont_sh"//nshift//".fits")
            blkavg ("norm_star_cont_sh"//nshift//".fits[*,"//slitreg//"]", \
                    "norm_star_cont_sh"//nshift//"_1d.fits", 1, numextrows, \
                    option="average")
            del ("norm_star_cont_sh"//nshift//".fits")
            imstat ("norm_star_cont_sh"//nshift//"_1d.fits["//rmsregion//"]", \
                    fields="mean,stddev", lower=INDEF, upper=INDEF, \
                    binwidth=0.1, form-) | scan(mn,sig)
            thesig = sig/mn
            del ("norm_star_cont_sh"//nshift//"_1d.fits", verify-)
            print ("Shift ", xsh, ": RMS = ", thesig)
            print ("  ", xsh, "  ", thesig, >> "shiftdata.dat")
        }
        #
        #********************************************************************
        # Create format for STSDAS table that will contain the shift data
        #
        if (access ("shiftdata.tab")){
            del ("shiftdata.tab", verify-)
        }
        if (access ("shiftdata.cd")){
            del ("shiftdata.cd", verify-)
        }
        print ("# Format for shift table", >> "shiftdata.cd")
        print ("SHIFT	R	f7.4	Pixels", >> "shiftdata.cd")
        print ("RMS		R	f10.6	Counts", >> "shiftdata.cd")
        #
        #********************************************************************
        # Create STSDAS table containing the shift data and determine shift
        # that delivers the best RMS by an inverse-variance-weighted average. 
        #
        tcreate ("shiftdata.tab", "shiftdata.cd", "shiftdata.dat", \
                 uparfile="", nskip=0, nlines=0, nrows=0, extrapar=5, \
                 tbltype="default", extracol=0)
        tcalc ("shiftdata.tab", "WEIGHT", "1.0/sqr(RMS)", datatype="real")
        tcalc ("shiftdata.tab", "ORIG_ROW", "rownum", datatype="real")
        tcalc ("shiftdata.tab", "W_ROWNUM", "ORIG_ROW*WEIGHT", datatype="real")
        tsort ("shiftdata.tab", "RMS", ascend=yes)
        tstat ("shiftdata.tab", "ORIG_ROW", outtable="", rows="1-1", \
               lowlim=INDEF, highlim=INDEF) 
        min_row = nint(tstat.mean)
        tstat ("shiftdata.tab", "ORIG_ROW", outtable="", \
               lowlim=INDEF, highlim=INDEF) 
        nrows = tstat.nrows
        tstat ("shiftdata.tab", "SHIFT", outtable="", rows="1-1", \
               lowlim=INDEF, highlim=INDEF) 
        min_shift = tstat.mean
        #
        tsort ("shiftdata.tab", "ORIG_ROW", ascend=yes)
        #
        #
        # If >= 2 measurements on either side of lowest measured shift, 
        # then determine weighted average of the 5 best shifts. 
        #
        if (min_row >= 3 && min_row <= nrows-2) {
            f_row = min_row-2
            l_row = min_row+2
            tstat ("shiftdata.tab", "W_ROWNUM", outtable="", \ 
                   rows=f_row//"-"//l_row, \
                   lowlim=INDEF, highlim=INDEF)
            w_shift_av = tstat.mean
            tstat ("shiftdata.tab", "WEIGHT", outtable="", \
                   rows=f_row//"-"//l_row, \
                   lowlim=INDEF, highlim=INDEF)
            weight_av = tstat.mean
            theshift = w_shift_av/weight_av
            theshift = (theshift-1.0)*shift_step+beg_shift
        }
        #
        # Else if only 1 measurement on one side of lowest measured shift, 
        # then determine weighted average of the 3 best shifts
        #
        else if (min_row >= 2 && min_row <= nrows-1) {
            f_row = min_row-1
            l_row = min_row+1
            tstat ("shiftdata.tab", "W_ROWNUM", outtable="", \ 
                   rows=f_row//"-"//l_row, lowlim=INDEF, highlim=INDEF)
            w_shift_av = tstat.mean
            tstat ("shiftdata.tab", "WEIGHT", outtable="", \
  		rows=f_row//"-"//l_row, lowlim=INDEF, highlim=INDEF)
            weight_av = tstat.mean
            theshift = w_shift_av/weight_av
            theshift = (theshift-1.0)*shift_step+beg_shift
        }
        #
        # Else just take the lowest measured shift and print out warning
        #
        else{
            print (" ")
            print ("WARNING: Best shift found on the edge of the specified", \
                   " shift range.")
            print ("You are advised to try again after adjusting the shift", \
                   " range accordingly.")
            theshift = min_shift
        }
        #
        print (" ")
        print (" Best shift : ", theshift, " pixels")
        del ("shiftdata.cd,shiftdata.tab,shiftdata.dat", verify-)
        # 
        #********************************************************************
        # Finally, apply the best shift and create output image
        # "theinflat_sh.fits"
        #
        if (access (theinflat//"_sh.fits")){
            del (theinflat//"_sh.fits", verify-)}
        
        blkavg(flt, theinflat//"_bn"//".fits", \
               bincols/fltbincols, binlines/fltbinlines)
        imshift (theinflat//"_bn"//".fits"//suba, theinflat//"_sh.fits", \
                 xshift=theshift, yshift=0.0, interp_type="linear", \
                 boundary_typ="nearest", constant=0.)
        del (theinflat//"_bn"//".fits", verify-)
        hedit (theinflat//"_sh.fits", "SHIFTED", "YES", add+, ver-, del-, \
               show-, update+)
        print (" Shifted flat : ", theinflat,"_sh.fits")
        print ("                ", \
               "(Can be used as input flat for an ev. next iteration)")
    }
    #
    #************************************************************************
    # Make fringe flat-corrected star spectra with fringe flats of which the 
    # fringe amplitude is scaled differently, get de-fringing statistics and 
    # print out RMSs.  
    #
    if (do_scale) {
        print (" ")
        print (" DETERMINING BEST SCALING OF AMPLITUDE OF FRINGES IN FLAT")
        print (" ")
        ilow = nint(beg_scale*100.)
        iupp = nint(end_scale*100.)
        istep = nint(scale_step*100.)
        j = strlen (opti_spreg)
        if (substr (opti_spreg, j-4, j-4) == ":"){
     	    fcol = int(substr (opti_spreg, 1, j-5))
            lcol = int(substr (opti_spreg, j-3, j))
        }
        else if (substr (opti_spreg, j-3, j-3) == ":"){
            fcol = int(substr (opti_spreg, 1, j-4))
            lcol = int(substr (opti_spreg, j-2, j))
        }
        else if (substr (opti_spreg, j-2, j-2) == ":"){
            fcol = int(substr (opti_spreg, 1, j-3))
            lcol = int(substr (opti_spreg, j-1, j))
        }
        else {
            print ("Cannot find `:' in parameter `opti_spreg' (", \
                   opti_spreg, ")")
            return
        }
        #
        if (access ("op1file")){
            del ("op1file", verify-)
            }
        if (access ("op2file")){
            del ("op2file", verify-)
            }
        if (access ("scaledata.dat")){
            del ("scaledata.dat", verify-)
            }
        #
        if (do_shift) {
            hedit (theinflat//"_sh.fits", "NEXTEND", "", delete+, add-, \
                   verify-, show-, update+, > "dev$null")
            print ((theinflat)//"_sh.fits", >> "op1file")
            } 
        else if (substr (shifted, 1, 2) == "YE"){
            hedit (theinflat//".fits", "NEXTEND", "", delete+, add-, \
                   verify-, show-, update+, > "dev$null")
            print ((theinflat)//".fits", >> "op1file")
            }
        else {
            print ((theinflat)//".fits[sci,1]", >> "op1file")
            } 
        list = "op1file"
        print ((theinspec)//".fits[sci,1]", >> "op2file")
        l1 = "op2file"
        while (fscan (list,flt) != EOF && fscan (l1,str) != EOF){ }
        imstat (flt//"["//totoptreg//"]", fields="mean", lower=INDEF, \
                upper=INDEF, binwidth=0.1, form- ) | scan(mn)
  	
        for (i = ilow; i <= iupp; i += istep) {
            if (access ("flat_sc"//i//".fits")){
                del ("flat_sc"//i//".fits", verify-)}
            if (access ("star_cont_sc"//i//".fits")){
   	  	del ("star_cont_sc"//i//".fits", verify-)}
            if (access ("norm_star_cont_sc"//i//".fits")){
   	  	del ("norm_star_cont_sc"//i//".fits", verify-)}
            if (access ("norm_star_cont_sc"//i//"_1d.fits")){
   	  	del ("norm_star_cont_sc"//i//"_1d.fits", verify-)}
  	  
            scalefac = i/100.
            imcalc.verbose=no ### default is NO.
            #  hedit (flt, "NEXTEND", 1, verify-, show-, update+) # FOR IMCALC
            imcalc (flt, "flat_sc"//i//".fits", \
                    "if x > "//fcol//" && x < "//lcol//" then ((im1-"//mn//")*"//scalefac//"+"//mn//") else im1")
            imarith (str, "/", "flat_sc"//i//".fits", \
                     "star_cont_sc"//i//".fits", verbose-, noact-)
            del ("flat_sc"//i//".fits", verify-)
            #
            j = strlen (opt_elem)
            if (substr (opt_elem, j-4, j) == "G750M"){
                response ("star_cont_sc"//i//".fits["//totoptreg//"]", \   
                          "star_cont_sc"//i//".fits["//totoptreg//"]", \
                          "norm_star_cont_sc"//i//".fits", \
                          interactive=no,threshold=1.0E-20,naverage=2, \
                          function="spline3", order=1, low_reject=3.0, \
                          high_reject=3., niterate=2, sample="*", grow=0.)
                }
            else if (substr (opt_elem, j-4, j) == "G750L"){
                response ("star_cont_sc"//i//".fits["//totoptreg//"]", \   
                          "star_cont_sc"//i//".fits["//totoptreg//"]", \
                          "norm_star_cont_sc"//i//".fits", \
                          interactive=no,threshold=1.0E-20,naverage=2, \
                          function="spline3", order=15, low_reject=3.0, \
                          high_reject=3., niterate=2, sample="*", grow=0.)
                }
            else {
                print ("Sorry, this task is only meant for G750L or ", \
                       "G750M spectra")
                }
            del ("star_cont_sc"//i//".fits", verify-)
            blkavg ("norm_star_cont_sc"//i//".fits[*,"//slitreg//"]", \
                    "norm_star_cont_sc"//i//"_1d.fits", 1, numextrows, \
                    option="average")
            del ("norm_star_cont_sc"//i//".fits", verify-)
            imstat ("norm_star_cont_sc"//i//"_1d.fits["//rmsregion//"]", \
                    fields="mean,stddev", lower=INDEF, upper=INDEF, \
                    binwidth=0.1, form-) | scan(mean,sig) 
            thesig = sig/mean
            print ("Fringes scaled ", scalefac, ": RMS = ", thesig, \
                   "; mean = ", mean)
            print ("  ", scalefac, "  ", thesig, >> "scaledata.dat")
            del ("norm_star_cont_sc"//i//"_1d.fits", verify-)
        }
        del ("op*file", verify-)
        #
        #********************************************************************
        # Create format for STSDAS table that will contain the scale data
        #
        if (access ("scaledata.tab")){
            del ("scaledata.tab", verify-)
            }
        if (access ("scaledata.cd")){
            del ("scaledata.cd", verify-)
            }
        print ("# Format for scale table", >> "scaledata.cd")
        print ("SCALE	R	f7.4	Factor", >> "scaledata.cd")
        print ("RMS		R	f10.6	Counts", >> "scaledata.cd")
        #
        #********************************************************************
        # Create STSDAS table containing the scale data and determine scale
        # that delivers the best RMS by an inverse-variance-weighted average. 
        #
        tcreate ("scaledata.tab", "scaledata.cd", "scaledata.dat", \
                 uparfile="", nskip=0, nlines=0, nrows=0, extrapar=5, \
                 tbltype="default", extracol=0)
        tcalc ("scaledata.tab", "WEIGHT", "1.0/sqr(RMS)", datatype="real")
        tcalc ("scaledata.tab", "ORIG_ROW", "rownum", datatype="real")
        tcalc ("scaledata.tab", "W_ROWNUM", "ORIG_ROW*WEIGHT", datatype="real")
        tsort ("scaledata.tab", "RMS", ascend=yes)
        tstat ("scaledata.tab", "ORIG_ROW", outtable="", rows="1-1", \
               lowlim=INDEF, highlim=INDEF) 
        min_row = nint(tstat.mean)
        tstat ("scaledata.tab", "ORIG_ROW", outtable="", lowlim=INDEF, \
               highlim=INDEF) 
        nrows = nint(tstat.nrows)
        lastrowm2 = nrows-2
        lastrowm1 = nrows-1
        tstat ("scaledata.tab", "SCALE", outtable="", rows="1-1", \
               lowlim=INDEF, highlim=INDEF) 
        min_scale = tstat.mean

        tsort ("scaledata.tab", "ORIG_ROW", ascend=yes)
        #
        # If >= 2 measurements on either side of scale with lowest RMS,
        # then determine weighted average of the 5 best scales. 
        #
        if (min_row >= 3 && min_row <= lastrowm2) {
            f_row = min_row-2
            l_row = min_row+2
            tstat ("scaledata.tab", "W_ROWNUM", outtable="", \ 
                   rows=f_row//"-"//l_row, lowlim=INDEF, highlim=INDEF)
            w_scale_av = tstat.mean
            tstat ("scaledata.tab", "WEIGHT", outtable="", \
                   rows=f_row//"-"//l_row, lowlim=INDEF, highlim=INDEF)
            weight_av = tstat.mean
            thescale = w_scale_av/weight_av
            thescale = (thescale-1.0)*scale_step+beg_scale
            }
        #
        # Else if only 1 measurement on one side of scale with lowest RMS, 
        # then determine weighted average of the 3 best scales
        #
        else if (min_row >= 2 && min_row <= lastrowm1) {
            f_row = min_row-1
            l_row = min_row+1
            tstat ("scaledata.tab", "W_ROWNUM", outtable="", \ 
                   rows=f_row//"-"//l_row, lowlim=INDEF, highlim=INDEF)
            w_scale_av = tstat.mean
            tstat ("scaledata.tab", "WEIGHT", outtable="", \
                   rows=f_row//"-"//l_row, lowlim=INDEF, highlim=INDEF)
            weight_av = tstat.mean
            thescale = w_scale_av/weight_av
            thescale = (thescale-1.0)*scale_step+beg_scale
            }
        #
        # Else just take the scale with lowest RMS and print out warning
        #
        else{
            print (" ")
            print ("WARNING: Best scale found on the edge of the specified", \
                   " scale range.")
            print ("You are advised to try again after adjusting the scale", \
                   " range accordingly.")
            thescale = min_scale
            }
        print (" ")
        print (" Best scale : ", thescale)
        del ("scaledata.cd,scaledata.tab,scaledata.dat", verify-)
        #
        #********************************************************************
        # Finally, apply the best scaling factor and create output image 
        # "theoutflat.fits"
        #
        imcalc (flt, theoutflat//".fits", \
                "if x > "//fcol//" && x < "//lcol//" then ((im1-"//mn//")*"//thescale//"+"//mn//") else im1")
        imcalc.verbose=yes
        }
    if (!do_scale && do_shift) {
        rename (theinflat//"_sh.fits", theoutflat//".fits")
    }
    print (" ")
    print ("Output flat : ", theoutflat, ".fits ", \
           " (to be used as input to task `DEFRINGE')")
    opti_spreg = "INDEF"
    rmsregion  = "INDEF"
    #
    # DONE!
    #
end
