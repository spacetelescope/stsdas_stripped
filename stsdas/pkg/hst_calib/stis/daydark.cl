procedure daydark (infiles,basedark)

file 	infiles	 {"",prompt="Input dark image(s) to be combined/normalized"}
file	basedark {"",prompt="Name of baseline superdark file"}
file	biasfile {"",prompt="Name of reference bias file to be used"}
file 	outfile  {"day_dark.fits", \ 
		  prompt="Name of output normalized daily superdark"}
string  houstemp {INDEF,prompt="Housing temperature(s) for these exposures"}
int     maxiter  {40,min=0,prompt="Maximum number of iterations for imstat"}
real    lower    {INDEF,prompt="Initial lower limit for imstat"}
real    upper    {INDEF,prompt="Initial upper limit for imstat"}
bool    verbose  {no,prompt="Show results of imstat iterations?"}
bool    print    {no,prompt="Print final results of imstat iterations?"}
struct *imglist	 {prompt="Internal use only, do not edit"}
string  Version  {"20Sep10",prompt="Date of Installation"}


begin

   int i, j, ncombine, nrptexp, nexp, nimset, nextend, nfiles, ntemps
   int extcount
   real gain, x, y, hotthres, mjd, cal_version, m_fivesig
   real juliandate, temptemp
   real ref_temp, drk_vs_t      # reference temperature, slope of dark vs temp.
   string expr, inexpr
   string op1file, opfile, s1, s2, s3, cal_ver, thecal_ver
   string c_infiles, c_outfile, c_basedark, c_houstemp, intemp, temp
   string theinfiles, theinbase, theoutfile, thebiasfile, thebasedark
   string img, theimg, joinfiles, joinedfile, crcorr, basedrk, inlist
   bool do_cal, do_scale, got_version
   #
   # THE NEXT FOUR DECLARATIONS ARE FOR THE ITERATIVE STATISTICS STEP
   #
   real mn, sig, med, mod, min, max, ll, ul, twosig, trisig, fivesig 
   real p_onesig, p_twosig, p_trisig, p_fivesig, basemn, basemed, basesig
   int nx, npx, m, numiter
#
   print ("DAYDARK Version ",(Version), \ 
		" - Creation of Daily STIS/CCD Superdark")
#
   set imtype=fits
   delete.go_ahead=yes
#*********************************************************************
# Check if necessary IRAF/STSDAS packages are loaded
#
   if (! defpac ("stsdas")) {
	print("Please load 'stsdas' package before running this task")
	bye
   }
   if (! defpac ("imgtools")) {
	print("Please load 'imgtools' package before running this task")
	bye
   }
   if (! defpac ("ttools")) {
	print("Please load 'ttools' package before running this task")
	bye
   }
   if (! defpac ("mstools")) {
	print("Please load 'mstools' package before running this task")
	bye
   }
#*********************************************************************
# Avoid multiple queries of input parameters
#
  c_infiles  = infiles
  c_outfile  = outfile
  c_basedark = basedark
  c_houstemp = houstemp
#
#*********************************************************************
# Strip ".fits" file extensions to prevent confusion
#
   i = strlen (c_infiles)
   if (substr (c_infiles, i-4, i) == ".fits") 
	theinfiles = substr (c_infiles, 1, i-5)
   else 
 	theinfiles = c_infiles
#
#   i = strlen (theinfiles)
#   if (substr (theinfiles, i-3, i) == "_raw") 
#	theinbase = substr (theinfiles, 1, i-4)
#   else 
# 	theinbase = theinfiles
#
   i = strlen (c_outfile)
   if (substr (c_outfile, i-4, i) == ".fits") 
	theoutfile = substr (c_outfile, 1, i-5)
   else 
 	theoutfile = c_outfile
#
   i = strlen (biasfile)
   if (substr (biasfile, i-4, i) == ".fits") 
	thebiasfile = substr (biasfile, 1, i-5)
   else 
 	thebiasfile = biasfile
#
   i = strlen (c_basedark)
   if (substr (c_basedark, i-4, i) == ".fits") 
	thebasedark = substr (c_basedark, 1, i-5)
   else 
 	thebasedark = c_basedark
#
#
   if (access (theoutfile//".fits")) 
 	del (theoutfile//".fits", verify-)
#
#*********************************************************************   
# Expand input file list into temporary files
#
   inlist =  mktemp("tmp$daydark")
#
   sections (c_infiles, option="fullname",>inlist)
   nfiles = int(sections.nimages)
   imglist = inlist
 
   print (" ")
   print ("Nr. of input files: ", nfiles)
#
#*********************************************************************
# Join the individual files into a multi-imset file. 
# Check first for the number of imsets in the individual files. 
# 
   joinfiles  = mktemp("jfiles")
   joinedfile = mktemp("joined")
   j = 0
   while (fscan(imglist,img) != EOF) {
        i = strlen (img)
        if (substr (img, i-4, i) == ".fits"){
             theimg = substr (img, 1, i-5)}
        else{
             theimg = img}
	imgets (img//"[0]", "nextend")
	nimset = int(imgets.value)/3
	if (nimset > 1){
		j = j+1
		print ("     Input file ", img, " has ", nimset, "imsets")
     	   	if (access ("flist"//j)) {
	   	   del ("flist"//j, verify-)}
		for (i = 1; i <= nimset; i += 1) {
		   print (theimg//"_"//i//".fits", >> "flist"//j)
		   print (theimg//"_"//i//".fits", >>joinfiles)
		   }
		mssplit (img, "@flist"//j, extension="*", retain-, \
			>& "dev$null")
		   }
 	else {
		print ("     Input file ", img, " has ", nimset, "imset")
		print (img, >>joinfiles)
		}
   	}
   msjoin ("@"//joinfiles, joinedfile//".fits", >& "dev$null")
#
#*********************************************************************
# Delete temporary files and corresponding file lists 
#
   for (i = 1; i <= j; i += 1) {
	del ("@flist"//i, verify-)
	del ("flist"//i, verify-)
   }
#
#*********************************************************************
# If CR correction has already been done on the input (joined) file, 
# then put boolean 'do_cal' to 'no', in which case 
# calstis will not be run. Else to 'yes'.
#
   do_cal = yes
   imgets (joinedfile//".fits[0]", "CRCORR")
   crcorr = imgets.value
   i = strlen (crcorr)
   if (substr (crcorr, 1, 8) == "COMPLETE") {
      print ("Cosmic ray rejection already done for input file(s). ", \
		"Skipping calstis step.")
      do_cal = no
      }
   else if (nfiles == 1){
   	imgets (joinedfile//".fits[0]", "NEXTEND")
   	nimset = int(imgets.value)/3
	if (nimset == 1){
		print ("You only entered 1 single-imset raw file, so I ", \
			"cannot do CR rejection.")
		print ("Try again with a list of raw files, or a ", \
			"`msjoin'ed raw file.")
		return
	}
    }
#
#*********************************************************************
# Check to see if housing temperature scaling needs to be done.
#
   do_scale = no
   imgets (joinedfile//".fits[0]", "TEXPSTRT")
   mjd = real(imgets.value)
   if (mjd >= 52102.00010){
        do_scale = yes
   }
   
#
#*********************************************************************
# If "do_cal = yes", perform basic calstis reduction first (overscan 
# correction, cosmic ray rejection (if no dark scaling), and bias level 
# subtraction)
#
   if (do_cal) {
#
      if (access ("calstis.log")) 
 	del ("calstis.log", verify-)

      if (access ("op1file")) 
 	del ("op1file", verify-)
#
      hedit (joinedfile//".fits[0]", "DQICORR", "PERFORM", verify-, show-, \
	update+)
      hedit (joinedfile//".fits[0]", "BLEVCORR", "PERFORM", verify-, show-, \
	update+)
      hedit (joinedfile//".fits[0]", "BIASCORR", "PERFORM", verify-, show-, \
	update+)
      if (do_scale) {
         hedit (joinedfile//".fits[0]", "CRCORR", "OMIT", verify-, show-, \
           update+)
      }
      else {
         hedit (joinedfile//".fits[0]", "CRCORR", "PERFORM", verify-, show-, \
           update+)
      }
      hedit (joinedfile//".fits[0]", "DARKCORR", "OMIT", verify-, show-, \
	update+)
      hedit (joinedfile//".fits[0]", "FLATCORR", "OMIT", verify-, show-, \
	update+)

      print ((joinedfile)//".fits", >> "op1file")
#
      list = "op1file"
      while (fscan (list,s1) != EOF){
	keypar (s1//"[0]","nextend")
	nextend = int(keypar.value)
	keypar (s1//"[0]","nrptexp")
	nrptexp = int(keypar.value)
 	nimset = nextend/3
	if (nextend <= 3) {
		print ("Sorry, your input image seems to have \
			only 1 imset.")
		print ("This task can only handle images with the  \
			NEXTEND keyword being equal to 3*N (N > 1).")
		delete ("op1file", verify-)
		bye
	}
	print(" ")
	print("Keyword NRPTEXP: ",nrptexp, \
		" while total nr. of imsets = ",nimset)
	if (nrptexp != nimset) {
		hedit (s1//"[0]", "NRPTEXP", nimset, verify-, show-, update+)
		hedit (s1//"[0]", "CRSPLIT", "1", verify-, show-, update+)
	   	print (">>>> Updated keyword NRPTEXP to ",nimset)
	   	print ("    (and put keyword CRSPLIT at 1)")
	  	}
	print(" ")
      }
      del ("op*file", verify-)
#
# Change APERTURE to '50CCD' and APER_FOV to '50x50' for ocrreject to work 
# correctly (i.e., so that it doesn't flag regions outside the original 
# APERTURE) 
#
      hedit (s1//"[0]", "APERTURE", "50CCD", verify-, show-, update+)
      hedit (s1//"[0]", "APER_FOV", "50x50", verify-, show-, update+)
#
# If parameter "biasfile" is specified, use it as BIASFILE in calstis
#
      if (thebiasfile != "" && thebiasfile != " "){
	 hedit (s1//"[0]", "BIASFILE", thebiasfile//".fits", ver-, show-, \
 	 update+)
      }
#
      print ("Running calstis on input file ...")
      calstis (joinedfile//".fits", savetmp-, verbose-, >& "calstis.log")
#
#*********************************************************************
# Start loop to read the housing temperature from the header of each science
# extension, and then apply the scaling to each extension of the _flt file.
#
      if (do_scale) {
         # Get the temperature scaling parameters from the primary header
         # of the basedark reference file, if the keywords exist.
         keypar (thebasedark // ".fits[0]", "ref_temp", silent=yes)
         if (keypar.found)
            ref_temp = real (keypar.value)
         else
            ref_temp = 18.      # reference temperature, degrees Celsius
         keypar (thebasedark // ".fits[0]", "drk_vs_t", silent=yes)
         if (keypar.found)
            drk_vs_t = real (keypar.value)
         else
            drk_vs_t = 0.07     # slope of dark count rate vs temperature
     	 #
	 # expand input housing temperature array into temporary files
	 #
	 intemp =  mktemp("tmp$houstemp")
	 sections (c_houstemp, option="fullname",>intemp)
	 ntemps = int(sections.nimages)
   	 list = intemp
	 #
	 print (" ")
         print ("Scaling the darks to the reference temperature ...")
         print (" ")
         extcount = 1
	 #
         imgets (joinedfile//"_flt.fits[sci,"//extcount//"]","OCCDHTAV", \
		>& "dev$null")
    	 if (real(imgets.value) != 0. && nimset != ntemps && ntemps >= 2) {
		print ("WARNING: You entered ", ntemps, "input ", \
	  	  "temperatures, while there are ", nimset, "input ", \	
	  	  "imsets.")
		print ("For now, we will use the info in the header ", \
		  "keywords.")
		print (" ")
 	 }
	 #
         while (extcount <= nimset) {
            imgets (joinedfile//"_flt.fits[sci,"//extcount//"]","EXPSTART")
            juliandate = real(imgets.value)
#            print ("Julian date for exposure "//extcount//" is: ", juliandate)
#
            imgets (joinedfile//"_flt.fits[sci,"//extcount//"]","OCCDHTAV", \
		>& "dev$null")
#
	    if (nimset == ntemps) {
		print ("Overriding header keyword by input housing temperature")
		tprint (intemp, showrow-, showhdr-, showunit-, columns="c1", \
			prparam-, prdata+, rows=extcount, sp_col="", lgroup=0, \
			option="plain") | scan (temptemp)
                expr = "im1 / (1.0 + " // drk_vs_t // " * (" // temptemp // \
                       " - " // ref_temp // "))"
		#
		# If OCCDHTAV keyword doesn't yet exist, direct row 'extcount' of 
		# the list of temperatures into newly created header keyword 
		# OCCDHTAV of the appropriate imset. 
		#
		if (real(imgets.value) == 0.) {
		   print ("Will create header keywords from input temperatures.")
                   hedit (joinedfile//"_flt.fits[sci,"//extcount//"]", \
			"OCCDHTAV", temptemp, verify-, show+, update+, add+)
		}
                print("   Housing temperature for exposure nr. "//extcount//" is ", \
		   temptemp, "K")
	    }
            else if (real(imgets.value) != 0.) {
#                imgets(joinedfile//"_flt.fits[sci,"//extcount//"]","OCCDHTAV")
                temptemp = real(imgets.value)
               print("   Housing temperature for exposure nr. "//extcount//" is ", \
		  temptemp, "K")
               # for example:  expr = "im1 / (1.0 + 0.07 * (25.7 - 18.))"
               expr = "im1 / (1.0 + " // drk_vs_t // " * (" // temptemp // \
                      " - " // ref_temp // "))"
            }
            else {
                print ("ERROR. The housing temperature is not in the headers, ") 
		print (" ")
		print ("AND the number of input files, ", nimset, \
		  " is not equal to the number of input temperatures, ", \
		  ntemps, ".") 
		print ("Check the CCD housing temp table for entries ", \
		  "near julian date ", juliandate)
		error (1, \
		  "Please update input parameter `houstemp'.")
	    }
#            print ("Scaling exposure "//extcount//" by the housing temperature ...")
            imcalc (joinedfile//"_flt.fits[sci,"//extcount//"]", joinedfile//"_flt.fits[sci,"//extcount//"]", expr, pixtype="real", nullval=0., verbose-)
#
            extcount = extcount + 1
         }
#
         hedit(joinedfile//"_flt.fits[0]", "CRCORR", "PERFORM", ver-, show-, update+)
         hedit (joinedfile//"_flt.fits[0]", "REF_TEMP", ref_temp, \
                add=yes, verify=no, show=no, update=yes)
         hedit (joinedfile//"_flt.fits[0]", "DRK_VS_T", drk_vs_t, \
                add=yes, verify=no, show=no, update=yes)
#
         print(" ")
         print("Combining the scaled darks, eliminating cosmic rays ...")
#
	 unlearn ocrreject
         ocrreject (joinedfile//"_flt.fits", joinedfile//"_crj.fits", all+, \
		crrejtab="", scalense="", initgues="", skysub="", crsigmas="", \
	 	crmask="", verbose+, >> "calstis.log")
      }
   }
   print (" ")
#
#*********************************************************************
# Get exposure time and gain from header of (e.g., weekly) superdark 
# and divide by the quotient (exptime / gain) to get a superdark 
# in electrons/sec.
#
   if (do_cal) {
      s1 = joinedfile//"_crj.fits"
      if (!access (s1)) {
         print ("Error running calstis; see calstis.log:")
         tail ("calstis.log", nlines=20)
         error (1, "calstis error")
      }
   } else {
      s1 = joinedfile//".fits"
   }
#
   keypar (s1//"[0]","texptime")
   x = real(keypar.value)
   keypar (s1//"[0]","atodgain")
   gain = real(keypar.value)
   #
   # If calstis release of input is after Feb 6, 1998 (v1.7) then get 
   # keyword 'ncombine' from science header instead of from main header. 
   #
   imgets (s1//"[0]", "cal_ver")
   cal_ver = imgets.value
   i = strlen (cal_ver)
   got_version = no
   for (j = 1; j <= i; j += 1){
     if (substr (cal_ver, j, j) == ".") {
       thecal_ver = substr (cal_ver, j-1, j+1)
       cal_version = real(thecal_ver)
       got_version = yes
     }
   }
   if (!got_version) {
     print ("Sorry, cannot identify CALSTIS Version; I read: ", cal_ver)
     return 
   }

   if (cal_version >= 1.7) {
   	keypar (s1//"[1]","ncombine")
   	ncombine = int(keypar.value)
   }
   else {
   	keypar (s1//"[0]","ncombine")
   	ncombine = int(keypar.value)
   }
   keypar (s1//"[0]","nrptexp")
   nrptexp = int(keypar.value)
   print ("Normalizing image to get units of electrons/sec ...")
#  
   print(" ")
   print("Keywords NRPTEXP: ",nrptexp)
   print("        NCOMBINE: ",ncombine, \ 
   	"   (just checking for constistency :-)")
   print(" ")
   y = x / gain
   print("Exposure time: ", x, "seconds")
   print("     CCD gain: ", gain, "electrons/ADU")
   print("  Dividing by: ", y)
   msarith (s1, "/", y, theoutfile//"_tmp.fits", verbose=0)
   hedit (theoutfile//"_tmp.fits[0]", "TEXPTIME", 1.0, verify-, \
   	show-, update+)
   if (access ("opfile")) 
 	del ("opfile", verify-)
#***************************************************************************
# Perform iterative statistics on this normalized superdark 
# (i.e., neglecting hot pixels in the process)
#
   img = theoutfile//"_tmp.fits[1]"
	imstat(img,fields="mean,stddev,npix,midpt,mode,min,max", 
           lower=lower,upper=upper,for-) | scan(mn,sig,npx,med,mod,min,max)
        m = 1
#       if (verbose) print(img//" :")
        while (m <= maxiter)  {
           if (verbose)  {
               print(m, " ",img,": mean=",mn," rms=",sig)
                    print("   npix=",npx," median=",med," mode=",mod)
                    print("   min=",min, "max=", max)
                }
           ll = mn - (5*sig)
           ul = mn + (5*sig)
           if (lower != INDEF && ll < lower) ll = lower
           if (upper != INDEF && ul > upper) ul = upper
           imstat(img,fields="mean,stddev,npix,midpt,mode,min,max",
              lower=ll,upper=ul,for-) | scan(mn,sig,nx,med,mod,min,max)
           if (nx == npx)
                break
           npx = nx
           m = m + 1
        }
#
	numiter = m 
	m_fivesig  = med - (5*sig)
	p_fivesig  = med + (5*sig)
	p_onesig = med + (1*sig)
	p_twosig = med + (2*sig)
	p_trisig = med + (3*sig)
        if (print) {
	   print(" ")
           print("Statistics on normalized dark: ")
	   print("   mean=",mn,"  median=",med," rms=",sig," npix=",npx)
	   print("   Number of Iterations: ", numiter)
	}
	print(" ")
	print("Updating hot pixels above a level of ", p_fivesig, \
	   "electrons/sec")
	print(" ")
   if (access ("opfile")) 
 	del ("opfile", verify-)
#***************************************************************************
# Perform iterative statistics on the baseline superdark 
# (i.e., neglecting hot pixels in the process)
#
   img = thebasedark//".fits[sci]"
	imstat(img,fields="mean,midpt,stddev,npix", 
           lower=lower,upper=upper,for-) | scan(basemn,basemed,basesig,npx)
        m = 1
#       if (verbose) print(img//" :")
        while (m <= maxiter)  {
           if (verbose)  {
               print(m, " ",img,": mean=",basemn," median=",basemed, \ 
		" rms=",basesig)
               }
           ll = basemn - (5*basesig)
           ul = basemn + (5*basesig)
           if (lower != INDEF && ll < lower) ll = lower
           if (upper != INDEF && ul > upper) ul = upper
           imstat(img,fields="mean,midpt,stddev,npix",
              lower=ll,upper=ul,for-) | scan(basemn,basemed,basesig,nx)
           if (nx == npx)
                break
           npx = nx
           m = m + 1
        }
#
	numiter = m 
	twosig = basemed + (2*basesig)
	trisig = basemed + (3*basesig)
	fivesig = basemed + (5*basesig)
        if (print && verbose) {
	   print(" ")
           print("Statistics on baseline dark: ")
	   print("   mean=",basemn,"  median=",basemed," rms=",basesig, \ 
		" npix=",npx)
	   print("   Number of Iterations: ", numiter)
	   print(" ")
	}
   if (access ("opfile")) 
 	del ("opfile", verify-)
#***************************************************************************
# Update hot pixels not in baseline dark but in daily superdark
#   (hot pixels defined as > 5 sigma above its median value)
#
   if (access ("basedark_med.fits")) {
 	del ("basedark_med.fits", verify-)}
   if (access ("DQ_new.fits")) {
 	del ("DQ_new.fits", verify-)}
   if (access ("ERR_new.fits")) {
 	del ("ERR_new.fits", verify-)}
   if (access ("superdark.fits")) {
 	del ("superdark.fits", verify-)}
   if (access (theoutfile//"_sci.fits")) {
 	del (theoutfile//"_sci.fits", verify-)}
   if (access (theoutfile//"_err.fits")) {
 	del (theoutfile//"_err.fits", verify-)}
   if (access (theoutfile//"_dq.fits")) {
 	del (theoutfile//"_dq.fits", verify-)}
   basedrk  = mktemp("basedrk")
   if (access (basedrk//"_sci.fits")) {
 	del (basedrk//"_sci.fits", verify-)}
   if (access (basedrk//"_err.fits")) {
 	del (basedrk//"_err.fits", verify-)}
   if (access (basedrk//"_dq.fits")) {
 	del (basedrk//"_dq.fits", verify-)}
   if (access ("tempor.fits")) {
 	del ("tempor.fits", verify-)}
   if (access ("tempor1.fits")) {
 	del ("tempor1.fits", verify-)}
   if (access ("tempor2.fits")) {
 	del ("tempor2.fits", verify-)}
   if (access ("tempor3.fits")) {
 	del ("tempor3.fits", verify-)}
#
# imcopy extensions from the normalized "daily" superdark into temporary 
# output files (sci, err, dq)
#
   imcopy (theoutfile//"_tmp.fits[sci,1]", theoutfile//"_sci.fits", verbose-)
   imcopy (theoutfile//"_tmp.fits[err,1]", theoutfile//"_err.fits", verbose-)
   imcopy (theoutfile//"_tmp.fits[dq,1]",  theoutfile//"_dq.fits",  verbose-)
#
   imcopy (thebasedark//".fits[sci,1]", basedrk//"_sci.fits", verbose-)
   imcopy (thebasedark//".fits[err,1]", basedrk//"_err.fits", verbose-)
   imcopy (thebasedark//".fits[dq,1]", basedrk//"_dq.fits", verbose-)
#
# Create median filtered version of baseline dark: "basedark_med.fits"
#
   median (basedrk//"_sci.fits", "basedark_med.fits", xwindow=5., ywindow=5., \
	boundary="nearest", constant=0., verbose-)
#
# If daily > 5 sigma above its median AND base < 5 sigma above its median 
# (i.e., new hot pixels) then max(baseline,daily) else base -> tempor1.fits
#
   expr = "if im1 .ge. "//p_fivesig//" .and. im2 .le. "//fivesig//" then max(im1,im2) else im2"
   imcalc (theoutfile//"_sci.fits[0],"//basedrk//"_sci.fits[0]", "tempor1.fits", \
	expr, pixtype="real", nullval=0., verbose-) 
#
# If baseline > 5 sigma above *daily* median AND daily < 2 sigma above 
# *baseline* median (i.e., pixels significantly annealed out) then daily else 
# "tempor1" -> tempor2.fits
#
   expr = "if im2 .ge. "//p_fivesig//" .and. im1 .lt. "//twosig//" then im1 else im4"
   imcalc (theoutfile//"_sci.fits[0],"//basedrk//"_sci.fits[0],basedark_med.fits[0],tempor1.fits[0]", \
	"tempor2.fits", expr, pixtype="real", nullval=0., verbose-) 
#
# If baseline > 3 sigma above its median AND daily > 5 sigma above its median
# (i.e, somewhat hot in both) then max(base,daily) else "tempor2" -> tempor3.fits
#
   expr = "if im2 .ge. "//trisig//" .and. im1 .ge. "//p_fivesig//" then max(im1,im2) else im3"
   imcalc (theoutfile//"_sci.fits[0],"//basedrk//"_sci.fits[0],tempor2.fits[0]", \
	"tempor3.fits", expr, pixtype="real", nullval=0., verbose-) 
#
# If baseline > 27 e/s (i.e., saturated) then daily else "tempor3"
# -> superdark.fits. This should be the science extension of the final product.
#
   expr = "if im2 .ge. 27. then im1 else im3"
   imcalc (theoutfile//"_sci.fits[0],"//basedrk//"_sci.fits[0],tempor3.fits[0]", \
	"superdark.fits", expr, pixtype="real", nullval=0., verbose-) 
#
#***************************************************************************
# Update DQ extension of normalized dark by assigning the value 16 to the 
# new hot pixels (0.1 e/s above median of 'superdark.fits'), and put the result 
# in temporary image. 
#
   flprc
   imarith ("basedark_med.fits", "+", "0.1", "tempor.fits", divzero=0., \
	pixtype="real", verbose-, noact-)
   imcalc (theoutfile//"_dq.fits[0],superdark.fits[0],tempor.fits[0]", \
	"DQ_new.fits", "if im2 .ge. im3 then 16 else im1", pixtype="int", \
	verbose-)
#
#***************************************************************************
# Update ERR extension of new superdark by assigning the ERR values of the 
# basedark except for the new hot pixels that are updated from the daily
# superdark, for which the error extension of the daily superdark is taken. 
# Put the result in temporary ERR image. 
#
   s1 = thebasedark
   s2 = basedrk
   s3 = theoutfile
   flprc
   inexpr = s2//"_err.fits[0],DQ_new.fits[0],"//s2//"_dq.fits[0],"//s3//"_err.fits[0]"
   imcalc (inexpr, "ERR_new.fits", "if im2 .lt. im3 then im1 else im4", \
	pixtype="real", nullval=0., verbose-)
#
#***************************************************************************
# Copy new superdark into image extension of original super-de-buper dark
# and store into new reference dark; copy ERR extension from normalized 
# weekly superdark, and DQ extension from temporary file created before. 
#
   rename (theoutfile//"_tmp.fits", theoutfile//".fits", field="all")
#
   imcopy ("superdark.fits", theoutfile//"[sci,1][*,*]", verb-)
   imcopy ("ERR_new.fits[0]", theoutfile//"[err,1][*,*]", verb-)
   imcopy ("DQ_new.fits[0]",  theoutfile//"[dq,1][*,*]", verb-)
   hedit (theoutfile//".fits[0]", "FILENAME", theoutfile//".fits", \
	verify-, show-, update+)
#
#***************************************************************************
# Get rid of all intermediate junk files
#
   del ("basedark_med.fits,superdark.fits,DQ_new.fits", \
	verify-)
   del ("ERR_new.fits", verify-)
   del (joinedfile//"*", verify-)
   del (joinfiles//"*", verify-)
   del (s1//"_*.fits", verify-)
   del (s3//"_*.fits", verify-)
   if (do_cal && do_scale) {
       del (intemp//"*", verify-)
   }
   del (basedrk//"_sci.fits*", verify-)
   del (basedrk//"_err.fits*", verify-)
   del (basedrk//"_dq.fits*", verify-)
   del (inlist//"*", verify-)
   del ("tempor.fits,tempor1.fits,tempor2.fits,tempor3.fits", verify-)
   if (access ("flist")) {
      del ("flist", verify-)}
#
#***************************************************************************
# Print out relevant information
#
   print ("Resulting daily superdark file: ", theoutfile//".fits")
   print (" ")
   print ("     File name of 'basedark' image (", thebasedark, \
	".fits) has been")
   print ("     stored in 'DARKFILE' keyword in main header of ", \
	theoutfile//".fits")
   hedit (theoutfile//".fits[0]", "DARKFILE", thebasedark//".fits", \
	verify-, show-, update+)
#
# DONE!
# 
   biasfile = ""
end
