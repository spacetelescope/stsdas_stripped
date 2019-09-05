# zpp -- paper products for a quick look at GHRS Calibration data
#
# Version 1.0 Aug95  S. Hulbert
# Version 2.0 Oct95  L.E. Sherbert
# Version 2.1 Oct95  L.E. Sherbert
#      # Expanded font sizes and reordered words and increased oms plot size 
# Version 2.11 Oct95  L.E. Sherbert
#      # Added version number and name to parameters and output
# Version 2.2 Nov95 S.J. Hulbert
#      # Added provision for d1h files
#      # combines fp-splits and repeats; stats overwrite it though:(
# Version 2.3 15Jan96  L.E.Sherbert 
#      # Adding CARPOS and YDEF to Observation Summary
#      # Improved jitter axis labels and Added units to jit keywords
#      # Added ** before any switches in *.c1h still set to PERFORM unless 
#      #  that switch is ECH_CORR in which case, the value of GRATING will 
#      #  be checked.  If grating doesn't start with 'ECH' then 'ok' will 
#      #  be added before the switch.  Else ** will be added to bring the 
#      #  switch setting to the attention of those looking at the data.  
#      #  Theoretically, similar checks could be done for all switches, but we 
#      #  need to know when switches might still allowably say "PERFORM" 
#      #  in calibrated output.
#      # Added check of trailer file for Error and ERROR
#      # Added switch to specify image, statistics or both
#      # Added switch to turn off trace print statements
# Version 2.31 16 Jan 96 L.E.Sherbert
#      # May want a "combine?" switch to turn off specalign for some images like RAPIDS
#      #   Or may just want to circumvent certain PKTFMT codes altogether.
#	#   Or may just want to train users to be smart about their job
#	# Added bunit to OBSERVATION Summary
#	# OMS Summary now plotting SI_V{23}_AVG rather than V{23}_DOM
#	# OMS Summary plot forced to size of SSA unless it is an ACQ*.
# Version 2.40 03 Apr 96 L.E.Sherbert
#      # Want to verify presence of *.jit or *.jih file ELSE look for *.cmi or *.cmh
#      # Want to fix bug which creates full page d1h picture
#      # Organized the top left block for images, adding a few ACQ-specific keywords
#      #  and separating ACQ-specific from IMAGE specific.
# Version 2.41 05 Aug 96 L.E.Sherbert
#	# Modified an if statement so it will admit to skipping 
#	#  DEFCALs even if oms1 doesn't exist
#	# Don't create  if (!defcal): moved mktmp statement for igicom2
#	# Slight modifications to some trace statements; want more clarification
#	# Only collect statistics if imagestat='s' or 'b'
#	# Report statistics of combined image when imagestat='b' (that way stats won't
#	#  won't overwrite image :)
#	# Format values for STEPTIME, FLUXFND, MINWAVE, MAXWAVE, 
#	#  and EXPTIME (which is wrong for ACQs)
#
# Version 2.5	29 Oct 97 	W. J. Hack	
#	# Revised to directly read and use FITS formatted GHRS files
#
procedure zpp (rootname, imtype, output, temproot)

file rootname = "" 		{prompt=">observation rootname"}
file output = "" 		{prompt=">output igi file name"}
string	temproot = ""		{prompt=">temporary root name"}
char    imtype = "" 		{prompt="Image type of root (FITS or GEIS)"}

string imagestat = "both" 	{prompt=">image, statistics, or both?"}
bool verbose = no 		{prompt=">print out trace lines for debugging?"}
#string device = "stdgraph" 	{prompt=">output device"}
string tasknm  = "PaperProd"   	{prompt=">Task name.  Do not modify.", mode='h'}
string version = "2.5"	{prompt=">Version number.  Do not modify.", mode='h'}
struct *listptr    		{prompt=">Do not modify.", mode='h'}

begin

file 	tmp0allnms, tmp1allnms, tmp1noext, tmp2uniq
file 	tmpoff, tmpspec, tmpwave, tmptrail
string 	ctmp0allnms, ctmp1noext
string 	dev, rootn, dirroot
string	obsroot
file 	igicom2
string	tmproot, geisflux
string 	stats
file 	raw, flux, wave, udl, shp, oms1, oms2, acq, trl
bool 	defcal, image, statnim, trace
string 	imstat
int 	len, i, j, iext, cenwave, count, gcountmax, winct
string 	aper, grating, instr, targname, ra, dec, equinox, pid
string 	v2_rms, v3_rms, nrecent, nlosses, carpos, ydef
string 	qual, com1, com2, qdate
real 	pos
real	v2min, v3min, v2max, v3max, width
real	exptime
string 	dio, phc, vig, abs, net, dqi, ccr1, ccr2, ccr3, ccr4, ccr5, ccr6
string 	ccr7, ccr8, ccr9, ccra, ccrb, ccrc, ccrd, ccg2
string 	dqi_corr, exp_corr, dio_corr, ppc_corr, map_corr, dop_corr, phc_corr
string 	vig_corr, mer_corr, gwc_corr, adc_corr, mdf_corr, mnf_corr, ply_corr
string 	bck_corr, iac_corr, ech_corr, flx_corr, hel_corr, vac_corr, gcount
string 	today
string 	rptobs, steppatt, steptime, fp_split, comb_add, fincode
string 	obsmode, detector, sclamp, minwave, maxwave, pktfmt, date_obs
string 	time_obs, pep_expo, bunitf, bunitw
string 	srchsize, locate, map, rtbright, zfluxm, orientat
string 	mapfnd, fluxfnd, bright, faint
string 	gsfail,recentr,lockloss,gsseprms,guidecmd,guideact,gsd_id,gsr_id
file 	fluxstat  # name for file to get statistics from: flux or tmpspec
bool	b1, b2
bool	oms_fits
file	oms
int	nrows
char	ftype

# get the inputs
rootn = rootname
igicom2 = output
tmproot = temproot
ftype = imtype

if (igicom2 == "" || igicom2 == " ") 
    error (1, "output file name is blank")
#dev = device
imstat = imagestat
trace = verbose

# get today's date
time | scan (line)
today = line
if (trace) print (today)

# check that stplot is loaded
if (!defpac ("stplot"))
    error (1, "The stplot package must be loaded to run plot")

tmp0allnms = tmproot//"_tmp0allnms"
tmp1noext = tmproot//"_tmpp1noext"
tmp2uniq = tmproot//"_tmp2uniq"

# get the rootname
# rootn = fparse.directory // substr(fparse.root,1,8) // "*"
if (ftype == "geis") {
	rootn = rootn // "*.???"
} else {
	rootn = rootn // "*.fits"
}
if (trace) print("Input file [rootn] is ", rootn)
files (rootn, sort+, > tmp0allnms)

# loop through the list of files
list = tmp0allnms
while (fscan (list, ctmp0allnms) != EOF) {

    fparse (ctmp0allnms, verbose=no)
    len = strlen (fparse.root)
    if (ftype == "fits") { len = len - 4 }
    if (substr(fparse.root,len,len) != "j") {
	if (ftype == "geis") { 
	       print (fparse.directory//fparse.root, >> tmp1noext)
	} else {
		rootn = substr (fparse.root, 1, len)
		print (fparse.directory//rootn, >> tmp1noext)
	}
    }
}
# END loop through the list of files
if (trace) print("List of all files [tmp0allnms] is ",tmp0allnms)
delete (tmp0allnms)

# make a unique list of rootnames
tproject (tmp1noext, tmp2uniq, "*", uniq+)
if (trace) print("List of uniq rootnames [tmp2uniq] is ",tmp2uniq)
delete (tmp1noext)

# loop through the list of rootnames
list = tmp2uniq
while (fscan (list, dirroot) != EOF) {
    if (trace) print("Entry from tmp2uniq [dirroot] is: ",dirroot)

    # Build the few filenames of interest
    len = strlen (dirroot)

    if (ftype == "geis") {
	raw  = dirroot//".d0h"
	acq  = dirroot//".d1h"
	flux = dirroot//".c1h"
	wave = dirroot//".c0h"
	trl  = dirroot//".trl"
	udl  = dirroot//".ulh"
	shp  = dirroot//".shh"
    } else {
	raw  = dirroot//"_d0f.fits[0]"
	acq  = dirroot//"_d1f.fits"
	flux = dirroot//"_c1f.fits"
	wave = dirroot//"_c0f.fits[0]"
	trl  = dirroot//"_trl.fits[0]"
	udl  = dirroot//"_ulf.fits"
	shp  = dirroot//"_shf.fits"
    }

    # Build filename for jitter files, starting with GEIS formatted version...
    oms1 = substr(dirroot,1,len-1) // "j.jih"
    oms2 = substr(dirroot,1,len-1) // "j.jit"
    oms_fits = no

   if (!access(oms1) && !access(oms2)) {
	if(trace) print("PP_WARNING: didn't find *.ji* so am trying *.cm*")
	oms1 = substr(dirroot,1,len-1) // "j.cmh"
	oms2 = substr(dirroot,1,len-1) // "j.cmi"
	if (!access(oms1) && !access(oms2)) {
		if (trace) print("PP_WARNING: didn't find GEIS jitter files, so am trying FITS files")
		oms1 = substr(dirroot,1,len-1) // "j_jif.fits"
		oms2 = substr(dirroot,1,len-1) // "j_jit.fits"
		oms_fits=yes
		if (oms_fits && trace) print("PP_WARNING: found FITS jitter files...")
       }
    }


    if (access(shp)) {
	if (ftype == "fits") shp = shp//"[0]"
      	keypar (shp, "rootname", silent+)
	obsroot = keypar.value
    } else
	obsroot = dirroot
   
    if (trace) {
   	print("Creating filenames needed for paperproducts:")
   	print("      raw is ", raw, "; acq is ", acq)
   	print(" and flux is ", flux, "; wave is ", wave, "; udl is ", udl)
   	print(" and oms1 is ", oms1, "; oms2 is ", oms2)
    }

    defcal =  no
    image = no
    if (access(udl)) {
	if (ftype == "fits") udl = udl//"[0]"
      	keypar (udl, "obsmode", silent+); obsmode = keypar.value
      	if (obsmode == 'DEFCAL') defcal = yes
      	if (obsmode == 'IMAGE')  image  = yes
      	if (substr (obsmode, 1, 3) == 'ACQ' && access(acq)) {
	    if (ftype == "fits") acq = acq//"[0]"
            image = yes
            raw = acq
        }
          
        if (trace) {
            print("Creating booleans which help decide what plots to create:")
            print(" obsmode= ", obsmode, "; defcal= ", 
			defcal," & image= ",image)
        }  
    }  
    
    #print(raw," ", flux," ", wave," ", udl," ", oms1," ", oms2," ", 
		#obsmode," ", defcal," ", image, >> tmp0allnms)

    # breaking into two lines since the files anmesa can be long
    print(raw," ", flux," ", wave," ", udl," ", oms1, >> tmp0allnms)
    print(oms2, " ", obsmode," ", defcal," ", image, >> tmp0allnms)
}  

list = tmp0allnms
while (fscan (list, raw, flux, wave, udl, oms1) != EOF) {
    while (fscan (list, oms2, obsmode, defcal, image) != EOF) {
      }
    fparse(raw,verbose=no)
    dirroot = fparse.root
    if (trace) {
   	print("Verifying names.  dirroot is:",dirroot)
   	print("Beg Loop: ",raw," ", flux," ", wave," ", udl," ", 
		oms1," ", oms2," ", obsmode," ", defcal," ", image)
    }

    #############################################################
    # get the SCIENCE data header info
    if (access(flux)) {
	if (ftype == "fits") flux = flux//"[0]"
    	if (trace) 
	    print(" Flux exists. collecting keyword values for ", flux)
       	keypar (flux, "aperture", silent+); aper = keypar.value
       	keypar (flux, "grating", silent+); grating = keypar.value
       	keypar (flux, "instrument", silent+); instr = keypar.value
       	keypar (flux, "targname", silent+); targname = keypar.value
       	keypar (flux, "ra_targ", silent+); ra = keypar.value
       	keypar (flux, "dec_targ", silent+); dec = keypar.value
       	keypar (flux, "equinox", silent+); equinox = keypar.value
       	keypar (flux, "proposid", silent+); pid = keypar.value
	    
       	keypar (flux, "rptobs", silent+); rptobs = keypar.value
       	keypar (flux, "steppatt", silent+); steppatt = keypar.value
       	keypar (flux, "steptime", silent+); steptime = keypar.value
       	keypar (flux, "fp_split", silent+); fp_split = keypar.value
       	keypar (flux, "comb_add", silent+); comb_add = keypar.value
       	keypar (flux, "carpos", silent+); carpos = keypar.value
       	keypar (flux, "ydef", silent+); ydef = keypar.value
       	keypar (flux, "fincode", silent+); fincode = keypar.value
       	keypar (flux, "obsmode", silent+); obsmode = keypar.value
       	keypar (flux, "detector", silent+); detector = keypar.value
       	keypar (flux, "sclamp", silent+); sclamp = keypar.value
       	keypar (flux, "minwave", silent+); minwave = keypar.value
       	keypar (flux, "maxwave", silent+); maxwave = keypar.value
       	keypar (flux, "pktfmt", silent+); pktfmt = keypar.value
	    
       	keypar (flux, "date-obs", silent+); date_obs = keypar.value
       	keypar (flux, "time-obs", silent+); time_obs = keypar.value
       	keypar (flux, "exptime", silent+); exptime = real(keypar.value)
       	keypar (flux, "pep_expo", silent+); pep_expo = keypar.value
	    
       	keypar (flux, "bunit", silent+); bunitf = keypar.value

	if (ftype == "geis") {
		keypar (flux, "gcount", silent+); gcount = keypar.value
	} else {
		keypar (flux, "naxis2", silent+); gcount = keypar.value
	}
    
	#############################################################
	# get the calibration info
       	keypar (flux, "diohfile", silent+); dio = keypar.value
       	keypar (flux, "phchfile", silent+); phc = keypar.value
       	keypar (flux, "vighfile", silent+); vig = keypar.value
       	keypar (flux, "abshfile", silent+); abs = keypar.value
       	keypar (flux, "nethfile", silent+); net = keypar.value
       	keypar (flux, "dqihfile", silent+); dqi = keypar.value
       	keypar (flux, "ccr1", silent+); ccr1 = keypar.value
       	keypar (flux, "ccr2", silent+); ccr2 = keypar.value
       	keypar (flux, "ccr3", silent+); ccr3 = keypar.value
       	keypar (flux, "ccr4", silent+); ccr4 = keypar.value
       	keypar (flux, "ccr5", silent+); ccr5 = keypar.value
       	keypar (flux, "ccr6", silent+); ccr6 = keypar.value
       	keypar (flux, "ccr7", silent+); ccr7 = keypar.value
       	keypar (flux, "ccr8", silent+); ccr8 = keypar.value
       	keypar (flux, "ccr9", silent+); ccr9 = keypar.value
       	keypar (flux, "ccra", silent+); ccra = keypar.value
       	keypar (flux, "ccrb", silent+); ccrb = keypar.value
       	keypar (flux, "ccrc", silent+); ccrc = keypar.value
       	keypar (flux, "ccrd", silent+); ccrd = keypar.value
       	keypar (flux, "ccg2", silent+); ccg2 = keypar.value
       	keypar (flux, "dqi_corr", silent+); dqi_corr = keypar.value
       	keypar (flux, "exp_corr", silent+); exp_corr = keypar.value
       	keypar (flux, "dio_corr", silent+); dio_corr = keypar.value
       	keypar (flux, "ppc_corr", silent+); ppc_corr = keypar.value
       	keypar (flux, "map_corr", silent+); map_corr = keypar.value
       	keypar (flux, "dop_corr", silent+); dop_corr = keypar.value
       	keypar (flux, "phc_corr", silent+); phc_corr = keypar.value
       	keypar (flux, "vig_corr", silent+); vig_corr = keypar.value
       	keypar (flux, "mer_corr", silent+); mer_corr = keypar.value
       	keypar (flux, "gwc_corr", silent+); gwc_corr = keypar.value
       	keypar (flux, "adc_corr", silent+); adc_corr = keypar.value
       	keypar (flux, "mdf_corr", silent+); mdf_corr = keypar.value
       	keypar (flux, "mnf_corr", silent+); mnf_corr = keypar.value
       	keypar (flux, "ply_corr", silent+); ply_corr = keypar.value
       	keypar (flux, "bck_corr", silent+); bck_corr = keypar.value
       	keypar (flux, "iac_corr", silent+); iac_corr = keypar.value
       	keypar (flux, "ech_corr", silent+); ech_corr = keypar.value
       	keypar (flux, "flx_corr", silent+); flx_corr = keypar.value
       	keypar (flux, "hel_corr", silent+); hel_corr = keypar.value
       	keypar (flux, "vac_corr", silent+); vac_corr = keypar.value
	if (ftype == "fits") flux = substr(flux,1,strlen(flux)-3)
    } else {
       	print (" No FLUX file found for ",obsroot,".")
       	if (image) {
    	    if (trace) 
		print (" Image exists. collecting keyword values for ", raw)
            keypar (raw, "proposid", silent+); pid = keypar.value
            keypar (raw, "instrument", silent+); instr = keypar.value
            keypar (raw, "aperture", silent+); aper = keypar.value
            keypar (raw, "grating", silent+); grating = keypar.value
            keypar (raw, "obsmode", silent+); obsmode = keypar.value
            keypar (raw, "steptime", silent+); steptime = keypar.value
	
            keypar (raw, "rtbright", silent+); rtbright = keypar.value
            keypar (raw, "bright", silent+); bright = keypar.value
            keypar (raw, "faint", silent+); faint = keypar.value
  
            keypar (raw, "srchsize", silent+); srchsize = keypar.value
            keypar (raw, "locate", silent+); locate = keypar.value
            keypar (raw, "mapfnd", silent+); mapfnd = keypar.value
            keypar (raw, "fluxfnd", silent+); fluxfnd = keypar.value
	
            keypar (raw, "targname", silent+); targname = keypar.value
            keypar (raw, "ra_targ", silent+); ra = keypar.value
            keypar (raw, "dec_targ", silent+); dec = keypar.value
            keypar (raw, "equinox", silent+); equinox = keypar.value
            keypar (raw, "date-obs", silent+); date_obs = keypar.value
            keypar (raw, "time-obs", silent+); time_obs = keypar.value
	
            keypar (raw, "pktfmt", silent+); pktfmt = keypar.value
            keypar (raw, "fincode", silent+); fincode = keypar.value
            keypar (raw, "pep_expo", silent+); pep_expo = keypar.value
  
            keypar (raw, "map", silent+); map = keypar.value
            keypar (raw, "orientat", silent+); orientat = keypar.value
            keypar (raw, "exptime", silent+); exptime = real(keypar.value)
            keypar (udl, "zfluxm", silent+); zfluxm = keypar.value
	
	    if (ftype == "geis") {
		keypar (raw, "gcount", silent+); gcount = keypar.value
	    } else {
		keypar (raw, "naxis2", silent+); gcount = keypar.value
	    }
      }
    }

    #############################################################
    # get the oms1 info
    if (!defcal && access(oms1)) {
    	if (trace) 
	    print(" Oms1 exists. collecting keyword values for ", oms1)

	if (oms_fits) {
		t_oms(oms1)
		oms = t_oms.output
        } else	oms = oms1

       	keypar (oms, "v2_rms", silent+); v2_rms = keypar.value
        keypar (oms, "v3_rms", silent+); v3_rms = keypar.value
       	keypar (oms, "nrecent", silent+); nrecent = keypar.value
       	keypar (oms, "nlosses", silent+); nlosses = keypar.value

	keypar (oms, "gsfail", silent+); gsfail = keypar.value
       	keypar (oms, "recentr", silent+); recentr = keypar.value
       	keypar (oms, "lockloss", silent+); lockloss = keypar.value
       	keypar (oms, "gsseprms", silent+); gsseprms = keypar.value
       	keypar (oms, "guidecmd", silent+); guidecmd = keypar.value
       	keypar (oms, "guideact", silent+); guideact = keypar.value
       	keypar (oms, "gsd_id", silent+); gsd_id = keypar.value
       	keypar (oms, "gsr_id", silent+); gsr_id = keypar.value
    } else if (!defcal && !access(oms1))
       	print (" No OMS file found for ",obsroot,".")
    else
        print (" Skipping DEFCAL ", obsroot, ".")

    # ##################################################################
    # make a temporary *name* for the igi commands 
    # this command *will* make a new one each time
	
    if ((!defcal) && (image || access(flux) || access(oms1) || access(oms2)) ) {
	if (trace) 
	    print (" File not DEFCAL & either Flux or Oms1 or Oms2 exists")

   	# create igi command file 2
	if (trace) print(" IGI command file name: ",igicom2)

   	# start to fill up igi command file 2
        print ("erase", >> igicom2)
        print ("fontset hard", >> igicom2)

        # bottom label
        print ("justify 6; expand 0.5; vmove .1 .02;label Space Telescope Science Institute  3700 San Martin Drive  Baltimore, MD 21218",>>igicom2)
        #print ("vmove .5 .02; putlabel 5 ",tasknm," ",version, >> igicom2)
        print ("vmove .5 .02; putlabel 5 ",tasknm," ", >> igicom2)
        print ("justify 4; expand 0.5; vmove .9 .02;label ",today,>>igicom2)

        # set up four windows
      	print ("window 2 2", >> igicom2)
    }

    #-------------------------------------------------------------------#
    # Observation Info goes here

    if (access(flux)) {
	if(ftype == "fits") flux = flux//"[0]"

   	if (trace) 
	    print (" Flux info going into igi script")
      	print ("window 3", >> igicom2)

   	# draw box
      	print ("vmove 0 0;vdraw 0 1;vdraw 1 1;vdraw 1 0;vdraw 0 0", >> igicom2)

   	# label box
      	print ("justify 5; expand 0.6; vmove .5 .95;label OBSERVATION SUMMARY", 
		>> igicom2)
      	print ("justify 6; expand 0.5", >> igicom2)

   	# fill the box
      	print ("vmove 0.1 0.875; label PROPOSAL #: ",pid,>>igicom2)
      	print ("vmove 0.1 0.825; label INSTRUMENT: ",instr,>>igicom2)
      	print ("vmove 0.1 0.775; label APERTURE: ",aper ,>>igicom2)
      	print ("vmove 0.1 0.725; label GRATING: ",grating,>>igicom2)
      	print ("vmove 0.1 0.675; label OBSMODE: ",obsmode,>>igicom2)
      	printf ("vmove 0.1 0.625; label EXPTIME: %0.2f sec\n", exptime,
		>>igicom2)
      	cenwave = nint((real(minwave) + real(maxwave))/2.)
      	printf ("vmove 0.1 0.575; label CENWAVE: %d A\n",cenwave,>>igicom2)

      	print ("vmove 0.1 0.475; label STEPPATT: ",steppatt,>>igicom2)
      	printf ("vmove 0.1 0.425; label STEPTIME: %0.3f sec\n", real(steptime),
		>>igicom2)
      	print ("vmove 0.1 0.375; label FP_SPLIT: ",fp_split,>>igicom2)
      	print ("vmove 0.1 0.325; label RPTOBS: ",rptobs,>>igicom2)
      	print ("vmove 0.1 0.275; label COMB_ADD: ",comb_add,>>igicom2)
      	print ("vmove 0.1 0.225; label CARPOS: ",carpos,>>igicom2)
      	print ("vmove 0.1 0.175; label YDEF: ",ydef,>>igicom2)
	    
  	# right side
      	print ("justify 4", >> igicom2)
      	print ("vmove 0.9 0.875; label TARGET: ",targname,>>igicom2)
      	printf ("vmove 0.9 0.825; label RA: %0.2H\n",real(ra),>>igicom2)
      	printf ("vmove 0.9 0.775; label DEC: %0.1h\n",real(dec),>>igicom2)
      	print ("vmove 0.9 0.725; label EQUINOX: ",equinox,>>igicom2)
	    
      	printf ("vmove 0.9 0.625; label MINWAVE: %9.3f A\n", real(minwave),
		>>igicom2)
      	printf ("vmove 0.9 0.575; label MAXWAVE: %9.3f A\n", real(maxwave),
		>>igicom2)

      	print ("vmove 0.9 0.475; label PKTFMT: ",pktfmt,>>igicom2)
      	print ("vmove 0.9 0.425; label FINCODE: ",fincode,>>igicom2)
      	print ("vmove 0.9 0.375; label DATE-OBS (dd/mm/yy): ",date_obs,>>igicom2)
      	print ("vmove 0.9 0.325; label TIME-OBS: ",time_obs,>>igicom2)
      	print ("vmove 0.9 0.275; label PEP_EXPO: ",pep_expo,>>igicom2)
      	print ("vmove 0.9 0.225; label BUNIT: ",bunitf,>>igicom2)
	    
	# Check trailer for errors
      	print ("expand 0.8", >> igicom2)
	    
	tmptrail = mktemp(tmproot//"trl")
	match ("E{rror}",trl,stop-,print-,meta+, >> tmptrail)
        listptr = tmptrail  # Everywhere it works, the structure is assigned a string...
      	while (fscan (listptr,line) != EOF) {
            if (strlen(line) != 0) 
                print ("vmove 0.1 0.10; putlabel 6 *** Check TRAILER for Errors ***",>>igicom2)
		break
        }
      	if (access(tmptrail)) delete(tmptrail)

	# Announce number of calibrated groups
      	print ("vmove 0.1 0.05; putlabel 6 # Calibrated Groups: ", 
		gcount,>>igicom2)

	#-----------------------------------------------------------------#
	# Calibration Summary
      	print ("window 4", >> igicom2)

   	# draw box
      	print ("vmove 0 0;vdraw 0 1;vdraw 1 1;vdraw 1 0;vdraw 0 0", >> igicom2)

   	# label box
      	print ("justify 5; expand 0.6; vmove .5 .95;label CALIBRATION STATUS SUMMARY", >> igicom2)
      	print ("justify 6; expand 0.5", >> igicom2)

   	# fill box
      	print ("vmove 0.1 0.875; label DIOHFILE: ", dio ,>>igicom2)
	
      	print ("vmove 0.1 0.840; label PHCHFILE: ", phc ,>>igicom2)
      	print ("vmove 0.1 0.805; label VIGHFILE: ", vig ,>>igicom2)
      	print ("vmove 0.1 0.770; label ABSHFILE: ", abs ,>>igicom2)
      	print ("vmove 0.1 0.735; label NETHFILE: ", net ,>>igicom2)
      	print ("vmove 0.1 0.700; label DQIHFILE: ", dqi ,>>igicom2)
      	print ("vmove 0.1 0.665; label CCR1: ", ccr1 ,>>igicom2)
      	print ("vmove 0.1 0.630; label CCR2: ", ccr2 ,>>igicom2)
      	print ("vmove 0.1 0.595; label CCR3: ", ccr3 ,>>igicom2)
      	print ("vmove 0.1 0.560; label CCR4: ", ccr4 ,>>igicom2)
      	print ("vmove 0.1 0.525; label CCR5: ", ccr5 ,>>igicom2)
      	print ("vmove 0.1 0.490; label CCR6: ", ccr6 ,>>igicom2)
      	print ("vmove 0.1 0.455; label CCR7: ", ccr7 ,>>igicom2)
      	print ("vmove 0.1 0.420; label CCR8: ", ccr8 ,>>igicom2)
      	print ("vmove 0.1 0.385; label CCR9: ", ccr9 ,>>igicom2)
      	print ("vmove 0.1 0.350; label CCRA: ", ccra ,>>igicom2)
      	print ("vmove 0.1 0.315; label CCRB: ", ccrb ,>>igicom2)
      	print ("vmove 0.1 0.280; label CCRC: ", ccrc ,>>igicom2)
      	print ("vmove 0.1 0.245; label CCRD: ", ccrd ,>>igicom2)
      	print ("vmove 0.1 0.210; label CCG2: ", ccg2 ,>>igicom2)

	print ("vmove 0.6 0.875; label DQI_CORR: ", dqi_corr ,>>igicom2)
      	print ("vmove 0.6 0.840; label EXP_CORR: ", exp_corr ,>>igicom2)
      	print ("vmove 0.6 0.805; label DIO_CORR: ", dio_corr ,>>igicom2)
      	print ("vmove 0.6 0.770; label PPC_CORR: ", ppc_corr ,>>igicom2)
      	print ("vmove 0.6 0.735; label MAP_CORR: ", map_corr ,>>igicom2)
      	print ("vmove 0.6 0.700; label DOP_CORR: ", dop_corr ,>>igicom2)
      	print ("vmove 0.6 0.665; label PHC_CORR: ", phc_corr ,>>igicom2)
      	print ("vmove 0.6 0.630; label VIG_CORR: ", vig_corr ,>>igicom2)
      	print ("vmove 0.6 0.595; label MER_CORR: ", mer_corr ,>>igicom2)
      	print ("vmove 0.6 0.560; label GWC_CORR: ", gwc_corr ,>>igicom2)
      	print ("vmove 0.6 0.525; label ADC_CORR: ", adc_corr ,>>igicom2)
      	print ("vmove 0.6 0.490; label MDF_CORR: ", mdf_corr ,>>igicom2)
      	print ("vmove 0.6 0.455; label MNF_CORR: ", mnf_corr ,>>igicom2)
      	print ("vmove 0.6 0.420; label PLY_CORR: ", ply_corr ,>>igicom2)
      	print ("vmove 0.6 0.385; label BCK_CORR: ", bck_corr ,>>igicom2)
      	print ("vmove 0.6 0.350; label IAC_CORR: ", iac_corr ,>>igicom2)
      	print ("vmove 0.6 0.315; label ECH_CORR: ", ech_corr ,>>igicom2)
      	print ("vmove 0.6 0.280; label FLX_CORR: ", flx_corr ,>>igicom2)
      	print ("vmove 0.6 0.245; label HEL_CORR: ", hel_corr ,>>igicom2)
      	print ("vmove 0.6 0.210; label VAC_CORR: ", vac_corr ,>>igicom2)
	    
   	# Bring non-nominal switches to the attention of peruser
      	if (dqi_corr == 'PERFORM') 
	    print ("vmove 0.6 0.875; putlabel 4 ** ", >>igicom2)
      	if (exp_corr == 'PERFORM') 
	    print ("vmove 0.6 0.840; putlabel 4 ** ", >>igicom2)
      	if (dio_corr == 'PERFORM') 
	    print ("vmove 0.6 0.805; putlabel 4 ** ", >>igicom2)
      	if (ppc_corr == 'PERFORM') 
	    print ("vmove 0.6 0.770; putlabel 4 ** ", >>igicom2)
      	if (map_corr == 'PERFORM') 
	    print ("vmove 0.6 0.735; putlabel 4 ** ", >>igicom2)
      	if (dop_corr == 'PERFORM') 
	    print ("vmove 0.6 0.700; putlabel 4 ** ", >>igicom2)
      	if (phc_corr == 'PERFORM') 
	    print ("vmove 0.6 0.665; putlabel 4 ** ", >>igicom2)
      	if (vig_corr == 'PERFORM') 
	    print ("vmove 0.6 0.630; putlabel 4 ** ", >>igicom2)
      	if (mer_corr == 'PERFORM') 
	    print ("vmove 0.6 0.595; putlabel 4 ** ", >>igicom2)
      	if (gwc_corr == 'PERFORM') 
	    print ("vmove 0.6 0.560; putlabel 4 ** ", >>igicom2)
      	if (adc_corr == 'PERFORM') 
	    print ("vmove 0.6 0.525; putlabel 4 ** ", >>igicom2)
      	if (mdf_corr == 'PERFORM') 
	    print ("vmove 0.6 0.490; putlabel 4 ** ", >>igicom2)
      	if (mnf_corr == 'PERFORM') 
	    print ("vmove 0.6 0.455; putlabel 4 ** ", >>igicom2)
      	if (ply_corr == 'PERFORM') 
	    print ("vmove 0.6 0.420; putlabel 4 ** ", >>igicom2)
      	if (bck_corr == 'PERFORM') 
	    print ("vmove 0.6 0.385; putlabel 4 ** ", >>igicom2)
      	if (iac_corr == 'PERFORM') 
	    print ("vmove 0.6 0.350; putlabel 4 ** ", >>igicom2)
	#if (ech_corr == 'PERFORM' && substr(grating,1,3) == 'ECH' ) 
	    #print ("vmove 0.6 0.315; putlabel 4 ** ", >>igicom2)
      	if (ech_corr == 'PERFORM') {
	    if (substr(grating,1,3) != 'ECH' )
		print ("vmove 0.6 0.315; putlabel 4 ok ", >>igicom2)
	    else
		print ("vmove 0.6 0.315; putlabel 4 ** ", >>igicom2)
        }
      	if (flx_corr == 'PERFORM') 
	    print ("vmove 0.6 0.280; putlabel 4 ** ", >>igicom2)
      	if (hel_corr == 'PERFORM') 
	    print ("vmove 0.6 0.245; putlabel 4 ** ", >>igicom2)
      	if (vac_corr == 'PERFORM') 
	    print ("vmove 0.6 0.210; putlabel 4 ** ", >>igicom2)

   	# add rootname label
	if (trace) 
	    print("Add rootname label: dirroot for label 1 is:",dirroot)
      	printf ("justify 5; expand 1.4; vmove 0.5 0.1;label %s GHRS: %s\n",
		"\\fB", obsroot,>>igicom2)
      	print ("lweight 2", >>igicom2)
      	print ("vmove .2 .05;vdraw .2 .15;vdraw .8 .15;vdraw .8 .05;vdraw .2 .05", >> igicom2)
      	print ("lweight 1", >>igicom2)

	#-----------------------------------------------------------------#
	# Image Statistics Summary
	#-----------------------------------------------------------------#
   	# draw box in lower left corner
      	print ("window 1", >> igicom2)
      	print ("vmove 0 0.05;vdraw 0 1;vdraw 1 1;vdraw 1 0.05;vdraw 0 0.05", 
		>> igicom2)

   	# label box
      	print ("justify 5; expand 0.5; vmove .5 .98;label IMAGE STATISTICS SUMMARY", >> igicom2)

   	# add a plot if the user asks for it
      	if (substr(imstat,1,1) == 'b' || substr(imstat,1,1) == 'i') {
            if (int(gcount) == 1) {
                print ("location 0.12 0.98 0.125 0.87", >> igicom2)
            	print ("xsection ",wave, >> igicom2)
            	print ("ysection ",flux, >> igicom2)
            	print ("limits",>> igicom2)
            	print ("box",>> igicom2)
            	print ("connect",>> igicom2)
            	print ("xlabel WAVELENGTH",>> igicom2)
		print ("vmove 0.02 0.89; putlabel 9 'FLUX'", >> igicom2)
            	#print ("ylabel FLUX",>> igicom2)
            	#print ("title ",obsroot,>> igicom2)
            	fluxstat=flux	# is fluxstat ok as a string or does it need to be a file?
            } else {
            	# SJH not sure where to put this
            	tmpspec = mktemp(tmproot//"spec")

		# Added by JC Hsu, 9/23/96
		if (obsmode == "RAPID") {
		    if(ftype == "geis") {
		    	rcombine (flux, tmpspec, "average", group = "-",
				nbin = INDEF, dataqual = no, errors = no)
		    } else {
			improject (flux, tmpspec, 2, average=yes, highcut=0.,
					lowcut=0., pixtype="real", verbose=no)
		    }
            	    print ("xsection ",wave, >> igicom2)
		   # tmpspec = tmpspec // ".c1h"    
	        } else {
            	    tmpoff = mktemp(tmproot//"off")
            	    tmpwave = mktemp(tmproot//"wave")
		    if (ftype == "fits") {
		    # Convert FITS necessary FITS files on the fly to GEIS...
			geisflux = mktemp(tmproot)
			strfits( flux, "", geisflux//".c1h", oldirafname=no, mode="h", >& "dev$null")
			strfits( wave, "", geisflux//".c0h", oldirafname=no, mode="h", >& "dev$null")
            	        poffsets (input=geisflux//".c1h",output=tmpoff,method="wave",wsource="image",wavelength="",zero_point="",zero_wave="",delta=20,window=100,fwidth=1,zero_fwidth=1,sections=1,niter=4,nsig=1.,verbose-,usecorr-,correlations="",interactive-)
		    } else {

            	    poffsets (input=flux,output=tmpoff,method="wave",wsource="image",wavelength="",zero_point="",zero_wave="",delta=20,window=100,fwidth=1,zero_fwidth=1,sections=1,niter=4,nsig=1.,verbose-,usecorr-,correlations="",interactive-) 

		    }

            	    specalign (input=tmpoff,output=tmpspec,wavelength=tmpwave,granularity="",spec_err="",gran_err="",niter=0,function="legendre",nterms=4,file_col="file",group_col="group",goff_col="goff",wave_col="wave",wgroup_col="wgroup",wshift_col="wshift")
            	    print ("xsection ",tmpwave, >> igicom2)
                }
            	print ("ysection ",tmpspec, >> igicom2)
            	print ("location 0.12 0.98 0.125 0.87", >> igicom2)
            	print ("limits",>> igicom2)
            	print ("box",>> igicom2)
            	print ("connect",>> igicom2)
            	print ("xlabel WAVELENGTH",>> igicom2)
		print ("vmove 0.02 0.89; putlabel 9 'FLUX'", >> igicom2)
            	#printf ("ylabel 'FLUX'\n", >> igicom2)
            	#print ("title Averaged Spectrum for ",obsroot,>> igicom2)
            	fluxstat=tmpspec # if >1 calibrated group, get statistics from combined image
            }
      	}

   	# print out image statistics
      	if (substr(imstat,1,1) == 's' || substr(imstat,1,1) == 'b') {

   	    # actually get the image statistics for all groups
   	    # if >1 calibrated group, get statistics from combined image
   	    # if no image requested, take statistics from reg flux file 
	    # regardless of #gps

	    if (trace) print(" Create fluxstat if it is blank; warn if create fluxstat here.")
	    if (fluxstat == '') {
		fluxstat=flux

   		# No image requested, therefore warn user that there is a 
		# max of 34 lines avail for stats
		print(" PP_WARNING:  Due to space limitations and imagestat=stat, ")
		print("              IMAGE STATISTICS SUMMARY can only list statistics for ")
		print("              up to 1st 34 calibrated groups.")
            }
            gstat (fluxstat, masks="", groups="*",
			fields="npix,mean,midpt,stddev,min,max", >& "dev$null")

            pos = 0.95
	    printf ("vmove 0.25 %f; justify 6; expand 0.4\n", pos, >> igicom2)
	    printf ("label 'Image Statistics for %s.C1H:'\n", obsroot, 
			>> igicom2)
            pos = pos - 0.025
	    print ("justify 5; expand 0.4", >> igicom2)
	    printf ("vmove 0.3 %f; label 'NPIX'\n", pos, >> igicom2)
	    printf ("vmove 0.4 %f; label 'MEAN'\n", pos, >> igicom2)
	    printf ("vmove 0.5 %f; label 'MIDPT'\n", pos, >> igicom2)
	    printf ("vmove 0.6 %f; label 'STDDEV'\n", pos, >> igicom2)
	    printf ("vmove 0.7 %f; label 'MIN'\n", pos, >> igicom2)
	    printf ("vmove 0.8 %f; label 'MAX'\n", pos, >> igicom2)

            pos = pos - 0.025
	    printf ("vmove 0.3 %f; label '%5d'\n", pos, gstpar.npix, >> igicom2)
	    printf ("vmove 0.4 %f; label '%10.3g'\n", pos, gstpar.mean, >> igicom2)
	    printf ("vmove 0.5 %f; label '%10.3g'\n", pos, gstpar.midpt, >> igicom2)
	    printf ("vmove 0.6 %f; label '%10.3g'\n", pos, gstpar.stddev, >> igicom2)
	    printf ("vmove 0.7 %f; label '%10.3g'\n", pos, gstpar.min, >> igicom2)
	    printf ("vmove 0.8 %f; label '%10.3g'\n", pos, gstpar.max, >> igicom2)
        }  # if imstat is stat or both

    } else if (image) {
        if (trace) 
	    print (" Image (RAW) data going into igi script." )

        #-------------------------------------------------------------------#
        # Observation Summary
        #-------------------------------------------------------------------#
        # draw box in upper left corner
        print ("window 3", >> igicom2)
        print ("vmove 0 0;vdraw 0 1;vdraw 1 1;vdraw 1 0;vdraw 0 0", >> igicom2)

        # label box
        print ("justify 5; expand 0.6; vmove .5 .95;label OBSERVATION SUMMARY", 
		>> igicom2)
        print ("justify 6; expand 0.5", >> igicom2)

        # fill the box
        # left side
        print ("vmove 0.1 0.875; label PROPOSAL #: ",pid,>>igicom2)
        print ("vmove 0.1 0.825; label INSTRUMENT: ",instr,>>igicom2)
        print ("vmove 0.1 0.775; label APERTURE: ",aper ,>>igicom2)
        print ("vmove 0.1 0.725; label GRATING: ",grating,>>igicom2)
        print ("vmove 0.1 0.675; label OBSMODE: ",obsmode,>>igicom2)
        printf("vmove 0.1 0.625; label STEPTIME: %0.3f sec\n",real(steptime),
		>>igicom2)

        print ("vmove 0.1 0.525; label RTBRIGHT: ",rtbright,>>igicom2)
        print ("vmove 0.1 0.475; label BRIGHT: ",bright,>>igicom2)
        print ("vmove 0.1 0.425; label FAINT: ",faint,>>igicom2)
    
        print ("vmove 0.1 0.325; label SRCHSIZE: ",srchsize, >> igicom2)
        print ("vmove 0.1 0.275; label LOCATE: ",locate, >> igicom2)
        print ("vmove 0.1 0.225; label MAPFND: ",mapfnd,>>igicom2)
        printf("%s %8d\n","vmove 0.1 0.175; label FLUXFND: ",int(fluxfnd),
		>>igicom2)

        # right side
        print ("justify 4", >> igicom2)
        print ("vmove 0.9 0.875; label TARGET: ",targname,>>igicom2)
        printf ("vmove 0.9 0.825; label RA: %0.2H\n",real(ra),>>igicom2)
        printf ("vmove 0.9 0.775; label DEC: %0.1h\n",real(dec),>>igicom2)
        print ("vmove 0.9 0.725; label EQUINOX: ",equinox,>>igicom2)
        print ("vmove 0.9 0.675; label DATE-OBS (dd/mm/yy): ",date_obs,>>igicom2)
        print ("vmove 0.9 0.625; label TIME-OBS: ",time_obs,>>igicom2)
    
        print ("vmove 0.9 0.525; label PKTFMT: ",pktfmt,>>igicom2)
        print ("vmove 0.9 0.475; label FINCODE: ",fincode,>>igicom2)
        print ("vmove 0.9 0.425; label PEP_EXPO: ",pep_expo,>>igicom2)
    
        print ("vmove 0.9 0.325; label MAP: ",map, >> igicom2)
        print ("vmove 0.9 0.275; label ORIENTAT: ",orientat, >> igicom2)

	# deal with exptime = 0.
        if (substr(obsmode,1,3) == 'ACQ' && rtbright == 'yes') 
	    exptime = real(steptime) * (real(srchsize))**2
	
        printf ("vmove 0.9 0.225; label EXPTIME: %0.2f sec\n",exptime,>>igicom2)
        print ("vmove 0.9 0.175; label ZFLUXM: ",zfluxm, >> igicom2)
    
        print ("expand 0.8", >> igicom2)
        print ("vmove 0.1 0.05; putlabel 6 # Image Groups: ", gcount,>>igicom2)

        # add rootname label
        print ("window 4", >> igicom2)
        if (trace) print("Add rootname label: dirroot for label 2 is:",dirroot)
      	printf ("justify 5; expand 1.4; vmove 0.5 0.1;label %s GHRS: %s\n",
		"\\fB", obsroot,>>igicom2)
        print ("lweight 2", >>igicom2)
        print ("vmove .2 .05;vdraw .2 .15;vdraw .8 .15;vdraw .8 .05;vdraw .2 .05", >> igicom2)
        print ("lweight 1", >>igicom2)

        # add the image
        print ("window 1", >> igicom2)
        print ("expand 0.4", >> igicom2)
        print ("vmove 0 0.05;vdraw 0 1;vdraw 1 1;vdraw 1 0.05;vdraw 0 0.05", 
			>> igicom2)
        print ("location 0.25 0.85 0.2 0.90",>> igicom2)
        print ("zsection ", raw, >> igicom2)
        print ("zrange", >> igicom2)
        print ("limits", >> igicom2)
        print ("pixmap", >> igicom2)
        print ("box", >> igicom2)
        print ("expand 0.8", >> igicom2)
        print ("title ", targname, >> igicom2)
        print ("xlabel X", >> igicom2)
        print ("ylabel Y", >> igicom2)
    }

    #-------------------------------------------------------------------#
    # oms1 data
    if (!defcal && access(oms1)) {
        if (trace) 
	    print (" OMS1 jih data going into igi script.")
        print ("window 2", >> igicom2)

        # draw box
        print ("vmove 0 0.05;vdraw 0 1;vdraw 1 1;vdraw 1 0.05;vdraw 0 0.05", 
		>> igicom2)

        # label box
        print ("justify 5; expand 0.6; vmove .5 .95;label OMS SUMMARY for ", 
		obsroot, >> igicom2)

        # fill box
        print ("vmove 0.71 0.85; expand 0.5; putlabel 6 V2 RMS Jitter (mas): ",
		v2_rms,>>igicom2)
        print ("vmove 0.71 0.8; putlabel 6 V3 RMS Jitter (mas): ",v3_rms,
		>>igicom2)
        print ("vmove 0.71 0.7; putlabel 6 Number of Recenterings: ",nrecent,
		>>igicom2)
        print ("vmove 0.71 0.65; putlabel 6 Number of Losses of Lock: ",
		nlosses,>>igicom2)

        # add a plot if there is data in the jit table
	# only for landscape orientation
        print ("location 0.13 0.697 0.15 0.9", >>igicom2)
#       if (tinfo.nrows > 0) {

      	if (access(oms2)) {
	       tinfo (oms2, ttout-)
	    if(tinfo.nrows > 1) { 
	    	tstat (oms2, "si_v2_avg", outtable="")
     	    	nrows = min(tstat.nrows, tinfo.nrows)
	    } else {
		nrows = 0
		if (trace) print("Jitter table ",oms2," was empty...")
	    }
        } else {
	    nrows =  0
        }
#	print ("NROWS = ",nrows)

	if (nrows > 0) {
	    b1 = (tstat.vmin != INDEF)
	    if (b1) {
	        v2min = tstat.vmin
	        v2max = tstat.vmax
            }
	    tstat (oms2, "si_v3_avg", outtable="")
	    b2 = (tstat.vmin != INDEF)
	    if (b2) {
	        v3min = tstat.vmin
	        v3max = tstat.vmax
            }
	    
	    # if the V2 V3 columns are INDEF, skip
	    if (b1 && b2) {
   		if (trace) print (" OMS2 jit data going into igi script.")
		width = max((v2max-v2min), (v3max-v3min))
          	print ("data ",oms2, >>igicom2)
          	print ("xcol si_v2_avg", >> igicom2)
          	print ("ycol si_v3_avg", >> igicom2)
          	printf ("limits %f %f %f %f\n",v2min, (v2min+width), v3min,
			(v3min+width), >> igicom2)
          	if (substr(obsmode,1,3) != 'ACQ') 
		    print ("limits -0.11 0.11 -0.11 0.11",>> igicom2)
          	print ("margin",>> igicom2)
          	print ("fmtick %0.2f", >> igicom2)
          	print ("box",>> igicom2)
          	print ("connect",>> igicom2)
          	print ("xlabel V2 (arcsec)",>> igicom2)
		print ("angle 90; vmove 0.03 0.5; putlabel 5 'V3 (arcsec)'", >> igicom2)
		print ("angle 0", >> igicom2)
            } else 
          	print ("vmove 0.4 0.2; putlabel 5 'V2/V3 INDEF'",>>igicom2)
            } else {

            # the jit table is empty, so no plot
            print ("vmove 0.4 0.2; putlabel 5 'jit file is empty'",>>igicom2)
       }
    } 
    #  else 
    #    there are no OMS files available at all.

    # print ("end",>> igicom2)
    #if (!defcal) igi (device=dev, < igicom2)

    ## if (trace) print(" Verify: dirroot before end of loop ", dirroot)
}

# END loop through the list of rootnames
## if (trace) print(" Verify: dirroot after end of loop ", dirroot)

# output plot

# clean up
if (!trace) {
    #delete (igicom2)
    delete (tmp2uniq)
}
end
