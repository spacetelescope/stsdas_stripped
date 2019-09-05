#
#  MSCOMBINE  --  Runs GCOMBINE on NICMOS/STIS images.
#
#
#
#
#  Limitations of this version:
#  ------------------------
#
#  - No "sum" combination yet (available only in wfpc.combine).
#  - No compressed HDUs at the output file.
#  - No log file, just STDOUT printout.
#  - No scaling by exposure time in variable-time NICMOS images.
#
#
#  26-Mar-97:  Task created (I. Busko)
#  07-Apr-97:  New gcombine (IB)
#  08-Apr-97:  Output DQ, added noise model (IB)
#  10-Apr-97:  Fixed problem with output initialization (IB)
#  18-Apr-97:  Added "correct" SAMP and TIME in NICMOs files (IB)
#  18-Apr-97:  Exposure time can be read from the PIXVALUE TIME keyword (IB)
#  07-May-97:  PIXVALUE in TIME HDU is handled only if scale=exposure (IB)
#  07-May-97:  Append .fits extension in output file name (IB)
#  29-may-97:  Rename to mscombine (IB)
#  06-Aug-97:  Fix the explicit FITS extension name problem (IB)
#  20-Aug-97:  SAMP extension is type short (IB)
#  28-Aug-97:  Add INHERIT- to bloathdu calls (IB)
#  21-Feb-02:  Add BUNIT to time and samples extensions in NICMOS files (IB)
#  17-Oct-02:  Append ".hhh" to image names toutsci through touttim2 (PEH).
#  20-Jun-03:  Remove duplicate assignments of nsmod_e and nsmod_w in the
#		call to gcombine (PEH).
#  10-Jul-03:  When copying the temporary NICMOS images to output, swap
#		TIME and SAMP, so that SAMP is written before TIME. (PEH).
#  14-Mar-07:  Added WFC3 IR channel support by setting the instrument
#		to NICMOS. The WFC3 UVIS channel will be handled automatically
#		as another generic 3-extensions-per-imset instrument.
#		(H. Bushouse)

procedure mscombine (input, output)

char	input    = ""         {prompt=">List of file names to combine"}
char	output   = ""         {prompt=">Output file name"}
pset	dqbits   = ""         {prompt=">DQ bits to reject input pixels (pset)"}
bool	nsmod_e  = no         {prompt=">Use noise model for computing errors ?"}
char	reject   = "none"     {prompt=">Type of rejection",enum="none|minmax|\
                               ccdclip|ccdcrrej|rsigclip|rsigcrrej|avsigclip|\
                               avsigcrrej|errclip|errcrrej"}
char	combine  = "average"  {prompt=">Type of combine operation", \
                               enum="average|median"}
char	weight   = "none"     {prompt=">Type of weighting scheme"}
bool	nsmod_w  = no         {prompt=">Use noise model for weighting ?"}
real	blank    = 0.         {prompt=">Output value when zero pixels survive"}
char	scale    = "none"     {prompt=">Image scaling"}
char	zero     = "none"     {prompt=">Image zero level offset"}
char	statsec  = ""         {prompt=">Image section for computing statistics"}
char	expname  = "PIXVALUE" {prompt=">Image header exposure time keyword"}
real	lthreshold = INDEF    {prompt=">Lower threshold"}
real	hthreshold = INDEF    {prompt=">Upper threshold"}
int	nlow     = 1          {prompt=">minmax: Number of low pixels to reject"}
int	nhigh    = 1          {prompt=">minmax: Number of high pixels to reject"}
int	nkeep    = 1          {prompt=">Min. to keep (pos) or max. to reject \
                                        (neg)"}
bool	mclip    = yes        {prompt=">Use median in clipping algorithms ?"}
real	lsigma   = 3.         {prompt=">Lower sigma clipping factor"}
real	hsigma   = 3.         {prompt=">Upper sigma clipping factor"}
char	rdnoise  = "0."       {prompt=">Readout noise (electrons)"}
char	gain     = "1."       {prompt=">Gain (electrons/DN)"}
char	snoise   = "0."       {prompt=">Sensitivity noise (fraction)"}
file	tempdir  = "tmp$"     {prompt=">Directory for temporary files"}
char	version  = "21Feb02"  {prompt=">Date of installation"}

begin
	# These hold input parameters.
	char t_input, t_output, t_reject, t_combine, t_weight, t_temp
	char t_scale, t_zero, t_sec, t_expname
	char t_rdnoise, t_gain, t_snoise
	real t_blank, t_lthr, t_uthr, t_lsig, t_hsig
	int  t_nlow, t_nhigh, t_nkeep
	bool t_mclip, t_nsmode, t_nsmodw

	# These are for internal use.
	file infile, infile1
	file tempfile
	file tempsci, temperr, tempdqf, tempmsk, tempmsk1, temptim, tempsam
	file listsci, listerr, listdqf, listmsk, listmsk1, listtim, listsam
	file toutsci, touterr, toutdqf, toutsam, touttem, touttim, touttim1
	file touttim2, listrej, temprej, listmsk2
	char msg, instrument, pixvalue, outext, bunit
	struct timest
	int  extver, nextend, f1, count, rejmask
	int  bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8
	int  bit9, bit10, bit11, bit12, bit13, bit14, bit15

	# Check for the presence of pre-requisite tasks/packages.
	msg = ""
	if (!deftask("parkey"))   msg = msg // " ttools"
	if (!deftask("imarith"))  msg = msg // " images"
	if (!deftask("mknoise"))  msg = msg // " artdata"
	if (!deftask("fparse"))   msg = msg // " tools"
	if (!deftask("gcombine")) msg = msg // " imgtools"
	if (strlen(msg) > 0) {
	    printf ("Please, load packages: %s\n", msg)
	    bye
	}

	# Reassign input task parameters.
	t_input   = input
	t_output  = output
	t_nsmode  = nsmod_e
	t_reject  = reject
	t_combine = combine
	t_weight  = weight
	t_nsmodw  = nsmod_w
	t_blank   = blank
	t_scale   = scale
	t_zero    = zero
	t_sec     = statsec
	t_expname = expname
	t_lthr    = lthreshold
	t_uthr    = hthreshold
	t_nlow    = nlow
	t_nhigh   = nhigh
	t_nkeep   = nkeep
	t_mclip   = mclip
	t_lsig    = lsigma
	t_hsig    = hsigma
	t_temp    = tempdir
	t_rdnoise = rdnoise
	t_gain    = gain
	t_snoise  = snoise

	# Build pixel rejection mask.
	rejmask = 0
	bit1  = 0
	bit2  = 0
	bit3  = 0
	bit4  = 0
	bit5  = 0
	bit6  = 0
	bit7  = 0
	bit8  = 0
	bit9  = 0
	bit10 = 0
	bit11 = 0
	bit12 = 0
	bit13 = 0
	bit14 = 0
	bit15 = 0
	if (dqbits.bit1)  bit1  = 1 
	if (dqbits.bit2)  bit2  = 1 
	if (dqbits.bit3)  bit3  = 1 
	if (dqbits.bit4)  bit4  = 1 
	if (dqbits.bit5)  bit5  = 1 
	if (dqbits.bit6)  bit6  = 1 
	if (dqbits.bit7)  bit7  = 1 
	if (dqbits.bit8)  bit8  = 1 
	if (dqbits.bit9)  bit9  = 1 
	if (dqbits.bit10) bit10 = 1 
	if (dqbits.bit11) bit11 = 1 
	if (dqbits.bit12) bit12 = 1 
	if (dqbits.bit13) bit13 = 1 
	if (dqbits.bit14) bit14 = 1 
	if (dqbits.bit15) bit15 = 1 
	rejmask = rejmask + bit1
	rejmask = rejmask + bit2  * 2
	rejmask = rejmask + bit3  * 4
	rejmask = rejmask + bit4  * 8
	rejmask = rejmask + bit5  * 16
	rejmask = rejmask + bit6  * 32
	rejmask = rejmask + bit7  * 64
	rejmask = rejmask + bit8  * 128
	rejmask = rejmask + bit9  * 256
	rejmask = rejmask + bit10 * 512
	rejmask = rejmask + bit11 * 1024
	rejmask = rejmask + bit12 * 2048
	rejmask = rejmask + bit13 * 4096
	rejmask = rejmask + bit14 * 8192
	rejmask = rejmask + bit15 * 16384

	# Set called tasks' parameters.
	bloathdu.dirtemp = t_temp

	time | scan (timest)
	printf ("MSCOMBINE Version 14Mar07 at %s\n", timest)

	# Get instrument and number of IMSETs from first file in input list.
	pickfile (t_input, 1)
	infile = pickfile.output
        imgets (infile // "[0]", "INSTRUME")
        instrument = imgets.value
        imgets (infile // "[0]", "NEXTEND")
        nextend = int (imgets.value)

	# WFC3/IR is same as NICMOS.
	if (instrument == "WFC3" && nextend > 6)
	    instrument = "NICMOS"

        if (instrument == "NICMOS")
            nextend /= 5
        else
            nextend /= 3

	# Check output file name extension.
	fparse (t_output, verbose-)
	if (fparse.extension != ".fits" && fparse.extension != ".fit")
	    t_output = t_output // ".fits"

	# This is the root name for temporaries.
	tempfile = t_temp // "mscomb"

	# This is the size of the input file list.
	countfile (t_input)
	count = countfile.output

	# Create names for file lists and other temporaries.
	listsci  = mktemp (tempfile)
	listerr  = mktemp (tempfile)
	listdqf  = mktemp (tempfile)
	listmsk  = mktemp (tempfile)
	listmsk1 = mktemp (tempfile)
	listmsk2 = mktemp (tempfile)
	listrej  = mktemp (tempfile)
        if (instrument == "NICMOS") {
	    listtim = mktemp (tempfile)
	    listsam = mktemp (tempfile)
	}
	toutsci  = mktemp (tempfile) // ".hhh"
	touterr  = mktemp (tempfile) // ".hhh"
	toutdqf  = mktemp (tempfile) // ".hhh"
	toutsam  = mktemp (tempfile) // ".hhh"
	touttem  = mktemp (tempfile) // ".hhh"
	touttim  = mktemp (tempfile) // ".hhh"
	touttim1 = mktemp (tempfile) // ".hhh"
	touttim2 = mktemp (tempfile) // ".hhh"

	# Copy primary header.
	delete (t_output, yes, >& "dev$null")
	imcopy (infile // "[0]", t_output, verbose-)

	# Loop over IMSETs.
	for (extver = 1; extver <= nextend; extver+=1) {

	    printf ("Begin processing IMSET %d:\n", extver)

	    # Scan file list and process each in turn.
	    for (f1 = 1; f1 <= count; f1+=1) {
	        pickfile (t_input, f1)
	        infile = pickfile.output

	        # Create file names for the current HDUs to be extracted.
	        tempsci  = mktemp (tempfile) // ".hhh"
	        temperr  = mktemp (tempfile) // ".hhh"
	        tempdqf  = mktemp (tempfile) // ".hhh"
	        tempmsk  = mktemp (tempfile) // ".hhh"
	        tempmsk1 = mktemp (tempfile) // ".hhh"
	        temprej  = mktemp (tempfile) // ".hhh"
                if (instrument == "NICMOS") {
	            temptim = mktemp (tempfile) // ".hhh"
	            tempsam = mktemp (tempfile) // ".hhh"
	        }

	        # Extract the HDUs into separate files using
	        # bloathdu to expand eventual compressed HDUs.
	        print ("Expanding file ", infile, " ...")
	        bloathdu (infile // "[SCI,"  // extver // ",INHERIT-]", tempsci)
	        bloathdu (infile // "[ERR,"  // extver // ",INHERIT-]", temperr)
	        bloathdu (infile // "[DQ,"   // extver // ",INHERIT-]", tempdqf)
                if (instrument == "NICMOS") {
	            bloathdu (infile // "[TIME," // extver // ",INHERIT-]",
                              temptim)
	            bloathdu (infile // "[SAMP," // extver // ",INHERIT-]", 
                              tempsam)
	        }

	        # Erase FITS-related keywords.
	        hedit (tempsci, "EXTNAME", "", add-, del+, ver-, show-, update+)
	        hedit (tempsci, "EXTVER",  "", add-, del+, ver-, show-, update+)
	        hedit (temperr, "EXTNAME", "", add-, del+, ver-, show-, update+)
	        hedit (temperr, "EXTVER",  "", add-, del+, ver-, show-, update+)
	        hedit (tempdqf, "EXTNAME", "", add-, del+, ver-, show-, update+)
	        hedit (tempdqf, "EXTVER",  "", add-, del+, ver-, show-, update+)
                if (instrument == "NICMOS") {
	            hedit (temptim, "EXTNAME", "", add-, del+, ver-, show-, upd+)
	            hedit (temptim, "EXTVER",  "", add-, del+, ver-, show-, upd+)
	            hedit (tempsam, "EXTNAME", "", add-, del+, ver-, show-, upd+)
	            hedit (tempsam, "EXTVER",  "", add-, del+, ver-, show-, upd+)
	        }

	        # Build mask from DQ HDU and mask parameter.
	        unlearn mknoise
	        imgets (tempdqf, "i_naxis1")
	        mknoise.ncols = int (imgets.value)
	        imgets (tempdqf, "i_naxis2")
	        mknoise.nlines = int (imgets.value)
	        mknoise.background = real (rejmask)
	        mknoise.rdnoise = 0.
	        mknoise.poisson = no
	        mknoise (tempmsk)
	        unlearn chpixtype
	        chpixtype (tempmsk, tempmsk, "short", verbose-)
	        addmasks (tempdqf // "," // tempmsk, tempmsk1, "im1 && im2", \
                          flags = " ") 

	        # Use PIXVALUE keyword in TIME HDU as source of exposure time
	        # for scaling. This is done by copying the PIXVALUE keyword
	        # and its value into the SCI HDU header.
                if (instrument == "NICMOS"    && 
                    t_scale    == "exposure"  &&
                    t_expname  == "PIXVALUE") {
	            imgets (infile // "[TIME," // extver // "]", "i_naxis")
	            if (int(imgets.value) == 0) {
	                hselect (temptim, "PIXVALUE", "yes") | scan (pixvalue)
	                hedit (tempsci, "PIXVALUE", pixvalue, add+, del-,\
                               ver-, show-, update+)
	            } else
	                error (0, "Variable exposure time.")
	        }

	        # Append file names into appropriate lists.
	        print (tempsci,  >> listsci)
	        print (temperr,  >> listerr)
	        print (tempdqf,  >> listdqf)
	        print (tempmsk,  >> listmsk)
	        print (tempmsk1, >> listmsk1)
	        print (temprej,  >> listrej)
                if (instrument == "NICMOS") {
	            print (temptim, >> listtim)
	            print (tempsam, >> listsam)
	        }
	    }

	    # Now that all HDUs are isolated as separate files,
            # input everything into gcombine.
	    gcombine ("@" // listsci, toutsci,      \
                      groups     = "1",             \
                      masks      = "@" // listmsk1, \
                      errmap     = "@" // listerr,  \
                      rej_cnt    = touttem,         \
                      rej_list   = "@" // listrej,  \ 
                      out_err    = touterr,         \
                      nsmod_e    = t_nsmode,        \
                      logfile    = "STDOUT",        \
                      reject     = t_reject,        \
                      combine    = t_combine,       \
                      weight     = t_weight,        \
                      nsmod_w    = t_nsmodw,        \
                      outtype    = "r",             \
                      blank      = t_blank,         \
                      scale      = t_scale,         \
                      zero       = t_zero,          \
                      statsec    = t_sec,           \
                      expname    = t_expname,       \
                      darkname   = "",              \
                      lthreshold = t_lthr,          \
                      hthreshold = t_uthr,          \
                      nlow       = t_nlow,          \
                      nhigh      = t_nhigh,         \
                      nkeep      = t_nkeep,         \
                      mclip      = t_mclip,         \
                      lsigma     = t_lsig,          \
                      hsigma     = t_hsig,          \
                      rdnoise    = t_rdnoise,       \
                      gain       = t_gain,          \
                      snoise     = t_snoise)

	    # Assemble output DQ array. It should contain all bit flags
            # that weren't masked out by the global pixmask and whose
            # pixels weren't rejected by gcombine. The logic below is
            # based on gcombine writting rejection maps with 32767 in
            # non-rejected pixels.
	    print ("Building output DQ ...")
	    for (f1 = 1; f1 <= count; f1+=1) {
	        pickfile ("@" // listdqf, f1)
	        tempdqf = pickfile.output
	        pickfile ("@" // listrej, f1)
	        tempmsk = pickfile.output
	        tempmsk1 = mktemp (tempfile) // ".hhh"
                # tempmsk should have 32767 in non-rejected pixels.
	        addmasks (tempdqf // "," // tempmsk, tempmsk1, "im1 && im2", \
                          flags = " ") 
	        print (tempmsk1, >> listmsk2)
	    }
	    mknoise.background = 0.0
	    mknoise (toutdqf)
	    chpixtype (toutdqf, toutdqf, "short", verbose-)
	    tempmsk1 = mktemp (tempfile) // ".hhh"
	    for (f1 = 1; f1 <= count; f1+=1) {
	        pickfile ("@" // listmsk2, f1)
	        tempdqf = pickfile.output
	        imrename (toutdqf, tempmsk1, verbose-)
 	        addmasks (tempdqf // "," // tempmsk1, toutdqf, "im1 || im2", \
                          flags = " ") 
	        imdelete (tempmsk1, verify-)
	    }

	    if (instrument == "NICMOS") {
	        print ("Building output SAMP ...")

	        # Prepare the rejection images to be used as multiplication
                # masks instead of boolean masks. This assumes that gcombine
	        # used the value 32767 for non-masked pixels.
	        for (f1 = 1; f1 <= count; f1+=1) {
	            pickfile ("@" // listrej, f1)
	            infile = pickfile.output
	            imarith (infile, "/", 32767, infile, pixtype="", verbose-)
	            if (f1 == 1) {
	                keypar (infile, "BUNIT", silent=yes)
	                bunit = keypar.value
	            }
	        }

	        # Multiply each SAMP image by its corresponding mask.
	        for (f1 = 1; f1 <= count; f1+=1) {
	            pickfile ("@" // listsam, f1)
	            infile = pickfile.output
	            pickfile ("@" // listrej, f1)
	            infile1 = pickfile.output
	            imarith (infile, "*", infile1, infile, pixtype="", verbose-)
	        }

	        # Add all masked sample images to build the output SAMP.
	        # Start from a clear image and add each sample image in turn.
	        mknoise (toutsam)
	        chpixtype (toutsam, toutsam, "short", verbose-)
	        for (f1 = 1; f1 <= count; f1+=1) {
	            pickfile ("@" // listsam, f1)
	            tempsam = pickfile.output
	            imarith (toutsam, "+", tempsam, toutsam, pixtype="", verb-)
	        }

	        # SAMP extension is type short !
	        chpixtype (toutsam, toutsam, "s", verbose-)

	        # Must explictly add the BUNIT keyword since the final
	        # product may be used by msarith, which checks for BUNIT
	        # inconsistencies.
	        parkey (bunit, toutsam, "BUNIT", add=yes)

	        # This code replaces the above SAMP computation if we want the
	        # output SAMP to contain a report of the number of _images_ 
	        # used to build the result, not the actual total number of 
	        # samples. In this case we don't need the individual rejection 
	        # maps from gcombine.
	        #
	        # The rejection count image must be subtracted from the total
	        # number of input images to generate a "samples" image.
	        # imcalc (touttem, toutsam, str(count) // " - im1")

	        print ("Building output TIME ...")

	        # Multiply each TIME image by its corresponding mask.
	        # This assumes that the masks are already in the 0-1 format.
	        for (f1 = 1; f1 <= count; f1+=1) {
	            pickfile ("@" // listtim, f1)
	            infile = pickfile.output
	            pickfile ("@" // listrej, f1)
	            infile1 = pickfile.output
	            imarith (infile, "*", infile1, infile, pixtype="", verbose-)
	            if (f1 == 1) {
	                keypar (infile, "BUNIT", silent=yes)
	                bunit = keypar.value
	            }	        }

	        # Add all masked time images to build the output TIME.
	        # Start from a clear image and add each time image in turn.
	        mknoise (touttim)
	        for (f1 = 1; f1 <= count; f1+=1) {
	            pickfile ("@" // listtim, f1)
	            temptim = pickfile.output
	            imarith (touttim, "+", temptim, touttim, pixtype="", verb-)
	        }

	        # Divide by the number of images to get average exposure time.
	        # The number of images that were actually used to build
	        # each pixel is the total number of input images minus the
	        # number of images rejected by gcombine.
	        #
	        # This is meant to scale the exposure time of each pixel
	        # in such a way as to be compatible with the output SCI 
	        # pixel, which is either an average or a median of the input 
	        # pixels. The procedure below will generate the "correct"
	        # exposure time	only when gcombine is run with "scale=none". 
	        # The scaling problem can be fully tackled only with a 
	        # full-fledged new gcombine that will scale the individual 
	        # pixel exposure times _inside_ the combining algorithms.
	        imarith (touttem, "-", real(count), touttim1, pixtype="real", \
                         calctype="real", verb-)
	        imcalc (touttim1, touttim2, "-im1", pixtype="real", \ 
                        nullval=0.0, verb-)
	        imarith (touttim, "/", touttim2, touttim, pixtype="real", verb-)

	        # Must explictly add the BUNIT keyword since the final
	        # product may be used by msarith, which checks for BUNIT
	        # inconsistencies.
	        parkey (bunit, touttim, "BUNIT", add=yes)
	    }

	    # Append results into output file. 
	    printf ("Assembling output IMSET %d ...\n", extver)
	    flprcache
	    imcopy (toutsci, t_output // "[SCI," // extver // ",append]", 
                    verb-)
	    imcopy (touterr, t_output // "[ERR," // extver // ",append]", 
                    verb-)
	    imcopy (toutdqf, t_output // "[DQ,"  // extver // ",append]", 
                    verb-)
	    if (instrument == "NICMOS") {
	        imcopy (toutsam, t_output // "[SAMP," // extver // ",append]", 
                        verb-)
	        imcopy (touttim, t_output // "[TIME," // extver // ",append]", 
                        verb-)
	    }

	    # Delete all temporary images and list names for this IMSET.
	    print ("Deleting temporary files ...")
	    imdelete ("@" // listsci,  verify-)
	    imdelete ("@" // listerr,  verify-)
	    imdelete ("@" // listdqf,  verify-)
	    imdelete ("@" // listmsk,  verify-)
	    imdelete ("@" // listmsk1, verify-)
	    imdelete ("@" // listmsk2, verify-)
	    imdelete ("@" // listrej,  verify-)
	    if (instrument == "NICMOS") {
	        imdelete ("@" // listtim, verify-)
	        imdelete ("@" // listsam, verify-)
                imdelete (touttim,  verify-)
                imdelete (touttim1, verify-)
                imdelete (touttim2, verify-)
	    }
	    imdelete (toutsci,  verify-)
	    imdelete (touterr,  verify-)
	    imdelete (toutdqf,  verify-)
	    imdelete (touttem,  verify-)
	    delete (listsci,  verify-)
	    delete (listerr,  verify-)
	    delete (listdqf,  verify-)
	    delete (listmsk,  verify-)
	    delete (listmsk1, verify-)
	    delete (listmsk2, verify-)
	    delete (listrej,  verify-)
	    if (instrument == "NICMOS") {
	        delete (listtim, verify-)
	        delete (listsam, verify-)
	        imdelete (toutsam, verify-)
	    }
	}

	time | scan (timest)
	printf ("Ending MSCOMBINE at %s\n", timest)
end
