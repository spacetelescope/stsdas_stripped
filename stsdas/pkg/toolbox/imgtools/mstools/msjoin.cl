################################################################################
#
# MSJOIN - IRAF Script procedure to join HST pipeline multiextension 
# FITS files. To join means to reconstruct a single FITS 
# file from individual FITS files that previously had been split using
# MSSPLIT.  An IMSET consists of associated data arrays that have 
# been stored as FITS image extensions.  The NICMOS and WFC3/IR IMSETs 
# consist of (SCI, ERR, DQI, SAMP, and TIME); the STIS, ACS, and WFC3/UVIS
# IMSETs consist of (SCI, ERR, DQI).
#
# Version    Author          Date
# -------------------------------
#   1.0      M. De La Pena   29 May 1997
#   1.1      M. De Le Pena   12 June 1997: Mods necessary to conform to the
#                            updated FITS kernel.
#   1.2      M. De La Pena   01 July 1997: Further mods for updated kernel.
#   1.3      M. De La Pena   22 Aug  1997: On output, specify EXTNAME & EXTVER.
#   1.4      H. Bushouse     01 May  2007: Add WFC3 support.
#
################################################################################

procedure msjoin (inimg, outimg)

file   inimg               {prompt = "Images to join"}
file   outimg              {prompt = "Output image name"}

struct *din_list           {prompt = "Internal script use only"}
string Version = "May07"   {prompt = "Date of installation"}

begin

    # Declarations.
    file in_images, out_images
    file in_file_list, infile 
    file out_file_list, outfile

    string instrument                # FITS INSTRUMENT keyword value.
    string detector                  # FITS DETECTOR keyword value.
    int    exten                     # Current EXTVER being processed.
    int    noprimary                 # Flag to copy PHU of first file.
    int    as_nextend                # New NEXTEND value for assembled file.
    int    NICMOS_EXT, STIS_EXT      # Number of extensions in an IMSET.
    
    # Assign the number of known image extensions per IMSET for this data.
    NICMOS_EXT = 5
    STIS_EXT   = 3

    # Reassign the input parameter values.
    in_images  = inimg
    out_images = outimg

    # Join the specified files.
    # EXTVER will begin incrementing from 1 in the new output file.
    exten = 0

    # Expand the input image template into a text file list.
    in_file_list = mktemp ("tmp$InList")
    files (in_images, sort-, > in_file_list)

    # Scan through the list of images.
    din_list   = in_file_list
    noprimary  = 1
    as_nextend = 0
    while (fscan (din_list, infile) != EOF) {

        # Increment the EXTVER for the new file.
        exten += 1

        # Determine which instrument.
        imgets (infile // "[0]", "INSTRUME")
        instrument = imgets.value

	# Determine which WFC3 detector.
	if (instrument == "WFC3") {
	    imgets (infile // "[0]", "DETECTOR")
	    detector = imgets.value
	    if (detector == "IR") {
		instrument = "WFC3IR"
	    }
	}

        # Create the output file and copy the Primary Header Unit from
        # only the first input file in the list.
        if (noprimary == 1) {
            imcopy (infile // "[0]", out_images, verbose+, mode = "ql")
            noprimary = 0
        }

        # Copy all the associated the image extensions.
        imcopy (infile // "[SCI, noinherit]", out_images // 
                "[SCI," // exten // ",inherit,append]", verbose-, 
                mode = "ql")
        imcopy (infile // "[ERR, noinherit]", out_images // 
                "[ERR," // exten // ",inherit,append]", verbose-, 
                mode = "ql")
        imcopy (infile // "[DQ, noinherit]", out_images // 
                "[DQ," // exten // ",inherit,append]", verbose-, 
                mode = "ql")

        # Increment the count of the number of image extensions
        # added thus far.
        as_nextend += 3

        if (instrument == "NICMOS" || instrument == "WFC3IR") {
            imcopy (infile // "[SAMP, noinherit]", out_images // 
                    "[SAMP," // exten // ",inherit,append]", verbose-,
                    mode = "ql")
            imcopy (infile // "[TIME, noinherit]", out_images // 
                    "[TIME," // exten // ",inherit,append]", verbose-, 
                    mode = "ql")

            # Increment the count of the number of image extensions
            # to accommodate the additional NICMOS extensions.
            as_nextend += 2
        }
    }

    # Update the FITS keywords.
    hedit (out_images // "[0]", field="FILENAME", value=out_images, 
           add-, delete-, verify-, show-, update+, mode="ql")
    hedit (out_images // "[0]", field="NEXTEND", value=as_nextend,
           add-, delete-, verify-, show+, update+, mode="ql")

    # Clean up.
    din_list = ""
    delete (in_file_list, ver-, >& "dev$null")
end
