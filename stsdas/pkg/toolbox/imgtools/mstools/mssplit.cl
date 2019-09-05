################################################################################
#
# MSSPLIT - IRAF Script procedure to split HST pipeline multiextension FITS 
# files, in particular for NICMOS, STIS, ACS, and WFC3.  To split means to 
# extract designated IMSETs 
# from the input image and write the IMSETs out to new files, one IMSET per 
# file.  An IMSET consists of associated data arrays which have 
# been stored as FITS image extensions.  NICMOS and WFC3/IR IMSETs consist of 
# (SCI, ERR, DQI, SAMP, and TIME); all other IMSETs consist of (SCI, ERR, DQI).
#
# Version    Author          Date
# -------------------------------
#   1.0      M. De La Pena   29 May 1997
#   1.1      M. De La Pena   12 June 1997: Mods necessary to conform to the
#                            updated FITS kernel.
#   1.2      M. De La Pena   01 July 1997: Further mods for updated kernel.
#   1.3      H. Bushouse     01 May  2007: Added WFC3 support.              
#
################################################################################

procedure mssplit (inimg, outimg)

file   inimg              {prompt = "Image from which IMSETs will be split"}
file   outimg             {prompt = "Output image name(s)"}
string extension = "1"    {prompt = "Selected FITS image EXTVER(s) to be split"}
bool   retain = no        {prompt = "Retain the original EXTVER value?"}

struct *dout_list         {prompt = "Internal script use only"}
struct *dext_list         {prompt = "Internal script use only"}
string Version = "May07"  {prompt = "Date of installation"}

begin

    # Declarations.
    bool ext_retain
    file in_images, out_images
    file in_file_list, infile 
    file out_file_list, outfile
    string exten_opt, ext_file_list

    string instrument                # FITS INSTRUMENT keyword value.
    string detector                  # FITS DETECTOR keyword value.
    string root                      # Root name of input file.
    int    exten                     # Current EXTVER being processed.
    int    exten_out                 # EXTVER for output IMSET.
    int    ex_nextend                # Original NEXTEND value for split check.
    int    NICMOS_EXT, STIS_EXT      # Number of extensions in an IMSET.
    int    nimages                   # Number of images in expansion list.
    int    nsections                 # Number of EXTVER to expand.
    int    nread                     # Number of values read by fscan.
    
    # Assign the number of known image extensions per IMSET for this data.
    NICMOS_EXT = 5
    STIS_EXT   = 3

    # Reassign the input parameter values.
    in_images  = inimg
    out_images = outimg
    exten_opt  = extension
    ext_retain = retain

    # Determine which instrument and the number of EXTVERs in the file.
    imgets (in_images // "[0]", "INSTRUME")
    instrument = imgets.value
    imgets (in_images // "[0]", "NEXTEND")
    ex_nextend = int(imgets.value)
    if (instrument == "NICMOS")
        ex_nextend /= 5
    else if (instrument == "WFC3") {
        imgets (in_images // "[0]", "DETECTOR")
	detector = imgets.value
	if (detector == "IR") {
	    instrument = "WFC3IR"
	    ex_nextend /= 5
	} else
	    ex_nextend /= 3
    } else
        ex_nextend /= 3

    # Expand the output image template into a text file list.
    out_file_list = mktemp ("tmp$OutList")
    sections (out_images, option = "fullname", > out_file_list)

    # Check the number of image names to use for the expansion.
    # If nimages == 0, then use the root name concatenated with the
    # EXTVER as the name of the output files.
    nimages = sections.nimages
    root    = ""
    if (nimages == 0) 
        root = substr(in_images, 1, strlen(in_images) - 5)
    else
        dout_list = out_file_list

    # Expand the list of the EXTVER into a text file list.
    ext_file_list = mktemp ("tmp$ExtList")
    sections (exten_opt, > ext_file_list)

    # Check the number of EXTVER to expand.  If it is equal to an
    # asterisk, expand all the IMSETs.  Otherwise, generate a list file.
    # If expanding all IMSETs and no output files were designed, set
    # the number of output files to the number of IMSETs in the input image.
    # If expanding specified number of IMSETs and no output files were
    # designated, set the number of output files to the number of specified
    # IMSETs.
    if (extension == "*") {
        nsections = ex_nextend
        if (nimages == 0) 
            nimages = ex_nextend
    }
    else {
        nsections = sections.nimages
        if (nimages == 0) 
            nimages = nsections
        dext_list = ext_file_list
    }

    # Check the number of output files and the number of EXTVERs specified
    # is consistent.
    if (nimages != nsections)
        error (1, "Number of output images != number of specified EXTVERs.")
       
    # Process the number of requested EXTVERs.
    # 'exten' is a counter as it assumes the EXTVERs present in the file.
    # 'exten_out' is the EXTVER for the output IMSETs - default is 1.
    exten     = 0
    exten_out = 1
    for (i = 0; i < nsections; i += 1) {

         if (extension == "*")
             exten += 1
         else
             nread = fscan (dext_list, exten)
 
         # If RETAIN == YES, then set 'exten_out' equal to 'exten'
         # which is actually a counter.  The default is 'exten_out = 1'.
         if (retain)
             exten_out = exten
                
         if (root != "") {
             if (exten < 10) 
                 outfile = root // "0" // exten // ".fits"
             else
                 outfile = root // exten // ".fits"
         }
         else
             nread = fscan (dout_list, outfile)

         # Check the EXTVER specified does not exceed the actual number
         # of those available in the file.
         if (exten > ex_nextend) 
             error (1, "Specified EXTVER exceeds the maximum EXTVER of the input file.")

         # Create the output file and capture the desired image extensions.
         imcopy (in_images // "[0]", outfile, verbose+, mode = "ql")
         imcopy (in_images // "[SCI," // exten // ",noinherit]", 
                 outfile // "[SCI," // exten_out // ",inherit,append]",
                 verbose-, mode = "ql")
         imcopy (in_images // "[ERR," // exten // ",noinherit]", 
                 outfile // "[ERR," // exten_out // ",inherit,append]", 
                 verbose-, mode = "ql")
         imcopy (in_images // "[DQ," // exten // ",noinherit]", 
                 outfile // "[DQ," // exten_out // ",inherit,append]", 
                 verbose-, mode = "ql")

         if (instrument == "NICMOS" || instrument == "WFC3IR") {
             imcopy (in_images // "[SAMP," // exten // ",noinherit]", 
                     outfile // "[SAMP," // exten_out // ",inherit,append]", 
                     verbose-, mode = "ql")
             imcopy (in_images // "[TIME," // exten // ",noinherit]", 
                     outfile // "[TIME," // exten_out // ",inherit,append]", 
                     verbose-, mode = "ql")

             # Update the NEXTEND FITS keyword for NICMOS and WFC3/IR.
             hedit (outfile // "[0]", field="NEXTEND", value=NICMOS_EXT, 
                    add-, delete-, verify-, show-, update+, mode="ql")
         }
         else {
             # Update the NEXTEND FITS keyword.
             hedit (outfile // "[0]", field="NEXTEND", value=STIS_EXT, 
                    add-, delete-, verify-, show-, update+, mode="ql")
         }

         # Update the FILENAME FITS keyword.
         hedit (outfile // "[0]", field="FILENAME", value=outfile, 
                add-, delete-, verify-, show-, update+, mode="ql")
    }

        # Clean up.
        dout_list = ""
        dext_list = ""
        delete (out_file_list, ver-, >& "dev$null")
        delete (ext_file_list, ver-, >& "dev$null")
end
