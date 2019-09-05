#
#  BLOATHDU  --  De-compresses a compressed FITS HDU.
#
#
#
#
#  28-Mar-97:  Task created (I. Busko)
#  04-Apr-97:  Directory for temporaries (IB)
#  06-Aug-97:  Added ".hhh" extension to output file name (IB)
#  01-Dec-97:  Temporary GEIS file for moveheader (IB)
#  21-Aug-98:  Eliminated use of moveheader; have mknoise read input header
#              instead (H. Bushouse)

procedure bloathdu (input, output)

file	input    = ""        {prompt=">FITS HDU to decompress"}
file	output   = ""        {prompt=">Output file name"}
file	dirtemp  = "tmp$"    {prompt=">Directory for temporary files"}
char	version  = "21Aug98" {prompt=">Date of installation"}

begin
	file t_input, t_output, t_drc, tempfile
	char msg, pixvalue
	int  nx1, nx2

	# Check for the presence of pre-requisite tasks/packages.
	msg = ""
	if (!deftask("mknoise"))  msg = msg // " artdata"
	if (!deftask("gcombine")) msg = msg // " imgtools"
	if (strlen(msg) > 0) {
	    printf ("Please, load packages: %s\n", msg)
	    bye
	}

	t_input   = input
	t_output  = output
	t_drc     = dirtemp
	pixvalue = ""
	nx1 = 0
	nx2 = 0
	tempfile = t_drc // "bloat"

	# The output name must have a .hhh extension,
        # otherwise gcombine can't handle it.
	fparse (t_output, verbose-)
	if (fparse.extension != ".hhh")
	    t_output = fparse.directory // fparse.root // ".hhh"

	# Get NAXIS value. 
	imgets (t_input, "i_naxis")
	if (int(imgets.value) == 2) {

	    # Just copy the input, it is not compressed.
	    imcopy (t_input, t_output, verbose-)

	} else if (int(imgets.value) == 0) {

	    # Compressed HDU: use mknoise to build a pixel array of the
	    # proper size and filled up with pixvalue, using the header
	    # from the input file to populate the output file header.
	    hselect (t_input, "PIXVALUE", "yes") | scan (pixvalue)
	    hselect (t_input, "NPIX1", "yes")  | scan (nx1)
	    hselect (t_input, "NPIX2", "yes")  | scan (nx2)
	    unlearn mknoise
	    mknoise.ncols = nx1
	    mknoise.nlines = nx2
	    mknoise.background = real(pixvalue)
	    mknoise.rdnoise = 0.
	    mknoise.poisson = no
	    mknoise.header = t_input
	    mknoise (t_output)

	} else
	    error (0, "Invalid NAXIS value.")
end
