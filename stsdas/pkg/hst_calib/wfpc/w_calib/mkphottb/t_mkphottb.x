include	"mkphottb.h"

#  MKPHOTTB -- Create a synthetic photometry table for WFPC pipeline use
#
#  "Dark with excessive bright"
#						-John Milton
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#  "infile"		Input science data file template name
#  "outtable"		name of the output photometry calibration table
#
#  "graphtab"		name of the graph table used
#  "comptab"		name of the component table used
#
#  Date		Author			Description
#  ----		------			-----------
#  06-Aug-1991  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure t_mkphottb ()

int	fd			# file descriptor
char	infile[SZ_FNAME], outtable[SZ_FNAME]
char	comptab[SZ_FNAME], graphtab[SZ_FNAME]
real	phot[4]
char	path[SZ_COMMAND]
char	mess[SZ_LINE]
char	linebuf[SZ_LINE]
int	nrows, k
int	detector
char	photmode[SZ_PHOTMODE]
char	camera[SZ_CAMERA]
char	filtnam1[SZ_FILTNAM1]
char	filtnam2[SZ_FILTNAM2]
char	flat[SZ_FLAT]
pointer	tp				# output table descripter
pointer	colidn[NCOL]			# column descripters

pointer	open() 
int	getline()
int	nowhite()
#==============================================================================
begin

	# get CL parameters
	call clgstr ("infile", infile, SZ_FNAME)
	call clgstr ("outtable", outtable, SZ_FNAME)
	call clgstr ("graphtab", graphtab, SZ_FNAME)
	call clgstr ("comptab", comptab, SZ_FNAME)

	# open the input text file 
	fd = open (infile, READ_ONLY, TEXT_FILE)

	# open the output table
	call mkph_out (outtable, tp, colidn)

	# process each line in the input file
	nrows = 0
	while (getline (fd, linebuf) != EOF) {
	    k = nowhite (linebuf, photmode, SZ_PHOTMODE)

	    # skip the blank lines
	    if (photmode[1] == EOS) next
	    nrows = nrows + 1
	    call strupr (photmode)

	    # dissect photmode into components
	    iferr (call mkph_dissect (photmode, camera, detector, 
			filtnam1, filtnam2, flat)) {
		call sprintf (mess, SZ_LINE, 
			"Photmode '%s' has incorrect syntax")
		    call pargstr (photmode)
		call error (1, mess)
	    }

	    # calculate the photometric quantities
	    call getphotx (photmode, graphtab, comptab, path, SZ_COMMAND, phot)

	    # write one row to the output table
	    call tbrptt (tp, colidn[ID_PHOTMODE], photmode, SZ_PHOTMODE, 1, 
				nrows)
	    call tbrptr (tp, colidn[ID_PHOTFLAM], phot[1], 1, nrows)
	    call tbrptr (tp, colidn[ID_PHOTZPT], phot[2], 1, nrows)
	    call tbrptr (tp, colidn[ID_PHOTPLAM], phot[3], 1, nrows)
	    call tbrptr (tp, colidn[ID_PHOTBW], phot[4], 1, nrows)
	    call tbrptt (tp, colidn[ID_CAMERA], camera, SZ_CAMERA, 1, nrows)
	    call tbrpti (tp, colidn[ID_DETECTOR], detector, 1, nrows)
	    call tbrptt (tp, colidn[ID_FILTNAM1], filtnam1, SZ_FILTNAM1, 1, 
				nrows)
	    call tbrptt (tp, colidn[ID_FILTNAM2], filtnam2, SZ_FILTNAM2, 1, 
				nrows)
	    call tbrptt (tp, colidn[ID_FLAT], flat, SZ_FLAT, 1, nrows)
	}

	# close the input file and the output table
	call close (fd)
	call tbtclo (tp)
end
