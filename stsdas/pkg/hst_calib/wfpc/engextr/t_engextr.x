# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"
include	<imhdr.h>

#  t_engextr -- Extract WFPC engineering information
#
#  "Wit's an unruly engine, wildly striking
#  Sometimes a friend, sometimes the engineer."
#						- George Herbert
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#
#  Date		Author			Description
#  ----		------			-----------
#  01-May-1991  J.-C. Hsu		rewrite in SPP
#------------------------------------------------------------------------------

procedure t_engextr ()

pointer	fin, fout	 			# file template pointer
char	outtbl[SZ_FNAME]
char	outdir[SZ_PATHNAME]
int	nfin, i
int	outflag
int	loc[NCMD]
int	errval
bool	verbose

pointer	imtopen()
int	imtlen()
int	isdirectory()
#==============================================================================
begin

	# read CL parameters
	call engextr_in (fin, outtbl, errval, loc, verbose)
	nfin = imtlen (fin)

	if (nfin < 1)
	    call error (1, "no input file specified/input file does not exist")

	# find out if OUTTBL is a directory name or a file template or
	# unspecified
	if (outtbl[1] == EOS)
	    call error (1, "no output table specified")
	else if (isdirectory(outtbl, outdir, SZ_PATHNAME) > 0)
	    outflag = DIR
	else {
	    fout = imtopen (outtbl)

	    # number of output tables must be the same as that of input files
	    if (imtlen(fout) == nfin)
		outflag = TEMPL
	    else
		call error (1, 
			"number of files in input and output do not match")
	}

	# extract engineering data for each file
	do i = 1, nfin {
	    call engextr_do (fin, outflag, fout, outdir, errval, loc, verbose)
	}
	
	# close file templates
	call imtclose (fin)
	if (outflag == TEMPL)
	    call imtclose (fout)
end
