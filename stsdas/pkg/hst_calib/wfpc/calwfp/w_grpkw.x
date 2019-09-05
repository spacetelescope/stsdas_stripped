include <imhdr.h>
include <imio.h>
include <error.h>
include "wrdata.h"

#################################################################################
# WR_GROUP1 --	Write 1st set of group parameter values to text file.  		#

procedure wr_group1 ( im, grpnum, fd )

pointer	im			# image with group parameters
int	grpnum			# group number
int	fd			# file descriptor for output text
char	text[SZ_LINE]		# text line buffer
char	groupid[2]		# group ID string (e.g. "_2")

double	crval1, crval2		# WF/PC Science Image Header Group
real	crpix1, crpix2		# Parameter keywords (total of 37)
real	cd1_1, cd1_2, cd2_1, cd2_2
real	datamin, datamax
bool	mir_revr
real	orientat
int	fillcnt, errcnt
double	fpkttime, lpkttime
char	ctype1[8], ctype2[8]

double	imgetd()
real	imgetr()
int	imgeti()
bool	imgetb()

errchk	imgetb, imgeti, imgetr, imgetd, imgstr,
		sprintf, pargi, putline

begin
	if ( grpnum == 1 )		# setup group id string
		call sprintf ( groupid, 2, "_A" )
	else if ( grpnum == 2 )
		call sprintf ( groupid, 2, "_B" )
	else if ( grpnum == 3 )
		call sprintf ( groupid, 2, "_C" )
	else if ( grpnum == 4 )
		call sprintf ( groupid, 2, "_D" )
	else
		call wr_error ( "Bad group number in wr_group" )

	iferr ( crval1   = imgetd ( im, "CRVAL1" ) )
		call wr_kwerr ( "CRVAL1" )
	call sprintf ( text, SZ_LINE, "CRVAL1%2.2s  = %e\n" )
	    call pargstr ( groupid )
	    call pargd ( crval1 )
	call putline ( fd, text )

	iferr ( crval2   = imgetd ( im, "CRVAL2" ) )
		call wr_kwerr ( "CRVAL2" )
	call sprintf ( text, SZ_LINE, "CRVAL2%2.2s  = %e\n" )
	    call pargstr ( groupid )
	    call pargd ( crval2 )
	call putline ( fd, text )

	iferr ( crpix1   = imgetr ( im, "CRPIX1" ) )
		call wr_kwerr ( "CRPIX1" )
	call sprintf ( text, SZ_LINE, "CRPIX1%2.2s  = %e\n" )
	call pargstr ( groupid )
	    call pargr ( crpix1 )
	call putline ( fd, text )

	iferr ( crpix2   = imgetr ( im, "CRPIX2" ) )
		call wr_kwerr ( "CRPIX2" )
	call sprintf ( text, SZ_LINE, "CRPIX2%2.2s  = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( crpix2 )
	call putline ( fd, text )

	iferr ( cd1_1    = imgetr ( im, "CD1_1" ) )
		call wr_kwerr ( "CD1_1" )
	call sprintf ( text, SZ_LINE, "CD1_1%2.2s   = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( cd1_1 )
	call putline ( fd, text )

	iferr ( cd1_2    = imgetr ( im, "CD1_2" ) )
		call wr_kwerr ( "CD1_2" )
	call sprintf ( text, SZ_LINE, "CD1_2%2.2s   = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( cd1_2 )
	call putline ( fd, text )

	iferr ( cd2_1    = imgetr ( im, "CD2_1" ) )
		call wr_kwerr ( "CD2_1" )
	call sprintf ( text, SZ_LINE, "CD2_1%2.2s   = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( cd2_1 )
	call putline ( fd, text )

	iferr ( cd2_2    = imgetr ( im, "CD2_2" ) )
		call wr_kwerr ( "CD2_2" )
	call sprintf ( text, SZ_LINE, "CD2_2%2.2s   = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( cd2_2 )
	call putline ( fd, text )

	iferr ( datamin  = imgetr ( im, "DATAMIN" ) )
		call wr_kwerr ( "DATAMIN" )
	call sprintf ( text, SZ_LINE, "DATAMIN%2.2s = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( datamin )
	call putline ( fd, text )

	iferr ( datamax  = imgetr ( im, "DATAMAX" ) )
		call wr_kwerr ( "DATAMAX" )
	call sprintf ( text, SZ_LINE, "DATAMAX%2.2s = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( datamax )
	call putline ( fd, text )

	iferr ( mir_revr = imgetb ( im, "MIR_REVR" ) )
		call wr_kwerr ( "MIR_REVR" )

#  Insert fix to get around FMTIO bug for boolean values
#	call sprintf ( text, SZ_LINE, "MIR_REVR%2.2s= %b\n" )
	if (mir_revr) 
	    call sprintf ( text, SZ_LINE, "MIR_REVR%2.2s= T\n" )
	else
	    call sprintf ( text, SZ_LINE, "MIR_REVR%2.2s= F\n" )
	    call pargstr ( groupid )
#	    call pargb ( mir_revr )
	call putline ( fd, text )

	iferr ( orientat = imgetr ( im, "ORIENTAT" ) )
		call wr_kwerr ( "ORIENTAT" )
	call sprintf ( text, SZ_LINE, "ORIENTAT%2.2s= %e\n" )
	    call pargstr ( groupid )
	    call pargr ( orientat )
	call putline ( fd, text )

	iferr ( fillcnt  = imgeti ( im, "FILLCNT" ) )
		call wr_kwerr ( "FILLCNT" )
	call sprintf ( text, SZ_LINE, "FILLCNT%2.2s = %d\n" )
	    call pargstr ( groupid )
	    call pargi ( fillcnt )
	call putline ( fd, text )

	iferr ( errcnt   = imgeti ( im, "ERRCNT" ) )
		call wr_kwerr ( "ERRCNT" )
	call sprintf ( text, SZ_LINE, "ERRCNT%2.2s  = %d\n" )
	    call pargstr ( groupid )
	    call pargi ( errcnt )
	call putline ( fd, text )

	iferr ( fpkttime = imgetd ( im, "FPKTTIME" ) )
		call wr_kwerr ( "FPKTTIME" )
	call sprintf ( text, SZ_LINE, "FPKTTIME%2.2s= %e\n" )
	    call pargstr ( groupid )
	    call pargd ( fpkttime )
	call putline ( fd, text )

	iferr ( lpkttime = imgetd ( im, "LPKTTIME" ) )
		call wr_kwerr ( "LPKTTIME" )
	call sprintf ( text, SZ_LINE, "LPKTTIME%2.2s= %e\n" )
	    call pargstr ( groupid )
	    call pargd ( lpkttime )
	call putline ( fd, text )

	iferr ( call imgstr ( im, "CTYPE1", ctype1, 8 ) )
		call wr_kwerr ( "CTYPE1" )
	call sprintf ( text, SZ_LINE, "CTYPE1%2.2s  = %8s\n" )
	    call pargstr ( groupid )
	    call pargstr ( ctype1  )
	call putline ( fd, text )

	iferr ( call imgstr ( im, "CTYPE2", ctype2, 8 ) )
		call wr_kwerr ( "CTYPE2" )
	call sprintf ( text, SZ_LINE, "CTYPE2%2.2s  = %8s\n" )
	    call pargstr ( groupid )
	    call pargstr ( ctype2 )
	call putline ( fd, text )

end

#################################################################################
# WR_GROUP2 --	Write 2nd set of group parameter values to text file.  		#

procedure wr_group2 ( im, grpnum, fd )

#  Calling arguments:
pointer	im			# image descriptor (with group parameters)
int	grpnum			# group number
int	fd			# file descriptor for output text

#  Local variables:
char	text[SZ_LINE]		# text line buffer
char	groupid[2]		# group ID string (e.g. "_2")
int	detector		# WF/PC Science Image Header Group Params
real	dezero, biaseven, biasodd
real	goodmin, goodmax, dmean
int	gpixels, softerrs, calibdef
int	staticd, atodsat, datalost, badpixel
char	photmode[48]
real	photf, photzpt, photp, photbw

#  Functions used:
int     imaccf()		# Test if header keyword exists
real	imgetr()		# Get REAL header keyword
int	imgeti()		# Get INT header keyword

errchk	imgeti, imgetr, imgstr,	sprintf, pargi, putline

begin
	if ( grpnum == 1 )		# setup group id string
		call sprintf ( groupid, 2, "_A" )
	else if ( grpnum == 2 )
		call sprintf ( groupid, 2, "_B" )
	else if ( grpnum == 3 )
		call sprintf ( groupid, 2, "_C" )
	else if ( grpnum == 4 )
		call sprintf ( groupid, 2, "_D" )
	else
		call wr_error ( "Bad group number in wr_group" )

	iferr ( detector = imgeti ( im, "DETECTOR" ) )
		call wr_kwerr ( "DETECTOR" )
	call sprintf ( text, SZ_LINE, "DETECTOR%2.2s= %d\n" )
	    call pargstr ( groupid )
	    call pargi ( detector )
	call putline ( fd, text )

	iferr ( dezero = imgetr ( im, "DEZERO" ) )
		call wr_kwerr ( "DZERO" )
	call sprintf ( text, SZ_LINE, "DEZERO%2.2s  = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( dezero )
	call putline ( fd, text )

	if ( (imaccf (im, "BIASEVEN") == NO) && 
		(imaccf (im, "BIASODD") == NO) ) 
	    call wr_message (
		"Cannot write BIASEVEN, BIASODD keywords to ASCII file")
	else {
	    iferr ( biaseven = imgetr ( im, "BIASEVEN" ) ) 
		call wr_kwerr ( "BIASEVEN" )
	    call sprintf ( text, SZ_LINE, "BIASEVEN%2.2s= %e\n" )
	        call pargstr ( groupid )
	        call pargr ( biaseven )
	    call putline ( fd, text )

	    iferr ( biasodd = imgetr ( im, "BIASODD" ) )
		call wr_kwerr ( "BIASODD" )
	    call sprintf ( text, SZ_LINE, "BIASODD%2.2s = %e\n" )
	        call pargstr ( groupid )
	        call pargr ( biasodd )
	    call putline ( fd, text )
	}

	iferr ( goodmin  = imgetr ( im, "GOODMIN" ) )
		call wr_kwerr ( "GOODMIN" )
	call sprintf ( text, SZ_LINE, "GOODMIN%2.2s = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( goodmin )
	call putline ( fd, text )

	iferr ( goodmax  = imgetr ( im, "GOODMAX" ) )
		call wr_kwerr ( "GOODMAX" )
	call sprintf ( text, SZ_LINE, "GOODMAX%2.2s = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( goodmax )
	call putline ( fd, text )

	iferr ( dmean = imgetr ( im, "DATAMEAN" ) )
		call wr_kwerr ( "DATAMEAN" )
	call sprintf ( text, SZ_LINE, "DATAMEAN%2.2s= %e\n" )
	    call pargstr ( groupid )
	    call pargr ( dmean )
	call putline ( fd, text )

	iferr ( gpixels  = imgeti ( im, "GPIXELS" ) )
		call wr_kwerr ( "GPIXELS" )
	call sprintf ( text, SZ_LINE, "GPIXELS%2.2s = %d\n" )
	    call pargstr ( groupid )
	    call pargi ( gpixels )
	call putline ( fd, text )

	iferr ( softerrs = imgeti ( im, "SOFTERRS" ) )
		call wr_kwerr ( "SOFTERRS" )
	call sprintf ( text, SZ_LINE, "SOFTERRS%2.2s= %d\n" )
	    call pargstr ( groupid )
	    call pargi ( softerrs )
	call putline ( fd, text )

	iferr ( calibdef = imgeti ( im, "CALIBDEF" ) )
		call wr_kwerr ( "CALIBDEF" )
	call sprintf ( text, SZ_LINE, "CALIBDEF%2.2s= %d\n" )
	    call pargstr ( groupid )
	    call pargi ( calibdef )
	call putline ( fd, text )

	iferr ( staticd  = imgeti ( im, "STATICD" ) )
		call wr_kwerr ( "STATICD" )
	call sprintf ( text, SZ_LINE, "STATICD%2.2s = %d\n" )
	    call pargstr ( groupid )
	    call pargi ( staticd )
	call putline ( fd, text )

	iferr ( atodsat  = imgeti ( im, "ATODSAT" ) )
		call wr_kwerr ( "ATODSAT" )
	call sprintf ( text, SZ_LINE, "ATODSAT%2.2s = %d\n" )
	    call pargstr ( groupid )
	    call pargi ( atodsat )
	call putline ( fd, text )

	iferr ( datalost = imgeti ( im, "DATALOST" ) )
		call wr_kwerr ( "DATALOST" )
	call sprintf ( text, SZ_LINE, "DATALOST%2.2s= %d\n" )
	    call pargstr ( groupid )
	    call pargi ( datalost )
	call putline ( fd, text )

	iferr ( badpixel = imgeti ( im, "BADPIXEL" ) )
		call wr_kwerr ( "BADPIXEL" )
	call sprintf ( text, SZ_LINE, "BADPIXEL%2.2s= %d\n" )
	    call pargstr ( groupid )
	    call pargi ( badpixel )
	call putline ( fd, text )

	iferr ( call imgstr ( im, "PHOTMODE", photmode, 48 ) )
		call wr_kwerr ( "PHOTMODE" )
	call sprintf ( text, SZ_LINE, "PHOTMODE%2.2s= %48s\n" )
	    call pargstr ( groupid )
	    call pargstr ( photmode )
	call putline ( fd, text )

	iferr ( photf = imgetr ( im, "PHOTFLAM" ) )
		call wr_kwerr ( "PHOTFLAM" )
	call sprintf ( text, SZ_LINE, "PHOTFLAM%2.2s= %e\n" )
	    call pargstr ( groupid )
	    call pargr ( photf )
	call putline ( fd, text )

	iferr ( photzpt  = imgetr ( im, "PHOTZPT" ) )
		call wr_kwerr ( "PHOTZPT" )
	call sprintf ( text, SZ_LINE, "PHOTZPT%2.2s = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( photzpt )
	call putline ( fd, text )

	iferr ( photp = imgetr ( im, "PHOTPLAM" ) )
		call wr_kwerr ( "PHOTPLAM" )
	call sprintf ( text, SZ_LINE, "PHOTPLAM%2.2s= %e\n" )
	    call pargstr ( groupid )
	    call pargr ( photp )
	call putline ( fd, text )

	iferr ( photbw   = imgetr ( im, "PHOTBW" ) )
		call wr_kwerr ( "PHOTBW" )
	call sprintf ( text, SZ_LINE, "PHOTBW%2.2s  = %e\n" )
	    call pargstr ( groupid )
	    call pargr ( photbw )
	call putline ( fd, text )

end



