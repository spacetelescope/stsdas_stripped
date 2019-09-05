#			File:	u_grpkw.x

include <imhdr.h>
include <error.h>
include	"u_data.h"

#################################################################################
#										#
# U_WRT_GRP1 --	Write group parameter values to text file (first set).  	#
#		This routine is split into 3 procedures to get around IRAF 	#
#		limitation on number of string constants.  			#
#										#
#  Last Modified: 								#
#	11 Aug 1992 by RAShaw	Initial implementation				#
#	 4 Oct 1993 by CYZhang	Get DATAMIN and DATAMAX directly 		#
#				from IM_MIN and IM_MAX				#

procedure u_wrt_grp1 (im, grpnum, fd)

#  Calling arguments
pointer	im			# image with group parameters
int	grpnum			# group number
int	fd			# file descriptor for .CGR text

#  Local variables:
bool	bval			# Working variables:	TY_BOOL
char	cval[SZ_PHOT]		#			TY_CHAR
double	dval			# 			TY_DOUBLE
real	rval			#			TY_REAL
int	ival			#			TY_INT
char	text[SZ_FNAME]		# text line buffer

#  Functions used:
bool	imgetb()		# Get keyword value from header
double	imgetd()		# 
real	imgetr()		# 
int	imgeti()		# 

errchk	imgetb, imgeti, imgetr, imgetd, imgstr, sprintf, putline

begin

	rval = IM_MIN(im)
#	iferr ( rval = imgetr (im, "DATAMIN") )
#	    call u_kwerr ("DATAMIN")
	call sprintf (text, SZ_FNAME, "DATAMIN_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	rval = IM_MAX(im)
#	iferr ( rval = imgetr (im, "DATAMAX") )
#	    call u_kwerr ("DATAMAX")
	call sprintf (text, SZ_FNAME, "DATAMAX_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( bval = imgetb (im, "MIR_REVR") )
	    call u_kwerr ("MIR_REVR")

#  Insert if-test to get around FMTIO bug for boolean values
	if (bval) 
	    call sprintf (text, SZ_FNAME, "MIR_REVR_%1d= T\n")
	else
	    call sprintf (text, SZ_FNAME, "MIR_REVR_%1d= F\n")
	call pargi (grpnum)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "ORIENTAT") )
	    call u_kwerr ("ORIENTAT")
	call sprintf (text, SZ_FNAME, "ORIENTAT_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "FILLCNT") )
	    call u_kwerr ("FILLCNT")
	call sprintf (text, SZ_FNAME, "FILLCNT_%1d = %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "ERRCNT") )
	    call u_kwerr ("ERRCNT")
	call sprintf (text, SZ_FNAME, "ERRCNT_%1d  = %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( dval = imgetd (im, "FPKTTIME") )
	    call u_kwerr ("FPKTTIME")
	call sprintf (text, SZ_FNAME, "FPKTTIME_%1d= %e\n")
	    call pargi (grpnum)
	    call pargd (dval)
	call putline (fd, text)

	iferr ( dval = imgetd (im, "LPKTTIME") )
	    call u_kwerr ("LPKTTIME")
	call sprintf (text, SZ_FNAME, "LPKTTIME_%1d= %e\n")
	    call pargi (grpnum)
	    call pargd (dval)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "DETECTOR") )
	    call u_kwerr ("DETECTOR")
	call sprintf (text, SZ_FNAME, "DETECTOR_%1d= %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "DATAMEAN") )
	    call u_kwerr ("DATAMEAN")
	call sprintf (text, SZ_FNAME, "DATAMEAN_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( call imgstr (im, "CTYPE1", cval, 8) )
	    call u_kwerr ("CTYPE1")
	call sprintf (text, SZ_FNAME, "CTYPE1_%1d  = %8s\n")
	    call pargi (grpnum)
	    call pargstr (cval)
	call putline (fd, text)

	iferr ( call imgstr (im, "CTYPE2", cval, 8) )
	    call u_kwerr ("CTYPE2")
	call sprintf (text, SZ_FNAME, "CTYPE2_%1d  = %8s\n")
	    call pargi (grpnum)
	    call pargstr (cval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "CRPIX1") )
	    call u_kwerr ("CRPIX1")
	call sprintf (text, SZ_FNAME, "CRPIX1_%1d  = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "CRPIX2") )
	    call u_kwerr ("CRPIX2")
	call sprintf (text, SZ_FNAME, "CRPIX2_%1d  = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr (dval = imgetd (im, "CRVAL1"))
	    call u_kwerr ("CRVAL1")
	call sprintf (text, SZ_FNAME, "CRVAL1_%1d  = %e\n")
	    call pargi (grpnum)
	    call pargd (dval)
	call putline (fd, text)

	iferr (dval = imgetd (im, "CRVAL2"))
	    call u_kwerr ("CRVAL2")
	call sprintf (text, SZ_FNAME, "CRVAL2_%1d  = %e\n")
	    call pargi (grpnum)
	    call pargd (dval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "CD1_1") )
	    call u_kwerr ("CD1_1")
	call sprintf (text, SZ_FNAME, "CD1_1_%1d   = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "CD1_2") )
	    call u_kwerr ("CD1_2")
	call sprintf (text, SZ_FNAME, "CD1_2_%1d   = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "CD2_1") )
	    call u_kwerr ("CD2_1")
	call sprintf (text, SZ_FNAME, "CD2_1_%1d   = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "CD2_2") )
	    call u_kwerr ("CD2_2")
	call sprintf (text, SZ_FNAME, "CD2_2_%1d   = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

end


#################################################################################
#										#
# U_WRT_GRP2 --	Write group parameter values to text file (second set).  	#
#		This routine is split into 3 procedures to get around IRAF 	#
#		limitation on number of string constants.  			#
#										#
#  Last Modified: 								#
#	11 Aug 1992 by RAShaw	Initial implementation				#

procedure u_wrt_grp2 (im, grpnum, fd)

#  Calling arguments
pointer	im			# image with group parameters
int	grpnum			# group number
int	fd			# file descriptor for .CGR text

#  Local variables:
char	cval[SZ_PHOT]		# Working variables:	TY_CHAR
real	rval			#			TY_REAL
int	ival			#			TY_INT
char	text[SZ_FNAME]		# text line buffer

#  Functions used:
real	imgetr()		# Get keyword value from header
int	imgeti()		# 

errchk	imgetb, imgeti, imgetr, imgetd, imgstr, sprintf, putline

begin

	iferr ( rval = imgetr (im, "DEZERO") )
	    call u_kwerr ("DEZERO")
	call sprintf (text, SZ_FNAME, "DEZERO_%1d  = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "BIASEVEN") ) 
	    call u_kwerr ("BIASEVEN")
	call sprintf (text, SZ_FNAME, "BIASEVEN_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "BIASODD") )
	    call u_kwerr ("BIASODD")
	call sprintf (text, SZ_FNAME, "BIASODD_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "GOODMIN") )
	    call u_kwerr ("GOODMIN")
	call sprintf (text, SZ_FNAME, "GOODMIN_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "GOODMAX") )
	    call u_kwerr ("GOODMAX")
	call sprintf (text, SZ_FNAME, "GOODMAX_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "GPIXELS") )
	    call u_kwerr ("GPIXELS")
	call sprintf (text, SZ_FNAME, "GPIXELS_%1d = %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "SOFTERRS") )
	    call u_kwerr ("SOFTERRS")
	call sprintf (text, SZ_FNAME, "SOFTERRS_%1d= %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "CALIBDEF") )
	    call u_kwerr ("CALIBDEF")
	call sprintf (text, SZ_FNAME, "CALIBDEF_%1d= %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "STATICD") )
	    call u_kwerr ("STATICD")
	call sprintf (text, SZ_FNAME, "STATICD_%1d = %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "ATODSAT") )
	    call u_kwerr ("ATODSAT")
	call sprintf (text, SZ_FNAME, "ATODSAT_%1d = %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "DATALOST") )
	    call u_kwerr ("DATALOST")
	call sprintf (text, SZ_FNAME, "DATALOST_%1d= %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "BADPIXEL") )
	    call u_kwerr ("BADPIXEL")
	call sprintf (text, SZ_FNAME, "BADPIXEL_%1d= %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( ival = imgeti (im, "OVERLAP") )
	    call u_kwerr ("OVERLAP")
	call sprintf (text, SZ_FNAME, "OVERLAP_%1d = %d\n")
	    call pargi (grpnum)
	    call pargi (ival)
	call putline (fd, text)

	iferr ( call imgstr (im, "PHOTMODE", cval, SZ_PHOT) )
	    call u_kwerr ("PHOTMODE")
	call sprintf (text, SZ_FNAME, "PHOTMODE_%1d= %s\n")
	    call pargi (grpnum)
	    call pargstr (cval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "PHOTFLAM") )
	    call u_kwerr ("PHOTFLAM")
	call sprintf (text, SZ_FNAME, "PHOTFLAM_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "PHOTZPT") )
	    call u_kwerr ("PHOTZPT")
	call sprintf (text, SZ_FNAME, "PHOTZPT_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "PHOTPLAM") )
	    call u_kwerr ("PHOTPLAM")
	call sprintf (text, SZ_FNAME, "PHOTPLAM_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "PHOTBW") )
	    call u_kwerr ("PHOTBW")
	call sprintf (text, SZ_FNAME, "PHOTBW_%1d  = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

end

#################################################################################
#										#
# U_WRT_GRP3 --	Write group parameter values to text file (third set).  	#
#		This routine is split into 3 procedures to get around IRAF 	#
#		limitation on number of string constants.  			#
#										#
#  Last Modified: 								#
#	 2 Sep 1993 by CYZhang	Initial implementation				#

procedure u_wrt_grp3 (im, grpnum, fd)

#  Calling arguments
pointer	im			# image with group parameters
int	grpnum			# group number
int	fd			# file descriptor for .CGR text

#  Local variables:
real	rval			#			TY_REAL
char	text[SZ_FNAME]		# text line buffer

#  Functions used:
real	imgetr()		# Get keyword value from header

errchk	imgetb, imgeti, imgetr, imgetd, imgstr, sprintf, putline,
	    u_kwerr

begin

	iferr ( rval = imgetr (im, "MEDIAN") )
	    call u_kwerr ("MEDIAN")
	call sprintf (text, SZ_FNAME, "MEDIAN_%1d  = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "MEDSHADO") )
	    call u_kwerr ("MEDSHADO")
	call sprintf (text, SZ_FNAME, "MEDSHADO_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "HISTWIDE") )
	    call u_kwerr ("HISTWIDE")
	call sprintf (text, SZ_FNAME, "HISTWIDE_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "SKEWNESS") ) 
	    call u_kwerr ("SKEWNESS")
	call sprintf (text, SZ_FNAME, "SKEWNESS_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "MEANC10") )
	    call u_kwerr ("MEANC10")
	call sprintf (text, SZ_FNAME, "MEANC10_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "MEANC25") )
	    call u_kwerr ("MEANC25")
	call sprintf (text, SZ_FNAME, "MEANC25_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "MEANC50") )
	    call u_kwerr ("MEANC50")
	call sprintf (text, SZ_FNAME, "MEANC50_%1d = %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "MEANC100") )
	    call u_kwerr ("MEANC100")
	call sprintf (text, SZ_FNAME, "MEANC100_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

 	iferr ( rval = imgetr (im, "MEANC200") )
	    call u_kwerr ("MEANC200")
	call sprintf (text, SZ_FNAME, "MEANC200_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "MEANC300") )
	    call u_kwerr ("MEANC300")
	call sprintf (text, SZ_FNAME, "MEANC300_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

	iferr ( rval = imgetr (im, "BACKGRND") )
	    call u_kwerr ("BACKGRND")
	call sprintf (text, SZ_FNAME, "BACKGRND_%1d= %e\n")
	    call pargi (grpnum)
	    call pargr (rval)
	call putline (fd, text)

end
