include	<synphot.h>

# This file contains specform and formlist, copied from synphot$lib and
# renamed fspecf and forml respectively.

## SPECF -- Convert from one data form to another.
#
# Input:
#	NWAVE	= I4 number of wavelengths
#	WAVE	= R4 pivot wavelengths (in Angstroms)
#	SPECIN	= R4 input spectrum data
#	INFORM	= C* form of spectrum data on input (e.g. 'FNU')
#	OUTFORM	= C* form of spectrum desired on output
# Output:
#	SPECOUT	= R4 output spectrum data
#	OUTFORM	= C* form of output spectrum data 
#
# Spectra may be converted among any of the following forms:
# Flux densities:
#	FLAM	= ergs cm-2 s-1 A-1
#	FNU	= ergs cm-2 s-1 Hz-1
#	JY	= 10^-23 ergs cm-2 s-1 Hz-1
#	MJY	= 10^-26 ergs cm-2 s-1 Hz-1
#	PHOTNU	= photons cm-2 s-1 Hz-1
#	PHOTLAM = photons cm-2 s-1 A-1
#	COUNTS  = photons cm-2 s-1 pixel-1 (requires monotonic wavelengths)
# Magnitude systems:
#	STMAG	= -2.5 log( FLAM ) - 21.10
#	ABMAG	= -2.5 log( FNU ) - 48.60
#	VEGAMAG	= -2.5 log( f / f(vega) )
#	OBMAG	= -2.5 log( COUNTS )
# Jan 1987 Keith Horne @ STScI - original version
# E.Medeiros - SPP version
# Dec 1989 Dave Bazell  Change PHOTPIX to COUNTS and add OBMAG
# Dec 1989 DB  Replace 1000.0 and 100.0 overflow values with INDEFR
# Jul 1989 DB  Put vega spectrum in subroutine and add formlist subroutine
#              to list acceptable forms
# Oct 1990 DB  Replace *0.5 with /max(1,abs(ip-im)) in counts conversions
# Oct 1993 Bernie Simon Fix conversion from obmag to photlam

procedure fspecf (nwave, wave, specin, inform, specout, outform)

int	nwave			# i: number of array elements
real	wave[ARB] 		# i: wavelength set array
real	specin[ARB]		# i: input flux data array
char	inform[ARB]		# i: input flux form
real	specout[ARB]		# o: output flux data array
char	outform[ARB]		# i: output flux form
#--
int	inform_flag		#indicator switch for input data form
int	outform_flag		#indicator switch for output data form
int	im			#array element pointer
int	ip			#array element pointer
int	i			#loop pointer
int	forml()

real	factor			#convertion factor
real	dwave
real 	H 			#Planck's constant
real	C 			#speed of light in vacum

bool	streq			#boolian string match function value 

begin
	#INITIALIZE local variables
	H = 6.62620E-27
	C = 2.997925E+18

	inform_flag = forml ( inform )
	outform_flag = forml ( outform )

	if ( streq(inform, outform )){
	   do i = 1, nwave {
	      specout[i] = specin[i]
	   }
	}else{
	      switch (inform_flag) {
	         case 1,2:	# photlam
	            do i = 1, nwave {
		       specout[i] = specin[i]
	            }
		 case 3,4:	# counts
		    do i = 1, nwave {
		       if ( !IS_INDEFR (specin[i]) ){ 
		          im = MAX(i-1,1)
		          ip = MIN(i+1,nwave)
	                  dwave = abs(wave[ip]-wave[im]) / max(1,abs(ip-im))
	                  if (dwave <= 0 )
	                     dwave = 1.
		          specout[i] = specin[i] / dwave
		       }else{
                          specout[i] = specin[i]
		       }
		    }
		 case 5,6:	# flam
		    factor = 1. / ( H * C )
		    do i = 1, nwave {
                       if ( !IS_INDEFR (specin[i]) ){
		          specout[i] = factor * specin[i] * wave[i]
                       }else{
			  specout[i] = specin[i]
                       }
		    }
                 case 7,8:	# fnu
                    factor = 1. / H
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specin[i]) ){
                          specout[i] = factor * specin[i] / wave[i]
                       }else{
                          specout[i] = specin[i]
                       }
                    }
                 case 9,10:	# photnu
                    factor = C
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specin[i]) ){
                          specout[i] = factor * specin[i] / 
                                         ( wave[i] * wave[i] )
                       }else{
                          specout[i] = specin[i]
                       }
                    }
                 case 11,12:	# jy
                    factor = 1.E-23 / H
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specin[i]) ){
                          specout[i] = factor * specin[i] / wave[i]
                       }else{
                          specout[i] = specin[i]
                       }
                    }
                 case 13,14:	# mjy
                    factor = 1.E-26 / H
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specin[i]) ){
                          specout[i] = factor * specin[i] / wave[i]
                       }else{
                          specout[i] = specin[i]
                       }
                    }
		 case 15,16:	# abmag
                    factor = 1. / H
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specin[i]) ){
                          specout[i] = factor / wave[i] *
                               10.**( -0.4 * ( specin[i] - ABZERO ))
                       }else{
                          specout[i] = specin[i]
                       }
                    }
                 case 17,18:	# stmag
                    factor = 1. / ( H * C )
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specin[i]) ){
                          specout[i] = factor * wave[i] *
                               10.**( -0.4 * (specin[i] - STZERO ))
                       }else{
                          specout[i] = specin[i]
                       }
                    }
		 case 19,20:	# vegamag
	            # Read in the vega spectrum.  rdvega returns actual 
	            # number of data points.
		    call error (1, "specf:  Vega deleted")
	         case 21,22:	# obmag
	            do i = 1, nwave {
		       if ( !IS_INDEFR (specin[i]) ){ 
		          im = MAX(i-1,1)
		          ip = MIN(i+1,nwave)
	                  dwave = abs(wave[ip]-wave[im]) * 0.5
	                  if ( dwave <= 0. )
	                     dwave = 1.
                          specout[i] = 10.**(-0.4 * specin[i]) / dwave
		       }else{
                          specout[i] = specin[i]
		       }
		    }

	         default:
                    call error (1, "bad form")
              }
	      switch (outform_flag) {
	         case 1,2:	#photlam
                    return
		 case 3,4:	#counts
		    do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
		          im = MAX(i-1,1)
		          ip = MIN(i+1,nwave)
	                  dwave = abs(wave[ip]-wave[im]) / max(1,abs(ip-im))
	                  if ( dwave <= 0. )
	                     dwave = 1.
		          specout[i] = specout[i] * dwave
                       }
		    }
		 case 5,6:	#flam
		    factor = H * C
		    do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
		          specout[i] = factor * specout[i] / wave[i]
                       }
		    }
                 case 7,8:	#fnu
                    factor = H
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
                          specout[i] = factor * specout[i] * wave[i]
                       }
                    }
                 case 9,10:	# photnu
                    factor = 1. / C
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
                          specout[i] = factor * specout[i] * 
                                          wave[i] * wave[i]
                       }
                    }
                 case 11,12:	# jy
                    factor = 1.E+23 * H
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
                          specout[i] = factor * specout[i] * wave[i]
                       }
                    }
                 case 13,14:	# mjy
                    factor = 1.E+26 * H
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
                          specout[i] = factor * specout[i] * wave[i]
                       }
                    }
		 case 15,16:	#abmag
                    factor = H
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
                          specout[i] = factor * specout[i] * wave[i]
                          if ( specout[i] > 0. ){
                             specout[i] = -2.5 * ALOG10( specout[i] ) + 
                                                   ABZERO
                          }else{
                             specout[i] = INDEFR
                          }
                       }
                    }
                 case 17,18:	# stmag  
                    factor = H * C
                    do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
                          specout[i] = factor * specout[i] / wave[i]
                          if ( specout[i] > 0. ){
                             specout[i] = -2.5 * ALOG10( specout[i] ) + 
							STZERO
                          }else{
                             specout[i] = INDEFR
                          }
                       }
                    }
		 case 19,20:	# vegamag
	            # Read in the vega spectrum.  rdvega returns actual 
	            # number of data points.
		    call error (1, "specf:  Vega deleted")
	         case 21,22:	# obmag
	            do i = 1, nwave {
                       if ( !IS_INDEFR (specout[i]) ){
		          im = MAX(i-1,1)
		          ip = MIN(i+1,nwave)
	                  dwave = abs(wave[ip]-wave[im]) * 0.5
	                  if ( dwave <= 0. )
	                     dwave = 1.
	                  if ( specout[i] > 0. ) {
		             specout[i] = -2.5 * alog10( specout[i]*dwave )
	                  } else
	                     specout[i] = INDEFR
                       }
		    }
                 default:
                    call error (1, "bad form")
              }
        }
        return
end

# FORML -- Return a number indicating the form of the data
# The input argument FORM must be lower case.  The synphot version
# converts FORM to lower case in-place, but this version doesn't do that.

int procedure forml( form )

char	form[ARB]	# form of data
bool	streq()
pointer	errmsg

string	badform		"Unknown form: %s"
string	validforms	"Valid forms:  photlam, counts, flam,\n\
              fnu, photnu, jy, mjy, abmag,\n\
              stmag,vegamag, obmag\n\n"

begin
	if ( streq( form, "photlam" ) )
	   return 1

	else if ( streq( form, "photpix" ) || streq( form, "counts" ) )
	   return 3

	else if ( streq( form, "flam" ) )
	   return 5

	else if ( streq( form, "fnu" ) )
	   return 7

	else if ( streq( form, "photnu" ) )
	   return 9

	else if ( streq( form, "jy" ) )
	   return 11

	else if ( streq( form, "mjy" ) )
	   return 13

	else if ( streq( form, "abmag" ) )
	   return 15

	else if ( streq( form, "stmag" ) )
	   return 17

	else if ( streq( form, "vegamag" ) )
	   return 19

	else if ( streq( form, "obmag" ) )
	   return 21

	else {
	   call eprintf( validforms )
	   call malloc( errmsg, SZ_LINE, TY_CHAR )
	   call sprintf( Memc[errmsg], SZ_LINE, badform )
	      call pargstr( form )
	   call error ( 1, Memc[errmsg] )
	}
end
