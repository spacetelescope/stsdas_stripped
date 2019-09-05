include	<imhdr.h>

# IMPINFO -- Print selected values of an image header according to instrument.
#            Check if an IPPPSSOOT file which represents the following
#            I   = instrument: i,j,l,n,o,u,v,w,x,y,z for WFC3, ACS, COS,
#			       NICMOS, STIS, WFPC2, HSP, WFPC, FOC, FOS, HRS
#            PPP = Program identification
#            SS  = Observation set identification
#            OO  = Observation identification
#            T   = Data source (realtime, tape recorded, etc.)
#            If INSTRUMENT = WFC3, ACS, COS, NICMOS, STIS, WFPC2, HSP, WFPC,
#	     FOC, FOS, or HRS an instrument specific header is printed.
#	     If the image name has 9 characters and the first character matches
#	     i,j,l,n,o,u-z, it is assumed to be an IPPPSSOOT file and that
#	     information is printed as well.
#            If there is no INSTRUMENT keyword, a general header is printed.
#
# 08-Jul-1997 H.Bushouse: Modified to accomodate NICMOS and STIS files.
# 04-Jun-2003 H.Bushouse: Modified to accomodate WFC3, ACS, and COS files.

procedure impinfo (image)

int	nch, inst
int     imaccf(), strlen()
double  ra, dec
double  imgetd

char	image[ARB], filtyp, equinox[SZ_FNAME]
char    instrument[SZ_FNAME], root[SZ_FNAME], date[SZ_FNAME]
char    pkttime[SZ_FNAME]
char    chrlwr()
bool    ipp
bool    streq() 

string  j2000         "J2000"
string  unavail       "unavailable"

pointer	im, sp
pointer	immap()
errchk  immap
define	done_ 91
define  HSP 1
define  WFPC 2
define  FOC 3
define  FOS 4
define  HRS 5
define  WFPC2 6
define  NICMOS 7
define  STIS 8
define	ACS 9
define	WFC3 10
define	COS 11
define  maxch 80

begin
	# Allocate automatic buffers.
	call smark (sp)

	im = immap (image, READ_ONLY, 0)

        # Find number of characters in root name 
        # Find length, if any, of directory specification

        iferr (call imgstr (im, "INSTRUME", instrument, SZ_FNAME))
           instrument[1] = EOS
        iferr (call imgstr (im, "ROOTNAME", root, SZ_FNAME))
           root[1] = EOS
        nch = strlen (root)
          
        # Get first character of root name

        filtyp = chrlwr(root[1]) 

        inst = 0
        if (streq (instrument, "HSP")) inst = HSP
         else if (streq (instrument, "WFPC")) inst = WFPC
          else if (streq (instrument, "FOC")) inst = FOC
           else if (streq (instrument, "FOS")) inst = FOS
            else if (streq (instrument, "HRS")) inst = HRS
             else if (streq(instrument, "WFPC2")) inst = WFPC2
              else if (streq(instrument, "NICMOS")) inst = NICMOS
               else if (streq(instrument, "STIS")) inst = STIS
                else if (streq(instrument, "ACS")) inst = ACS
                 else if (streq(instrument, "WFC3")) inst = WFC3
                  else if (streq(instrument, "COS")) inst = COS
                        
        # Get the general header information according to instrument

        switch (inst) {
         
           case HSP:
              if (nch == 9 && filtyp == 'v') ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr(image)
              }

              iferr (call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else if (imaccf (im, "FPKTTIME") == YES) {
                 call imgstr (im, "FPKTTIME", pkttime, SZ_FNAME)
                 call strcpy (pkttime, date, 11)
              } else call strcpy (unavail, date, 11)
           
              if (imaccf(im, "RA_APER") == YES ) { 
                 iferr (ra = imgetd (im, "RA_APER")) ra = 0.0 
                 iferr (dec = imgetd (im, "DEC_APER")) dec = 0.0 
              } else if (IM_NDIM(im) == 1) {
                 iferr (ra = imgetd (im, "CRVAL2")) ra= 0.0 
                 iferr (dec = imgetd (im, "CRVAL3")) dec= 0.0 
              } else {
                 iferr (ra = imgetd (im, "CRVAL1")) ra = 0.0 
                 iferr (dec = imgetd (im, "CRVAL2")) dec = 0.0 

              }  
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call hspbot (im)

           case WFPC:
              if (nch == 9 && filtyp == 'w')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }   
              if (imaccf (im, "PEQUINOX") == YES)
                 call imgstr (im, "PEQUINOX", equinox, SZ_FNAME)
              else iferr(call imgstr (im, "EPOCHTRG", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else iferr (call imgstr (im, "FPKTTIME", date, 11))
                 call strcpy (unavail, date, 11)
                   
              if (imaccf (im, "RA_TARG") == YES)
                 ra = imgetd (im, "RA_TARG")
              else iferr (ra = imgetd (im, "RTASNTRG"))
                 ra = 0.0 
              if (imaccf (im, "DEC_TARG") == YES)
                 dec = imgetd (im, "DEC_TARG")
              else iferr (dec = imgetd (im, "DECLNTRG"))
                 dec = 0.0 
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call wfpbot (im)
              
           case FOC:
              if (nch == 9 && filtyp == 'x')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }
              iferr (call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else if (imaccf (im, "FPKTTIME") == YES) {
                 call imgstr (im, "FPKTTIME", pkttime, SZ_FNAME)
                 call strcpy (pkttime, date, 11)
              } else call strcpy (unavail, date, 11)
           
              iferr (ra = imgetd (im, "CRVAL1"))
                    ra = 0.0 
              iferr (dec = imgetd (im, "CRVAL2"))
                    dec = 0.0 
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call focbot (im)
  
           case FOS:
              if (nch == 9 && filtyp == 'y')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }
              iferr (call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 11)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else if (imaccf (im, "FPKTTIME") == YES) {
                 call imgstr (im, "FPKTTIME", pkttime, SZ_FNAME)
                 call strcpy (pkttime, date, 11)
              } else call strcpy (unavail, date, 11)
           
              if (imaccf(im, "RA_APER1") == YES ) {
                 iferr (ra = imgetd (im, "RA_APER1")) ra = 0.0 
                 iferr (dec = imgetd (im, "DECAPER1")) dec = 0.0 
              } else if (IM_NDIM(im) == 1) {
                 iferr (ra = imgetd (im, "CRVAL2")) ra = 0.0 
                 iferr (dec = imgetd (im, "CRVAL3")) dec = 0.0 
              } else {
                 iferr (ra = imgetd (im, "CRVAL1")) ra = 0.0 
                 iferr (dec = imgetd (im, "CRVAL2")) dec = 0.0 
              }  
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call fosbot (im)

           case HRS:
              if (nch == 9 && filtyp == 'z')
                 ipp=TRUE 
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }
              iferr (call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else if (imaccf (im, "PKTTIME") == YES) {
                 call imgstr (im, "PKTTIME", pkttime, SZ_FNAME)
                 call strcpy (pkttime, date, 11)
              } else call strcpy (unavail, date, 11)
           
              if (imaccf(im, "RA_APER1") == YES ) {
                 iferr (ra = imgetd (im, "RA_APER1")) ra = 0.0 
                 iferr (dec = imgetd (im, "DECAPER1")) dec = 0.0 
              } else if (IM_NDIM(im) == 1) {
                 iferr (ra = imgetd (im, "CRVAL2")) ra = 0.0 
                 iferr (dec = imgetd (im, "CRVAL3")) dec = 0.0 
              } else {
                 iferr (ra = imgetd (im, "CRVAL1")) ra = 0.0 
                 iferr (dec = imgetd (im, "CRVAL2")) dec = 0.0 
              }  
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call hrsbot (im)

           case WFPC2:
              if (nch == 9 && filtyp == 'u')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }   
              if (imaccf (im, "PEQUINOX") == YES)
                 call imgstr (im, "PEQUINOX", equinox, SZ_FNAME)
              else iferr(call imgstr (im, "EPOCHTRG", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else iferr (call imgstr (im, "FPKTTIME", date, 11))
                 call strcpy (unavail, date, 11)
                   
              if (imaccf (im, "RA_TARG") == YES)
                 ra = imgetd (im, "RA_TARG")
              else iferr (ra = imgetd (im, "RTASNTRG"))
                 ra = 0.0 
              if (imaccf (im, "DEC_TARG") == YES)
                 dec = imgetd (im, "DEC_TARG")
              else iferr (dec = imgetd (im, "DECLNTRG"))
                 dec = 0.0 
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call wf2bot (im)
              
           case NICMOS:

              if (nch == 9 && filtyp == 'n')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }   
              if (imaccf (im, "PEQUINOX") == YES)
                 call imgstr (im, "PEQUINOX", equinox, SZ_FNAME)
              else iferr(call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else
                 call strcpy (unavail, date, 11)
                   
              if (imaccf (im, "RA_TARG") == YES)
                 ra = imgetd (im, "RA_TARG")
              else
                 ra = 0.0 
              if (imaccf (im, "DEC_TARG") == YES)
                 dec = imgetd (im, "DEC_TARG")
              else
                 dec = 0.0 
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call nicbot (im)
              
           case STIS:

              if (nch == 9 && filtyp == 'o')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }   
              if (imaccf (im, "PEQUINOX") == YES)
                 call imgstr (im, "PEQUINOX", equinox, SZ_FNAME)
              else iferr(call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else
                 call strcpy (unavail, date, 11)
                   
              if (imaccf (im, "RA_APER") == YES)
                 ra = imgetd (im, "RA_APER")
              else
                 ra = 0.0 
              if (imaccf (im, "DEC_APER") == YES)
                 dec = imgetd (im, "DEC_APER")
              else
                 dec = 0.0 
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              #call stsbot (im)
              
           case ACS:

              if (nch == 9 && filtyp == 'j')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }   
              if (imaccf (im, "PEQUINOX") == YES)
                 call imgstr (im, "PEQUINOX", equinox, SZ_FNAME)
              else iferr(call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else
                 call strcpy (unavail, date, 11)
                   
              if (imaccf (im, "RA_TARG") == YES)
                 ra = imgetd (im, "RA_TARG")
              else
                 ra = 0.0 
              if (imaccf (im, "DEC_TARG") == YES)
                 dec = imgetd (im, "DEC_TARG")
              else
                 dec = 0.0 
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call acsbot (im)
              
           case WFC3:

              if (nch == 9 && filtyp == 'i')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }   
              if (imaccf (im, "PEQUINOX") == YES)
                 call imgstr (im, "PEQUINOX", equinox, SZ_FNAME)
              else iferr(call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else
                 call strcpy (unavail, date, 11)
                   
              if (imaccf (im, "RA_TARG") == YES)
                 ra = imgetd (im, "RA_TARG")
              else
                 ra = 0.0 
              if (imaccf (im, "DEC_TARG") == YES)
                 dec = imgetd (im, "DEC_TARG")
              else
                 dec = 0.0 
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              call wf3bot (im)
              
           case COS:

              if (nch == 9 && filtyp == 'l')
                 ipp = TRUE
              else {
                 ipp = FALSE
                 call printf (
                 " File name: %s is not an IPPPSSOOT specification\n")
                      call pargstr (image)
              }   
              if (imaccf (im, "PEQUINOX") == YES)
                 call imgstr (im, "PEQUINOX", equinox, SZ_FNAME)
              else iferr(call imgstr (im, "EQUINOX", equinox, SZ_FNAME))
                 call strcpy (j2000, equinox, 5)
              if (imaccf (im, "DATE-OBS") == YES)
                 call imgstr (im, "DATE-OBS", date, SZ_FNAME)
              else
                 call strcpy (unavail, date, 11)
                   
              if (imaccf (im, "RA_TARG") == YES)
                 ra = imgetd (im, "RA_TARG")
              else
                 ra = 0.0 
              if (imaccf (im, "DEC_TARG") == YES)
                 dec = imgetd (im, "DEC_TARG")
              else
                 dec = 0.0 
              call gentop (im, ipp, root, instrument, ra, dec, date, equinox)
              #call cosbot (im)
              
       default:

              # Not an HST instrument. Print general header

              call nonhst(im, image)
       }
       
  
done_
	call imunmap (im)
	call sfree (sp)
end
