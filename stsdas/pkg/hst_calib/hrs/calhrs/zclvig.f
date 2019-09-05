        SUBROUTINE ZCLVIG(IREAD,VIGFIL,FDOP,MINVIG,DOPLER,FVIG,
     &     EPSET,ET,DATA,ERR,EPS,ISTAT)
*
*  Module number:
*
*  Module name: ZCLVIG
*
*  Keyphrase:
*  ----------
*       Remove vignetting
*
*  Description:
*  ------------
*       This routine removes the vignetting and low freq photcathode
*       resp. using a reference file that has a vignetting map.  This
*       map is has a vignetting vector for multiple line position and
*       carrousel positions.
*
*       At each line position the granularity is tabulated with a
*       constant starting sample for all lines and a constant delta
*       sample.  To compute the response for the data's line and sample,
*       tri-linear interpolation is used within the reference file over
*       carrousel position, line position and sample position.  If
*       doppler compensation is specified, the response is smoothed by a
*       wieghting function descrIg the motion of the data samples along
*       the photocathode.
*
*  FORTRAN name: ZCLVIG.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       VIGFILE                 I       vignetting reference file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zoxvig, zrdvig, zdopsm, zclint
*
*  SDAS:
*       usmput
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*       1.1     Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* INPUTS:
*
*       IREAD - readout number
*       VIGFIL - Name of the reference file
*       FDOP - perform doppler correction flag character*12
*       MINVIG - minimum response value to divide by
*       DOPLER - doppler weight array.  400 point real*4
*               DOPLER(i) contains the percent of time spent with
*               a doppler offset of (i-200) deflection units.
*
* INPUT/OUTPUT:
*       FVIG - calibration flag
*       EPSET - data quality for ET
*       ET - eng. trailer 24x7
*       DATA - data array 512 x 7
*       ERR - error array
*       EPS - data quality array
*
* OUTPUT:
*       ISTAT - error status
*
*---------------------------------------------------------------------------
      CHARACTER*64 VIGFIL
      CHARACTER*12 FDOP,FVIG
      REAL DOPLER(400),DATA(500,7),ERR(500,7),EPS(500,7),MINVIG
      REAL ET(24,7),EPSET(24,7)
      INTEGER ISTAT,IREAD
C     
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C     
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C     
C     HRS epsilons
C     
      INTEGER EPSFIL
      PARAMETER (EPSFIL = 800)
C     
C     /ZREFID/
C     
C     COMMON BLOCK containing id's for reference files which remain
C     open throughout the whole calibration.
C     
      INTEGER IDCCR2,IDCCR6,IDCCR8,IDCCR9,IDCCRC,IDPHC,IDVIG
      COMMON /ZREFID/ IDCCR2,IDCCR6,IDCCR8,IDCCR9,IDCCRC,IDPHC,
     *     IDVIG
C     
C     /HRSDEF/
C     Deflection pattern common block
C     NBINS - Number of supstep bins
C     NINIT - number of initial deflection pairs
C     IXDEF(5) - initial x-deflections
C     XDEF(7) - x-deflections for each bin
C     YDEF(7) - y-deflections for each bin
C     RCODES(7) - repeat codes for each bin
C     BINIDS(7) - substep bins ids for each bin
C     SAMPLE(7) - starting sample position for each spectra
C     LINE(7) - starting line position for each spectra
C     DELTAS(7) - sample position increment for each spectra
C     HOFF(7),VOFF(7) - horizontal and vertical offsets
C     DOPMAG, DOPZER - dopler magnitude and zero time
C     XDCAL, XDCALP - x-deflection calibration parameters
C     STPTIM - integration time at each step pattern position
C     
      INTEGER NBINS,NINIT,IXDEF(5),XDEF(7),YDEF(7),RCODES(7)
      INTEGER BINIDS(7),HOFF(7),VOFF(7)
      REAL SAMPLE(7),LINE(7),DELTAS(7),DOPMAG,XDCAL,XDCALP
      DOUBLE PRECISION DOPZER,STPTIM
      COMMON /HRSDEF/ DOPZER,STPTIM,NBINS,NINIT,IXDEF,XDEF,YDEF,
     *                RCODES,BINIDS,SAMPLE,LINE,DELTAS,HOFF,VOFF,
     *                DOPMAG,XDCAL,XDCALP
C     
C     /HRSMOD/
C     Common block containing observing mode parameters
C     
C     GRAT - grating mode
C     DET - detector
C     SCLAMP - spectral calibration lamp
C     CARPOS - carrousel position
C     OBSMOD - observation mode (DIR, ACC, TAR)
C     APER - aperture (LSA, SSA, SC1, SC2)
C     
      CHARACTER*3 OBSMOD,APER
      CHARACTER*5 GRAT
      INTEGER DET,SCLAMP,CARPOS
      COMMON /HRSMOD/ DET,SCLAMP,CARPOS
      COMMON /HRSMD1/ GRAT,OBSMOD,APER
C     
C     local variables
C     
      REAL LASTL,LASTDS,LASTS,LASTC
C     --->last line,sample,deltas and
C     carrousel position processed
      REAL SVIG(4800)
C     --->vignetting response sample positions
      REAL SIVIG(4800)
C     --->sample positions with 1 def. step spacing
      REAL VRESP(4800)
C     --->vignetting response vector
      REAL VIRESP(4800)
C     --->vig. resp. on 1 deflection step spaces
      REAL SRESP(4800)
C     --->doppler corrected response vector
      REAL IRESP(500)
C     --->response interpolated to data coord.
      INTEGER APERGRP(1000)
C     --->Groups appropriate for aperture.
      REAL LINES(1000)
C     --->line positions for each group
      REAL VCPOS(1000)
C     --->carrousel position for each group
      REAL UCPOS(1000)
C     --->vector of unique car. pos. in file
      REAL SAMP0,DELS
C     --->reference file parameters
      REAL SS,DS
      REAL S(500)
C     --->sample positions of the data
      INTEGER I,J,NS,NG,NU,K
      LOGICAL NEWDOP
      CHARACTER*80 CONTXT
C     
C     background diode processing
C     
      REAL SBACK(4),BRESP(4)
      INTEGER BPOS(4)
      DATA BPOS/1,2,11,12/
C----------------------------------------------------------------------------
C     
C     If first readout, open reference file
C     
      IF(IREAD.EQ.1)THEN
         CALL ZOXVIG(VIGFIL,GRAT,APER,1000,FVIG,SAMP0,DELS,
     &        APERGRP,LINES,VCPOS,NG,NS,UCPOS,NU,IDVIG,ISTAT)
         IF(ISTAT.NE.0)GO TO 1000
         LASTL = -999.0
         LASTDS = -999.0
         LASTS = -999.0
         LASTC= -999.0
C     
C     create vector of sample positions for response vectors
C     
         SS = SAMP0
         DO 50 J=1,NS
            SVIG(J)=SS
            SS = SS + DELS
 50      CONTINUE
C     
C     create vector of sample positions of 1/8 (one deflection step units)
C     
         SS = 0.0
         DO 60 J=1,4800
            SIVIG(J)=SS
            SS = SS + 0.125
 60      CONTINUE
      ENDIF
C     
C     Loop on substep bins
C     
      DO 500 I=1,NBINS
C     
C     get response for the bins line position if line position changed
C     
         IF((LASTL.NE.LINE(I)).OR.(LASTC.NE.CARPOS))THEN
            CALL ZRDVIG(IDVIG,VIGFIL,CARPOS,LINE(I),NG,APERGRP,
     *           LINES,VCPOS,NS,UCPOS,NU,VRESP,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
         ENDIF
C     
C     New doppler compensation smoothing needed if new readout of accum mode
C     and first bin or if line(I) is not equal to the last line processed.
C     
         IF ((FDOP.EQ.'PERFORM').AND.
     *        (((I.EQ.1).AND.(OBSMOD.EQ.'ACC')) .OR.
     *        (LINE(I).NE.LASTL) .OR. (CARPOS.NE.LASTC)))THEN
            NEWDOP=.TRUE.
            CALL ZCLINT(NS,SVIG,VRESP,4800,SIVIG,
     *           VIRESP)
            CALL ZDOPSM(VIRESP,DOPLER,SRESP)
         ELSE
            NEWDOP=.FALSE.
         ENDIF
C     
C     Create new interpolated response if LINE(I),SAMPLE(I), or DELTAS(I)
C     changed or new dopler correction
C     
         IF((LINE(I).NE.LASTL).OR.(DELTAS(I).NE.LASTDS).OR.
     *        (SAMPLE(I).NE.LASTS).OR. NEWDOP)THEN
            DS=DELTAS(I)
            SS=SAMPLE(I)
C     
C     compute sample position for each data point
C     
            DO 100 J=1,500
               S(J)=SS
               SS=SS+DS
 100        CONTINUE
C     
C     Compute sample positions of background diodes
C     
            SBACK(1)=SAMPLE(I)-17.0
            SBACK(2)=SAMPLE(I)-17.0
            SBACK(3)=S(500)+17.0
            SBACK(4)=S(500)+17.0
C     
C     Interpolate response for these data points
C     
            IF(FDOP.EQ.'PERFORM')THEN
               CALL ZCLINT(4800,SIVIG,SRESP,500,S,IRESP)
               CALL ZCLINT(4800,SIVIG,SRESP,4,SBACK,BRESP)
            ELSE
               CALL ZCLINT(NS,SVIG,VRESP,500,S,IRESP)
               CALL ZCLINT(NS,SVIG,VRESP,4,SBACK,BRESP) 
            ENDIF
         ENDIF
C     
C     divide by the response
C     
         DO 400 J=1,500
            IF(EPS(J,I).NE.EPSFIL)THEN
               IF(IRESP(J).GT.MINVIG)THEN
                  DATA(J,I)=DATA(J,I)/IRESP(J)
                  ERR(J,I)=ERR(J,I)/IRESP(J)
               ELSE
                  DATA(J,I)=0.0
                  ERR(J,I)=0.0
               ENDIF
            ENDIF
 400     CONTINUE
         DO 450 J=1,4
            K = BPOS(J)
            IF(EPSET(K,I).NE.EPSFIL)THEN
               IF(BRESP(J).GT.MINVIG)
     *              ET(K,I)=ET(K,I)/BRESP(J)
            ENDIF
 450     CONTINUE
         LASTL = LINE(I)
         LASTS = SAMPLE(I)
         LASTDS = DELTAS(I)
         LASTC = CARPOS
 500  CONTINUE
      IF(IREAD.EQ.1)THEN
         CONTXT='Vignetting removed using file '//VIGFIL
         CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
      ENDIF
      ISTAT=0
 1000 RETURN
      END
