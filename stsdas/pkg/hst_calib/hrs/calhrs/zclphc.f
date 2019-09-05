        SUBROUTINE ZCLPHC(IREAD,PHCFIL,FDOP,MINPHC,DOPLER,
     *     FPHC,DATA,ERR,EPS,ISTAT)
*
*  Module number:
*
*  Module name: zclphc
*
*  Keyphrase:
*  ----------
*       Remove photocathode granularity
*
*  Description:
*  ------------
*       This routine removes the photocathode granualarity using
*       a reference file that has a granularity map.  This map
*       is has a granularity vector for multiple line positions
*       At each line position the granularity is tabulated with
*       a constant starting sample for all lines and a constant
*       delta sample.  To compute the response for the data's line
*       and sample, bilinear interpolation is used within the reference
*       file.  If doppler compensation is specified,  the response is
*       smoothed by a weighting function describing the motion of
*       the data samples along the photocathode.
*
*  FORTRAN name: zclphc.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       PHCFILE                 I       Photocathode granularity file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zoxphc, zrdphc, zdopsm, zclint
*
*  SDAS:
*       usmput
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*       1.1     Nov 96  M. De La Pena   Added STPTIM to HRSDEF,APER to PHC file
*-------------------------------------------------------------------------------
*
* INPUTS:
*
*       IREAD - readout number
*       PHCFIL - Name of photocathode response reference file
*       FDOP - perform doppler correction flag character*12
*       MINPHC - minimum response value to divide by
*       DOPLER - doppler weight array.  400 point real*4
*               DOPLER(i) contains the percent of time spent with
*               a doppler offset of (i-200) deflection units.
*
* INPUT/OUTPUT:
*
*       FPHC - Calibration flag.
*       DATA - data array 512 x 7
*       ERR - error array
*       EPS - data quality array
*
* OUTPUT:
*       ISTAT - error status
*
*---------------------------------------------------------------------------
        CHARACTER*64 PHCFIL
        CHARACTER*12 FDOP,FPHC
        REAL DOPLER(400),DATA(500,7),ERR(500,7),EPS(500,7),MINPHC
        INTEGER ISTAT,IREAD
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C HRS epsilons
C
        INTEGER EPSFIL
        PARAMETER (EPSFIL = 800)
C
C                       /HRSDEF/
C Deflection pattern common block
C       NBINS - Number of supstep bins
C       NINIT - number of initial deflection pairs
C       IXDEF(5) - initial x-deflections
C       XDEF(7) - x-deflections for each bin
C       YDEF(7) - y-deflections for each bin
C       RCODES(7) - repeat codes for each bin
C       BINIDS(7) - substep bins ids for each bin
C       SAMPLE(7) - starting sample position for each spectra
C       LINE(7) - starting line position for each spectra
C       DELTAS(7) - sample position increment for each spectra
C       HOFF(7),VOFF(7) - horizontal and vertical offsets
C       DOPMAG, DOPZER - dopler magnitude and zero time
C       XDCAL, XDCALP - x-deflection calibration parameters
C       STPTIM - integration time at each step pattern position
C
        INTEGER NBINS,NINIT,IXDEF(5),XDEF(7),YDEF(7),RCODES(7)
        INTEGER BINIDS(7),HOFF(7),VOFF(7)
        REAL SAMPLE(7),LINE(7),DELTAS(7),DOPMAG,XDCAL,XDCALP
        DOUBLE PRECISION DOPZER,STPTIM
        COMMON /HRSDEF/ DOPZER,STPTIM,NBINS,NINIT,IXDEF,XDEF,YDEF,
     *                  RCODES,BINIDS,SAMPLE,LINE,DELTAS,HOFF,VOFF,
     *                  DOPMAG,XDCAL,XDCALP
C
C                       /HRSMOD/
C Common block containing observing mode parameters
C
C   GRAT - grating mode
C   DET - detector
C   SCLAMP - spectral calibration lamp
C   CARPOS - carrousel position
C   OBSMOD - observation mode (DIR, ACC, TAR)
C   APER - aperture (LSA, SSA, SC1, SC2)
C
        CHARACTER*3 OBSMOD,APER
        CHARACTER*5 GRAT
        INTEGER DET,SCLAMP,CARPOS
        COMMON /HRSMOD/ DET,SCLAMP,CARPOS
        COMMON /HRSMD1/ GRAT,OBSMOD,APER
C
C			/ZREFID/
C
C COMMON BLOCK containing id's for reference files which remain
C open throughout the whole calibration.
C
        INTEGER IDCCR2,IDCCR6,IDCCR8,IDCCR9,IDCCRC,IDPHC,IDVIG
        COMMON /ZREFID/ IDCCR2,IDCCR6,IDCCR8,IDCCR9,IDCCRC,IDPHC,
     *       IDVIG
C
C local variables
C
        REAL LASTL,LASTDS,LASTS
C                                    --->last line,sample,deltas processed
        REAL SPHC(4800)
C                                    --->photocathode response sample positions
        REAL SIPHC(4800)
C                                    --->sample positions with 1 def. step spacing
        REAL PRESP(4800)
C                                    --->photocathode response vector
        REAL PIRESP(4800)
C                                    --->photo. resp. on 1 deflection step spaces
        REAL SRESP(4800)
C                                    --->doppler corrected response vector
        REAL IRESP(500)
C                                    --->response interpolated to data coord.
        INTEGER GROUPS(1000)
C                                    --->group numbers sorted by line_pos
        REAL LINES(1000)
C                                    --->line positions for each group
        REAL SAMP0,DELS
C                                    --->reference file parameters
        REAL SS,DS
        REAL S(500)
C                                    --->sample positions of the data
        INTEGER I,J,NS,NG
        LOGICAL NEWDOP
        CHARACTER*80 CONTXT
        CHARACTER*7 FTYPE
        DATA FTYPE/'PHCFILE'/
C----------------------------------------------------------------------------
C
C If first readout, open reference file
C
        IF(IREAD.EQ.1)THEN
                CALL ZOXPHC(PHCFIL,GRAT,APER,1000,FPHC,SAMP0,DELS,
     *                      GROUPS,LINES,NG,NS,IDPHC,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
                LASTL = -999.0
                LASTDS = -999.0
                LASTS = -999.0
C
C create vector of sample positions for photocathed response vectors
C
                SS = SAMP0
                DO 50 J=1,NS
                        SPHC(J)=SS
                        SS = SS + DELS
50              CONTINUE
C
C create vector of sample positions of 1/8 (one deflection step units)
C
                SS = 0.0
                DO 60 J=1,4800
                        SIPHC(J)=SS
                        SS = SS + 0.125
60              CONTINUE
        ENDIF
C
C Loop on substep bins
C
        DO 500 I=1,NBINS
C
C get response for the bins line position if line position changed
C
                IF(LASTL.NE.LINE(I))THEN
                        CALL ZRDPHC(IDPHC,FTYPE,PHCFIL,LINE(I),NG,LINES,
     *                          GROUPS,NS,PRESP,ISTAT)
                        IF(ISTAT.NE.0)GO TO 1000
                ENDIF
C
C New doppler compensation smoothing needed if new readout of accum mode
C and first bin or if line(I) is not equal to the last line processed.
C
                IF ((FDOP.EQ.'PERFORM').AND.
     *             (((I.EQ.1).AND.(OBSMOD.EQ.'ACC')) .OR.
     *              (LINE(I).NE.LASTL)) )THEN
                                NEWDOP=.TRUE.
                                CALL ZCLINT(NS,SPHC,PRESP,4800,SIPHC,
     *                                  PIRESP)
                                CALL ZDOPSM(PIRESP,DOPLER,SRESP)
                        ELSE
                                NEWDOP=.FALSE.
                ENDIF
C
C Create new interpolated response if LINE(I),SAMPLE(I), or DELTAS(I)
C changed or new dopler correction
C
                IF((LINE(I).NE.LASTL).OR.(DELTAS(I).NE.LASTDS).OR.
     *                  (SAMPLE(I).NE.LASTS).OR. NEWDOP)THEN
                        DS=DELTAS(I)
                        SS=SAMPLE(I)
C
C compute sample position for each data point
C
                        DO 100 J=1,500
                                S(J)=SS
                                SS=SS+DS
100                     CONTINUE
C
C Interpolate response for these data points
C
                        IF(FDOP.EQ.'PERFORM')THEN
                            CALL ZCLINT(4800,SIPHC,SRESP,500,S,IRESP)
                           ELSE
                            CALL ZCLINT(NS,SPHC,PRESP,500,S,IRESP)
                        ENDIF
                ENDIF
C
C divide by the response
C
                DO 400 J=1,500
                        IF(EPS(J,I).NE.EPSFIL)THEN
                                IF(IRESP(J).GT.MINPHC)THEN
                                        DATA(J,I)=DATA(J,I)/IRESP(J)
                                        ERR(J,I)=ERR(J,I)/IRESP(J)
                                   ELSE
                                        DATA(J,I)=0.0
                                        ERR(J,I)=0.0
                                ENDIF
                        ENDIF
400             CONTINUE
                LASTL = LINE(I)
                LASTS = SAMPLE(I)
                LASTDS = DELTAS(I)
500     CONTINUE
        IF(IREAD.EQ.1)THEN
            CONTXT='Photocathode granularity removed using '//PHCFIL
            CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
        ISTAT=0
1000    RETURN
        END
