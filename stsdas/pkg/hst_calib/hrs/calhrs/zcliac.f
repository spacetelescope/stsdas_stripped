        SUBROUTINE ZCLIAC(PASS,CCR8,ORDER,NS,NSPEC,
     *                  WAVE,ISTAT)
*
*  Module number:
*
*  Module name: ZCLIAC
*
*  Keyphrase:
*  ----------
*       GHRS incidence angle correction
*
*  Description:
*  ------------
*       This routine adjusts the wavelenght array for the difference
*       in incidence angle of apertures LSA, SC1 and SC2 from the SSA.
*       Table CCR8 is searched for the correct grating, spectral order,
*       aperture, and carrousel position to obtain two coefficients,
*       A and B.  Interpolation of the coefficients (in carrousel position)
*       is used if an exact match is not found.  These coefficients are
*       then used to compute an offset by:
*
*               wave = wave + (A + B*s)/m
*
*       where:
*               wave is the wavelength
*               A and B are coefficients in CCR8
*               s is the photocathode sample position
*               m is the spectral order
*
*  FORTRAN name: zcliac.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccr8                    I       Incidence angle constant table
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zrccr8
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*	1.1	Sep 91	S. Hulbert	Implemented PASS flag, use provided APER
*       1.2     Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* INPUTS:
*       first - logical variable set to true for first call
*       ccr8 - name of the incidence angle constant table
*       order - spectral order number
*       nsout - number of output samples
*       nspec - number of output spectra
*
* INPUT/OUTPUTS:
*
*       wave - wavelength array nsout x nspec double precision
*       istat - error status
*
*-----------------------------------------------------------------------------
        INTEGER PASS
        CHARACTER*64 CCR8
        INTEGER ORDER,NS,NSPEC,ISTAT
        DOUBLE PRECISION WAVE(NS,NSPEC)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
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
C Local variables
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        REAL DELS,S,COEF(2),A,B
        INTEGER ISPEC,I
        CHARACTER*80 CONTXT
C--------------------------------------------------------------------------
C
C if small science aperture, no need to do offset
C
        IF(APER.NE.'SSA')THEN
C
C get coefficients for the grating, order, aperture, carrousel position
C
                CALL ZRCCR8(PASS,CCR8,GRAT,APER,ORDER,CARPOS,
     *                          COEF,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR: No incidence angle performed'
                        A=0.0
                        B=0.0
                        GO TO 999
                  ELSE
                        A = COEF(1)
                        B = COEF(2)
                ENDIF
C
C Perform correction
C
                DO 100 ISPEC=1,NSPEC
                        S=SAMPLE(ISPEC)
C                                    --->starting sample position
                        DELS=DELTAS(ISPEC)
                        DO 50 I=1,NS
                                WAVE(I,ISPEC) = WAVE(I,ISPEC)+
     *                                             (A+B*S)/ORDER
                                S=S+DELS
50                      CONTINUE
100             CONTINUE
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
