C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YWSHFT(WAVE1,WAVE2,NX,IPIX,NSHIFT,ISTAT)
*
*  Module number:
*
*  Module name: YWSHFT
*
*  Keyphrase:
*  ----------
*       Determine wavelength shift
*
*  Description:
*  ------------
*       This routine computes the wavelength shift to the
*       nearest pixel at reference point IPIX in WAVE1.
*
*  FORTRAN name: ywshft.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Sep 89  D. Lindler      Designed and coded
*	1.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*-------------------------------------------------------------------------------
*
* Inputs:
*       wave1 - first wavelength vector
*       wave2 - second wavelenght vector
*       nx - number of points in wave1 and wave2
*       ipix - reference pixel
*
* Outputs:
*       nshift - spectral shift in pixels
*       istat - error status
*
*-------------------------------------------------------------------------------
        REAL WAVE1(*),WAVE2(*)
        INTEGER NX,IPIX,NSHIFT,ISTAT
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Local variables
C
        CHARACTER*80 CONTXT
        INTEGER I,IPOS
        REAL W
C
C-----------------------------------------------------------------------------
        IF((IPIX.LT.0).OR.(IPIX.GT.NX))THEN
                CONTXT='ERROR: COMBPX in CCS4 outside data range'
                GO TO 999
        ENDIF
C
C Find wavelengths in WAVE2 such that they bound WAVE(IPIX)
C
        W = WAVE1(IPIX)
        IF(W.LE.0)THEN
                CONTXT='ERROR: COMBPX outside wavelength range of '//
     *                          '1st pass direction'
                GO TO 999
        ENDIF
        IPOS = -1
        DO 100 I=1,NX-1
                IF(WAVE2(I).LT.W)THEN
                        IF(WAVE2(I+1).GE.W) IPOS = I
                    ELSE
                        IF(WAVE2(I+1).LE.W) IPOS = I
                ENDIF
100     CONTINUE

        CONTXT='ERROR: wavelength at COMBPX outside range for '//
     *          '2nd pass direction'
        IF(IPOS.LT.0)GO TO 999
        IF((WAVE2(IPOS).LE.0).OR.(WAVE2(IPOS+1).LE.0))GO TO 999
        IF(ABS(WAVE2(IPOS)-W) .GT. ABS(WAVE2(IPOS+1)-W))IPOS=IPOS+1
        NSHIFT=IPOS-IPIX
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
