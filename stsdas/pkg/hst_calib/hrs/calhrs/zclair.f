        SUBROUTINE ZCLAIR(PASS,NS,NSPEC,WAVE)
*
*  Module number:
*
*  Module name: zclair
*
*  Keyphrase:
*  ----------
*       vacumm to air wavelength conversion
*
*  Description:
*  ------------
*       This routine converts vacumm wavelengths to air wavlengths
*       above 2000 Angstroms.
*
*  FORTRAN name: zclair.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*  Subroutines Called:
*  -------------------
*       none
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*       1.1     sep 91  S. Hulbert      Implemented PASS flag
*-------------------------------------------------------------------------------
*
* INPUTS:
*	PASS - integer variable set to 1 on first call, -1 on last
*       NS  - number of samples in spectra
*       NSPEC - number of spectra
*
* INPUT/OUTPUT:
*       WAVE - wavelenght array NS x NSPEC real*8
*
*------------------------------------------------------------------------
        INTEGER PASS
        INTEGER NS,NSPEC
        DOUBLE PRECISION WAVE(NS,NSPEC)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
C Local variables
C
        INTEGER I,ISPEC,ISTAT
        DOUBLE PRECISION W,W2,FACT
C
C Loop on spectra
C
        DO 100 ISPEC=1,NSPEC
                IF(WAVE(NS,ISPEC).LE.2000.0)GO TO 100
C                                    --->All wavelenghts above 2000
C
C Loop on data points
C
                DO 50 I=1,NS
                    W = WAVE(I,ISPEC)
                    IF(W.GT.2000.0D0)THEN
                        W2 = W*W
                        FACT = 1.0D0 + 2.735182D-4 + 131.4182/W2 +
     *                          2.76249D8/(W2*W2)
                        WAVE(I,ISPEC) = W/FACT
                    ENDIF
50              CONTINUE
100     CONTINUE
        IF(PASS.EQ.FIRST)THEN
         CALL ZMSPUT('Wavelengths converted to air above 2000 Angstroms'
     *                    ,STDOUT,0,ISTAT)
        ENDIF
        RETURN
        END
