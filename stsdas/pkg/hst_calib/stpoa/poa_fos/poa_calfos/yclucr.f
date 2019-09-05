C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLUCR (RESULT, NX, MAXLEN, NUPDAT, UCORR1, 
     *                     ISTAT)
*
*  Module number:
*
*  Module name: YCLUCR
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       Correct the U Stokes parameter and corresponding U error due to 
*       post-COSTAR effects and rederive the linear and circular 
*       polarization, as well as the phase angle.  In addition, rederive 
*       all the associated errors.
*
*  FORTRAN name: YCLUCR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yclnrm, ymsput
*
*  SDAS:
*
*
*  History:
*  --------
*  Version      Date    Author		  Description
*       1       Mar97   M.D. De La Pena   Original Implementation for CALFOS
*                                         based upon code from R. Allen.
*       1.1     Jul97   M.D. De La Pena   Mods for new parameter
*-------------------------------------------------------------------------------
*
* INPUTS:
*       RESULT - Array containing Stokes parameters, linear and circular 
*                polarization, polarization angle, and error estimates
*       NX     - Number of channels
*       MAXLEN - Maximum number of channels
*       NUPDAT - Number of quantities being updated
*       UCORR1 - Array of corrections for the U Stokes parameter
*       ISTAT  - Return status
*
* OUTPUT:
*       RESULT - Modified input array
*       ISTAT  - Modified return status
*----------------------------------------------------------------------------
C
C UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Passed Parameters
C
      INTEGER          NX, MAXLEN, NUPDAT, ISTAT
      REAL             RESULT(MAXLEN,NUPDAT)
      DOUBLE PRECISION UCORR1(MAXLEN)
C
C Local Variables
C
      DOUBLE PRECISION P(14)
      DOUBLE PRECISION U, USIG, UC, UCSIG
      DOUBLE PRECISION DZERO
      INTEGER          I, CHAN
      CHARACTER*80     CONTXT
C
      PARAMETER (DZERO = 0.0D0)
C
C----------------------------------------------------------------------------
C
C Initialize P, the temporary holding array
C    
      DO 10 I = 1, NUPDAT
          P(I) = DZERO
10    CONTINUE
C
C Iterate over all channels
C
      DO 30 CHAN = 1, NX
C
C Setup the nominal U value and the corrected post-COSTAR U value and errors
C
          U     = RESULT(CHAN,3)
          USIG  = RESULT(CHAN,7)
          UC    = UCORR1(CHAN) * RESULT(CHAN,1)
          UCSIG = UCORR1(CHAN) * RESULT(CHAN,5)
C
          P(1) = RESULT(CHAN,1)
          P(2) = RESULT(CHAN,2)
          P(3) = U - UC
          P(4) = RESULT(CHAN,4)
          P(5) = RESULT(CHAN,5)
          P(6) = RESULT(CHAN,6)
          P(7) = DSQRT(USIG*USIG + UCSIG*UCSIG)
          P(8) = RESULT(CHAN,8)
C
C Compute the linear and circular polarization, polarization angle, 
C and errors
C
          CALL YCLNRM (P, NUPDAT, CHAN, ISTAT)
          IF (ISTAT .NE. 0) THEN
              CONTXT = 'ERROR in computing the corrected polarization'//
     *                 ' values.'
              GO TO 999
          END IF
C
C Store the updated P values in the RESULT array
C
          DO 20 I = 1, NUPDAT 
              RESULT(CHAN,I) = P(I)
20        CONTINUE
30    CONTINUE
C 
      ISTAT = 0
      GO TO 1000
999   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT = 1
1000  RETURN
      END
