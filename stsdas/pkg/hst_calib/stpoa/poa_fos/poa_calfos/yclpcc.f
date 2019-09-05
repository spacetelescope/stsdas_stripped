C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLPCC (RESULT, NX, MAXLEN, NUPDAT, UCORR1, QCORR1, 
     *                     PCORR1, ISTAT)
*
*  Module number:
*
*  Module name: YCLPCC
*
*  Keyphrase:
*  ----------
*       Polarimetry processing - PCC (post-COSTAR correction)
*
*  Description:
*  ------------
*       Driver routine to apply the post-COSTAR polarization corrections -
*       correct the Stokes parameters and rederive the linear and circular 
*       polarization, as well as the phase angle.  In addition, rederive 
*       all the associated errors.
*
*  FORTRAN name: YCLPCC
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yclucr, yclmcr, ymsput
*
*  SDAS:
*
*
*  History:
*  --------
*  Version      Date    Author		  Description
*       1       Apr97   M.D. De La Pena   Original Implementation - Driver 
*                                         routine for the corrections which are
*                                         based upon code from R. Allen.
*       1.1     Jul97   M.D. De La Pena   Mods for additional parameter
*-------------------------------------------------------------------------------
*
* INPUTS:
*       RESULT - Array containing Stokes parameters, linear and circular 
*                polarization, polarization angle, and error estimates
*       NX     - Number of channels
*       MAXLEN - Maximum number of channels
*       NUPDAT - Number of quantities being updated
*       UCORR1 - Array of corrections for the U Stokes parameter
*       QCORR1 - Array of corrections for the Q Stokes parameter
*       PCORR1 - Array of corrections for the phase angle
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
      INTEGER  NX, MAXLEN, NUPDAT, ISTAT
      REAL     RESULT(MAXLEN,NUPDAT)
      DOUBLE PRECISION UCORR1(MAXLEN), QCORR1(MAXLEN), PCORR1(MAXLEN)
C
C Local Variables
C
      DOUBLE PRECISION REFRAT(2070), RETARD(2070)
      DOUBLE PRECISION DONE, DPI, DGTORD
      INTEGER          I
      CHARACTER*80     CONTXT
C
      PARAMETER (DPI  = 3.1415926536D0)
      PARAMETER (DONE = 1.0D0)
C
C----------------------------------------------------------------------------
C
      DGTORD = DPI / 180.0D0
C
C Apply the U Stokes correction to update U and its associated error.
C Rederive the linear and circular polarization, as well as the phase angle
C and associated errors.
C
      CALL YCLUCR (RESULT, NX, MAXLEN, NUPDAT, UCORR1, ISTAT)
      IF (ISTAT .NE. 0) THEN
          CONTXT = 'ERROR in applying post-COSTAR Stokes U ' //
     *             'correction.'
          GO TO 999
      END IF
C
C Compute the reflection ratio of the mirrors, REFRAT = rp / rs
C Fill RETARD, the phase of the retarder, with PCORR1
C
      DO 10 I = 1, NX
         REFRAT(I) = (DONE + QCORR1(I))/(DONE - QCORR1(I))
         RETARD(I) = PCORR1(I) * DGTORD
10    CONTINUE
C
C Apply the correction to reverse the mirror reflection and modify the
C Stokes vectors
C
      CALL YCLMCR (RESULT, REFRAT, RETARD, NX, MAXLEN, NUPDAT, ISTAT)
      IF (ISTAT .NE. 0) THEN
          CONTXT = 'ERROR in applying the correction to reverse' //
     *             'the mirror reflection.'
          GO TO 999
      END IF
C
      ISTAT = 0
      GO TO 1000
999   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT = 1
1000  RETURN
      END
