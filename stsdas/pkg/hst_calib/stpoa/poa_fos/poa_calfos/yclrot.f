C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLROT (RESULT, NX, MAXLEN, RHO, ISTAT)
*
*  Module number:
*
*  Module name: YCLROT
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       Rotate the angle of polarization counter-clockwise into the
*       sky frame.  Update U, Q, the associated errors, and theta.
*
*  FORTRAN name: YCLROT
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*
*
*  History:
*  --------
*  Version      Date    Author		  Description
*       1       May97   M.D. De La Pena   Original Implementation for CALFOS
*                                         based upon code from R. Allen.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       RESULT - Array containing Stokes parameters, linear and circular 
*                polarization, polarization angle, and error estimates
*       NX     - Number of channels
*       MAXLEN - Maximum number of channels
*       RHO    - Angle of polarization in the sky frame
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
      INTEGER          NX, MAXLEN, ISTAT
      REAL             RESULT(MAXLEN,14)
      DOUBLE PRECISION RHO
C
C Local Variables
C
      INTEGER CHAN
      DOUBLE PRECISION XSC, SCALE, SIGMA, THETA
      DOUBLE PRECISION XQ, XU, Q, U, QVAR, UVAR, QSIG, USIG
      DOUBLE PRECISION SIN2RHO, COS2RHO
C
C----------------------------------------------------------------------------
C
      XSC    = 1.0D0
      SCALE  = 1.0D0
C
C Determine the scaling factor based upon the error of Stokes I
C
      DO 10 CHAN = 1, NX
          SIGMA = RESULT(CHAN,5)
          IF (SIGMA .NE. 0.0) THEN 
              IF (SIGMA .LT. 1.0D-08) XSC = 1.0D+12
              IF (SIGMA .LT. 1.0D-16) XSC = 1.0D+20
              IF (SIGMA .LT. 1.0D-24) XSC = 1.0D+28
              IF (XSC .GT. SCALE) SCALE = XSC
          ENDIF
   10 CONTINUE
C
C Recompute Q, U, the associated errors and theta in the sky frame
C
      SIN2RHO = DSIN(2.0D0 * RHO)
      COS2RHO = DCOS(2.0D0 * RHO)
      DO 20 CHAN = 1, NX
          XQ    = SCALE * RESULT(CHAN,2)
          XU    = SCALE * RESULT(CHAN,3)
          QSIG  = SCALE * RESULT(CHAN,6)
          USIG  = SCALE * RESULT(CHAN,7)
C
          Q     = XQ * COS2RHO - XU * SIN2RHO
          U     = XQ * SIN2RHO + XU * COS2RHO
          QVAR  = COS2RHO*COS2RHO*QSIG*QSIG + SIN2RHO*SIN2RHO*USIG*USIG 
          UVAR  = SIN2RHO*SIN2RHO*QSIG*QSIG + COS2RHO*COS2RHO*USIG*USIG
          THETA = RESULT(CHAN,11) + RHO
C
          RESULT(CHAN,2)  = Q / SCALE
          RESULT(CHAN,3)  = U / SCALE
          RESULT(CHAN,6)  = DSQRT(QVAR) / SCALE
          RESULT(CHAN,7)  = DSQRT(UVAR) / SCALE
          RESULT(CHAN,11) = THETA
   20 CONTINUE
C
      ISTAT = 0
      RETURN
      END
