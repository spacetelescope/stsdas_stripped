        SUBROUTINE YCLNRM (RESTMP, NUPDAT, CHAN, ISTAT)
*
*  Module number:
*
*  Module name: YCLNRM
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       Uses the updated U Stokes and U error data which have been modified
*       due to post-COSTAR effects to rederive the linear and circular 
*       polarization, as well as the phase angle.  In addition, rederive 
*       all the associated errors.
*
*  FORTRAN name: yclnrm
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*
*
*  History:
*  --------
*  Version      Date    Author		  Description
*       1       Mar97   M.D. De La Pena   Original Implementation for CALFOS
*                                         based upon code from R. Allen.
*       1.1     Jul97   M.D. De La Pena   Mods for new parameter
*       1.2     Feb98   M.D. De La Pena   Changed I cutoff to allow negative I
*                                         for scaled values.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       RESTMP - 1D-Array containing Stokes parameters, linear and circular 
*                polarization, polarization angle, and error estimates
*       NUPDAT - Number of quantities being updated
*       CHAN   - Current channel (pixel) being processed
*       ISTAT  - Return status
*
* OUTPUT:
*       RESTMP - Modified input array
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
      INTEGER          NUPDAT, CHAN, ISTAT
      DOUBLE PRECISION RESTMP(NUPDAT)
C
C Local Variables
C
      DOUBLE PRECISION PL, PC, TH
      DOUBLE PRECISION PLSIG, PCSIG, THSIG
      DOUBLE PRECISION I, Q, U, V
      DOUBLE PRECISION ISIG, QSIG, USIG, VSIG 
      DOUBLE PRECISION I2, Q2, U2, V2
      DOUBLE PRECISION IVAR, QVAR, UVAR, VVAR, PLVAR, PCVAR, THVAR
      DOUBLE PRECISION Q2U2, DENOM, FACTOR
C
      INTEGER      J
      DOUBLE PRECISION SCALE, DZERO, DPI
C
      PARAMETER (DPI   = 3.1415926536D0)
      PARAMETER (DZERO = 0.0D0)
C
C----------------------------------------------------------------------------
C
C Initialize the new variables to be computed
C
      PL = DZERO
      PC = DZERO
      TH = DZERO
      PLSIG = DZERO
      PCSIG = DZERO
      THSIG = DZERO
C
C Initialize the variables with I, Q, U, and V and associated error
C values for the specific channel being processed
C
      I = RESTMP(1)
      Q = RESTMP(2)
      U = RESTMP(3)
      V = RESTMP(4)
      ISIG = RESTMP(5)
      QSIG = RESTMP(6)
      USIG = RESTMP(7)
      VSIG = RESTMP(8)
C
C The I channel value must be greater than zero for further processing
C Let scaled I be negative - MDD 02/11/98
C
CMDD      IF (I .GT. DZERO) THEN
C
C Determine the scale parameter and rescale the data
C
          SCALE = 1.0D0
          IF (I .LT. 1.0D-08) SCALE = 1.0D+12
          IF (I .LT. 1.0D-16) SCALE = 1.0D+20
          IF (I .LT. 1.0D-24) SCALE = 1.0D+28
          I = SCALE * I
          Q = SCALE * Q
          U = SCALE * U
          V = SCALE * V
          ISIG = SCALE * ISIG
          QSIG = SCALE * QSIG
          USIG = SCALE * USIG
          VSIG = SCALE * VSIG
C
C Scaled I can be negative - MDD 02/11/98
C
      IF (ABS(I) .GT. 1.0D-8) THEN
C
C Compute the squares and the variances for I, Q, U, and V 
C
          I2 = I * I
          Q2 = Q * Q
          U2 = U * U
          V2 = V * V
          IVAR = ISIG * ISIG
          QVAR = QSIG * QSIG
          UVAR = USIG * USIG
          VVAR = VSIG * VSIG
C
C Compute the linear and circular polarization
C
          PL = DSQRT(Q2 + U2) / I
          PC = V / I
C
C Compute theta
C
          IF (PL .GT. DZERO) TH = 0.5D0 * DATAN2(U,Q)
          IF (TH .LT. DZERO) TH = TH + DPI
C
C Compute the linear polarization variance and standard error
C Compute the theta variance and standard error
C
          Q2U2 = Q2 + U2
          IF (Q2U2 .GT. DZERO) THEN
              DENOM  = I2 * Q2U2
              FACTOR = Q2U2 * Q2U2
C
              PLVAR  = (Q2*QVAR + U2*UVAR)/DENOM + (Q2U2*IVAR)/(I2*I2)
              PLSIG  = DSQRT(PLVAR)
C
              THVAR  = (Q2*UVAR + U2*QVAR)/(4.0D0*FACTOR)
              THSIG  = DSQRT(THVAR)
          ENDIF
C
C Compute the circular polarization variance and standard error
C
          PCVAR = VVAR/I2 + V2*IVAR/(I2*I2)
          PCSIG = DSQRT(PCVAR)
C
C Store PL, PC, and THETA
C
          RESTMP(9)  = PL
          RESTMP(10) = PC
          RESTMP(11) = TH
C
C Store the standard errors for PL, PC, and THETA
C
          RESTMP(12) = PLSIG
          RESTMP(13) = PCSIG
          RESTMP(14) = THSIG
C
      ELSE
C
C When I is <= zero, set PL, PC, and THETA (and associated errors)
C to zero.
C
          DO 10 J = 9, 14
              RESTMP(J) = DZERO
10        CONTINUE
C
      ENDIF
C
      ISTAT = 0
      RETURN
      END
