        SUBROUTINE YCLCRT (CHIRAD, DELTRAD, RETMAT, ISTAT)
*
*  Module number:
*
*  Module name: YCLCRT
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       Creates Mueller Matrix for Retarder
*
*  FORTRAN name: YCLCRT
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
*       1       Mar97   M.D. De La Pena   Original Implementation for CALFOS
*                                         based upon code from R. Allen.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       CHIRAD  - Angle of the plane of incidence in radians
*       DELTRAD - Angle of retardance in radians
*
* INPUT/OUTPUT:
*       RETMAT  - Mueller matrix for the retarder
*       ISTAT   - Modified return status
*----------------------------------------------------------------------------
C
C Passed Parameters
C
      DOUBLE PRECISION CHIRAD, DELTRAD, RETMAT(4,4)
      INTEGER ISTAT
C
C Local Variables
C
      DOUBLE PRECISION D, E, G, RATIO
      DOUBLE PRECISION DZERO, DONE, DTWO
C
      PARAMETER (DZERO = 0.0D0)
      PARAMETER (DONE  = 1.0D0)
      PARAMETER (DTWO  = 2.0D0)
C
C----------------------------------------------------------------------------
C
      RATIO = DELTRAD / DTWO
      D = DCOS(DTWO * CHIRAD) * DSIN(RATIO)
      E = DSIN(DTWO * CHIRAD) * DSIN(RATIO)
      G = DCOS(RATIO)
C
C Fill the retardation matrix
C
      RETMAT(1,1) = DONE
      RETMAT(1,2) = DZERO
      RETMAT(1,3) = DZERO
      RETMAT(1,4) = DZERO
      RETMAT(2,1) = DZERO
      RETMAT(2,2) = D * D + G * G - E * E
      RETMAT(2,3) = DTWO * D * E
      RETMAT(2,4) = -DTWO * E * G
      RETMAT(3,1) = DZERO
      RETMAT(3,2) = RETMAT(2,3)
      RETMAT(3,3) = -D * D + G * G + E * E
      RETMAT(3,4) = DTWO * D * G
      RETMAT(4,1) = DZERO
      RETMAT(4,2) = -RETMAT(2,4)
      RETMAT(4,3) = -RETMAT(3,4)
      RETMAT(4,4) = DTWO * G * G - DONE
C
      ISTAT = 0
      RETURN
      END
