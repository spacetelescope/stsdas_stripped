        SUBROUTINE YCLCPL (RS, RP, PSIRAD, NLPMAT, ISTAT)
*
*  Module number:
*
*  Module name: YCLCPL
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       Creates Mueller Matrix for Polarizer
*
*  FORTRAN name: YCLCPL
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
*       RS - Reflectivity perpendicular to the plane of incidence
*       RP - Reflectivity parallel to the plane of incidence
*       PSIRAD - Polarizing axis
*
* INPUT/OUTPUT:
*       NLPMAT - Mueller matrix for the polarizer
*       ISTAT  - Modified return status
*----------------------------------------------------------------------------
C
C Passed Parameters
C RS=1.0 and RP=0.0 for a perfect polarizer
C
      DOUBLE PRECISION RS, RP, PSIRAD, NLPMAT(4,4)
      INTEGER ISTAT
C
C Local Variables
C
      DOUBLE PRECISION C2, S2, RSADRP, RSSURP, RSMURP
      DOUBLE PRECISION DHALF, DZERO, DTWO
C
      PARAMETER (DZERO = 0.0D0)
      PARAMETER (DHALF = 0.5D0)
      PARAMETER (DTWO  = 2.0D0)
C
C----------------------------------------------------------------------------
C
      C2 = DCOS(DTWO * PSIRAD)
      S2 = DSIN(DTWO * PSIRAD)
C
C Define arithmetic relationships used multiple times
C
      RSADRP = RS + RP
      RSMURP = RS * RP
      RSSURP = RS - RP
C
C Fill the matrix
C
      NLPMAT(1,1) = DHALF * RSADRP
      NLPMAT(1,2) = DHALF * C2 * RSSURP
      NLPMAT(1,3) = DHALF * S2 * RSSURP
      NLPMAT(1,4) = DZERO
      NLPMAT(2,1) = NLPMAT(1,2)
      NLPMAT(2,2) = DHALF * C2 * C2 * RSADRP + S2 * S2 * DSQRT(RSMURP)
      NLPMAT(2,3) = DHALF * C2 * S2 * RSADRP - S2 * C2 * DSQRT(RSMURP)
      NLPMAT(2,4) = DZERO
      NLPMAT(3,1) = NLPMAT(1,3)
      NLPMAT(3,2) = NLPMAT(2,3)
      NLPMAT(3,3) = DHALF * S2 * S2 * RSADRP + C2 * C2 * DSQRT(RSMURP)
      NLPMAT(3,4) = DZERO
      NLPMAT(4,1) = DZERO
      NLPMAT(4,2) = DZERO
      NLPMAT(4,3) = DZERO
      NLPMAT(4,4) = DSQRT(RSMURP)
C
      ISTAT = 0
      RETURN
      END
