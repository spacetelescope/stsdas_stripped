C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLMCR (RESULT, REFRAT, RETARD, NX, MAXLEN, NUPDAT,
     *                     ISTAT)
*
*  Module number:
*
*  Module name: YCLMCR
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       Inverts mirror reflection
*
*  FORTRAN name: YCLMCR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yclcrt, yclcpl, yclcst, ymsput
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
*       REFRAT - Array of the reflection ratio of the mirrors, rp/rs
*       RETARD - Array of the phase of a retarder
*       NX     - Number of channels
*       MAXLEN - Maximum number of channels
*       NUPDAT - Number of quantities being updated
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
      DOUBLE PRECISION RETARD(MAXLEN), REFRAT(MAXLEN)
C
C Local Variables
C
      CHARACTER*80 CONTXT
      INTEGER      CHAN, I, J, K
      DOUBLE PRECISION CHIRAD, DELTRAD, PSIRAD
      DOUBLE PRECISION SUM, RP, RS, DPI
      DOUBLE PRECISION EQUIV(4,4), RETMAT(4,4), NLPMAT(4,4)
C
C CHIRAD is the angle of the plane of incidence = 0.0 in the COSTAR frame
C PSIRAD is the polarizing axis which is perpendicular to the plane of
C   incidence == 90 degrees in the COSTAR frame
C DELTRAD is the retardance
C RS is the reflectivity perpendicular to the plane of incidence
C RP is the reflectivity parallel to the plane of incidence
C
      PARAMETER (CHIRAD = 0.0D0)
      PARAMETER (RS     = 1.0D0)
      PARAMETER (DPI    = 3.1415926536D0)
C
C----------------------------------------------------------------------------
C
C Loop over all the channels, DELTRAD and RP are a function of channel
C
      PSIRAD = DPI / 2.0D0
C
      DO 40 CHAN = 1, NX
          DELTRAD = -RETARD(CHAN)
          RP      = 1.0D0 / REFRAT(CHAN)
C
C Generate the Mueller Matrix for the retarder
C
          CALL YCLCRT (CHIRAD, DELTRAD, RETMAT, ISTAT)
          IF (ISTAT .NE. 0) THEN
              CONTXT = 'ERROR in computing the Mueller Matrix for' //
     *                 ' the retarder.'
              GO TO 999
          END IF
C
C Generate the Mueller Matrix for the polarizer
C
          CALL YCLCPL (RS, RP, PSIRAD, NLPMAT, ISTAT)
          IF (ISTAT .NE. 0) THEN
              CONTXT = 'ERROR in computing the Mueller Matrix for' //
     *                 ' the polarizer.'
              GO TO 999
          END IF
C
          DO 30 I = 1, 4
              DO 20 J =1, 4
                  SUM = 0.0D0
                  DO 10 K = 1, 4
                      SUM = SUM + NLPMAT(I,K) * RETMAT(K,J)
   10             CONTINUE
                  EQUIV(I,J) = SUM
   20         CONTINUE
   30     CONTINUE
C
          CALL YCLCST(RESULT, MAXLEN, NUPDAT, CHAN, EQUIV, ISTAT)
          IF (ISTAT .NE. 0) THEN
              CONTXT = 'ERROR in computing the normalized Stokes' //
     *                 ' parameters.'
              GO TO 999
          END IF
C
   40 CONTINUE
C
      ISTAT = 0
      GO TO 1000
999   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT = 1
1000  RETURN
      END
