        SUBROUTINE YCLCST (RESULT, MAXLEN, NUPDAT, CHAN, EQUIV, ISTAT)
*
*  Module number:
*
*  Module name: YCLCST
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       Routine which recomputes all the Stokes parameters, the linear and 
*       circular polarization, the phase angle, and associated errors.
*
*  FORTRAN name: YCLCST
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yclnrm
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
*       1.2     Feb98   M.D. De La Pena   Changed I cutoff to allow negative I
*                                         for scaled values.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       RESULT - Array containing Stokes parameters, linear and circular 
*                polarization, polarization angle, and error estimates
*       MAXLEN - Maximum number of channels
*       NUPDAT - Number of quantities being updated
*       CHAN   - Current channel being processed
*       EQUIV  - Matrix of BLAH
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
      DOUBLE PRECISION EQUIV(4,4)
      INTEGER MAXLEN, NUPDAT, CHAN, ISTAT
      REAL    RESULT(MAXLEN,NUPDAT)
C
C Local Variables
C
      CHARACTER*80     CONTXT
      DOUBLE PRECISION STOKIN(8), STOKOUT(8), P(14), SUM, SCALE
      DOUBLE PRECISION DZERO
      INTEGER I, J
C
      PARAMETER (DZERO = 0.0D0)
C
C----------------------------------------------------------------------------
C
C Initialize the temporary array
C
      DO 10 J = 1, NUPDAT
          P(J) = DZERO
10    CONTINUE
C
C Extract the Stokes parameters I, Q, U, V and the associated errors
C
      DO 20 J = 1, 8
          STOKIN(J) = RESULT(CHAN,J)
20    CONTINUE
C
C If the Stokes I for this channel is > 0.0, then proceed.
C Let the scaled Stokes I be negative - MDD 02/11/98
C
CMDD      IF (STOKIN(1) .GT. DZERO) THEN
C
          SCALE = 1.0D+00
          IF(STOKIN(1) .LT. 1.0D-08) SCALE = 1.0D+12
          IF(STOKIN(1) .LT. 1.0D-16) SCALE = 1.0D+20
          IF(STOKIN(1) .LT. 1.0D-24) SCALE = 1.0D+28
C
C Rescale the data
C
          DO 30 J = 1, 8
              STOKIN(J) = SCALE * STOKIN(J)
30        CONTINUE
C
C Scaled Stokes I can be negative - MDD 02/11/98
C
      IF (ABS(STOKIN(1)) .GT. 1.0D-8) THEN
C
C Recompute the Stokes I, Q, U, and V
C
          DO 50 I = 1, 4
              SUM = DZERO
              DO 40 J = 1, 4
                  SUM = SUM + EQUIV(I,J) * STOKIN(J)
40            CONTINUE
              STOKOUT(I) = SUM
50        CONTINUE
C
C Recompute the errors associated with the Stokes I, Q, U, and V
C
          DO 70 I = 1, 4
              SUM = DZERO
              DO 60 J = 1, 4
                  SUM = SUM + (EQUIV(I,J)*EQUIV(I,J)*
     *                        STOKIN(J+4)*STOKIN(J+4))
60            CONTINUE
              STOKOUT(I+4) = DSQRT(SUM)
70        CONTINUE
C
          DO 80 J = 1, 8
              P(J) = STOKOUT(J) / SCALE
80        CONTINUE
C
C Recompute the linear and circular polarization, polarization angle, 
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
          DO 90 I = 1, NUPDAT 
              RESULT(CHAN,I) = P(I)
90        CONTINUE
C
      ELSE
C
C When Stokes I is <= zero, set PL, PC, and THETA (and associated errors)
C to zero.
C
          DO 110 I = 9, NUPDAT
             RESULT(CHAN,I) = DZERO
110       CONTINUE
C
      ENDIF
C
      ISTAT = 0
      GO TO 1000
999   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT = 1
1000  RETURN
      END
