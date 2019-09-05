        SUBROUTINE ZCLPPC(PASS,CCG2,NBINS,DET,EPS,EPSET,DATA,ET,
     *            ERR,ISTAT)
*
*  Module number:
*
*  Module name: ZCLPPC
*
*  Keyphrase:
*  ----------
*       Perform HRS paired pulse correction
*  Description:
*  ------------
*       This routine performs the paired pulse correction
*       of GHRS data.  On the first call the paired pulse parameters
*       the paired pulse parameters are read from table ccg2.
*       if q0 is not equal to zero then the following equation
*       is used:
*                      y
*               x = --------
*                   (1 - yt)
*
*       where:
*               x is the true count rate
*               y is the observed count rate
*               t = q0 for y less than or equal to F
*               t = q0 + q1*(y-F) for y greater than F
*               q0, q1, and F are coefficients in CCG2
*
*       IF q0 is equal to zero then the following equation is used:
*
*               x = log(1-ty)/(-t)
*
*       where:
*               t = tau1 ( a coefficient in CCG2)
*               the log is a natural logarithm
*
*
*  FORTRAN name: ZCLPPC.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccg2                    I       paired pulse coefficient table
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zrccg2
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 88  D. Lindler      Designed and coded
*     1.1       Sep 91  S. Hulbert      Implemented PASS flag
*     1.2	Feb 92	S. Hulbert	Correct error array when correcting
*					data for pp
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       PASS - integer variable set to 1 on first call, -1 on last
*       CCG2 - name of the paired pulse coef. table
*       NBINS - number of substep bins
*       DET - detector number
*       EPS - epsilon (data quality) array for data 500xnbins
*       EPSET - epsilon array for the eng. trailer 24xnbins
*
* Input/Output
*
*       DATA - data array 500xnbins
*       ET - eng. trailer 24xnbins
*       ERR - propagated statistical errors
*
* Output
*
*       ISTAT - error status
*
*------------------------------------------------------------------------------
        INTEGER PASS
        INTEGER NBINS,DET,ISTAT
        CHARACTER*64 CCG2
        REAL DATA(500,7),ERR(500,7),EPS(500,7),ET(24,7),EPSET(24,7)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C HRS epsilons
C
	INTEGER EPSFIL
	PARAMETER (EPSFIL = 800)
	INTEGER EPSSAT
	PARAMETER (EPSSAT = 300)
	INTEGER EPSR20
	PARAMETER (EPSR20 = 190)
	INTEGER EPSR5
	PARAMETER (EPSR5 = 130)
C
C LOCAL VARIABLES ------------------------------------------------------
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
C   PAIRED PULSE COEFFICIENTS
C
        REAL Q0,Q1,F,THRESH,TAU1
	REAL RSAT, R20, R5
	REAL TEMPD
        INTEGER I,J
        CHARACTER*80 CONTXT
C
C-----------------------------------------------------------------------
C
C READ COEFFICIENTS IF FIRST CALL
C
        IF(PASS.EQ.FIRST)THEN
            CALL ZRCCG2(CCG2,DET,TAU1,THRESH,Q0,Q1,F,RSAT,R20,R5,ISTAT)
            IF(ISTAT.NE.0)GO TO 999
            IF(Q0.EQ.0.0)THEN
                WRITE(CONTXT,99)TAU1
99              FORMAT('Paired pulse coef.  tau1=',
     *                  E16.8)
              ELSE
                WRITE(CONTXT,199)Q0,Q1,F
199             FORMAT('Paired pulse coef.  Q0=',E16.8,'  Q1=',E16.8,
     *                 ' F=',F10.0)
            ENDIF
            CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
C
C Loop on substeps bins
C
        DO 100 I=1,NBINS
C
C correct main diode array
C
                DO 30 J=1,500
                    IF ((EPS(J,I) .NE. EPSFIL) .AND. 
     $			    (DATA(J,I) .GT. THRESH)) THEN
C
C saturated?
C
			IF (DATA(J,I) .GT. RSAT) THEN
			    DATA(J,I) = 0.0
			    ERR(J,I) = 0.0
			    IF (EPS(J,I) .LT. EPSSAT) EPS(J,I) = EPSSAT
			ELSE IF (DATA(J,I) .GT. R20) THEN
			    IF (EPS(J,I) .LT. EPSR20) EPS(J,I) = EPSR20
			ELSE IF (DATA(J,I) .GT. R5) THEN
			    IF (EPS(J,I) .LT. EPSR5) EPS(J,I) = EPSR5
			ENDIF 
C
C calculate pp correction
C
			TEMPD = DATA(J,I)
                        CALL ZCLPPF(DATA(J,I),Q0,Q1,F,TAU1)
			IF(TEMPD.GT.0.)THEN
			    ERR(J,I) = SQRT(DATA(J,I)/TEMPD)*ERR(J,I)
			ENDIF
		    ENDIF					
30              CONTINUE
C
C correct special diodes
C
                DO 50 J=1,12
                    IF((EPSET(J,I).NE.EPSFIL).AND.(ET(J,I).GT.THRESH))
     *                  CALL ZCLPPF(ET(J,I),Q0,Q1,F,TAU1)
50              CONTINUE
100     CONTINUE
        ISTAT=0
999     RETURN
        END
