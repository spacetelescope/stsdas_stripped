        SUBROUTINE ZCLPLY(IN,NS,ORDER,ISTAT)
*
*  Module number:
*
*  Module name: ZCLPLY
*
*  Keyphrase:
*  ----------
*       Polynomial smooth the background
*
*  Description:
*  ------------
*       This routine performs a polynomial smoothing of the background
*       with a user specified polynoimal order.
*
*  FORTRAN name: ZCLPLY.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91  S. Hulbert      Designed and coded
*-------------------------------------------------------------------------------
*
* INPUT:
*       NS - number of elements in Y
*       ORDER - order of polynomial to fit
*
* INPUT/OUTPUT:
*       IN - input array to filter
*
* OUTPUT:
*       ISTAT - error status
*
*----------------------------------------------------------------
        INTEGER NS,ISTAT,ORDER
        REAL IN(1)
C------------------------------------------------------------------------------
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(1)
      INTEGER*2        MEMS(1)
      INTEGER*4        MEMI(1)
      INTEGER*4        MEML(1)
      REAL             MEMR(1)
      DOUBLE PRECISION MEMD(1)
      COMPLEX          MEMX(1)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C------------------------------------------------------------------------------
      INTEGER TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C local variables
C
        INTEGER X, Y, SIGMA, COEF, YFIT, ISTATS(5)
C
C-------------------------------------------------------------------
C
C Check filter size
C
        IF ((ORDER.LT.0) .OR. (ORDER .GT. 9)) THEN
           CALL ZMSPUT('ERROR: invalid order for polynomial smoothing',
     *     	STDOUT+STDERR,0,ISTAT)
	   GO TO 999
	ENDIF
C
C allocate memory for temporary arrays
C
	CALL UDMGET (NS, TYDOUB, X, ISTATS(1))
	CALL UDMGET (NS, TYDOUB, Y, ISTATS(2))
	CALL UDMGET (NS, TYDOUB, SIGMA, ISTATS(3))
	CALL UDMGET (ORDER+1, TYDOUB, COEF, ISTATS(4))
	CALL UDMGET (NS, TYDOUB, YFIT, ISTATS(5))
	DO 200 I = 1, 5
	    IF (ISTATS(I) .NE. 0) THEN
		CALL ZMSPUT('ERROR allocating memory',
     *          	STDOUT+STDERR,0,ISTAT)
		GO TO 999
	    ENDIF
200	CONTINUE
C
C set up x and sigma vector
C
        DO 10 I=1,NS
            MEMD(Y+I-1) = DBLE(IN(I))
            MEMD(X+I-1) = DBLE(I)
            MEMD(SIGMA+I-1) = 1.0D0
10	CONTINUE
C
C do the fit
C
	CALL POLYFT (MEMD(X),MEMD(Y),MEMD(SIGMA),NS,ORDER,
     $		MEMD(COEF),MEMD(YFIT),ISTAT)
	IF (ISTAT .NE. 0) THEN
           CALL ZMSPUT('ERROR performing polynomial smoothing',
     *     	STDOUT+STDERR,0,ISTAT)
	   GO TO 999
	ENDIF
	DO 100 I = 1, NS
	    IN(I) = MEMD(YFIT+I-1)
100	CONTINUE
C
C deallocate
C
	CALL UDMFRE (X, TYDOUB, ISTATS(1))
	CALL UDMFRE (SIGMA, TYDOUB, ISTATS(2))
	CALL UDMFRE (COEF, TYDOUB, ISTATS(3))
	CALL UDMFRE (YFIT, TYDOUB, ISTATS(4))
	CALL UDMFRE (Y, TYDOUB, ISTATS(5))
	DO 210 I = 1, 5
	    IF (ISTATS(I) .NE. 0) THEN
		CALL ZMSPUT('ERROR deallocating memory',
     *          	STDOUT+STDERR,0,ISTAT)
		GO TO 999
	    ENDIF
210	CONTINUE
        ISTAT=0
	GO TO 1000
C
999	ISTAT=1
1000    RETURN
        END
