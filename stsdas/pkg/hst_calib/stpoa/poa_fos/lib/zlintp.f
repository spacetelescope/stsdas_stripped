C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE ZLINTP(X,Y,NX,XOUT,YOUT,NXOUT,STATUS)
C
C SUBROUTINE TO PERFORM LINEAR INTERPOLATION FOR DATA POINTS XOUT
C IN X,Y
C
C VERSION 1  NOV. 1985  D. LINDLER
C Modified  9-May-1994  H. Bushouse (check for NX=1)
C
C  X,Y ARRAYS OF NX POINTS TO BE INTERPOLATED IN
C  NX - SIZE OF X AND Y
C  XOUT - X VALUES WHERE Y VALUES ARE DESIRED
C  YOUT - INTERPOLATED Y VALUES
C  NXOUT - SIZE OF XOUT AND YOUT
C
	IMPLICIT NONE
        DOUBLE PRECISION X(*),XOUT(*)
        INTEGER STATUS
        DOUBLE PRECISION Y(*),YOUT(*)
        INTEGER NX,NXOUT
        INTEGER IPOS
C                                    ---> CURRENT POSITION IN X ARRAY
        INTEGER I,II
C                                    ---> INDEX IN XOUT AND YOUT
        DOUBLE PRECISION FRAC
C                                    ---> FRACTIONAL DISTANCE BETWEEN X(IPOS) ANC
C                                       X(IPOS+1) OR XOUT(I)
C
C INITIALIZATION
C
        IPOS=1
C                                    ---> START AT BEGGINING OF X,Y
        STATUS = 0
C
C LOOP ON VALUES IN XOUT
C
        DO 100 I=1,NXOUT
C
	   IF (NX.EQ.1) THEN
	       YOUT(I) = Y(1)
	       GO TO 100
	   END IF
C
C FIND  IPOS SUCH THAT X(IPOS) <= XOUT(I) < X(IPOS+1)
C
C DETERMINE IF WE SHOULD DECREASE IPOS
C
           DO 10 II=1,NX
                IF((IPOS.EQ.1).OR.(XOUT(I).GT.X(IPOS)))GO TO 20
                IPOS=IPOS-1
10         CONTINUE
C
C DETERMINE IF WE SHOULD INCREASE IPOS
C
20         DO 30 II=1,NX
                IF((IPOS.EQ.(NX-1)).OR.(XOUT(I).LE.X(IPOS+1))) GO TO 40
                IPOS=IPOS+1
30         CONTINUE
C
C
C DETERMINE FRACTIONAL DISTANCE BETWEEN X(IPOS) AND X(IPOS+1)
C
40         IF (X(IPOS) .EQ. X(IPOS+1)) THEN
                FRAC=0.0
             ELSE
                FRAC= (XOUT(I)-X(IPOS)) / (X(IPOS+1)-X(IPOS))
             ENDIF
C
C COMPUTE INTERPOLATED VALUE
C
           YOUT(I) = Y(IPOS) + FRAC * (Y(IPOS+1)-Y(IPOS))
100     CONTINUE
        RETURN
        END
