        SUBROUTINE ZCLINT(NX,X,Y,NXOUT,XOUT,YOUT)
*
*  Module number:
*
*  Module name: ZCLINT
*
*  Keyphrase:
*  ----------
*	Linear interpolation
*
*  Description:
*  ------------
*	This routine preforms linear interpolation within
*	input vectors X and Y at positions in vector XTAB.
*
*  FORTRAN name: zclint
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*	none
*
*  Subroutines Called:
*  -------------------
*	none
*
*  History:
*  --------
*  Version      Date        Author          Description
*	1	May 89	D. Lindler	Designed and coded
*-------------------------------------------------------------------------------
*
* INPUTS:
*
*	NX - number of points in input X and Y table
*	X - x-positions in input table
*	Y - y-positions in input table
*	NXOUT - number of points to interpolate
*	XOUT - points to interpolate
*
* OUTPUTS:
*	YOUT - interpolated values
*
*------------------------------------------------------------------------------
        REAL X(1),XOUT(1)
        REAL Y(1),YOUT(1)
C
C Local variables
C
        INTEGER NX,NXOUT
        INTEGER IPOS
C                                    ---> CURRENT POSITION IN X ARRAY
        INTEGER I,II
C                                    ---> INDEX IN XOUT AND YOUT
        REAL FRAC
C                                 ---> FRACTIONAL DISTANCE BETWEEN X(IPOS) AND
C                                       X(IPOS+1) OR XOUT(I)
C-------------------------------------------------------------------------------
C
C INITIALIZATION
C
        IPOS=1
C                                    ---> START AT BEGGINING OF X,Y
C LOOP ON VALUES IN XOUT
C
        DO 100 I=1,NXOUT
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
