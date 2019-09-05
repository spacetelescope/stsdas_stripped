        SUBROUTINE ZCLQTP(NX,X,Y,NXOUT,XOUT,YOUT)
*
*  Module number:
*
*  Module name: ZCLQTP
*
*  Keyphrase:
*  ----------
*       Quadratic (Lagrangian) interpolation
*
*  Description:
*  ------------
*       This routine preforms quadratic interpolation within
*       input vectors X and Y at positions in vector XTAB.
*       The input vector X should be monitonically increasing
*       or decreasing.
*
*  FORTRAN name: zclqtp
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*       none
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* INPUTS:
*
*       NX - number of points in input X and Y table
*       X - x-positions in input table
*       Y - y-positions in input table
*       NXOUT - number of points to interpolate
*       XOUT - points to interpolate
*
* OUTPUTS:
*       YOUT - interpolated values
*
*------------------------------------------------------------------------------
        DOUBLE PRECISION XOUT(1)
        REAL Y(1),YOUT(1),X(1)
        INTEGER NX,NXOUT
C
C Local variables
C
        INTEGER IPOS
C                                    ---> CURRENT POSITION IN X ARRAY
        INTEGER I,II,K
C                                    ---> INDEX IN XOUT AND YOUT
C                                       X(IPOS+1) OR XOUT(I)
        REAL A,B,C,D,X0,X1,X2,X3,XX
        INTEGER NX3
C-------------------------------------------------------------------------------
C
C INITIALIZATION
C
        IPOS=1
C                                    ---> START AT BEGGINING OF X,Y
        NX3 = NX-3
C                                    ---> Number of input points minus 3
C LOOP ON VALUES IN XOUT
C
        DO 100 I=1,NXOUT
          XX = XOUT(I)
C
C FIND  IPOS SUCH THAT X(IPOS) <= XOUT(I) < X(IPOS+1)
C
C DETERMINE IF WE SHOULD DECREASE IPOS
C
           DO 10 II=1,NX
                IF((IPOS.EQ.1).OR.(XX.GT.X(IPOS)))GO TO 20
                IPOS=IPOS-1
10         CONTINUE
C
C DETERMINE IF WE SHOULD INCREASE IPOS
C
20         DO 30 II=1,NX
                IF((IPOS.EQ.(NX-1)).OR.(XX.LE.X(IPOS+1)))
     *                   GO TO 40
                IPOS=IPOS+1
30         CONTINUE
C
C Determine position of the first of four points to use
C
40         K = IPOS-1
           IF(K.LT.1)K=1
           IF(K.GT.NX3)K=NX3
C
C get x values for the four points
C
           X0 = X(K)
           X1 = X(K+1)
           X2 = X(K+2)
           X3 = X(K+3)
C
C compute weights for average of two quadratics
C
           A = (XX-X1) * (XX-X2) / (X0-X1) / (X0-X2)
           B = ((XX-X0) / (X1-X0) + (XX-X3) / (X1-X3)) *
     *                                          (XX-X2) / (X1-X2)
           C = ((XX-X0) / (X2-X0) + (XX-X3) / (X2-X3)) *
     *                                          (XX-X1) / (X2-X1)
           D = (XX-X1) * (XX-X2) / (X3-X1) / (X3-X2)
C
C COMPUTE INTERPOLATED VALUE
C
           YOUT(I) = (A*Y(K) + B*Y(K+1) + C*Y(K+2) + D*Y(K+3))/2.0
100     CONTINUE
        RETURN
        END
