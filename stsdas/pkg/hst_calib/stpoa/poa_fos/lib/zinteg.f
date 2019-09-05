C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE ZINTEG(X,Y,N,XLOW,XHIGH,NI,RLOW,RHIGH,TOTAL)
*
* Module Number:
*
* Module Name: zinteg
*
* Keyphrase:
* ----------
* trapezoidal integration
*
* Description:
* ------------
* The input tabular function X, Y is integrated between XLOW and XHIGH
* using trapezoidal integration.  The effective indices of XLOW and
* XHIGH in X are also returned.
*
* Fortran Name: zinteg.for
*
* Keywords of accessed files and tables:
* --------------------------------------
*       none
*
* Subroutines Called:
* -------------------
* CDBS:
*       zindex
* SDAS:
*       none
*
* History:
* --------
* version         date            Author          Description
*    1          3/5/87          D. Lindler      Designed and coded
*
*-------------------------------------------------------------------------
*
* Input parameters
*
*       X - vector of x-positions (real*8)
*       Y - vector of y-positions (real*8)
*       N - number of points in X and Y
*       XLOW - vector of lower limits of integration (real*8)
*       XHIGH - vector of upper limits of integration (real*8)
*       NI - number of points in XLOW and XHIGH (integer)
*
* Output parameters
*
*       RLOW - vector of effective indices of XLOW (real*8)
*       RHIGH - vector of effective indices of XHIGH (real*8)
*       TOTAL - vector of integrations between XLOW and XHIGH (real*8)
*
	IMPLICIT NONE
        DOUBLE PRECISION X(*),Y(*),XLOW(*),XHIGH(*),TOTAL(*),
     *                      RLOW(*),RHIGH(*)
        INTEGER N,NI
*
* LOCAL VARIABLES
*
        DOUBLE PRECISION Y1,Y2,V1,V2,SUM,R1,R2
        INTEGER I,J
        INTEGER I1,I2
*---------------------------------------------------------------------------
C
C LOOP ON POINTS IN XLOW AND XHIGH
C
        DO 1000 I=1,NI
C
C COMPUTE INDICES OF XLOW(I) AND XHIGH(I)
C
                CALL ZINDEX(X,N,XLOW(I),I1,R1)
                CALL ZINDEX(X,N,XHIGH(I),I2,R2)
C
C INTEGRATE BETWEEN I1 AND I2 (COMPUTE TWICE THE INTEGRAL)
C WE WILL DIVIDE BY TWO AT THE END
C
                SUM=0.0
                IF(I2.NE.I1) THEN
                        DO 100 J=I1,I2-1
                                SUM=SUM+(Y(J)+Y(J+1))*(X(J+1)-X(J))
100                     CONTINUE
                ENDIF
C
C COMPUTE FUNCTION VALUE AT R1 AND R2
C
                Y1 = Y(I1) + (R1-I1)*(Y(I1+1)-Y(I1))
                Y2 = Y(I2) + (R2-I2)*(Y(I2+1)-Y(I2))
C
C COMPUTE INTEGRAL FROM I1 TO R1
C
                V1 = (Y(I1)+Y1)*(XLOW(I)-X(I1))
C
C COMPUTE INTEGRAL FROM I2 TO R2
C
                V2 = (Y(I2)+Y2)*(XHIGH(I)-X(I2))
C
C INTEGRAL FROM R1 TO R2  INTEGRAL(I1-I2) + INTEGRAL(I2-R2) - INTEGRAL(I1,R1)
C
                SUM=SUM+V2-V1
                TOTAL(I)=SUM/2.0
                RLOW(I)=R1
                RHIGH(I)=R2
1000    CONTINUE
        RETURN
        END
