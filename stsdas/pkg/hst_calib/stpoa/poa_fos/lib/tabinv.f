C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE TABINV(X,N,XVAL,RPOS)
*
*  Module number: HRS/FOS UTILITY
*
*  Module name: TABINV
*
*  Keyphrase:
*  ----------
*       find table position
*  Description:
*  ------------
*       this routine finds the position of XVAL in the vector
*       X.  If XVAL is not in the array X then linear interpolation
*       or extrapolation is used to give a relative postion.
*       The input vector x must be in ascending order.
*
*  FORTRAN name: tabinv.for
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
*    1         Oct. 87    D. Lindler    Designed and coded
*-------------------------------------------------------------------------------
C
C INPUT PARAMETERS
C
C       X - VECTOR OF VALUES TO SEARCH (REAL*8)
C       N - NUMBER OF ELEMENTS IN X (INTEGER)
C       XVAL - VALUE TO SEARCH FOR (REAL*8)
C
C OUTPUT PARAMETERS
C
C       RPOS - EFFECTIVE INDEX POSITION (REAL*8)
C
C----------------------------------------------------------------------------
	IMPLICIT NONE
        INTEGER N
        DOUBLE PRECISION X(N),XVAL,RPOS
C
C LOCAL VARIABLES
C
        INTEGER I,I1,IPOS
        DOUBLE PRECISION DIF
C
C LOOP TO FIND WHICH TWO VALUES X LIES BETWEEN
C
        DO 10 I=2,N
                IPOS=I
                IF(XVAL .LT. X(I)) GO TO 20
10      CONTINUE
C
C INTERPOLATE TO FIND EFFECTIVE INDEX
C
20      I1=IPOS-1
        DIF = X(IPOS)-X(I1)
        IF(DIF.EQ.0)THEN
                RPOS = I1+0.5
           ELSE
                RPOS = I1 + (XVAL-X(I1))/DIF
        ENDIF
        RETURN
        END
