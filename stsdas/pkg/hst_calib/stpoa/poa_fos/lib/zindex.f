C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE ZINDEX(XA,N,X,INDEX,RINDEX)
*
* Module Number: 13.10.3.1
*
* Module Name: zindex
*
* Keyphrase:
* ----------
* Compute effective index of data value in array
*
* Description:
* ------------
* Using Bisection, the location of a data value is found in an array.
* The index (INDEX) is set to the location where the input X lies
* between XA(INDEX) and XA(INDEX-1).  The effective index (including
* fractions of a data point) is returned in RINDEX.  If X is less than
* the minimum of XA then INDEX is set to 1.  If X is greater than the
* maximum of XA, INDEX is set to N-1.  When X is not in the bounds of
* XA, RINDEX is set to an extrapolated value.
*
* XA must be in ascending order.
*
* Fortran Name: zindex.for
*
* Keywords of accessed files and tables:
* --------------------------------------
*       none
*
* Subroutines Called:
* -------------------
* CDBS:
*       none
* SDAS:
*       none
*
* History:
* --------
* version         date            Author          Description
*    1          3/5/87          D. Lindler      Designed and coded
*    2          DEC 87          D. LINDLER      NEW SDAS STANDARDS
*-------------------------------------------------------------------------
*
* Input parameters
*
*       XA - vector to be searched (real*8)
*       N - number of points in XA (integer)
*       X - value to search for (real*8)
*
* Output parameters
*
*       INDEX - index postion of X (integer)
*       RINDEX - effective index including fractional pixels (real*8)
*
	IMPLICIT NONE
        DOUBLE PRECISION XA(*),X,RINDEX
        INTEGER N,INDEX
*
* local variables
*
        INTEGER HI,LOW
*
*---------------------------------------------------------------------
C
C INITIALIZE HI AND LOW TO END POINTS
C
        LOW=1
        HI=N
C
C BISECT ARRAY AND RESET HI OR LOW DEPENDING ON WHICH SIDE OF THEIR
C CENTER X LIES.
C
C STOP WHEN HI=LOW+1
C
1       IF((HI-LOW).GT.1)THEN
                INDEX=(HI+LOW)/2
                IF(XA(INDEX).GT.X) THEN
                        HI=INDEX
                   ELSE
                        LOW=INDEX
                ENDIF
                GO TO 1
        ENDIF
C
C COMPUTE FRACTIONAL INDEX USING INTERPOLATION
C
        INDEX = LOW
        RINDEX = INDEX + (X-XA(INDEX))/(XA(INDEX+1)-XA(INDEX))
        RETURN
        END
