C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YWMNMX(DATA,NS,RMIN,RMAX)
*
*  Module number:
*
*  Module name: YWMNMX
*
*  Keyphrase:
*  ----------
*       Find minimum and maximum.
*  Description:
*  ------------
*     This routine finds the minimum and maximum of a real*4 vector
*     for wavelengths.  The minimum must be strictly greater than 0.
*
*  FORTRAN name: YWMNMX.for
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
*   1           JUL 89  D. Lindler      Designed and coded
*   1.1		Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*   1.2         Dec 95  J. Eisenhamer   Do not included non-calculated
*                                       wavelengths in the min/max.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       data - floating point data vector
*       ns - number of data points in data
*
* OUTPUTS:
*       rmin - minimum of data (real*4)
*       rmax - maximum of data (real*4)
*
*-------------------------------------------------------------------------
        REAL DATA(*)
        INTEGER NS
        REAL RMIN,RMAX
C
C local variables
C
        INTEGER I
C--------------------------------------------------------------------------
        RMIN = 10000
        RMAX = DATA(1)
        IF (NS.GT.1)THEN
           DO 10 I=2,NS
              RMAX = MAX (RMAX, DATA(I))
              IF (DATA(I).GT.0.) RMIN = MIN (RMIN,DATA(I))
 10        CONTINUE
        ENDIF
        RETURN
        END
