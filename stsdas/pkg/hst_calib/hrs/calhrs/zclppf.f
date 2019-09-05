        SUBROUTINE ZCLPPF(VALUE,Q0,Q1,F,TAU1)
*
*  Module number:
*
*  Module name: ZCLPPF
*
*  Keyphrase:
*  ----------
*       Perform HRS paired pulse correction
*  Description:
*  ------------
*       This routine performs the paired pulse correction
*       of GHRS data.
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
*  FORTRAN name: ZCLPPF.FOR
*
*  Subroutines Called:
*  -------------------
*       NONE
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 88  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* Input parameters
*       Q0,Q1,F,TAU1 - all real*4 (paired pulse coefficients from
*                       relation CCG2.
*
* Input/Output
*       VALUE - data value (on output it is paired pulse corrected)
*
* Output
*
*       ISTAT - error status
*
*------------------------------------------------------------------------------
        REAL VALUE,Q0,Q1,F,TAU1
        REAL X,T
C
C CASE 1  Q0 is not zero
C
        IF (Q0 .NE. 0.0) THEN
                T=Q0
                IF(VALUE.GT.F)T=T+(VALUE-F)*Q1
                X=(1.0-VALUE*T)
                IF(X .LT. 1.0E-12) X=1.0E-12
                VALUE=VALUE/X
            ELSE
C
C CASE 2  Q0 is zero
C
                X=(1.0-VALUE*TAU1)
                IF(X .LT. 1.0E-12) X=1.0E-12
                VALUE=LOG(X)/(-TAU1)
        ENDIF
        RETURN
        END
