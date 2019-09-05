        SUBROUTINE ZDC(M,W,VAL,N)
*
*  Module number: 13.9.1.5
*
*  Module name: ZDC
*
*  Keyphrase:
*  ----------
*       Evaluate terms of dispersion equation
*  Description:
*  ------------
*
*  FORTRAN name: zdc
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*  Subroutines Called:
*  -------------------
*       none
*  History:
*  --------
*  Version      Date        Author          Description
*    1          Dec 87  D. Lindler      Designed and coded
*    2          Nov 92  J. Eisenhamer   Changed table coefficients to 8 and fix
*-------------------------------------------------------------------------------
C
C INPUT PARAMETERS
C
C       M - SPECTRAL ORDER (REAL*8)
C       W - WAVELENGTH (REAL*8)
C       N - NUMBER OF TERMS (INTEGER)
C
C OUTPUT PARAMETERS
C
C       VAL - VALUES FOR THE 8 TERMS
C
        DOUBLE PRECISION VAL(8),M,W
        INTEGER N
C
        VAL(1)=1.0
C                                    --->CONSTANT TERM
        VAL(2)=M*W
        VAL(3)=(M*W)**2
        VAL(4)=M
        VAL(5)=W
        VAL(6)=M*M*W
        VAL(7)=M*W*W
        VAL(8)=(M*W)**3
C
        RETURN
        END
