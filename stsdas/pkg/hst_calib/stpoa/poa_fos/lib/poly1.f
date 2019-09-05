C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE POLY1(X,AFUNC,MA)
*
*  Module number: FOS/HRS UTILITY ROUTINE
*
*  Module name: poly1
*
*  Keyphrase:
*  ----------
*	polynomial evaluation
*  Description:
*  ------------
*	This routine evaluates polynomial terms for use by
*	the least squares fit routine LFIT.
*  FORTRAN name: poly1.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  	none
*
*  Subroutines Called:
*  -------------------
*	none
*
*  History:
*  --------
*  Version      Date        Author          Description
*	1	Sept. 87   D. Lindler	Designed and coded
*-------------------------------------------------------------------------------
C
C INPUT PARAMETERS
C
C	X - X POSITION TO EVALUATE (REAL*8)
C	MA - NUMBER OF TERMS TO EVALUATE
C
C OUTPUT PARAMETERS
C
C	AFUNC - VECTOR OF LENGTH MA (REAL*8)
C		AFUNC(I) CONTAINS X**I
C
C-------------------------------------------------------------------------------
	IMPLICIT NONE
        DOUBLE PRECISION X,AFUNC
        INTEGER MA,I
        DIMENSION AFUNC(MA)
        AFUNC(1)=1.0D0
        IF (MA.GT.1) THEN
                DO 10 I=2,MA
                        AFUNC(I)=AFUNC(I-1)*X
10              CONTINUE
        ENDIF
        RETURN
        END
