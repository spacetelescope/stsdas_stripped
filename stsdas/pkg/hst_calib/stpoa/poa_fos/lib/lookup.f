C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        DOUBLE PRECISION FUNCTION LOOKUP(TAB,NTAB,POS)
*
*  Module number: HRS/FOS UTILITY
*
*  Module name: LOOKUP
*
*  Keyphrase:
*  ----------
*       Tabular function value
*  Description:
*  ------------
*       This routines looks for the value in a table
*       at a given position.
*       It is equivalent to result=TAB(POS) except
*       that POS is may be a fractional table position.
*       Therefore linear interpolation/extrapolation is
*       used to compute the value.
*
*  FORTRAN name: lookup.for
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
*     1         Sept. 87   D. Lindler   Designed and coded
*-------------------------------------------------------------------------------
C
C INPUT PARAMETERS
C
C       TAB - VECTOR OF VALUES (REAL*8)
C       NTAB - NUMBER OF VALUES IN TAB (INTEGER)
C       POS - POSITION IN TABLE DESIRED (REAL*8)
C
C OUTPUT PARAMETERS
C       RESULT IS RETURNED AS FUNCTION VALUE
C
C----------------------------------------------------------------------------
	IMPLICIT NONE
        DOUBLE PRECISION  TAB(*),POS
        INTEGER NTAB,IPOS
C
C DETERMINE FIRST OF PAIR OF TABLE VALUES TO USE
C
        IPOS=POS
        IF(IPOS.LT.1)IPOS=1
        IF(IPOS.GE.NTAB)IPOS=NTAB-1
C
C INTERPOLATE
C
        LOOKUP=TAB(IPOS)+(POS-IPOS)*(TAB(IPOS+1)-TAB(IPOS))
        RETURN
        END
