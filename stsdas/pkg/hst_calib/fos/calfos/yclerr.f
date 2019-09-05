        SUBROUTINE YCLERR(DATA,EPS,N,FILL,ERR,ISTAT)
*
*  Module number:
*
*  Module name: YCLERR
*
*  Keyphrase:
*  ----------
*       Compute statistical error
*
*  Description:
*  ------------
*       This routine computes the statistical error for the raw
*       data counts as the sqrt(counts).  
*	****We used to set the error for 0 counts to 1.0, but not anymore.
*
*  FORTRAN name: yclerr.for
*
*  Subroutines Called:
*  -------------------
*       none
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*       2       Mar 92  S. Hulbert      Don't set error to 1 for 0 counts
*					Check for negative data values
*       3       Nov 92  D. Bazell       Set the error for zero counts to 1
*                                       if processing spectropolarimetry
*                                       data.
*     3.1	Apr 93	H. Bushouse	Added declaration of STDOUT & STDERR;
*					declare passed arrays as (*), not (1).
*-------------------------------------------------------------------------------
*
* inputs:
*       data - raw count vector
*       eps - data quality vector
*       n - number of points in data and eps
*       fill - value of eps to flag fill data
*
* outputs:
*       err - error vector
*	istat - status
*-----------------------------------------------------------------------------
        INTEGER N, ISTAT
        REAL DATA(*),EPS(*),FILL,ERR(*)
C
C UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
	INTEGER STDOUT
	PARAMETER (STDOUT = 1)
	INTEGER STDERR
	PARAMETER (STDERR = 2)
C
C Common block containing ground mode
C
        CHARACTER * 18 GRNDMD
        COMMON /GMODE/ GRNDMD
C
C Local variables  
C
        INTEGER I
	CHARACTER*80 CONTXT
C
C Set the error equal to the square root of the counts for all modes.  
C We check later for spectropolarimetry mode.
C
        DO 100 I=1,N
                IF(EPS(I).NE.FILL)THEN
                     IF(DATA(I).GE.0)THEN
                         ERR(I)=SQRT(DATA(I))
                     ELSE
                         CONTXT='Negative data value found'
			 GO TO 999
                     ENDIF
                ELSE
                     ERR(I)=0.0
                ENDIF
100     CONTINUE
C
C Check if we are processing spectropolarimetry data.  If so then set the
C error for zero counts equal to 1.0, not 0.0
C
        IF (GRNDMD .EQ. 'SPECTROPOLARIMETRY') THEN
           DO 200 I=1,N
              IF (DATA(I).EQ.0) THEN
                 ERR(I) = 1.0
              ENDIF
 200       CONTINUE
        ENDIF

	ISTAT = 0
	GO TO 1000
 999    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT = 1
1000    RETURN
        END
