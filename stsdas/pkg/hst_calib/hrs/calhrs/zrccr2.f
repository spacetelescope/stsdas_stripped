        SUBROUTINE ZRCCR2(PASS,CCR2,DET,Y,NBINS,COEF,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCR2
*
*  Keyphrase:
*  ----------
*       Interpolate in table CCR2
*               (GHRS sample mapping function coefficients)
*
*  Description:
*  ------------
*
*       This routine reads table CCR2 and extracts the parameters
*       for the specified detector and ydeflection.  Using an input
*       list of ydeflections and row numbers for the table.  Interpolation
*       withing the table is used to compute the coefficients for
*       the ydeflection for each bin.  If the y-deflections for each
*       bin are the same as for the previous call no change to the
*       output coefficient table is done.
*
*  FORTRAN name: ZRCCR2.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR2                    I       Sample mapping function coef.
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT, utcfnd, utrgtr
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       APR 89  D. Lindler      Designed and coded
*	1.1	Sep 91	S. Hulbert	Implemented PASS flag and dynamic
*					memory allocation for table
*-------------------------------------------------------------------------------
*
* Input parameters
*       PASS - flag = 1 if it is the first call to this routine, -1 if last
*       CCR2 - name of the table file
*       DET - detector number (1 or 2)
*       Y - y deflections for each substep bin
*       NBINS - number of substep bins
*
* Output parameters
*
*       coef - table of sample coefficients (4 x nbins)
*       istat - ERROR status (integer)
**************************************************************************
        INTEGER PASS
        CHARACTER*64 CCR2
        INTEGER NBINS,ISTAT,DET
        REAL COEF(4,7)
        INTEGER Y(7)
C------------------------------------------------------------------------------
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(1)
      INTEGER*2        MEMS(1)
      INTEGER*4        MEMI(1)
      INTEGER*4        MEML(1)
      REAL             MEMR(1)
      DOUBLE PRECISION MEMD(1)
      COMPLEX          MEMX(1)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C------------------------------------------------------------------------------
      INTEGER TYINT
      PARAMETER (TYINT=4)
      INTEGER TYREAL
      PARAMETER (TYREAL=6)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
C LOCAL VARIABLES -------------------------------------------
C
        REAL FRAC,C1(4),C2(4),Y1,Y2,YD,YLAST(7)
        INTEGER COLIDS(4),ISTATS(4),I,BIN,ROW1,ROW2
        INTEGER N
	INTEGER ROWS,YDEFS
C					---> array pointers
        INTEGER IDCCR2
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(4)
        LOGICAL NULL(4)
        DATA COLNAM/'S0','B','C','E'/
C
C--------------------------------------------------------------------------
C
C Processing for first call
C
        IF (PASS.EQ.FIRST) THEN
C
C open table and get index vectors
C
                CALL ZXCCR2(CCR2,DET,IDCCR2,YDEFS,ROWS,N,ISTAT)
                IF(ISTAT.NE.0) GO TO 1000
C
C Get column ids. for coefficients
C
                CALL UTCFND(IDCCR2,COLNAM,4,COLIDS,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR locating needed columns in'//
     *                  ' CCR2 table '//CCR2
                        GO TO 999
                ENDIF
C
C set previous y-deflections done to -1
C
                DO 5 I=1,NBINS
                        YLAST(I)=-1.0
5               CONTINUE
        ENDIF
C
C Have the y-deflections changed since last call
C
        DO 10 I=1,NBINS
                IF(YLAST(I).NE.Y(I)) GO TO 20
10      CONTINUE
        ISTAT=0
        GO TO 1000
C                                    --->no change in y-deflections
C
C y-deflections have changed. Need new coefficients. Loop on bins
C
20      DO 100 BIN=1,NBINS
           YD=Y(BIN)
C                                    --->Y-Deflection for this bin
C
C is the ydeflection the same as the last bin?
C
            IF(BIN.GT.1)THEN
                IF(YD.EQ.Y(BIN-1))THEN
C                                    --->Same coefficients
                        DO 30 I=1,4
                                COEF(I,BIN)=COEF(I,BIN-1)
30                      CONTINUE
                        GO TO 100
                ENDIF
            ENDIF
C
C Determine points in YDEFS array to interpolate between
C
            IF( (N.LE.2) .OR. (YD.LE.MEMR(YDEFS)) )THEN
                Y1=MEMR(YDEFS)
                Y2=MEMR(YDEFS+N-1)
                ROW1=MEMI(ROWS)
                ROW2=MEMI(ROWS+N-1)
              ELSE
                DO 40 I=1,N-1
                    IF((YD.GE.MEMR(YDEFS+I-1)).AND.
     $				(YD.LT.MEMR(YDEFS+I)))GO TO 50
40              CONTINUE
                I=N-1
50              ROW1=MEMI(ROWS+I-1)
                ROW2=MEMI(ROWS+I)
                Y1=MEMR(YDEFS+I-1)
                Y2=MEMR(YDEFS+I)
            ENDIF
C
C Read coefficients for the 2 rows
C
            CALL UTRGTR(IDCCR2,COLIDS,4,ROW1,C1,NULL,ISTATS(1))
            CALL UTRGTR(IDCCR2,COLIDS,4,ROW2,C2,NULL,ISTATS(2))
            IF((ISTATS(1).NE.0).OR.(ISTATS(2).NE.0))THEN
                CONTXT='ERROR reading CCR2 table '//CCR2
                GO TO 999
            ENDIF
C
C Interpolate
C
            IF(Y2.EQ.Y1)THEN
                    FRAC=0.0
                ELSE
                    FRAC=(YD-Y1)/(Y2-Y1)
            ENDIF
            DO 60 I=1,4
                COEF(I,BIN)=C1(I) + FRAC*(C2(I)-C1(I))
60          CONTINUE
            YLAST(BIN)=YD
100     CONTINUE
        ISTAT=0
C
C processing for last call
C
	IF(PASS.EQ.LAST)THEN
	    IF(ROWS.NE.0)CALL UDMFRE (ROWS, TYINT, ISTATS(1))
	    IF(YDEFS.NE.0)CALL UDMFRE (YDEFS, TYREAL, ISTATS(2))
	    IF (ISTATS(1).NE.0.OR.ISTATS(2).NE.0)THEN
                CONTXT='ERROR deallocating memory'
		GO TO 998
	   ENDIF
	ENDIF
C
        GO TO 1000
999     CALL UTTCLO(IDCCR2,ISTAT)
	IF(ROWS.NE.0)CALL UDMFRE (ROWS, TYINT, ISTATS(1))
	IF(YDEFS.NE.0)CALL UDMFRE (YDEFS, TYREAL, ISTATS(2))
998     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
