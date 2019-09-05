        SUBROUTINE ZRCCR6(PASS,IDCCR6,GRAT,CPOS,TOBS,TC,TEMP,
     *         COEF,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCR6
*
*  Keyphrase:
*  ----------
*       Get GHRS dispersion coefficients
*
*  Description:
*  ------------
*
*       This routine reads table CCR6 and extracts the coefficients
*       for the specified grating and carrousel.  Using an input
*       list of carrousel and row numbers for the table.  Interpolation
*       withing the table is used to compute the coefficients for
*       the given carrousel position.  The first dispersion coefficients are
*       also corrected for thermal motion by:
*
*               a0 = a0 + TC * (temp - tcal)
*
*               where:
*                       TC - is the motion parameter from CCR7
*                       temp - is the observation temperature.
*                       tcal - is the temperature for the stored
*                               dispersion coef.  The temperature
*                               column is selected by TOBS
*
*               If TC=0.0 or TEMP=999, no thermal correction is performed.
*
*  FORTRAN name: ZRCCR6.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR6                    I       Dispersion coefficient table.
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*	zxccr6
*  SDAS:
*       ZMSPUT, utcfnd, utrgtr
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       APR 89  D. Lindler      Designed and coded
*	1.1	Feb 91	S. Hulbert	Added 8th dispersion coeff (cubic term)
*	1.2	Sep 91	S. Hulbert	Implemented PASS flag and dynamic 
*					memory allocation for table
*		Dec91	S. Hulbert	Fix bug when time to get column for
*					reading temperature (check for nonzero 
*					TC)
*		1Jan92	S. Hulbert	Declare TYINT and TYREAL (bug fix)
*					CARPOS pointer misspelled
*-------------------------------------------------------------------------------
*
* Input parameters
*       PASS - integer flag equal to 1 for first call to routine, -1 for last
*       IDCCR6 - CCR6 Tabled descriptor.
*       GRAT - grating mode
*       CPOS - carrousel position for the observation
*       TOBS - temperature to use for correction (character*8)
*       TC - thermal motion (diodes/degree)
*       TEMP - temperature of the observation
*
* Output parameters
*
*       coef - table of sample coefficients (4 x nbins)
*       istat - ERROR status (integer)
**************************************************************************
        INTEGER PASS
        INTEGER IDCCR6
        CHARACTER*5 GRAT
        INTEGER ISTAT,CPOS
        CHARACTER*8 TOBS
        DOUBLE PRECISION TEMP,TC
        DOUBLE PRECISION COEF(8)
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
        DOUBLE PRECISION FRAC,C1(8),C2(8),CPOS1,CPOS2,TCAL1,TCAL2
        INTEGER COLIDS(9),ISTATS(9),I,ROW1,ROW2
        INTEGER N
	INTEGER ROWS,CARPOS
C					---> array pointers
	INTEGER NUMCOEF
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(8)
        LOGICAL NULL(8)
        DATA COLNAM/'A0','A1','A2','A3','A4','A5','A6','A7'/
C
C--------------------------------------------------------------------------
C
C Processing for first call
C
        IF (PASS.EQ.FIRST) THEN
C
C open table and get index vectors
C
                CALL ZXCCR6(GRAT,IDCCR6,CARPOS,ROWS,N,ISTAT)
                IF(ISTAT.NE.0) GO TO 1000
C
C Get column ids. for coefficients
C
		DO 100 I = 1, 8
                  CALL UTCFND(IDCCR6,COLNAM(I),1,COLIDS(I),ISTATS(I))
  100		CONTINUE
		DO 101 I = 1, 7
                   IF(ISTATS(I).NE.0)THEN
                      WRITE(CONTXT,108)COLNAM(I)
 108                  FORMAT('ERROR locating ',a15,' in CCR6 table ')
                      GO TO 999
                   ENDIF
  101		CONTINUE
                IF(ISTATS(8).NE.0)THEN
                   WRITE(CONTXT,109)
 109               FORMAT('WARNING unable to locate column A7 in',
     &                  ' CCR6 table ')
		    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		    NUMCOEF = 7	
		ELSE
		    NUMCOEF = 8
		ENDIF
C
C get column for reading temperature
C
                IF(ABS(TC).GT.0.0)THEN
                    CALL UTCFND(IDCCR6,TOBS,1,COLIDS(9),ISTAT)
                    IF(ISTAT.NE.0)THEN
                       WRITE(CONTXT,119)TOBS
 119                   FORMAT('ERROR locating column ',a,' in',
     *                  ' CCR6 table ')
                        GO TO 999
                    ENDIF
                 ENDIF
        ENDIF
C --------------------------------------------------------------------------
C
C Determine points in CARPOS array to interpolate between
C
            IF( (N.LE.2) .OR. (CPOS.LE.MEMR(CARPOS)) )THEN
                CPOS1=MEMR(CARPOS)
                CPOS2=MEMR(CARPOS+N-1)
                ROW1=MEMI(ROWS)
                ROW2=MEMI(ROWS+N-1)
              ELSE
                DO 40 I=1,N-1
                    IF((CPOS.GE.MEMR(CARPOS+I-1)).AND.
     $				(CPOS.LT.MEMR(CARPOS+I)))GO TO 50
40              CONTINUE
                I=N-1
50              ROW1=MEMI(ROWS+I-1)
                ROW2=MEMI(ROWS+I)
                CPOS1=MEMR(CARPOS+I-1)
                CPOS2=MEMR(CARPOS+I)
            ENDIF
C
C Read coefficients for the 2 rows
C
            CALL UTRGTD(IDCCR6,COLIDS,NUMCOEF,ROW1,C1,NULL,ISTATS(1))
            CALL UTRGTD(IDCCR6,COLIDS,NUMCOEF,ROW2,C2,NULL,ISTATS(2))
            IF((ISTATS(1).NE.0).OR.(ISTATS(2).NE.0))THEN
                CONTXT='ERROR reading CCR6 table '
                GO TO 999
            ENDIF
C
C correct  for thermal motion
C
            IF((TC.NE.0.D0).AND.(TEMP.NE.999.D0))THEN
                CALL UTRGTD(IDCCR6,COLIDS(9),1,ROW1,TCAL1,
     *                                          NULL,ISTATS(1))
                CALL UTRGTD(IDCCR6,COLIDS(9),1,ROW2,TCAL2,
     *                                          NULL,ISTATS(2))
                IF((ISTATS(1).NE.0).OR.(ISTATS(2).NE.0))THEN
                    CONTXT='ERROR reading CCR6 table '
                    GO TO 999
                ENDIF
                C1(1)=C1(1)+TC*(TEMP-TCAL1)
                C2(1)=C2(1)+TC*(TEMP-TCAL2)
            ENDIF
C
C Interpolate
C
            IF(CPOS2.EQ.CPOS1)THEN
                    FRAC=0.0
                ELSE
                    FRAC=(CPOS-CPOS1)/(CPOS2-CPOS1)
            ENDIF
            DO 60 I=1,NUMCOEF
                COEF(I)=C1(I) + FRAC*(C2(I)-C1(I))
60          CONTINUE
	    IF (NUMCOEF .EQ. 7) COEF(8) = 0.0
        ISTAT=0
C
C processing for last call
C
        IF(PASS.EQ.LAST)THEN
            IF(ROWS.NE.0)CALL UDMFRE (ROWS, TYINT, ISTATS(1))
            IF(CARPOS.NE.0)CALL UDMFRE (CARPOS, TYREAL, ISTATS(2))
            IF (ISTATS(1).NE.0.OR.ISTATS(2).NE.0)THEN
                CONTXT='ERROR deallocating memory'
                GO TO 998
           ENDIF
        ENDIF
C
        GO TO 1000
 999    CONTINUE
        IF(ROWS.NE.0)CALL UDMFRE (ROWS, TYINT, ISTATS(1))
        IF(CARPOS.NE.0)CALL UDMFRE (CARPOS, TYREAL, ISTATS(2))
998     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
