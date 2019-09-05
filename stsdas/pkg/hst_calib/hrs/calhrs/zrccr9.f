        SUBROUTINE ZRCCR9(PASS,CCR9,GRAT,ORDER,CPOS,COEF,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCR9
*
*  Keyphrase:
*  ----------
*       Get GHRS echelle interpoation coefficients
*
*  Description:
*  ------------
*
*       This routine reads table CCR9 and extracts the coefficients
*       for the specified grating, order, and carrousel position.
*       Upon the first call, a table of carrousel positions and table
*       row numbers are made for the input grating and order number.
*       The grating,  and order number should not change on subsequent
*       calls. Interpolation within the table is used to compute the
*       coefficients for the given carrousel position.
*
*  FORTRAN name: ZRCCR9.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR9                    I       Echelle interpolation coef.
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
*					memory allocation for storing the table
*		1Jan92	S. Hulbert	Bug fix in memory deallocation (CARPOS
*					pointer had been misnamed)
*       1.2     6Oct94  J. Eisenhamer   Removed possibility of extrapolation
*                                       of coefficients.
*-------------------------------------------------------------------------------
*
* Input parameters
*       PASS - flag equal to 1 if first call to routine, -1 last
*       CCR9 - name of the table file
*       GRAT - grating mode
*       ORDER - spectral order
*       CPOS - carrousel position for the observation. If cpos is the
*               same as the one on the previous call, the output
*               array is left unchanged.
*
* Output parameters
*
*       coef - output coefficients (Real*8 2 elements from columns A and B)
*       istat - ERROR status (integer)
**************************************************************************
        INTEGER PASS
        CHARACTER*5 GRAT
        CHARACTER*64 CCR9
        INTEGER ISTAT,CPOS,ORDER
        REAL COEF(2)
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
C			/ZREFID/
C
C COMMON BLOCK containing id's for reference files which remain
C open throughout the whole calibration.
C
        INTEGER IDCCR2,IDCCR6,IDCCR8,IDCCR9,IDCCRC,IDPHC,IDVIG
        COMMON /ZREFID/ IDCCR2,IDCCR6,IDCCR8,IDCCR9,IDCCRC,IDPHC,
     *       IDVIG
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
C LOCAL VARIABLES -------------------------------------------
C
        DOUBLE PRECISION FRAC,C1(2),C2(2),CPOS1,CPOS2
        INTEGER CARPOS,ROWS
        INTEGER COLIDS(2),ISTATS(2),I,ROW1,ROW2,CLAST
	INTEGER N
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(2)
        LOGICAL NULL(2)
        DATA COLNAM/'A','B'/
C
C
C--------------------------------------------------------------------------
C     
C     Processing for first call
C     
        IF (PASS.EQ.FIRST) THEN
C     
C     open table and get index vectors
C     
           CALL ZXCCR9(CCR9,GRAT,ORDER,IDCCR9,
     *          CARPOS,ROWS,N,ISTAT)
           IF(ISTAT.NE.0) GO TO 1000
C     
C     Get column ids. for coefficients
C     
           CALL UTCFND(IDCCR9,COLNAM,2,COLIDS,ISTAT)
           IF(ISTAT.NE.0)THEN
              CONTXT='ERROR locating needed columns in'//
     *             ' CCR9 table '//CCR9
              GO TO 999
           ENDIF
           CLAST=-1
        ENDIF
C     --------------------------------------------------------------------------
C     
C     IF no change in carrousel position, no new coef. needed
C     
        IF(CLAST.NE.CPOS)THEN
C     
C     Determine points in CARPOS array to interpolate between.
C     If there is only one row of data, use it.
C     If the carrousel position is outside the range, use the closest values.
C     DO NOT EXTRAPOLATE.
C     
           IF( (N.LT.2) .OR. (CPOS.LE.MEMR(CARPOS)) )THEN
              CPOS1=MEMR(CARPOS)
              CPOS2=MEMR(CARPOS)
              ROW1=MEMI(ROWS)
              ROW2=MEMI(ROWS)
           ELSE IF (CPOS.GE.MEMR(CARPOS+N-1)) THEN
              CPOS1=MEMR(CARPOS+N-1)
              CPOS2=MEMR(CARPOS+N-1)
              ROW1=MEMI(ROWS+N-1)
              ROW2=MEMI(ROWS+N-1)
           ELSE
              DO 40 I=1,N-1
                 IF((CPOS.GE.MEMR(CARPOS+I-1)).AND.
     $                (CPOS.LT.MEMR(CARPOS+I)))GO TO 50
 40           CONTINUE
              I=N-1
 50           ROW1=MEMI(ROWS+I-1)
              ROW2=MEMI(ROWS+I)
              CPOS1=MEMR(CARPOS+I-1)
              CPOS2=MEMR(CARPOS+I)
           ENDIF
C     
C     Read coefficients for the 2 rows
C     
           CALL UTRGTD(IDCCR9,COLIDS,2,ROW1,C1,NULL,ISTATS(1))
           CALL UTRGTD(IDCCR9,COLIDS,2,ROW2,C2,NULL,ISTATS(2))
           IF((ISTATS(1).NE.0).OR.(ISTATS(2).NE.0))THEN
              CONTXT='ERROR reading CCR9 table '//CCR9
              GO TO 999
           ENDIF
C     
C     Interpolate
C     
           IF(CPOS2.EQ.CPOS1)THEN
              FRAC=0.0
           ELSE
              FRAC=(CPOS-CPOS1)/(CPOS2-CPOS1)
           ENDIF
           DO 60 I=1,2
              COEF(I)=C1(I) + FRAC*(C2(I)-C1(I))
 60        CONTINUE
           CLAST=CPOS
        ENDIF
        ISTAT=0
C     
C     processing for last call
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
 999    CALL UTTCLO(IDCCR9,ISTAT)
        IF(ROWS.NE.0)CALL UDMFRE (ROWS, TYINT, ISTATS(1))
        IF(CARPOS.NE.0)CALL UDMFRE (CARPOS, TYREAL, ISTATS(2))
 998    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
 1000   RETURN
        END
