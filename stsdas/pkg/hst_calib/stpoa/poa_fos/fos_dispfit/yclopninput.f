C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLOPNINPUT(INPUT,XCOL,WCOL,SELCOL,
     *                         ISTAT)
*
*  Module number:
*
*  Module name: YCLOPNINPUT
*
*  Keyphrase:
*  ----------
*       open input data with x,wave columns 
*
*  Description:
*  ------------
*       This routine opens the input data file, gets the X,WAVE data point
*       pairs which are not NULL.
*
*  FORTRAN name: yclopninput.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  
*
*  Subroutines Called:
*  -------------------
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Nov 01  A. Alexov       Copy of yclopn.f w/some changes
*-------------------------------------------------------------------------------
* inputs:
*       INPUT - name of the input file
*
* outputs:
*       ISTAT - error status
*
        CHARACTER*64 INPUT
        CHARACTER*18 XCOL,WCOL,SELCOL
        INTEGER ISTAT
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      CHARACTER*80 CONTXT
c
c the INPUT related var
      INTEGER NUM_IN_PNTS
      REAL*4  X_ARRAY(4096)
      REAL*8  WAVE_ARRAY(4096)
      COMMON /XWAVE_DATA/X_ARRAY, WAVE_ARRAY, NUM_IN_PNTS
C
C Local variables
C
      INTEGER NN, SEL
      REAL*4  X, SEL_REAL
      REAL*8  WAVE
C
C FILE I/O ACCESS MODES
C
        INTEGER   RDONLY
        PARAMETER (RDONLY = 1)

      INTEGER   TBNROW,CTYPE
      PARAMETER (TBNROW = 21)
      INTEGER IDIN,COLIDS(2),NROWS,COLIDS_EX
      CHARACTER*19 CNAM(2),CUNIT(2),CNAM_EX,CUNIT_EX
      CHARACTER*7  CFORMAT(2),CFORMAT_EX
      INTEGER   TYDOUB, TYREAL, TYPES(2), TYPES_EX
      PARAMETER (TYDOUB = 7)
      PARAMETER (TYREAL = 6)
      PARAMETER (TYINT = 4)
      DATA TYPES/TYREAL,TYDOUB/
      DATA TYPES_EX/TYINT/

      LOGICAL NULL_X,NULL_WAVE,NULL_SEL

C---------------------------------------------------------------------------
        NUM_IN_PNTS=0
        CNAM(1)=XCOL
        CNAM(2)=WCOL
        CNAM_EX=SELCOL
C
C Open table
C
        CALL UTTOPN(INPUT,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening input table '//INPUT
                GO TO 998
        ENDIF

C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading input table '//INPUT
                GO TO 999
        ENDIF
        IF(NROWS.GT.4096)THEN
                CONTXT='ERROR input tab exceeds max num of rows (4096)'
                GO TO 999
        ENDIF
C
C Get column ids.
        IF(SELCOL.EQ.'') THEN
           CALL UTCFND(IDIN,CNAM,2,COLIDS,ISTAT)
           IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in input table '//
     *                  INPUT
                GO TO 999
           ENDIF
        ELSE
           CALL UTCFND(IDIN,CNAM,2,COLIDS,ISTAT)
           IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in input table '//
     *                  INPUT
                GO TO 999
           ENDIF
           CALL UTCFND(IDIN,CNAM_EX,1,COLIDS_EX,ISTAT)
           IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in input table '//
     *                  INPUT
                GO TO 999
           ENDIF
        ENDIF
C
C check to make sure the data type for the x,wave columns are correct
        CALL UTCINF(IDIN,COLIDS(1),CNAM(1),CUNIT(1),CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in input table '//
     *                  INPUT
                GO TO 999
        ENDIF

        IF (CTYPE.NE.TYREAL) THEN
                CONTXT='ERROR X column is not type real in table '//
     *                  INPUT
                GO TO 999
        ENDIF

        CALL UTCINF(IDIN,COLIDS(2),CNAM(2),CUNIT(2),CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in input table '//
     *                  INPUT
                GO TO 999
        ENDIF

        IF (CTYPE.NE.TYDOUB) THEN
                CONTXT='ERROR WAVE col is not type double in table '//
     *                  INPUT
                GO TO 999
        ENDIF

        IF(SELCOL.NE.'') THEN
           CALL UTCINF(IDIN,COLIDS_EX,CNAM_EX,CUNIT_EX,CFORMAT_EX,CTYPE,
     *                 ISTAT)
           IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in input table '//
     *                  INPUT
                GO TO 999
           ENDIF

           IF (CTYPE.NE.TYREAL.AND.CTYPE.NE.TYINT) THEN
                CONTXT='ERROR SELECT col != type real or int in tab '//
     *                  INPUT
                GO TO 999
           ENDIF
        ENDIF

c get the data
        DO 2214 NN = 1, NROWS
              CALL UTRGTR(IDIN,COLIDS(1),1,NN,X,NULL_X,ISTAT)
              IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading input table '//INPUT
                    GO TO 999
              ENDIF
              CALL UTRGTD(IDIN,COLIDS(2),1,NN,WAVE,NULL_WAVE,ISTAT)
              IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading input table '//INPUT
                    GO TO 999
              ENDIF

              IF(SELCOL.NE.'') THEN
                 IF(CTYPE.EQ.TYINT) THEN
                    CALL UTRGTI(IDIN,COLIDS_EX,1,NN,SEL,NULL_SEL,ISTAT)
                 ELSE
                    CALL UTRGTR(IDIN,COLIDS_EX,1,NN,SEL_REAL,NULL_SEL,
     *                          ISTAT)
                    SEL=INT(SEL_REAL)
                 ENDIF
                 IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading input table '//INPUT
                    GO TO 999
                 ENDIF
              ENDIF

              IF(SELCOL.EQ.'') THEN
                 IF ((NULL_X).OR.(NULL_WAVE)) THEN
                 ELSE
                    NUM_IN_PNTS=NUM_IN_PNTS+1
                    X_ARRAY(NUM_IN_PNTS)=X
                    WAVE_ARRAY(NUM_IN_PNTS)=WAVE
C option to write the table entries to STDOUT
ccc                 WRITE(CONTXT, 899), NUM_IN_PNTS, X, WAVE
ccc899              FORMAT('ROW ', I4, ': X=', F10.5, ', WAVE=', F10.5)
ccc                 CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                 ENDIF
              ELSE
C SLECTION column -> if 0, then do not use, else use the value
                 IF ((NULL_X).OR.(NULL_WAVE).OR.(SEL.EQ.0)) THEN
                 ELSE
                    NUM_IN_PNTS=NUM_IN_PNTS+1
                    X_ARRAY(NUM_IN_PNTS)=X
                    WAVE_ARRAY(NUM_IN_PNTS)=WAVE
                 ENDIF
              ENDIF

 2214   CONTINUE      

C close the table
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
C
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END

