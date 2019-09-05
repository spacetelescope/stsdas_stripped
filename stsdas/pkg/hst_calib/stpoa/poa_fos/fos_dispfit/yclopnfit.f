C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLOPNFIT(FITTAB,PREV_ITERATION,CONT_FIT,ISTAT)
*
*  Module number:
*
*  Module name: YCLOPNFIT
*
*  Keyphrase:
*  ----------
*       open input fit table and read columns
*
*  Description:
*  ------------
*       This routine opens the input fit file, gets the initial values.
*
*  FORTRAN name: yclopnfit.for
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
*       INPUT - name of the input fit file
*
* outputs:
*       ISTAT - error status
*
        CHARACTER*64 FITTAB
        INTEGER PREV_ITERATION,ISTAT
        LOGICAL CONT_FIT
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
        LOGICAL MOD_VAL(10)
        REAL*8  INIT_GUESS(10), ACTUAL_VAL(10), ERROR_VAL(10)
        COMMON /FIT_DATA/INIT_GUESS, ACTUAL_VAL, ERROR_VAL, MOD_VAL
C
C Local variables
C
      INTEGER NN
C
C FILE I/O ACCESS MODES
C
        INTEGER   RDONLY
        PARAMETER (RDONLY = 1)

      INTEGER   TBNROW,CTYPE
      PARAMETER (TBNROW = 21)
      INTEGER IDIN,COLIDS(2),NROWS
      CHARACTER*19 CNAM(2),CUNIT(2)
      CHARACTER*7  CFORMAT(2)
      CHARACTER*19  FIXED_OPEN
      INTEGER   TYDOUB, TYREAL, TYPES(2)
      PARAMETER (TYDOUB = 7)
      PARAMETER (TYREAL = 6)
      DATA TYPES/TYREAL,TYDOUB/

      LOGICAL NULL

C---------------------------------------------------------------------------
        NUM_IN_PNTS=0
        CNAM(1)='MODIFIABLE'
        IF(CONT_FIT) THEN
           CNAM(2)='FIT_VALUE'
        ELSE
           CNAM(2)='INITIAL_GUESS'
        ENDIF

C
C Open table
C
        CALL UTTOPN(FITTAB,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening fittab table '//FITTAB
                GO TO 998
        ENDIF

C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading fittab table '//FITTAB
                GO TO 999
        ENDIF
        if(NROWS.GT.10)THEN
                CONTXT='ERROR fittab tab exceeds max num of rows (10)'
                GO TO 999
        ENDIF
C
C Get column ids.
        CALL UTCFND(IDIN,CNAM,2,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR locating needed columns in fittab table '//
     *                 FITTAB
               GO TO 999
        ENDIF
C
C check to make sure the data type for the FIT_VALUE col is correct
        CALL UTCINF(IDIN,COLIDS(2),CNAM(2),CUNIT(2),CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in fittab table '//
     *                  FITTAB
                GO TO 999
        ENDIF

        IF (CTYPE.NE.TYDOUB) THEN
                CONTXT='ERROR FIT_VALUE column is not type double in '//
     *                  FITTAB
                GO TO 999
        ENDIF

        CALL UTCINF(IDIN,COLIDS(1),CNAM(1),CUNIT(1),CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in fittab table '//
     *                  FITTAB
                GO TO 999
        ENDIF

        IF (CTYPE.GT.0) THEN
                CONTXT='ERROR MODIFIABLE col is not type char in '//
     *                  FITTAB
                GO TO 999
        ENDIF

c get the data
        DO 2214 NN = 1, NROWS
              CALL UTRGTD(IDIN,COLIDS(2),1,NN,INIT_GUESS(NN),NULL,ISTAT)
              IF((ISTAT.NE.0.OR.NULL))THEN
                    CONTXT='ERROR reading fittab table '//FITTAB
                    GO TO 999
              ENDIF
                 
              CALL UTRGTT(IDIN,COLIDS(1),1,NN,FIXED_OPEN,NULL,ISTAT)
              IF((ISTAT.NE.0.OR.NULL))THEN
                    CONTXT='ERROR reading fittab table '//FITTAB
                    GO TO 999
              ELSE
                 IF(FIXED_OPEN.EQ.'OPEN') MOD_VAL(NN)=.TRUE.
              ENDIF

C option to write the table entries to STDOUT
ccc                 WRITE(CONTXT, 899), NN, INIT_GUESS(NN), FIXED_OPEN
ccc899              FORMAT('ROW ', I4, ': GUESS=', F20.10, ', MOD=', A6)
ccc                 CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)

 2214   CONTINUE      

C get the previous number of iterations
        CALL UTHGTI(IDIN,'NUM_ITER',PREV_ITERATION,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR could not get NUM_ITER from fittab '//
     *                 FITTAB
               GO TO 999
        ENDIF

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

