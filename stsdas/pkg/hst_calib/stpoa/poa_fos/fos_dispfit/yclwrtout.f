C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLWRTOUT(NUM_IN_PNTS,ACTCH, NACT, ISTAT)
*
*  Module number:
*
*  Module name: YCLWRTOUT
*
*  Keyphrase:
*  ----------
*      write the output results to a fit table
*
*  Description:
*  ------------
*       This routine opens the input output file, writes the results
*       and populates the header with updated values.
*
*  FORTRAN name: yclwrtout.for
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
*       1       Nov 01  A. Alexov       Copy of yclopnfit.f w/some changes
*-------------------------------------------------------------------------------
* inputs:
        INTEGER NUM_IN_PNTS, NACT
        REAL*4  ACTCH
*
* outputs:
*       ISTAT - error status
*
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
C Common block containing confiquration parameters
C
        CHARACTER*64 INPUT,FITTAB,OUTPUT
        INTEGER      ITERATION, PREV_ITERATION, PRINT
        REAL*4       RELAX, CHISQ
        LOGICAL      CONT_FIT,USE_PARAMS
        CHARACTER*4  DEP_VAR
        CHARACTER*18 DEP_COL,IDEP_COL,SEL_COL,NEW_COL
        REAL*8       A,B,C,D,E,O,P,Q,R,S
        CHARACTER*5  A_MOD, B_MOD, C_MOD, D_MOD, E_MOD
        CHARACTER*5  O_MOD, P_MOD, Q_MOD, R_MOD, S_MOD

        COMMON /CONFG1/RELAX,INPUT,FITTAB,OUTPUT,ITERATION,PRINT,
     *          PREV_ITERATION, CHISQ, CONT_FIT,
     *          USE_PARAMS,DEP_VAR,DEP_COL,IDEP_COL,SEL_COL,NEW_COL
        COMMON /CONFG2/A, B, C, D, E, O, P, Q, R, S
        COMMON /CONFG3/A_MOD, B_MOD, C_MOD, D_MOD,
     *           E_MOD, O_MOD, P_MOD, Q_MOD, R_MOD, S_MOD

        LOGICAL MOD_VAL(10)
        REAL*8  INIT_GUESS(10), ACTUAL_VAL(10), ERROR_VAL(10)
        COMMON /FIT_DATA/INIT_GUESS, ACTUAL_VAL, ERROR_VAL, MOD_VAL
C
      INTEGER NN,ITER
C
C FILE I/O ACCESS MODES
C
        INTEGER   RDWRIT
        PARAMETER (RDWRIT = 2)

      INTEGER   TBNROW,CTYPE
      PARAMETER (TBNROW = 21)
      INTEGER IDIN,COLIDS(4),NROWS
      CHARACTER*19 CNAM(4),CUNIT(4)
      CHARACTER*7  CFORMAT(4)
      CHARACTER*19  FIXED_OPEN
      INTEGER   TYDOUB, TYREAL
      PARAMETER (TYDOUB = 7)
      PARAMETER (TYREAL = 6)
      LOGICAL   EXISTS

C---------------------------------------------------------------------------
        CNAM(1)='INITIAL_GUESS'
        CNAM(2)='MODIFIABLE'
        CNAM(3)='FIT_VALUE'
        CNAM(4)='FIT_ERROR'

C Check to make sure the output file exists

        CALL UTTACC(OUTPUT,EXISTS,ISTAT)
        IF (EXISTS.EQ.FALSE.OR.ISTAT.NE.0)  THEN
                CONTXT='ERROR output table does not exist: '//OUTPUT
                GO TO 998
        ENDIF
C
C Open table
C
        CALL UTTOPN(OUTPUT,RDWRIT,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening output table '//OUTPUT
                GO TO 998
        ENDIF

C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading output table '//OUTPUT
                GO TO 999
        ENDIF
        IF(NROWS.GT.10)THEN
                CONTXT='ERROR output tab exceeds max num of rows (10)'
                GO TO 999
        ENDIF
C
C Get column ids.
        CALL UTCFND(IDIN,CNAM,4,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR locating needed columns in output table '//
     *                 OUTPUT
               GO TO 999
        ENDIF
C
C check to make sure the data types are correct

        CALL UTCINF(IDIN,COLIDS(1),CNAM(1),CUNIT(1),CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in output table '//
     *                  OUTPUT
                GO TO 999
        ENDIF

        IF (CTYPE.NE.TYDOUB) THEN
               CONTXT='ERROR INIT_GUESS column is not type double in '//
     *                  OUTPUT
                GO TO 999
        ENDIF

        CALL UTCINF(IDIN,COLIDS(2),CNAM(2),CUNIT(2),CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in output table '//
     *                  OUTPUT
                GO TO 999
        ENDIF

        IF (CTYPE.GT.0) THEN
                CONTXT='ERROR MODIFIABLE col is not type char in '//
     *                  OUTPUT
                GO TO 999
        ENDIF

        CALL UTCINF(IDIN,COLIDS(3),CNAM(3),CUNIT(3),CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in output table '//
     *                  OUTPUT
                GO TO 999
        ENDIF

        IF (CTYPE.NE.TYDOUB) THEN
                CONTXT='ERROR FIT_VALUE column is not type double in '//
     *                  OUTPUT
                GO TO 999
        ENDIF

        CALL UTCINF(IDIN,COLIDS(4),CNAM(4),CUNIT(4),CFORMAT,CTYPE,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in output table '//
     *                  OUTPUT
                GO TO 999
        ENDIF

        IF (CTYPE.NE.TYDOUB) THEN
                CONTXT='ERROR FIT_ERROR column is not type double in '//
     *                  OUTPUT
                GO TO 999
        ENDIF


c if continueing the fit, then header values should be added to 
c previous results; otherwise, place current values into the header
        CALL UTHPTI(IDIN,'NUM_DATA',NUM_IN_PNTS,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR could not put NUM_ITER to output '//
     *                 OUTPUT
               GO TO 999
        ENDIF

        ITER=(PREV_ITERATION+NACT)
        CALL UTHPTI(IDIN,'NUM_ITER',ITER,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR could not put NUM_ITER to output '//
     *                 OUTPUT
               GO TO 999
        ENDIF

        CALL UTHPTT(IDIN,'DEP_NAM',DEP_COL,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR could not put DEP_COL to output '//
     *                 OUTPUT
               GO TO 999
        ENDIF

        CALL UTHPTT(IDIN,'IDEP_NAM',IDEP_COL,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR could not put IDEP_COL to output '//
     *                 OUTPUT
               GO TO 999
        ENDIF

        CALL UTHPTR(IDIN,'RELAXFAC',RELAX,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR could not put RELAXFAC to output '//
     *                 OUTPUT
               GO TO 999
        ENDIF

        CALL UTHPTR(IDIN,'CHI_SQ',ACTCH,ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR could not put CHI_SQ to output '//
     *                 OUTPUT
               GO TO 999
        ENDIF

C write the data to the table
        DO 2214 NN = 1, NROWS
ccc           IF (CONT_FIT) THEN
              CALL UTRPTD(IDIN,COLIDS(1),1,NN,INIT_GUESS(NN),ISTAT)
              IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR writing row to output table '//OUTPUT
                    GO TO 999
              ENDIF

              IF (MOD_VAL(NN)) THEN
                 FIXED_OPEN='OPEN'
              ELSE
                 FIXED_OPEN='FIXED'
              ENDIF   
           
              CALL UTRPTT(IDIN,COLIDS(2),1,NN,FIXED_OPEN,ISTAT)
              IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR writing row to output table '//OUTPUT
                    GO TO 999
              ENDIF
ccc           ENDIF
 
           CALL UTRPTD(IDIN,COLIDS(3),1,NN,ACTUAL_VAL(NN),ISTAT)
           IF(ISTAT.NE.0)THEN
                 CONTXT='ERROR writing row to output table '//OUTPUT
                 GO TO 999
           ENDIF

           CALL UTRPTD(IDIN,COLIDS(4),1,NN,ERROR_VAL(NN),ISTAT)
           IF(ISTAT.NE.0)THEN
                 CONTXT='ERROR writing row to output table '//OUTPUT
                 GO TO 999
           ENDIF
             
ccC option to write the table entries to STDOUT
ccc                 WRITE(CONTXT, 899), NN, INIT_GUESS(NN), FIXED_OPEN
ccc899              FORMAT('ROW ', I4, ': GUESS=', F10.5, ', MOD=', A6)
ccc                 CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)

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

