C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLNEWCOL(ISTAT)
*
*  Module number:
*
*  Module name: YCLNEWCOL
*
*  Keyphrase:
*  ----------
*      calculate a new column based on the coef's;  write to output
*
*  Description:
*  ------------
*       This routine opens the input file, calculates the new column
*       based on the coeffs; writes the results to the file.
*
*  FORTRAN name: yclnewcol.for
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

        REAL*4  X_ARRAY(4096)
        REAL*8  WAVE_ARRAY(4096)
        REAL*4  CALCX_ARRAY(4096)
        REAL*8  CALCW_ARRAY(4096)
        COMMON /XWAVE_DATA/X_ARRAY, WAVE_ARRAY, NUM_IN_PNTS
        COMMON /CALC_DATA/CALCX_ARRAY, CALCW_ARRAY
C
      INTEGER NN
      LOGICAL NULL
      REAL*8  VAL_DOUB, YOUT
      REAL*4  VAL_REAL
C
C FILE I/O ACCESS MODES
C
        INTEGER   RDWRIT
        PARAMETER (RDWRIT = 2)

      INTEGER   TBNROW,CTYPE(3)
      PARAMETER (TBNROW = 21)
      INTEGER IDIN,COLIDS(3),NROWS
      CHARACTER*19 CNAM(3),CUNIT(3)
      CHARACTER*7  CFORMAT(3)
      INTEGER   TYDOUB, TYREAL
      PARAMETER (TYDOUB = 7)
      PARAMETER (TYREAL = 6)
      LOGICAL   EXISTS

      INCLUDE 'fiti.inc'                        
      DOUBLE PRECISION FZDERIV(FZPARMAX)
      INCLUDE 'fitc.inc'   
C---------------------------------------------------------------------------

        IF(DEP_VAR.EQ.'X') THEN  ! dep=x (fit_wave2x)
           CNAM(1)=IDEP_COL   ! read the wavelength column to calc new X
           CNAM(3)=DEP_COL   
        ELSE  ! dep=wave  (fit_x2wave)
           CNAM(1)=IDEP_COL    ! read the x column to calc new WAVELENGTH
           CNAM(3)=DEP_COL    
        ENDIF
        CNAM(2)=NEW_COL
        CUNIT(2)=' '
        CFORMAT(2)=' '
c        CTYPE(2)=TYDOUB

C Check to make sure the output file exists

        CALL UTTACC(INPUT,EXISTS,ISTAT)
        IF (EXISTS.EQ.FALSE.OR.ISTAT.NE.0)  THEN
                CONTXT='ERROR input table does not exist: '//INPUT
                GO TO 998
        ENDIF
C
C Open table
C
        CALL UTTOPN(INPUT,RDWRIT,IDIN,ISTAT)
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
        CALL UTCFND(IDIN,CNAM(1),1,COLIDS(1),ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR locating needed column in input table '//
     *                 INPUT
               GO TO 999
        ENDIF
        CALL UTCFND(IDIN,CNAM(3),1,COLIDS(3),ISTAT)
        IF(ISTAT.NE.0)THEN
               CONTXT='ERROR locating needed column in input table '//
     *                 INPUT
               GO TO 999
        ENDIF
C
C Find out the datatype of the (3) column, in order to create the new
C column with the same type
C
        CALL UTCINF(IDIN,COLIDS(3),CNAM(3),CUNIT(3),CFORMAT(3),
     *              CTYPE(3),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in input table '//
     *                  INPUT
                GO TO 999
        ENDIF
        CTYPE(2)=CTYPE(3)  ! set the new column type in case need it
C
C Check if the new column exists, if not, then create it (using the
C same data type as the cname(3) column)
C
        CALL UTCFND(IDIN,CNAM(2),1,COLIDS(2),ISTAT)
        ! if the column is not found, then create it
        IF(ISTAT.NE.0)THEN
           CALL UTCDEF(IDIN,CNAM(2),CUNIT(2),CFORMAT(2),CTYPE(2),1,
     *                 COLIDS(2),ISTAT)
           IF(ISTAT.NE.0)THEN
                CONTXT='ERROR creating needed column in input table '//
     *               INPUT
                GO TO 999
           ENDIF
        ELSE  ! column has been found in the input file
           CONTXT='WARNING overwriting previous values in new column '//
     *             NEW_COL
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CALL UTCINF(IDIN,COLIDS(2),CNAM(2),CUNIT(2),CFORMAT(2),
     *                 CTYPE(2),ISTAT)  ! get the ctype(2) information
           IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in input table '//
     *                  INPUT
                GO TO 999
           ENDIF
        ENDIF
C
C check to make sure the data types are correct

        CALL UTCINF(IDIN,COLIDS(1),CNAM(1),CUNIT(1),CFORMAT(1),
     *              CTYPE(1),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in input table '//
     *                  INPUT
                GO TO 999
        ENDIF
        CALL UTCINF(IDIN,COLIDS(2),CNAM(2),CUNIT(2),CFORMAT(2),
     *              CTYPE(2),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting column info in input table '//
     *                  INPUT
                GO TO 999
        ENDIF

        IF ((CTYPE(1).NE.TYDOUB).AND.(CTYPE(1).NE.TYREAL)) THEN
               CONTXT='ERROR col type is neither double nor real in '//
     *                  INPUT
                GO TO 999
        ENDIF
        IF ((CTYPE(2).NE.TYDOUB).AND.(CTYPE(2).NE.TYREAL)) THEN
               CONTXT='ERROR col type is neither double nor real in '//
     *                  INPUT
                GO TO 999
        ENDIF

C get the data
        DO 2213 NN = 1, NROWS

           IF(CTYPE(1).EQ.TYREAL) THEN
              CALL UTRGTR(IDIN,COLIDS(1),1,NN,VAL_REAL,NULL,ISTAT)
              IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading input table '//INPUT
                    GO TO 999
              ENDIF
              IF (NULL) THEN  ! don't calculate or write to output table
ccc                      WRITE (CONTXT,9001) 'ROW #', NN, ', IDEP=INDEF',  
ccc     *                   ', YOUT=INDEF'
ccc                      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ELSE
                      IF (DEP_VAR.EQ.'X') THEN  ! wave input, x output
                         CALL FIT_WAVE2X(1,VAL_REAL,10,FZVALUE(1),
     *                               YOUT,FZDERIV(1))
                         CALCX_ARRAY(NN)=REAL(YOUT)
ccc                         WRITE (CONTXT,9000) 'ROW #', NN, ', WAVE IN=',  
ccc     *                      VAL(NN), ', X CALC=', YOUT
                         IF (CTYPE(2).EQ.TYREAL) THEN
                            CALL UTRPTR(IDIN,COLIDS(2),1,NN,
     *                         REAL(YOUT),ISTAT)
                            IF(ISTAT.NE.0)THEN
                               CONTXT='ERROR writing to input table '
     *                             //INPUT
                               GO TO 999
                            ENDIF
                         ELSE
                            CALL UTRPTD(IDIN,COLIDS(2),1,NN,
     *                         YOUT,ISTAT)
                            IF(ISTAT.NE.0)THEN
                               CONTXT='ERROR writing to input table '
     *                             //INPUT
                               GO TO 999
                            ENDIF
                         ENDIF
                      ELSE                      ! x input, wave output
                         CALL FIT_X2WAVE(1,VAL_REAL,10,FZVALUE(1),
     *                               YOUT,FZDERIV(1))
ccc                         WRITE (CONTXT,9000) 'ROW #', NN, ', X IN=',  
ccc     *                      VAL(NN), ', WAVE CALC=', YOUT
                         CALCW_ARRAY(NN)=YOUT
                         IF (CTYPE(2).EQ.TYREAL) THEN
                            CALL UTRPTR(IDIN,COLIDS(2),1,NN,
     *                         REAL(YOUT),ISTAT)
                            IF(ISTAT.NE.0)THEN
                               CONTXT='ERROR writing to input table '
     *                             //INPUT
                               GO TO 999
                            ENDIF
                         ELSE
                            CALL UTRPTD(IDIN,COLIDS(2),1,NN,
     *                         YOUT,ISTAT)
                            IF(ISTAT.NE.0)THEN
                               CONTXT='ERROR writing to input table '
     *                             //INPUT
                               GO TO 999
                            ENDIF
                         ENDIF
                      ENDIF
ccc                      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ENDIF
           ELSE   ! when data type is double
              CALL UTRGTD(IDIN,COLIDS(1),1,NN,VAL_DOUB,NULL,ISTAT)
              IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading input table '//INPUT
                    GO TO 999
              ENDIF
              IF (NULL) THEN  ! don't calculate or write to output table
ccc                      WRITE (CONTXT,9001) 'ROW #', NN, ', IDEP=INDEF',  
ccc     *                   ', YOUT=INDEF'
ccc                      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ELSE
                      VAL_REAL=REAL(VAL_DOUB)
                      IF (DEP_VAR.EQ.'X') THEN  ! wave input, x output
                         CALL FIT_WAVE2X(1,VAL_REAL,10,FZVALUE(1),
     *                               YOUT,FZDERIV(1))
                         CALCX_ARRAY(NN)=REAL(YOUT)
ccc                         WRITE (CONTXT,9000) 'ROW #', NN, ', WAVE IN=',  
ccc     *                      VAL(NN), ', X CALC=', YOUT
                         IF (CTYPE(2).EQ.TYREAL) THEN
                            CALL UTRPTR(IDIN,COLIDS(2),1,NN,
     *                         REAL(YOUT),ISTAT)
                            IF(ISTAT.NE.0)THEN
                               CONTXT='ERROR writing to input table '
     *                             //INPUT
                               GO TO 999
                            ENDIF
                         ELSE
                            CALL UTRPTD(IDIN,COLIDS(2),1,NN,
     *                         YOUT,ISTAT)
                            IF(ISTAT.NE.0)THEN
                               CONTXT='ERROR writing to input table '
     *                             //INPUT
                               GO TO 999
                            ENDIF
                         ENDIF
                      ELSE                      ! x input, wave output
                         CALL FIT_X2WAVE(1,VAL_REAL,10,FZVALUE(1),
     *                               YOUT,FZDERIV(1))
                         CALCW_ARRAY(NN)=YOUT
ccc                         WRITE (CONTXT,9000) 'ROW #', NN, ', X IN=',  
ccc     *                      VAL(NN), ', WAVE CALC=', YOUT
                         IF (CTYPE(2).EQ.TYREAL) THEN
                            CALL UTRPTR(IDIN,COLIDS(2),1,NN,
     *                         REAL(YOUT),ISTAT)
                            IF(ISTAT.NE.0)THEN
                               CONTXT='ERROR writing to input table '
     *                             //INPUT
                               GO TO 999
                            ENDIF
                         ELSE
                            CALL UTRPTD(IDIN,COLIDS(2),1,NN,
     *                         YOUT,ISTAT)
                            IF(ISTAT.NE.0)THEN
                               CONTXT='ERROR writing to input table '
     *                             //INPUT
                               GO TO 999
                            ENDIF
                         ENDIF
                      ENDIF
ccc                      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ENDIF

           ENDIF

ccc 9000                 FORMAT(A,I,A,D17.8,A,D17.8)
ccc 9001                 FORMAT(A,I,A,A)

 2213      CONTINUE      


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

