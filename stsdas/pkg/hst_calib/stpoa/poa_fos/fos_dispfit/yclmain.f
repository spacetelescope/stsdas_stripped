C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLMAIN(ISTAT)
*
*  Module number:
*
*  Module name: yclmain
*
*  Keyphrase:
*  ----------
*       main calling routine for dispersion coeffs
*
*  Description:
*  ------------
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*
*  Subroutines Called:
*  -------------------
*
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*     /////////////////////////////////////////////////////////////////////
*            Start of Post Operational Archive versions (poa_calfos)
*     /////////////////////////////////////////////////////////////////////
*     1.0       Nov 01  A. Alexov         copy of yclprc, as main driver
*-------------------------------------------------------------------------------
*
* INPUTS:
*
* OUTPUTS:
*       istat - error status
*
        IMPLICIT NONE

        INTEGER ISTAT
C------------------------------------------------------------------------------
C
C Common block containing confiquration parameters
C
        CHARACTER*64 INPUT,FITTAB,OUTPUT
        INTEGER      ITERATION, PREV_ITERATION, NACT, PRINT
        REAL*4       RELAX, CHISQ, ACTCH, V1, V2
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

C Local variables
        CHARACTER*80 CONTXT
        INTEGER NUM_IN_PNTS

	INTEGER I
        REAL*4  X_ARRAY(4096)
        REAL*8  WAVE_ARRAY(4096)
        REAL*4  CALCX_ARRAY(4096)
        REAL*8  CALCW_ARRAY(4096)
        LOGICAL MOD_VAL(10)
        REAL*8  INIT_GUESS(10), ACTUAL_VAL(10), ERROR_VAL(10)
        COMMON /FIT_DATA/INIT_GUESS, ACTUAL_VAL, ERROR_VAL, MOD_VAL
        COMMON /XWAVE_DATA/X_ARRAY, WAVE_ARRAY, NUM_IN_PNTS
        COMMON /CALC_DATA/CALCX_ARRAY, CALCW_ARRAY

ccc        INCLUDE 'fiti.inc'
ccc        INCLUDE 'fiti.inc'
C
C-----------------------------------------------------------------------------

C initialize all X and WAVE data to 0.0 values
        DO 100 I = 1, 4096
           X_ARRAY(I)=0.0
           WAVE_ARRAY(I)=0.0
 100    CONTINUE
C initialize all GUESS values 
        DO 101 I = 1, 10
           INIT_GUESS(I)=0.0
           ACTUAL_VAL(I)=0.0
           ERROR_VAL(I)=0.0
           MOD_VAL(I)=.FALSE.
 101    CONTINUE

C OPEN input files
C
        IF(DEP_VAR.EQ.'WAVE') THEN
           CALL YCLOPNINPUT(INPUT,IDEP_COL,DEP_COL,SEL_COL,ISTAT)
           IF(ISTAT.NE.0) GO TO 999
        ELSE IF(DEP_VAR.EQ.'X') THEN
           CALL YCLOPNINPUT(INPUT,DEP_COL,IDEP_COL,SEL_COL,ISTAT)
           IF(ISTAT.NE.0) GO TO 999
        ELSE
           CONTXT='ERROR: dependant var name must be either X or WAVE'
           ISTAT=1
           GO TO 999
        ENDIF

C If using a new fit, then the parameters are the initial values, 
C otherwise, use the fit table to fill the initial guesses using 
C the FIT_VALUE values from the previous fit
        IF(USE_PARAMS)THEN
           INIT_GUESS(1)=A
           IF(A_MOD.EQ.'OPEN') MOD_VAL(1)=.TRUE.
           INIT_GUESS(2)=B
           IF(B_MOD.EQ.'OPEN') MOD_VAL(2)=.TRUE.
           INIT_GUESS(3)=C
           IF(C_MOD.EQ.'OPEN') MOD_VAL(3)=.TRUE.
           INIT_GUESS(4)=D
           IF(D_MOD.EQ.'OPEN') MOD_VAL(4)=.TRUE.
           INIT_GUESS(5)=E
           IF(E_MOD.EQ.'OPEN') MOD_VAL(5)=.TRUE.
           INIT_GUESS(6)=O
           IF(O_MOD.EQ.'OPEN') MOD_VAL(6)=.TRUE.
           INIT_GUESS(7)=P
           IF(P_MOD.EQ.'OPEN') MOD_VAL(7)=.TRUE.
           INIT_GUESS(8)=Q
           IF(Q_MOD.EQ.'OPEN') MOD_VAL(8)=.TRUE.
           INIT_GUESS(9)=R
           IF(R_MOD.EQ.'OPEN') MOD_VAL(9)=.TRUE.
           INIT_GUESS(10)=S
           IF(S_MOD.EQ.'OPEN') MOD_VAL(10)=.TRUE.
        ELSE
           CALL YCLOPNFIT(FITTAB,PREV_ITERATION,CONT_FIT,ISTAT)
           IF(ISTAT.NE.0) GO TO 999  
        ENDIF

C call the fitting function
        V1=0.
        V2=0.
        CALL FTTNTR(V1,V2,NACT,ACTCH,ISTAT)
        IF(ISTAT.NE.0) GO TO 999

C write the output table with results
        CALL YCLWRTOUT(NUM_IN_PNTS, ACTCH, NACT, ISTAT)
        IF(ISTAT.NE.0) GO TO 999

C write the new column if NEW_COL is not blank
        IF(NEW_COL.NE.'')THEN
           CALL YCLNEWCOL(ISTAT)
           IF(ISTAT.NE.0) GO TO 999
        ENDIF


        IF(ISTAT.EQ.0) GO TO 1000

 999    ISTAT=1
1000    RETURN
        END
