C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLDSP
*
*  Module number:
*
*  Module name: ycldsp
*
*  Keyphrase:
*  ----------
*       Calculate dispersion coefficients for FOS ccs6 tables.
*
*  Description:
*  ------------
*       This routine performed the reading of input and output
*       files for the main routine of calculating the dispersion
*       coeffs for the FOS ccs6 tables.
*               input - Input data table with x and wavelengths
*               fittab - Input data table with previous fit or blank table
*               output - Output fit file name
*
*  FORTRAN name:
*
*
*  History:
*  --------
*     /////////////////////////////////////////////////////////////////////
*            Start of Post Operational Archive versions (fos_dispfit)
*     /////////////////////////////////////////////////////////////////////
*     1.0       Nov 01  A. Alexov       Created (copy of poa_calfos to start)
*-------------------------------------------------------------------------------
C
C     Version number
C
      CHARACTER*14 VERSN
      PARAMETER (VERSN = '1.0-Nov-2001')
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
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
     *          USE_PARAMS,DEP_VAR,DEP_COL,IDEP_COL,SEL_COL, NEW_COL
        COMMON /CONFG2/A, B, C, D, E, O, P, Q, R, S
        COMMON /CONFG3/A_MOD, B_MOD, C_MOD, D_MOD,
     *           E_MOD, O_MOD, P_MOD, Q_MOD, R_MOD, S_MOD

C Local variables
C
        INTEGER ISTAT,ISTAT_IN(32)
C                                    --->error status
        CHARACTER*80 CONTXT
C                                    --->text message
        INTEGER I, DEBUGGER
        CHARACTER*6  FIT_DIR
C
C -----------------------------------------------------------------------
C
C POA_CALFOS Version info
C
        CONTXT='*** PFOS_DISPFIT - Version '//VERSN//' ***'
        CALL UMSPUT(CONTXT,STDOUT,0,ISTAT)

C get parameter settings
C

c if working in debugger mode, then must set the parameters internally

        DEBUGGER=.FALSE.

c H13=10000.00  3.7550  -3.9961  0.00  9906.52  298.307  0.98994  4.1680  -255.000  2.1461E-03
c H19=14583.64  3.7550  -3.9726  0.00  9896.02  298.307  0.98994  4.1680  -255.000  2.1461E-03
c H27=20833.33  3.7550  -3.9363  0.00  9915.31  298.307  0.98994  4.1680  -255.000  2.1461E-03
c H40=30553.01  3.7550  -3.9186  0.00  9923.50  298.307  0.98994  4.1680  -255.000  2.1461E-03
c H57=44444.45  3.7550  -3.8286  0.00  9917.77  298.307  0.98994  4.1680  -255.000  2.1461E-03
c L15=69807.50  3.7550     -0.4162   0.00  9906.52  298.307  0.98994  4.1680  -255.000  2.1461E-03
c L65=251059.84  3.7550     -0.4150   0.00  9906.52  298.307   0.98994  4.1680  -255.000  2.1461E-03

        IF (DEBUGGER) THEN 
           INPUT="ex.fits"
           FITTAB="test.fits"
           OUTPUT="test2.fits"
           ITERATION=100
           RELAX=0.1
           CONT_FIT=.TRUE.
           PRINT=50
           USE_PARAMS=.FALSE.
           FIT_DIR="x2wave"
           DEP_VAR="WAVE"
           IDEP_COL="X"
           DEP_COL="WAVE"
           SEL_COL="ss"
           NEW_COL="test7"
           A=30553.010000000
           A_MOD="FIXED"
           B=3.7550000000000
           B_MOD="FIXED"
           C=-3.9186
           C_MOD="FIXED"
           D=0.0
           D_MOD="FIXED"
           E=9900.0000000000
           E_MOD="OPEN"
           O=298.307
           O_MOD="FIXED"
           P=0.98994000000000
           P_MOD="FIXED"
           Q=4.1680000000000
           Q_MOD="FIXED"
           R=-255.00000000000
           R_MOD="FIXED"
           S=0.21461000000000E-02
           S_MOD="FIXED"
        ELSE
           CALL UCLGST('input',INPUT,ISTAT_IN(1))
           CALL UCLGST('fittab',FITTAB,ISTAT_IN(2))
           CALL UCLGST('output',OUTPUT,ISTAT_IN(3))
           CALL UCLGSI('iterations',ITERATION,ISTAT_IN(4))
           IF(ITERATION.LT.0)  CONT_FIT=.TRUE.
           CALL UCLGSR('relax',RELAX,ISTAT_IN(5))
           CALL UCLGSI('print',PRINT,ISTAT_IN(6))
           CALL UCLGSB('use_params',USE_PARAMS,ISTAT_IN(7))
           CALL UCLGST('fit_dir',FIT_DIR,ISTAT_IN(8))

           IF (FIT_DIR.EQ.'wave2x') THEN
              DEP_VAR='X'
              CALL UCLGST('x_col',DEP_COL,ISTAT_IN(9))
              CALL UCLGST('wave_col',IDEP_COL,ISTAT_IN(10))
           ELSE IF (FIT_DIR.EQ.'x2wave') THEN
              DEP_VAR='WAVE'
              CALL UCLGST('wave_col',DEP_COL,ISTAT_IN(9))
              CALL UCLGST('x_col',IDEP_COL,ISTAT_IN(10))
           ELSE
              CONTXT='ERROR invalid entry for FIT_DIR parameter: '//
     *                FIT_DIR
              ISTAT=1
              GO TO 999
           ENDIF

           CALL UCLGST('sel_col',SEL_COL,ISTAT_IN(11))
           CALL UCLGST('newcalc_col',NEW_COL,ISTAT_IN(32))
           CALL UCLGSD('a',A,ISTAT_IN(12))
           CALL UCLGST('a_mod',A_MOD,ISTAT_IN(13))
           CALL UCLGSD('b',B,ISTAT_IN(14))
           CALL UCLGST('b_mod',B_MOD,ISTAT_IN(15))
           CALL UCLGSD('c',C,ISTAT_IN(16))
           CALL UCLGST('c_mod',C_MOD,ISTAT_IN(17))
           CALL UCLGSD('d',D,ISTAT_IN(18))
           CALL UCLGST('d_mod',D_MOD,ISTAT_IN(19))
           CALL UCLGSD('e',E,ISTAT_IN(20))
           CALL UCLGST('e_mod',E_MOD,ISTAT_IN(21))
           CALL UCLGSD('o',O,ISTAT_IN(22))
           CALL UCLGST('o_mod',O_MOD,ISTAT_IN(23))
           CALL UCLGSD('p',P,ISTAT_IN(24))
           CALL UCLGST('p_mod',P_MOD,ISTAT_IN(25))
           CALL UCLGSD('q',Q,ISTAT_IN(26))
           CALL UCLGST('q_mod',Q_MOD,ISTAT_IN(27))
           CALL UCLGSD('r',R,ISTAT_IN(28))
           CALL UCLGST('r_mod',R_MOD,ISTAT_IN(29))
           CALL UCLGSD('s',S,ISTAT_IN(30))
           CALL UCLGST('s_mod',S_MOD,ISTAT_IN(31))

           DO 10 I=1,32
              IF(ISTAT_IN(I).NE.0)THEN
                  CONTXT='ERROR getting value of CL parameter'
                  ISTAT=1
                  GO TO 999
              ENDIF
 10        CONTINUE 
           ISTAT=0

        ENDIF


C run the main program

        CALL YCLMAIN(ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR in returning from main program'
           ISTAT=1
        ENDIF
C
C print completion message
C
        IF(ISTAT.EQ.0)THEN
                CONTXT='Reduction completed for input file '//INPUT
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
           ELSE
                CONTXT='Reduction NOT completed for input file '//
     *                           INPUT
                CALL UMSPUT(CONTXT,STDOUT,0,ISTAT)
                CALL YERROR(ISTAT,CONTXT)
        ENDIF
        GOTO 1000

 999    ISTAT=1
 1000   RETURN
        END
