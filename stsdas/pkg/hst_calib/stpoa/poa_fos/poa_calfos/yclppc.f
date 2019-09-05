C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLPPC(FRAME,NAME,N,DET,FILL,DATA,ERR,EPS,
     *                    PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YCLPPC
*
*  Keyphrase:
*  ----------
*       Perform FOS paired pulse correction
*  Description:
*  ------------
*       This routine performs the paired pulse correction
*       of FOS data.  On the first call the paired pulse parameters
*       the paired pulse parameters are read from table ccg2.
*       if q0 is not equal to zero then the following equation
*       is used:
*                      y
*               x = --------
*                   (1 - yt)
*
*       where:
*               x is the true count rate
*               y is the observed count rate
*               t = q0 for y less than or equal to F
*               t = q0 + q1*(y-F) for y greater than F
*               q0, q1, and F are coefficients in CCG2
*
*       IF q0 is equal to zero then the following equation is used:
*
*               x = log(1-ty)/(-t)
*
*       where:
*               t = tau1 ( a coefficient in CCG2)
*               the log is a natural logarithm
*
*
*  FORTRAN name: YCLPPC.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccg2                    I       paired pulse coefficient table
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yrccg2, ymsput
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       jul 89  D. Lindler      Designed and coded
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       name - name of the ccg2 reference table
*       n - number of points to calibrate
*       det - detector name
*       fill - flag value for fill data
*
* INPUT/OUTPUT:
*       data - data array
*       err - error array
*       eps - data quality vector
*
* OUTPUT:
*	pedgre - CCG2 PEDIGREE keyword string
*	descrp - CCG2 DESCRIP  keyword string
*       istat - error status
*
*----------------------------------------------------------------------------
        CHARACTER*64 NAME
        INTEGER N,FRAME,ISTAT
        CHARACTER*5 DET
	CHARACTER*68 PEDGRE,DESCRP
        REAL DATA(N),ERR(N),EPS(N),FILL
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C LOCAL VARIABLES ------------------------------------------------------
C
C saturation epsilons
C
        REAL EPSSAT
        PARAMETER (EPSSAT = 300)
        REAL EPSR20
        PARAMETER (EPSR20 = 190)
        REAL EPSR5
        PARAMETER (EPSR5 = 130)
        REAL Q0,Q1,F,THRESH,TAU1,T,RSAT,X,VALUE,R20,R5
        INTEGER I
        CHARACTER*130 CONTXT
C
C-----------------------------------------------------------------------
C
C READ COEFFICIENTS IF FIRST CALL
C
        IF(FRAME.EQ.1)THEN
            CALL YRCCG2(NAME,DET,TAU1,THRESH,Q0,Q1,F,RSAT,R20,R5,
     *                  PEDGRE,DESCRP,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
C
C If the reference table contains dummy data, then skip the correction
C
	    IF(PEDGRE(1:5).EQ.'DUMMY')THEN
	       CONTXT='WARNING: PEDIGREE = DUMMY for CCG2 '//NAME
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       CONTXT='         Paired pulse correction will be '//
     *                'skipped'
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       GO TO 1000
	    END IF
C
C Report the coefficients being used
C
            IF(Q0.EQ.0.0)THEN
                WRITE(CONTXT,99)TAU1
99              FORMAT('Paired pulse coef.  tau1=',
     *                  E16.8)
              ELSE
                CALL YMSPUT('Deadtime coefficients',STDOUT,0,
     *                           ISTAT)
                WRITE(CONTXT,199)Q0,Q1,F
199             FORMAT('  Q0=',E16.8,'  Q1=',E16.8,
     *                 ' F=',F10.0)
            ENDIF
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            WRITE(CONTXT,299)RSAT
299         FORMAT('Saturation limit = ',F12.0,
     *                          ' observed counts/second')
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            IF(THRESH.LT.0.0)THRESH=0.0
        ENDIF
C-------------------- end of frame 1 only processing --------------------------
C
C loop on data
C
        DO 500 I=1,N
            VALUE=DATA(I)
            IF((EPS(I).NE.FILL).AND.(VALUE.GT.THRESH))THEN
C
C Saturated?
C
                IF(VALUE.GT.RSAT)THEN
                        DATA(I)=0.0
                        ERR(I)=0.0
                        IF(EPS(I).LT.EPSSAT)EPS(I)=EPSSAT
                    ELSE
C
C Flag level of saturation
C
                        IF(VALUE.GT.R5)THEN
                          IF(VALUE.LT.R20)THEN
                                IF(EPS(I).LT.EPSR5)EPS(I)=EPSR5
                              ELSE
                                IF(EPS(I).LT.EPSR20)EPS(I)=EPSR20
                          ENDIF
                        ENDIF
C
C CASE 1  Q0 is not zero
C
                        IF (Q0 .NE. 0.0) THEN
                                T=Q0
                                IF(VALUE.GT.F)T=T+(VALUE-F)*Q1
                                X=(1.0-VALUE*T)
                                IF(X .LT. 1.0E-12) X=1.0E-12
                                VALUE=VALUE/X
                                ERR(I)=ERR(I)/X
                            ELSE
C
C CASE 2  Q0 is zero
C
                                X=(1.0-VALUE*TAU1)
                                IF(X .LT. 1.0E-12) X=1.0E-12
                                VALUE=LOG(X)/(-TAU1)
                                ERR(I)=ERR(I)*(VALUE/DATA(I))
                        ENDIF
                        DATA(I) = VALUE
               ENDIF
            ENDIF
500     CONTINUE
C
        ISTAT=0
1000    RETURN
        END
