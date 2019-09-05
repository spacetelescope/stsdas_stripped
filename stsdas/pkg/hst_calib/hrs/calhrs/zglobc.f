      SUBROUTINE ZGLOBC(PASS,IDCCRC,ROW,CARPOS,TNAMES,TCS,JD,TEMPS,
     &     CAP,COEF,ISTAT)
*
*  Module number:
*
*  Module name: zglobc
*
*  Keyphrase:
*  ---------
*     Calculate dispersion coefficients from global coefficients.
*
*  Description:
*  -----------
*     This routine reads the global coefficients from the CCRC table,
*     calculates the dispersion coefficients for the equation:
*
*     s = a0 + a1*m*w + a2*m*m*w*w + a3*m + a4*w +
*         a5*m*m*w + a6*m*w*w + a7*m*m*m*w*w*w
*
*     and applies the following dispersion corrections:
*
*     COR(1) = TCS(1)*(TEMPS(1)-REFTMP(1)) + TCS(2)*(TEMPS(2)-REMTMP(2))
*     COR(2) = JD(3)*(obstime - JD(1))
*     COR(3) = TCS(3)*(TEMPS(3)-REFTMP(3))
*
*     F00 = COR(1) + COR(2)
*     F10 = COR(3)
*
*  FORTRAN name: ztherm.f
*
*  Keywords of accessed files and tables:
*  -------------------------------------
*  Name                          I/O    Description / Comments
*     CCRC                       I      Dispersion coef. table
*
*  Subroutines Called:
*
*  History:
*  --------
*  Version      Date        Author          Description
*
*    2.0        Feb 98     M.D. De La Pena  Fixed calls to UTCFND & UTRGTD.
*                                           An array was being passed for
*                                           the status when a scalar was
*                                           required. Put the calls inside the
*                                           loop so each call can be checked.
*
*---------------------------------------------------------------------------
*
*  INPUTS:
*     PASS   - How many times this routine is called.
*     IDCCRC - Table descriptor for CCRC.
*     ROW    - Row number from CCRC table to read values from.
*     CARPOS - Carrousel position
*     TNAMES - Names of the temperature monitors to use.
*     TCS    - Thermal scale factors.
*     JD     - Time scale factors.
*     CAP    - Values of CAP_A and CAP_C.
*
*  OUTPUTS:
*     COEF   - The dispersion coefficients.
*     ISTAT  - If 0 all is OK.
*
*---------------------------------------------------------------------------
      INTEGER PASS,IDCCRC,ROW,CARPOS,ISTAT
      CHARACTER*8 TNAMES(3)
      DOUBLE PRECISION TCS(3),JD(3),CAP(2),TEMPS(3)
      DOUBLE PRECISION COEF(8)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Number of columns to read from the table.
C
      INTEGER NCOLS
      PARAMETER (NCOLS=19)
C
      INTEGER FIRST
      PARAMETER (FIRST = 1)
      INTEGER LAST
      PARAMETER (LAST = -1)
C
C Local variables
C
      CHARACTER*15 COLNAM(NCOLS)
      INTEGER COLIDS(NCOLS),TSTAT,I
      CHARACTER*161 CONTXT
      DOUBLE PRECISION GLOBAL(NCOLS)
      DOUBLE PRECISION CPOS,CPOS2,COR(3)
      DOUBLE PRECISION F0,F1,F2,F3,F4,F5
      DOUBLE PRECISION MCEN,WCEN,K,K2,K3
      LOGICAL NULL(NCOLS)
C
      DATA COLNAM/'MCENTER','F00','F01','F02',
     &     'F10','F11','F12','F20','F21','F22','F30','F31','F32',
     &     'F40','F41','F42','F50','F51','F52'/
C---------------------------------------------------------------------------
C
C Initializations
C     
      IF(PASS.EQ.FIRST)THEN
C
C Read in the global coefficients from the table.
C
         TSTAT = 0
         DO 1000 I = 1, NCOLS
            CALL UTCFND(IDCCRC,COLNAM(I),1,COLIDS(I),TSTAT)
            IF(TSTAT.NE.0)THEN
               WRITE(CONTXT,9)COLNAM(I)
 9             FORMAT('ERROR: Could not find column ',a15,
     &              ' in CCRC table')
               CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               ISTAT = TSTAT
               GO TO 999
            ENDIF
 1000    CONTINUE
         DO 2000 I = 1, NCOLS
            CALL UTRGTD(IDCCRC,COLIDS(I),1,ROW,GLOBAL(I),NULL(I),TSTAT)
            IF(TSTAT.NE.0)THEN
               WRITE(CONTXT,1009)COLNAM(I)
 1009          FORMAT('ERROR: Could read value from column ',a15,
     &              ' in CCRC table')
               CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               ISTAT = TSTAT
               GO TO 999
            ENDIF
 2000    CONTINUE
C
C Get the thermal correction.
C
         CALL ZTHERM(IDCCRC,ROW,TNAMES,TCS,TEMPS,COR(1),ISTAT)
         IF(ISTAT.NE.0)THEN
            WRITE(CONTXT,1019)
 1019       FORMAT('No thermal correction will be applied')
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            COR(1)=0.D0
         ENDIF
C
C Get the time correction.
C
         CALL ZTIME(JD,COR(2),ISTAT)
         IF(ISTAT.NE.0)THEN
            WRITE(CONTXT,1028)
 1028       FORMAT('No time correction will be applied')
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            COR(1)=0.D0
         ENDIF
C
C Get the thermal linear correction.
C
         CALL ZTEMP(IDCCRC,ROW,TNAMES(3),TCS(3),TEMPS(3),
     &        COR(3),ISTAT)
         IF(ISTAT.NE.0)THEN
            WRITE(CONTXT,1029)
 1029       FORMAT('No thermal linear correction will be applied')
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            COR(1)=0.D0
         ENDIF
C
C And of first pass initializations.
C
      ENDIF
C
C Compute global coefficients based on carrousel position.
C
      CPOS = CARPOS
      CPOS2 = CPOS * CPOS
      F0 = COR(1) + COR(2) + GLOBAL(2) + GLOBAL(3)*CPOS +
     &     GLOBAL(4) * CPOS2
      F1 = COR(3) + GLOBAL(5) + GLOBAL(6)*CPOS + 
     &     GLOBAL(7)*CPOS2
      F2 = GLOBAL(8) + GLOBAL(9)*CPOS +GLOBAL(10)*CPOS2
      F3 = GLOBAL(11) + GLOBAL(12)*CPOS +GLOBAL(13)*CPOS2
      F4 = GLOBAL(14) + GLOBAL(15)*CPOS +GLOBAL(16)*CPOS2
      F5 = GLOBAL(17) + GLOBAL(18)*CPOS +GLOBAL(19)*CPOS2
C
C Compute the dispersion coefficients.
C
      MCEN = GLOBAL(1)
      WCEN = (CAP(1)/MCEN)*SIN((CAP(2)-CPOS)/10430.378D0)
      K = MCEN * WCEN
      K2 = K * K
      K3 = K * K2
      COEF(1) = F0 - F1*K + F2*K2 - F3*K3 - F4*WCEN - F5*MCEN
      COEF(2) = F1 - 2*F2*K + 3*F3*K2
      COEF(3) = F2 - 3*F3*K
      COEF(4) = F5
      COEF(5) = F4
      COEF(6) = 0.0D0
      COEF(7) = 0.0D0
      COEF(8) = F3
C
      ISTAT = 0
 999  RETURN
      END
