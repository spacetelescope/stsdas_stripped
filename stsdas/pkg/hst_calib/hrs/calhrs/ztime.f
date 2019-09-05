      SUBROUTINE ZTIME(JD,COR,ISTAT)
*
*  Module number:
*
*  Module name: ztime
*
*  Keyphrase:
*  ---------
*     Determine dispersion correction due to time.
*
*  Description:
*  -----------
*     The correction for the constant coefficient of the dispersion
*     is calculated.
*
*  FORTRAN name: ztime.f
*
*  Keywords of accessed files and tables:
*  -------------------------------------
*  Name                          I/O    Description / Comments
*
*  Subroutines Called:
*
*---------------------------------------------------------------------------
*
*  INPUTS:
*     JD - The time constants.
*
*  OUTPUTS:
*     COR    - Correction to apply to the global coefficients.
*     ISTAT  - If 0 all is OK.
*
*---------------------------------------------------------------------------
      DOUBLE PRECISION JD(3)
      DOUBLE PRECISION COR
      INTEGER ISTAT
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C                         /HRSGPR/
C
C Common block for output group parameter storage
C
C        PKTIME - packet times in MJD
C        ERRCNT - error counts (.d0h)
C        FILLCN - fill counts  (.d0h)
C        SCOEF - sample coefficients
C        EXPO - exposure times
C
        INTEGER ERRCNT(7),FILLCN(7)
        REAL SCOEF(4,7),EXPO(7)
        DOUBLE PRECISION PKTIME(7)
        COMMON /HRSGPR/PKTIME,ERRCNT,FILLCN,SCOEF,EXPO
C
C Local variables
C
      CHARACTER*161 CONTXT
      DOUBLE PRECISION TIME
C
C---------------------------------------------------------------------------
C
C If the zero time is given, calculate the motion.
C
      COR = 0.D0
      IF(JD(1).EQ.0.D0)THEN
         WRITE(CONTXT,99)
 99      FORMAT('Warning: no time coefficients are found')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         ISTAT=1
      ELSE
         TIME=MIN(JD(2),MAX(JD(1),PKTIME(1)))
         COR = (TIME - JD(1)) * JD(3)
         WRITE(CONTXT,2009)COR
 2009    FORMAT('Time motion correction = ',g10.3,' sample units')
         CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
         ISTAT=0
      ENDIF
C
 999  RETURN
      END
