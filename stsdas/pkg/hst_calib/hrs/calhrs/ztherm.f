      SUBROUTINE ZTHERM(IDCCRC,ROW,TNAMES,TCS,TEMPS,
     &     COR,ISTAT)
*
*  Module number:
*
*  Module name: ztherm
*
*  Keyphrase:
*  ---------
*     Determine thermal motion correction to the dispersion coefficient.
*
*  Description:
*  -----------
*     This routine computes a two-temperature thermal correction
*     to be applied to the constant term of the calculated wavelength
*     dispersion solution.
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
*---------------------------------------------------------------------------
*
*  INPUTS:
*     IDCCRC - Table descriptor for CCRC.
*     ROW    - Row number from CCRC table to read values from.
*     TNAMES - Names of the thermal monitors to use.
*     TCS    - Scale factors for the correction.
*     TEMPS  - Observation temperatures
*
*  OUTPUTS:
*     COR    - Correction to apply to the constant dispersion.
*     ISTAT  - If 0 all is OK.
*
*---------------------------------------------------------------------------
      INTEGER IDCCRC,ROW,ISTAT
      CHARACTER*8 TNAMES(3)
      DOUBLE PRECISION TCS(3), TEMPS(3)
      DOUBLE PRECISION COR
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Local variables
C
      INTEGER I, ID, STAT
      CHARACTER*161 CONTXT
      LOGICAL NULL
      DOUBLE PRECISION REFTMP, D
C
C---------------------------------------------------------------------------
C
C Check to see if there is enough information to compute the correction.
C
      IF(((TNAMES(1).EQ.' ').OR.(TCS(1).EQ.0.D0).OR.
     &     (TEMPS(1).EQ.-999.D0)).AND.
     &     ((TNAMES(2).EQ.' ').OR.(TCS(2).EQ.0.D0).OR.
     &     (TEMPS(2).EQ.-999.D0)))THEN
         WRITE(CONTXT,199)
 199     FORMAT('Warning: no thermal constants available to ',
     &        'compute a thermal correction')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         ISTAT=1
         GO TO 999
      ENDIF
C
C Loop on the thermistor values
C
      COR = 0.0D0
      DO 1000 I = 1, 2
C
C See if a temperature exists.
C
         IF(TNAMES(I).EQ.' ')GO TO 1000
C
C get reference temperature
C
         CALL UTCFND(IDCCRC,TNAMES(I),1,ID,ISTAT)
         IF(ISTAT.NE.0)THEN
            WRITE(CONTXT,1019)TNAMES(I)
 1019       FORMAT('ERROR: Could not find column ',a8,
     &           'in CCRC table.')
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
            WRITE(CONTXT,1018)I
 1018       FORMAT('Thermal motion constant ',i1,
     &           ' will be undefined')
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
            GO TO 1000
         ENDIF
         CALL UTRGTD(IDCCRC,ID,1,ROW,REFTMP,NULL,ISTAT)
         IF(ISTAT.NE.0)THEN
            WRITE(CONTXT,1509)TNAMES(I),ROW
 1509       FORMAT('ERROR: Could not get value of column ',a8,
     &           ' in row ',i4)
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
            WRITE(CONTXT,1018)I
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
            GO TO 1000
         ENDIF
C
C Calculate correction.
C
         D = TCS(I) * (TEMPS(I) - REFTMP)
         COR = COR + D
         WRITE(CONTXT,1529)TNAMES(I), D
 1529    FORMAT('Thermal correction from ',a10,' = ',g10.3,
     &        ' sample units')
         CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
         ISTAT=0
C
 1000 CONTINUE
 999  CONTINUE
      RETURN
      END
