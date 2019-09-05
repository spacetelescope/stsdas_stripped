      SUBROUTINE ZTEMP(IDCCRC,ROW,TNAME,TC,TEMP,COR,ISTAT)
*
*  Module number:
*
*  Module name: ztemp
*
*  Keyphrase:
*  ---------
*     Determine linear dispersion change with temperature.
*
*  Description:
*  -----------
*     This routine calculates the correction to the linear constant 
*     of the wavelength dispersion.
*
*  FORTRAN name: ztemp.f
*
*  Keywords of accessed files and tables:
*  -------------------------------------
*  Name                          I/O    Description / Comments
*     CCRC                       I      Dispersion coef. table
*
*  Subroutines Called:
*
* History:
* -------
*  Version      Date        Author          Description
*      1.1      Jun 98  M. De La Pena   Fixed syntax on FORMAT statements
*
*---------------------------------------------------------------------------
*
*  INPUTS:
*     IDCCRC - Table descriptor for CCRC.
*     ROW    - Row number from CCRC table to read values from.
*     TNAME  - Name of the temperature monitor to use.
*     TC     - Correction scale factor.
*     TEMP   - Observation temperature.
*
*  OUTPUTS:
*     COR    - Correction to apply to the global coefficients.
*     ISTAT  - If 0 all is OK.
*
*---------------------------------------------------------------------------
      INTEGER IDCCRC,ROW,ISTAT
      CHARACTER*8 TNAME
      DOUBLE PRECISION TC, TEMP
      DOUBLE PRECISION COR
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Local Variables
C
      INTEGER ID, STAT
      CHARACTER*161 CONTXT
      LOGICAL NULL
      DOUBLE PRECISION REFTMP
C
C---------------------------------------------------------------------------
C
C Check that there is enough information.
C
      IF((TNAME.EQ.' ').OR.(TC.EQ.0.D0).OR.(TEMP.EQ.-999.D0))THEN
         WRITE(CONTXT,109)
 109     FORMAT('Warning: no linear thermal constants are specified.')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         ISTAT=1
         GO TO 999
      ENDIF
C
C get reference temperature
C
      CALL UTCFND(IDCCRC,TNAME,1,ID,ISTAT)
      IF(ISTAT.NE.0)THEN
         WRITE(CONTXT,1019)TNAME
 1019    FORMAT('Error: could not find column ',a8,
     &        ' in CCRC table')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
         WRITE(CONTXT,1018)
 1018    FORMAT('Linear thermal motion constant ',
     &        ' will be undefined')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
         GO TO 999
      ENDIF
      CALL UTRGTD(IDCCRC,ID,1,ROW,REFTMP,NULL,ISTAT)
      IF(ISTAT.NE.0)THEN
         WRITE(CONTXT,1509)TNAME,ROW
 1509    FORMAT('Error: could not read value from column ',a8,
     &        ', row ',i4,' from CCRC table')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
         WRITE(CONTXT,1018)
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,STAT)
         GO TO 999
      ENDIF
C
C Compute correction.
C
      COR = TC * (TEMP - REFTMP)
      WRITE(CONTXT,1529)TNAME, COR
 1529 FORMAT('Temperature dispersion correction for ',a10,
     &     ' = ',g10.3,' sample units/m*lambda')
      CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
C
 1000 ISTAT = 0
 999  CONTINUE
      RETURN
      END
