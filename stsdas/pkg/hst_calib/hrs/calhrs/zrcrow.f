      SUBROUTINE ZRCROW(IDCCRC,GRAT,ROW,ISTAT)
*
*  Module number:
*
*  Module name: zrcrow
*
*  Keyphrase:
*  ---------
*     Determine row of CCRC to retrieve values from.
*
*  Description:
*  -----------
*     The last row in the table that matches the grating mode is used
*     as a source for values relating to the dispersion coefficients.
*
*  FORTRAN name: zrcrow.f
*
*  Keywords of accessed files and tables:
*  -------------------------------------
*  Name                          I/O    Description / Comments
*     CCRC                       I      Dispersion coef. table
*
*  Subroutines Called:
*  ------------------
*  SDAS:
*     zmsput
*---------------------------------------------------------------------------
*
*  INPUTS:
*     IDCCRC - Table descriptor for CCRC.
*     GRAT   - Grating in use.
*
*  OUTPUTS:
*     ROW    - Row number from CCRC table to read values from.
*     ISTAT  - 0 if OK.
*
*---------------------------------------------------------------------------
      INTEGER IDCCRC,ROW
      CHARACTER*5 GRAT
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C     THIS SECTION IS FOR PARAMETERS RELEVANT TO TABLE I/O.
C
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C
C Local Variables
C
      INTEGER NROWS,ISTAT,GRATID,I
      CHARACTER*161 CONTXT
      CHARACTER*8 COLNAM,TGRAT
      LOGICAL NULL
      DATA COLNAM/'GRATING'/
C
C---------------------------------------------------------------------------
C
      CALL UTPGTI(IDCCRC,TBNROW,NROWS,ISTAT)
      IF(ISTAT.NE.0)THEN
         WRITE(CONTXT,1009)
 1009    FORMAT('ERROR: Could not get number of rows from CCRC table')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,I)
         GO TO 999
      ENDIF
C
C Get column ID for grating.
C
      CALL UTCFND(IDCCRC,COLNAM,1,GRATID,ISTAT)
      IF(ISTAT.NE.0)THEN
         WRITE(CONTXT,1019)COLNAM
 1019    FORMAT('ERROR: Could not find column ',a,' in CCRC table')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         ISTAT=1
         GO TO 999
      ENDIF
C
C find last row that contains the appropriate grating.
C
      ROW=0
      DO 2000 I = 1, NROWS
         CALL UTRGTT(IDCCRC,GRATID,1,I,TGRAT,NULL,ISTAT)
         IF (ISTAT.NE.0)THEN
            WRITE(CONTXT,2009)COLNAM,I
 2009       FORMAT('ERROR: Could not get value of column ',a,
     &           ' at row ',i4)
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ISTAT=1
            GO TO 999
         ENDIF
         IF(TGRAT.EQ.GRAT)ROW=I
 2000 CONTINUE
C
C A line must be found.
C
      IF(ROW.LE.0)THEN
         WRITE(CONTXT,3009)GRAT
 3009    FORMAT('ERROR: No dispersion solution for grating ',a,
     &        ' found in CCRC table')
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         ISTAT=1
         GO TO 999
      ENDIF
C
C That's all folks.
C
         ISTAT=0
 999     RETURN
         END
