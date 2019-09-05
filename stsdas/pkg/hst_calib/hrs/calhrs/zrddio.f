        SUBROUTINE ZRDDIO(DIOFIL,FLAG,DET,DRESP,ISTAT)
*
*  Module number:
*
*  Module name: ZRDDIO
*
*  Keyphrase:
*  ----------
*       read diode response file
*
*  Description:
*  ------------
*       This routine reads the diode response file.
*
*  FORTRAN name: ZRDDIO.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*   DIOFIL                      I       Diode response reference file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       ZMSPUT, uimopn, uigl1r, uimgid, uimclo
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       diofil - reference file name character*64
*       flag - Calibration flag.
*       det - detector number (integer 1 or 2)
*
* Output parameters
*       dresp - diode response vector (real)
*       istat - error status (integer)
*-------------------------------------------------------------------------------
        CHARACTER*64 DIOFIL
        CHARACTER*12 FLAG
        INTEGER ISTAT,DET
        REAL DRESP(512)
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        INTEGER IDIN,DIMEN(8),NAXIS,DTYPE,IDET
C                                    --->file I/O parameters
C------------------------------------------------------------------------------
C
C open file
C
        CALL ZREFOP(DIOFIL,'DIOHFILE','DIO_CORR',FLAG,IDIN,ISTAT)
        IF(FLAG.NE.'PERFORM')THEN
           ISTAT=1
           GO TO 1000
        ENDIF
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR opening diode response file '//DIOFIL
           GO TO 999
        ENDIF
C
C Get file size parameters and verify
C
            CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading diode response file '//DIOFIL
                GO TO 998
            ENDIF
            IF((NAXIS.NE.1).OR.(DIMEN(1).NE.512)) THEN
                CONTXT='ERROR: diode response file has '//
     *                  'invalid dimensions '//DIOFIL
                GO TO 998
            ENDIF
C
C get detector number from file and verify
C
            CALL UHDGSI(IDIN,'DETECTOR',IDET,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='WARNING: DETECTOR keyword missing from the '//
     *                  'diode response file'
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ELSE
                IF(IDET.NE.DET)THEN
                    CONTXT='WARNING: DETECTOR keyword value in the '//
     *                  'diode response file does not match observation'
                    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                ENDIF
            ENDIF
C
C Read diode response values
C
            CALL UIGL1R(IDIN,DRESP,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading diode response file '//DIOFIL
                GO TO 998
            ENDIF
            CALL UIMCLO(IDIN,ISTAT)
            ISTAT=0
            GO TO 1000
C
C ERROR SECTION
C
998     CALL UIMCLO(IDIN,ISTAT)
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
