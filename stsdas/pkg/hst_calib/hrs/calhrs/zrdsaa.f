        SUBROUTINE ZRDSAA(SAAFIL,FLAG,SAA,ISTAT)
*
*  Module number:
*
*  Module name: ZRDSAA
*
*  Keyphrase:
*  ----------
*       read SAA contour image
*
*  Description:
*  ------------
*       This routine reads the SAA contour file.
*
*  FORTRAN name: ZRDSAA.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*   SAAFIL                      I       SAA contour reference file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       ZMSPUT, uimopn, uigs2i, uimgid, uimclo
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Nov 96  M. De La Pena   Original implementation
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       saafil - reference file name character*64
*       flag   - calibration flag
*
* Output parameters
*       saa   - SAA contour image (real)
*       istat - error status (integer)
*-------------------------------------------------------------------------------
      CHARACTER*12 FLAG
      CHARACTER*64 SAAFIL
      INTEGER      ISTAT
      INTEGER      SAA(360,180)
C
C FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C LOCAL VARIABLES
C
      CHARACTER*80 CONTXT
      INTEGER IDIN,DIMEN(8),NAXIS,DTYPE
C                                    --->file I/O parameters
C------------------------------------------------------------------------------
C
C Open file
C
      CALL ZREFOP(SAAFIL,'SAAHFILE','BMD_CORR',FLAG,IDIN,ISTAT)
      IF(FLAG.NE.'PERFORM')THEN
          ISTAT=1
          GO TO 1000
      ENDIF
      IF(ISTAT.NE.0)THEN
          CONTXT='ERROR opening SAA contour file '//SAAFIL
          GO TO 999
      ENDIF
C
C Get file size parameters and verify
C
      CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
      IF(ISTAT.NE.0)THEN
          CONTXT='ERROR reading SAA contour file '//SAAFIL
          GO TO 998
      ENDIF
      IF((NAXIS.NE.2).OR.(DIMEN(1).NE.360).OR.(DIMEN(2).NE.180))THEN
          CONTXT='ERROR: SAA contour file has '//
     *           'invalid dimensions '//SAAFIL
          GO TO 998
      ENDIF
C
C Read SAA contour image
C
      CALL UIGS2I(IDIN,1,360,1,180,SAA,ISTAT)
      IF(ISTAT.NE.0)THEN
          CONTXT='ERROR reading SAA contour file '//SAAFIL
          GO TO 998
      ENDIF
      CALL UIMCLO(IDIN,ISTAT)
      ISTAT=0
      GO TO 1000
C
C ERROR SECTION
C
998   CALL UIMCLO(IDIN,ISTAT)
999   CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT=1
1000  RETURN
      END
