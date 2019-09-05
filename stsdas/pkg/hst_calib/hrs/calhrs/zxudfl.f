        SUBROUTINE ZXUDFL(NAME,FLAG,ID,ISTAT)
*
*  Module number:
*
*  Module name: ZXUDFL
*
*  Keyphrase:
*  ----------
*       Update processing flags in output header
*
*  Description:
*  ------------
*       This routine updates processing flags from PERFORM to
*       COMPLETE in the output header or write SKIPPED if set.
*
*  FORTRAN name: zxudfl.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       uhdgst
*
*  History:
*  --------
*  Version      Date        Author          Description
*     1       Apr94         J. Eisenhamer   Created
*-------------------------------------------------------------------------------
*
* Input parameters
*  
*     name - Name of the header keyword to update.
*     flag - calibration flag
*     id - image descripter of image to update.
*
* Output parameter
*
*        ISTAT - error status
*
*---------------------------------------------------------------------------
      CHARACTER*8 NAME
      CHARACTER*12 FLAG
      INTEGER ISTAT
C
      IF(FLAG.EQ.'PERFORM')THEN
         CALL UHDAST(ID,NAME,'COMPLETE',' ',0,ISTAT)
         IF(ISTAT.NE.0)GO TO 999
      ENDIF
C
      IF(FLAG.EQ.'SKIPPED')THEN
         CALL UHDAST(ID,NAME,'SKIPPED',' ',0,ISTAT)
         IF(ISTAT.NE.0)GO TO 999
      ENDIF
C
 1000 CONTINUE
      ISTAT=0
 999  CONTINUE
      RETURN
      END
