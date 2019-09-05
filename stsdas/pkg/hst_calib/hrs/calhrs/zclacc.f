        SUBROUTINE ZCLACC(NAME,LABEL,TYPE,ISTAT)
*
*  Module number:
*
*  Module name: ZCLACC
*
*  Keyphrase:
*  ----------
*       Check that the reference relation tables and files exist
*  Description:
*  ------------
*       This routine inquires about the existence of a file and prints 
*       an error message if unable to find the files.
*
*  FORTRAN name: ZCLACC.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       none
*  SDAS:
*       uufacc, ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Sep 89      S J Hulbert     Designed and coded
*-------------------------------------------------------------------------------
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      CHARACTER*80 CONTXT
      CHARACTER*64 NAME 
      CHARACTER*8 LABEL
      CHARACTER*5 TYPE
      LOGICAL EXIST
      INTEGER ISTAT
C
C--------------------------------------------------------------------------
C
      IF (TYPE .EQ. 'TABLE') THEN
         CALL UTTACC(NAME,EXIST,ISTAT)
      ELSE IF (TYPE .EQ. 'FILE') THEN
         CALL UUFACC(NAME,EXIST,ISTAT)
      ENDIF
C
C notify, if any problems 
C
      IF (ISTAT .NE. 0 .OR. .NOT. EXIST) GO TO 999
C
      ISTAT = 0
      GO TO 1000
C
  999 CONTXT='ERROR: Reference '//TYPE//' not found for keyword '//LABEL
      CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      CONTXT='       = '//NAME
      CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT = 1
 1000 RETURN
      END
