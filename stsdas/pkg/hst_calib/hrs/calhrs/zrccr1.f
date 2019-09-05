        SUBROUTINE ZRCCR1(CCR1,DET,L0,A,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCR1
*
*  Keyphrase:
*  ----------
*       Read table CCR1 (GHRS line mapping function coefficients)
*
*  Description:
*  ------------
*       This routine reads table CCR1 and extracts the parameters
*       for the specified detector.
*
*  FORTRAN name: ZRCCR1.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR1                    I       Line mapping function coef.
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT, uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       APR 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCR1 - table name (character*64)
*       det - detector number (integer)
*
* Output parameters
*
*       L0 - constant term in line mapping function coef. (real*4)
*       A - linear term (real*4)
*       istat - ERROR status (integer)
**************************************************************************
        CHARACTER*64 CCR1
        INTEGER ISTAT,DET
        REAL L0,A
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
C     THIS SECTION IS FOR PARAMETERS RELEVANT TO TABLE I/O.
C
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C     END IRAF77.INC
C
C LOCAL VARIABLES -------------------------------------------
C
        INTEGER IDIN,NROWS,COLIDS(16),ROW,IDET,I,ISTATS(4)
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(3)
        LOGICAL NULL
        DATA COLNAM/'DETECTOR','L0','A'/
C
C--------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCR1,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCR1 table '//CCR1
                GO TO 998
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCR1 table '//CCR1
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,3,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCR1 table '//
     *                  CCR1
                GO TO 999
        ENDIF
C
C Locate row with correct detector number
C
        DO 10 ROW=1,NROWS
                CALL UTRGTI(IDIN,COLIDS(1),1,ROW,IDET,NULL,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR reading CCR1 table '//CCR1
                        GO TO 999
                ENDIF
                IF(DET.EQ.IDET)GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)DET
99      FORMAT('ERROR no row in CCR1 table found for detector ',I2)
        GO TO 999
C
C read values
C
20      CALL UTRGTR(IDIN,COLIDS(2),1,ROW,L0,NULL,ISTATS(1))
        CALL UTRGTR(IDIN,COLIDS(3),1,ROW,A,NULL,ISTATS(2))
        DO 30 I=1,2
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading CCR1 table '//CCR1
                GO TO 999
            ENDIF
30      CONTINUE
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
