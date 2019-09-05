        SUBROUTINE ZRCCR5(CCR5,GRAT,CAPA,LITA,CAPB,LITB,CAPC,
     *                     LITD,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCR5
*
*  Keyphrase:
*  ----------
*       Read table CCR5 (GHRS spectral order constants)
*
*  Description:
*  ------------
*       This routine reads table CCR5 and extracts the parameters
*       for the specified grating.
*
*  FORTRAN name: ZRCCR5.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR5                    I       spectral order constants
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
*       CCR5 - table name (character*64)
*	GRATING - grating mode ('ECH-A' or 'ECH-B')
*
* Output parameters
*
*	CAPA, LITA, CAPB, LITB, CAPC, LITD - grating coefficients
*		read from the table
*       istat - ERROR status (integer)
**************************************************************************
        CHARACTER*64 CCR5
        CHARACTER*5 GRAT
        INTEGER ISTAT
        DOUBLE PRECISION CAPA,CAPB,CAPC,LITA,LITB,LITD
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
        INTEGER IDIN,NROWS,COLIDS(7),ROW,I,ISTATS(7)
        CHARACTER*5 GMODE
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(7)
        LOGICAL NULL
        DATA COLNAM/'GRATING','CAP_A','LIT_A','CAP_B','LIT_B','CAP_C',
     *                        'LIT_D'/
C
C--------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCR5,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCR5 table '//CCR5
                GO TO 998
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCR5 table '//CCR5
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,7,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCR5 table '//
     *                  CCR5
                GO TO 999
        ENDIF
C
C Locate row with correct grating
C
        DO 10 ROW=1,NROWS
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,GMODE,NULL,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR reading CCR5 table '//CCR5
                        GO TO 999
                ENDIF
                IF(GMODE.EQ.GRAT)GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)GRAT
99      FORMAT('ERROR no row in CCR5 table found for grating ',A5)
        GO TO 999
C
C read values
C
20      CALL UTRGTD(IDIN,COLIDS(2),1,ROW,CAPA,NULL,ISTATS(1))
        CALL UTRGTD(IDIN,COLIDS(3),1,ROW,LITA,NULL,ISTATS(2))
        CALL UTRGTD(IDIN,COLIDS(4),1,ROW,CAPB,NULL,ISTATS(3))
        CALL UTRGTD(IDIN,COLIDS(5),1,ROW,LITB,NULL,ISTATS(4))
        CALL UTRGTD(IDIN,COLIDS(6),1,ROW,CAPC,NULL,ISTATS(5))
        CALL UTRGTD(IDIN,COLIDS(7),1,ROW,LITD,NULL,ISTATS(6))
        DO 30 I=1,6
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading CCR5 table '//CCR5
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
