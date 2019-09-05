        SUBROUTINE ZRCCRA(CCRA,GRAT,F,BETA,DELTA,R0,CPNORM,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCRA
*
*  Keyphrase:
*  ----------
*       Read table CCRA (GHRS Echelle ripple coefficients)
*
*  Description:
*  ------------
*       This routine reads table CCRA and extracts the parameters
*       for the specified grating.
*
*  FORTRAN name: ZRCCRA.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCRA                    I       Echelle ripple coef. table
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
*       CCRA - table name (character*64)
*	GRATING - grating mode (CHARACTER*5)
*
* Output parameters
*
*	F, BETA, DELTA, R0 - ripple parameters from the table (all real*4)
*       CPNORM - Center carrousel position for the echelles.
*       istat - ERROR status (integer)
**************************************************************************
        CHARACTER*64 CCRA
        CHARACTER*5 GRAT
        INTEGER ISTAT
        REAL F,BETA,DELTA,R0,CPNORM
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
        INTEGER IDIN,NROWS,COLIDS(6),ROW,I,ISTATS(6)
        CHARACTER*5 GMODE
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(6)
        LOGICAL NULL
        DATA COLNAM/'GRATING','F','BETA','DELTA','R0','CPNORM'/
C
C--------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCRA,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCRA table '//CCRA
                GO TO 998
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCRA table '//CCRA
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,6,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCRA table '//
     *                  CCRA
                GO TO 999
        ENDIF
C
C Locate row with correct grating
C
        DO 10 ROW=1,NROWS
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,GMODE,NULL,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR reading CCRA table '//CCRA
                        GO TO 999
                ENDIF
                IF(GMODE.EQ.GRAT)GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)GRAT
99      FORMAT('ERROR no row in CCRA table found for grating ',A5)
        GO TO 999
C
C read values
C
20      CALL UTRGTR(IDIN,COLIDS(2),1,ROW,F,NULL,ISTATS(1))
        CALL UTRGTR(IDIN,COLIDS(3),1,ROW,BETA,NULL,ISTATS(2))
        CALL UTRGTR(IDIN,COLIDS(4),1,ROW,DELTA,NULL,ISTATS(3))
        CALL UTRGTR(IDIN,COLIDS(5),1,ROW,R0,NULL,ISTATS(4))
        CALL UTRGTR(IDIN,COLIDS(6),1,ROW,CPNORM,NULL,ISTATS(5))
        DO 30 I=1,5
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading CCRA table '//CCRA
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
