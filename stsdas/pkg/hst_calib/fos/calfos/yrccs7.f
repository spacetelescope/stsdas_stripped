        SUBROUTINE YRCCS7(CCS7,DET,XSCALE,YSCALE,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS7
*
*  Keyphrase:
*  ----------
*       read gimp correction scale factors table
*
*  Description:
*  ------------
*       This routine reads the table containing scale factors for
*       the gimp correction.
*       Rows are selected by detector
*
*  FORTRAN name: YRCCS7.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS7                    I       table containing CCS7 coeffecients
*  Subroutines Called:
*  -------------------
*  CDBS
*       ymsput
*  SDAS:
*       uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91  S. Hulbert      Designed and coded
*     1.1       Jun 91  S. Hulbert      Added y-offset     
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCS7 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*
* Output parameters
*
*       XSCALE - x scale factor in diodes
*       YSCALE - y scale factor in ybase units
*	PEDGRE - CCS7 PEDIGREE keyword string
*	DESCRP - CCS7 DESCRIP  keyword string
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT
        CHARACTER*64 CCS7
        CHARACTER*5 DET
	CHARACTER*68 PEDGRE,DESCRP
        REAL XSCALE, YSCALE
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C     END IRAF77.INC
C
C local variables
C
        INTEGER IDIN,COLIDS(3),ROW,NROWS,ISTATS(3)
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(3), OLDNAM
        CHARACTER*5 DET1
        LOGICAL NULL
        DATA COLNAM/'DETECTOR','X_FACTOR','Y_FACTOR'/
        DATA OLDNAM/'SCALE_FACTOR'/
C
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCS7,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS7 table '//CCS7
                GO TO 998
        ENDIF
C
C get PEDIGREE and DESCRIP header keywords
C
        CALL UTHGTT(IDIN,'PEDIGREE',PEDGRE,ISTAT)
        CALL UTHGTT(IDIN,'DESCRIP',DESCRP,ISTAT)
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCS7 table '//CCS7
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM(1),1,COLIDS(1),ISTATS(1))
        IF(ISTATS(1).NE.0)THEN
            CONTXT='ERROR locating needed columns in CCS7 table '//CCS7
            GO TO 999
	ENDIF
        CALL UTCFND(IDIN,COLNAM(2),1,COLIDS(2),ISTATS(2))
        IF(ISTATS(2).NE.0)THEN
            CALL UTCFND(IDIN,OLDNAM,1,COLIDS(2),ISTATS(2))
            IF(ISTATS(2).NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS7 table '//
     $                  CCS7
                GO TO 999
	    ENDIF
	ENDIF
        CALL UTCFND(IDIN,COLNAM(3),1,COLIDS(3),ISTATS(3))
        IF(ISTATS(3).NE.0)THEN
            CONTXT='ERROR locating Y_FACTOR column in CCS7 table '
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            CONTXT='WARNING: Y_FACTOR will be set to 0.0'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
C
C Locate row with correct detector number
C
        DO 10 ROW=1,NROWS
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR reading CCS7 table '//CCS7
                        GO TO 999
                ENDIF
                IF(DET1.EQ.DET)GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)DET
99      FORMAT('ERROR no row in CCS7 table found for ',A5)
        GO TO 999
C
C read values
C
20      CALL UTRGTR(IDIN,COLIDS(2),1,ROW,XSCALE,NULL,ISTATS(2))
        IF(ISTATS(2).NE.0.)THEN
            CONTXT='ERROR reading CCS7 table '//CCS7
            GO TO 999
	ENDIF
        IF(ISTATS(3).EQ.0)THEN
            CALL UTRGTR(IDIN,COLIDS(3),1,ROW,YSCALE,NULL,ISTATS(3))
            IF(ISTATS(3).NE.0)THEN
                CONTXT='ERROR reading CCS7 table '//CCS7
                GO TO 999
	    ENDIF
	ELSE
	    YSCALE = 0.0
	ENDIF
        CALL UTTCLO(IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR closing CCS7 table '//CCS7
                GO TO 998
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
