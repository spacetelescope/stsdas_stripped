C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRCCS1(CCS1,DET,FGWA,APER,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS1
*
*  Keyphrase:
*  ----------
*       read aperture position table
*
*  Description:
*  ------------
*       If the input aperture is a paired aperture, this routine
*       reads the y-position of the upper and lower aperture of the
*       pair.
*
*  FORTRAN name: YRCCS1.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS1                    I       table containing CCS1 coeffecients
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
*       1       July 89  D. Lindler      Designed and coded
*	2	Mar 94	 H. Bushouse	 Mods to handle PEDIGREE keywords
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCS1 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       FGWA - grating mode
*       APER - aperture
*
* Output parameters
*
*	PEDGRE - CCS1 PEDIGREE keyword string
*	DESCRP - CCS1 DESCRIP  keyword string
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT
        CHARACTER*64 CCS1
        CHARACTER*5 DET
        CHARACTER*3 FGWA,APER
	CHARACTER*68 PEDGRE, DESCRP
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

        LOGICAL PAIRED
        REAL YUPPER,YLOWER
        COMMON /CCS1CM/PAIRED,YUPPER,YLOWER
C
C local variables
C
        INTEGER IDIN,COLIDS(4),ROW,I,ISTATS(4),NROWS
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(4)
        CHARACTER*5 DET1
        CHARACTER*3 FGWA1,APPAIR(4)
        LOGICAL NULL
        DATA COLNAM/'DETECTOR','FGWA_ID','YUPPER','YLOWER'/
        DATA APPAIR/'A-2','A-3','A-4','C-1'/
C
C---------------------------------------------------------------------------
C
C Is it a paired aperture
C
        DO 1 I=1,4
             IF(APER.EQ.APPAIR(I))GO TO 2
1       CONTINUE
        ISTAT=0
        PAIRED=.FALSE.
        GO TO 1000

2       PAIRED=.TRUE.
C
C Open table
C
        CALL UTTOPN(CCS1,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS1 table '//CCS1
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
                CONTXT='ERROR reading CCS1 table '//CCS1
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,4,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS1 table '//
     *                  CCS1
                GO TO 999
        ENDIF
C
C Locate row with correct detector number and fgwa_id
C
        DO 10 ROW=1,NROWS
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTATS(1))
                CALL UTRGTT(IDIN,COLIDS(2),1,ROW,FGWA1,NULL,ISTATS(2))
                IF((ISTATS(1).NE.0).OR.(ISTATS(2).NE.0))THEN
                        CONTXT='ERROR reading CCS1 table '//CCS1
                        GO TO 999
                ENDIF
                IF((DET1.EQ.DET).AND.(FGWA1.EQ.FGWA))GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)FGWA,DET
99      FORMAT('ERROR no row in CCS1 table found for ',A3,1X,A5)
        GO TO 999
C
C read values
C
20      CALL UTRGTR(IDIN,COLIDS(3),1,ROW,YUPPER,NULL,ISTATS(1))
        CALL UTRGTR(IDIN,COLIDS(4),1,ROW,YLOWER,NULL,ISTATS(2))
        DO 30 I=1,2
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading CCS1 table '//CCS1
                GO TO 999
            ENDIF
30      CONTINUE
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
