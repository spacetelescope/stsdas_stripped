        SUBROUTINE ZRCCRB(CCRB,GRAT,APER,ORDER,COEF,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCRB
*
*  Keyphrase:
*  ----------
*       Read table CCRB: scattered light correction coefficients
*
*  Description:
*  ------------
*       This routine reads table CCRB and extracts the scattered
*	light correction factors.
*
*  FORTRAN name: ZRCCRB.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCRB                    I       scattered light coef. table
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       zmsput, uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91      S. Hulbert      Designed and coded
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       ccrB - table name 
*	grating - grating mode 
*	aper - aperture
*	order - spectral order
*
* Output parameters
*
*	coef - scattered light coefficients (4)
*       istat - ERROR status (integer)
**************************************************************************
        CHARACTER*64 CCRB
        CHARACTER*5 GRAT
	CHARACTER*3 APER
	DOUBLE PRECISION COEF(4)
        INTEGER ORDER, ISTAT
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
        CHARACTER*5 GRAT0
        CHARACTER*3 APER0
	INTEGER ORDER0
        CHARACTER*80 CONTXT
        CHARACTER*8 COLNAM(7)
        LOGICAL NULL
        DATA COLNAM/'GRATING','SPORDER','APERTURE',
     $	'A_SCAT','B_SCAT','C_SCAT','D_SCAT'/
C
C--------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCRB,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCRB table '//CCRB
                GO TO 998
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCRB table '//CCRB
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,7,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCRB table '//
     *                  CCRB
                GO TO 999
        ENDIF
C
C Locate row with correct grating
C
        DO 10 ROW=1,NROWS
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,GRAT0,NULL,ISTATS(1))
                CALL UTRGTI(IDIN,COLIDS(2),1,ROW,ORDER0,NULL,ISTATS(2))
                CALL UTRGTT(IDIN,COLIDS(3),1,ROW,APER0,NULL,ISTATS(3))
		DO 100 I = 1, 3
                    IF(ISTATS(I).NE.0)THEN
                        CONTXT='ERROR reading CCRB table '//CCRB
                        GO TO 999
                    ENDIF
100		CONTINUE
                IF ((GRAT0 .EQ. GRAT) .AND. (ORDER0 .EQ. ORDER) .AND.
     $			(APER0 .EQ. APER)) GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)GRAT
99      FORMAT('ERROR no row in CCRB table found for grating ',A5,',')
        CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        WRITE(CONTXT,98)ORDER,APER
98      FORMAT('             order ',I4,', aperture ',A3)
        GO TO 999
C
C read values
C
20      CALL UTRGTD(IDIN,COLIDS(4),1,ROW,COEF(1),NULL,ISTATS(1))
        CALL UTRGTD(IDIN,COLIDS(5),1,ROW,COEF(2),NULL,ISTATS(2))
        CALL UTRGTD(IDIN,COLIDS(6),1,ROW,COEF(3),NULL,ISTATS(3))
        CALL UTRGTD(IDIN,COLIDS(7),1,ROW,COEF(4),NULL,ISTATS(4))
        DO 30 I=1,4
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading CCRB table '//CCRB
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
