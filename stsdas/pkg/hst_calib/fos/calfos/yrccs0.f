        SUBROUTINE YRCCS0(CCS0,DET,APER,AREA,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS0
*
*  Keyphrase:
*  ----------
*       read aperture area table
*
*  Description:
*  ------------
*       This routine reads the aperture area table.
*       It is called only for paired apertures and returns
*       the aperture area for both the upper and lower aperture
*       of the pair for the given detector and dispersor.
*
*  FORTRAN name: YRCCS0.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS0                    I       table containing CCS0 coeffecients
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
*       CCS0 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       APER - aperture
*
* Output parameters
*       AREA - vector containing the upper and lower areas
*	PEDGRE - CCS0 PEDIGREE keyword
*	DESCRP - CCS0 DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT
        CHARACTER*64 CCS0
	CHARACTER*68 PEDGRE, DESCRP
        CHARACTER*5 DET
        CHARACTER*3 APER
        REAL AREA(2)
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
        INTEGER IDIN,COLIDS(4),ROW,NROWS,IPOS
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(4)
        CHARACTER*5 DET1,APOS
        CHARACTER*3 APER1
        LOGICAL FOUND(2)
        LOGICAL NULL(4)
        DATA COLNAM/'DETECTOR','APER_ID','APER_POS','APER_AREA'/
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCS0,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS0 table '//CCS0
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
                CONTXT='ERROR reading CCS0 table '//CCS0
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,4,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS0 table '//
     *                  CCS0
                GO TO 999
        ENDIF
C
C Loop on rows
C
        FOUND(1)=.FALSE.
        FOUND(2)=.FALSE.
        DO 500 ROW=1,NROWS
C
C check detector
C
                CONTXT='ERROR reading CCS0 table '//CCS0
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(DET1.NE.DET) GO TO 500
C
C Check aper_id
C
                CALL UTRGTT(IDIN,COLIDS(2),1,ROW,APER1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(APER1.NE.APER) GO TO 500
C
C Determine if upper or lower
C
                CALL UTRGTT(IDIN,COLIDS(3),1,ROW,APOS,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IPOS=0
                IF(APOS.EQ.'UPPER')IPOS=1
                IF(APOS.EQ.'LOWER')IPOS=2
                IF(IPOS.EQ.0)GO TO 500
C
C read area
C
                FOUND(IPOS)=.TRUE.
                CALL UTRGTR(IDIN,COLIDS(4),1,ROW,AREA(IPOS),NULL,
     *                  ISTAT)
                IF(ISTAT.NE.0) GO TO 999
500     CONTINUE
        CALL UTTCLO(IDIN,ISTAT)
C
C Check to see if both apertures were found
C
        IF((.NOT.FOUND(1)).OR.(.NOT.FOUND(2)))THEN
            WRITE(CONTXT,99)DET,APER
99          FORMAT('ERROR: upper or lower area missing in ccs0 ',
     *                      A5,1X,A3)
            GO TO 999
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
