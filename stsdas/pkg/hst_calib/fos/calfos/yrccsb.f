      SUBROUTINE YRCCSB(CCSB,DET,FGWA,APER,APOS,COEF,WMIN,WMAX,
     *     REFAPR,FOUND,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCSB
*
*  Keyphrase:
*  ----------
*       read aperture throughput coefficient table
*
*  Description:
*  ------------
*       This routine reads the aperture throughput coefficient table.
*
*  FORTRAN name: YRCCSB.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCSB                    I       table containing CCSB coeffecients
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
*       1       Oct 94   H. Bushouse     Designed and coded
*       2       Sep 95   J. Eisenhamer   No longer error; set to DUMMY
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCSB - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       FGWA - grating mode
*       APER - aperture
*	APOS  - aperture position ('SINGLE', 'UPPER', or 'LOWER')
*
* Output parameters
*       COEF - throughput coefficients
*	WMIN - minimum wavelength for which coeff's apply
*	WMAX - maximum wavelength for which coeff's apply
*	REFAPR - reference aperture name
*	FOUND - number of table entries found for DET/FGWA/APER combo
*	PEDGRE - CCSB PEDIGREE keyword
*	DESCRP - CCSB DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER FOUND, ISTAT
      CHARACTER*64 CCSB
      CHARACTER*68 PEDGRE, DESCRP
      CHARACTER*5 DET
      CHARACTER*3 FGWA,APER,REFAPR
      CHARACTER*6 APOS
      REAL COEF(3,5,2), WMIN(5,2), WMAX(5,2)
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
      INTEGER IDIN,COLIDS(10),ROW,NROWS,IPOS
      CHARACTER*80 CONTXT
      CHARACTER*15 COLNAM(10)
      CHARACTER*5 DET1
      CHARACTER*3 FGWA1,APER1
      CHARACTER*6 APOS1
      LOGICAL NULL(10)
      DATA COLNAM/'DETECTOR','FGWA_ID','APER_ID','APER_POS',
     *     'C0','C1','C2','WMIN','WMAX', 'REF_APER'/
C---------------------------------------------------------------------------
C
C Open table
C
      CALL UTTOPN(CCSB,RDONLY,IDIN,ISTAT)
      IF(ISTAT.NE.0)THEN
         CONTXT='ERROR opening CCSB table '//CCSB
         GO TO 998
      ENDIF
C
C get PEDIGREE and DESCRIP header keywords
C
      CALL UTHGTT(IDIN,'PEDIGREE',PEDGRE,ISTAT)
      CALL UTHGTT(IDIN,'DESCRIP', DESCRP,ISTAT)
C
C get number of rows
C
      CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
      IF(ISTAT.NE.0)THEN
         CONTXT='ERROR reading CCSB table '//CCSB
         GO TO 999
      ENDIF
C
C Get column ids.
C
      CALL UTCFND(IDIN,COLNAM,10,COLIDS,ISTAT)
      IF(ISTAT.NE.0)THEN
         CONTXT='ERROR locating needed columns in CCSB table '//
     *        CCSB
         GO TO 999
      ENDIF
C
C Loop on rows
C
      FOUND = 0
      DO 500 ROW=1,NROWS
         CONTXT='ERROR reading CCSB table '//CCSB
C
C check detector
C
         CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
         IF(ISTAT.NE.0)GO TO 999
         IF(DET1.EQ.DET)THEN
C
C Check fgwa_id
C
            CALL UTRGTT(IDIN,COLIDS(2),1,ROW,FGWA1,NULL,ISTAT)
            IF(ISTAT.NE.0)GO TO 999
            IF(FGWA1.EQ.FGWA)THEN
C
C Check aper_id
C
               CALL UTRGTT(IDIN,COLIDS(3),1,ROW,APER1,NULL,ISTAT)
               IF(ISTAT.NE.0)GO TO 999
               IF(APER1.EQ.APER)THEN
C
C Check aper_pos
C
                  CALL UTRGTT(IDIN,COLIDS(4),1,ROW,APOS1,NULL,ISTAT)
                  IF(ISTAT.NE.0)GO TO 999
                  IF(APOS1.EQ.APOS)THEN
C
C Found an entry
C     
                     FOUND = FOUND + 1
                     IF (FOUND .GT. 5) THEN
                        WRITE(CONTXT,199)DET,FGWA,APER,APOS
 199                    FORMAT('ERROR: More than 5 rows found',
     *                       ' in CCSB for ',A5,1X,A3,1X,A3)
                        GO TO 999
                     ENDIF
C
C Determine aperture position for paired aperture
C
                     IPOS=1
                     IF(APOS1.EQ.'UPPER')IPOS=2
C
C read coefficient values
C
                     CALL UTRGTR(IDIN,COLIDS(5),3,ROW,
     *                    COEF(1,FOUND,IPOS),NULL,ISTAT)
                     IF(ISTAT.NE.0) GO TO 999
C
C read wmin/wmax values
C
                     CALL UTRGTR(IDIN,COLIDS(8),1,ROW,
     *                    WMIN(FOUND,IPOS),NULL,ISTAT)
                     IF(ISTAT.NE.0) GO TO 999
                     CALL UTRGTR(IDIN,COLIDS(9),1,ROW,
     *                    WMAX(FOUND,IPOS),NULL,ISTAT)
                     IF(ISTAT.NE.0) GO TO 999
c
c read reference aperture name
C
                     CALL UTRGTT(IDIN,COLIDS(10),1,ROW,
     *                    REFAPR,NULL,ISTAT)
                     IF(ISTAT.NE.0) GO TO 999
C
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
 500  CONTINUE
      CALL UTTCLO(IDIN,ISTAT)
C
C Check to see if at least one row found
C
      IF (FOUND.EQ.0) THEN
         WRITE(CONTXT,99)DET,FGWA,APER,APOS
 99      FORMAT('WARNING: No rows found in CCSB for ',A5,1X,A3,1X,A3,
     *        1X,A6)
         GO TO 996
      ENDIF
C
C That's all folks
C
      ISTAT=0
      GO TO 1000
 996  CALL UTTCLO(IDIN,ISTAT)
 997  CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      DESCRP=CONTXT
      PEDGRE='DUMMY'
      ISTAT=0
      GO TO 1000
 999  CALL UTTCLO(IDIN,ISTAT)
 998  CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT=1
 1000 RETURN
      END
