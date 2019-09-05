C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRCCS5(CCS5,DET,FGWA,APER,APOS,NXSTEP,NSHIFT,
     *                    PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS5
*
*  Keyphrase:
*  ----------
*       read sky shift table
*
*  Description:
*  ------------
*       This routine reads the table contianing the integer shifts for
*       the sky spectra.  Values are selected by detector, aperid, aper_pos,
*       fgwa_id, and nxsteps.
*
*  FORTRAN name: YRCCS5.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS5                    I       table containing CCS5 coeffecients
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
*       CCS5 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       FGWA - grating mode
*       APER - aperture
*       APOS - aperture position UPPER or LOWER
*       NXSTEP - number of x-steps
*
* Output parameters
*       NSHIFT - size of shift
*	PEDGRE - CCS5 PEDIGREE keyword
*	DESCRP - CCS5 DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT,NXSTEP
        CHARACTER*64 CCS5
	CHARACTER*68 PEDGRE, DESCRP
        CHARACTER*5 DET
        CHARACTER*6 APOS
        CHARACTER*3 FGWA,APER
        INTEGER NSHIFT
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
        INTEGER IDIN,COLIDS(6),ROW,NROWS
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(6)
        CHARACTER*5 DET1
        CHARACTER*6 APOS1
        CHARACTER*3 FGWA1,APER1
        INTEGER NX1
        LOGICAL NULL
        DATA COLNAM/'DETECTOR','APER_ID','APER_POS','FGWA_ID',
     *              'NXSTEPS','NSHIFT'/
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCS5,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS5 table '//CCS5
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
                CONTXT='ERROR reading CCS5 table '//CCS5
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,6,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS5 table '//
     *                  CCS5
                GO TO 999
        ENDIF
C
C Loop on rows
C
        DO 500 ROW=1,NROWS
C
C check detector
C
                CONTXT='ERROR reading CCS5 table '//CCS5
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(DET1.NE.DET) GO TO 500
C
C Check fgwa_id
C
                CALL UTRGTT(IDIN,COLIDS(4),1,ROW,FGWA1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(FGWA1.NE.FGWA) GO TO 500
C
C Check aper_id
C
                CALL UTRGTT(IDIN,COLIDS(2),1,ROW,APER1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(APER1.NE.APER) GO TO 500
C
C Check aper_pos
C
                CALL UTRGTT(IDIN,COLIDS(3),1,ROW,APOS1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(APOS.NE.APOS1) GO TO 500
C
C Check nxsteps
C
                CALL UTRGTI(IDIN,COLIDS(5),1,ROW,NX1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(NXSTEP.NE.NX1) GO TO 500
C
C read nshift
C
                CALL UTRGTI(IDIN,COLIDS(6),1,ROW,NSHIFT,NULL,ISTAT)
                IF(ISTAT.NE.0) GO TO 999
                GO TO 600
500     CONTINUE
C
C If we got here (no row found)
C
        WRITE(CONTXT,99)DET,FGWA,APER,APOS,NXSTEP
99      FORMAT('ERROR: No rows found in CCS5 for ',A5,1X,A3,1X,A3,1X,
     *             A6,' nxsteps=',I2)
        GO TO 999
600     CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
