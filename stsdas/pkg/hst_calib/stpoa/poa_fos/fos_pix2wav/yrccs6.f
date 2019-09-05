C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRCCS6(CCS6,DET,FGWA,APER,POLID,PAIRED,
     *                    COEF,XZERO,FOUND,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS6
*
*  Keyphrase:
*  ----------
*       read wavelength coefficient table
*
*  Description:
*  ------------
*       This routine reads the wavelength coefficient table.
*       If it is a polarizer mode or a paired aperture, two sets
*       of coefficients are returned. An logical vector found
*       is set to .True. for the sets of coefficients found.
*
*  FORTRAN name: YRCCS6.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS6                    I       table containing CCS6 coeffecients
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
*
*     /////////////////////////////////////////////////////////////////////
*            Start of Post Operational Archive versions (poa_calfos)
*     /////////////////////////////////////////////////////////////////////
*     1.1       Mar 01  A. Alexov         New ccs6 file format - 10 coeffs
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCS6 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       FGWA - grating mode
*       APER - aperture
*       POLID - polarizer id
*       PAIRED - logical flag set to .true. for paired aperture
*
* Output parameters
*       COEF - wavelength coefficients
*       XZERO - addition coef. for prisms
*       FOUND - Logical vector to flag found coeff.
*	PEDGRE - CCS6 PEDIGREE keyword
*	DESCRP - CCS6 DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT
        CHARACTER*64 CCS6
	CHARACTER*68 PEDGRE, DESCRP
        CHARACTER*5 DET
        CHARACTER*3 FGWA,APER
        CHARACTER*1 POLID
        LOGICAL PAIRED,FOUND(2)
        DOUBLE PRECISION COEF(10,2),XZERO(2)
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
        INTEGER IDIN,COLIDS(16),ROW,NROWS,IPOS
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(16)
        CHARACTER*5 DET1,APOS
        INTEGER PASS
        CHARACTER*3 FGWA1,APER1
        CHARACTER*1 POLID1
        LOGICAL NULL(16)
        DATA COLNAM/'DETECTOR','APER_ID','APER_POS','FGWA_ID',
     *              'POLAR_ID','PASS_DIR','COEFF_0','COEFF_1',
     *              'COEFF_2','COEFF_3','COEFF_4','XZERO', 
     *              'COEFF_5','COEFF_6','COEFF_7','COEFF_8'/
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCS6,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS6 table '//CCS6
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
                CONTXT='ERROR reading CCS6 table '//CCS6
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,16,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS6 table '//
     *                  CCS6
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
                CONTXT='ERROR reading CCS6 table '//CCS6
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
C Check polar-id
C
                CALL UTRGTT(IDIN,COLIDS(5),1,ROW,POLID1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(POLID1.NE.POLID) GO TO 500
C
C if polarizer mode, determine passdir
C
                IPOS = 1
                IF(POLID.NE.'C')THEN
                    CALL UTRGTI(IDIN,COLIDS(6),1,ROW,PASS,NULL,ISTAT)
                    IF(ISTAT.NE.0)GO TO 999
                    IF((PASS.GT.2).OR.(PASS.LT.1)) GO TO 500
                    IPOS = PASS
                ENDIF
C
C If paired aperture determine if upper or lower
C
                IF(PAIRED)THEN
                    CALL UTRGTT(IDIN,COLIDS(3),1,ROW,APOS,NULL,ISTAT)
                    IF(ISTAT.NE.0)GO TO 999
                    IPOS=0
                    IF(APOS.EQ.'UPPER')IPOS=1
                    IF(APOS.EQ.'LOWER')IPOS=2
                    IF(IPOS.EQ.0)GO TO 500
                ENDIF
C
C read values
C
                FOUND(IPOS)=.TRUE.
                CALL UTRGTD(IDIN,COLIDS(7),10,ROW,COEF(1,IPOS),NULL,
     *                  ISTAT)
                IF(ISTAT.NE.0) GO TO 999
                IF(FGWA.EQ.'PRI')THEN
                    CALL UTRGTD(IDIN,COLIDS(12),1,ROW,XZERO(IPOS),
     *                  NULL,ISTAT)
                    IF(ISTAT.NE.0)GO TO 999
                ENDIF
500     CONTINUE
        CALL UTTCLO(IDIN,ISTAT)
C
C Check to see if at least one row found
C
        IF((.NOT.FOUND(1)).AND.(.NOT.FOUND(2)))THEN
            WRITE(CONTXT,99)DET,FGWA,APER,POLID
99          FORMAT('ERROR: No rows found in CCS6 for ',A5,1X,A3,1X,
     *             A3,' polar_id= ',A1)
            GO TO 999
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
