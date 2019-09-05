C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRCCSD(CCSD,DET,FGWA,MJD,WAV,THRPT,NWV,PEDGRE,
     *			  DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCSD
*
*  Keyphrase:
*  ----------
*       read sensitivity time correction table
*
*  Description:
*  ------------
*       This routine reads the table that contains coefficients 
*	describing sensitivity changes as a function of wavelength and time.
*
*  FORTRAN name: yrccsd.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCSD                    I       table containing CCSD coeffecients
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
*       CCSD - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       FGWA - grating ID (PRI, CAM, L15, L65, H13, H19, ...)
*	MJD  - date of observation
*
* Output parameters
*	WAV   - wavelength array
*	THRPT - throughput array
*	NWV - number of wavelength entries found for DET/FGWA combo
*	PEDGRE - CCSD PEDIGREE keyword
*	DESCRP - CCSD DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
	IMPLICIT NONE
C
        INTEGER NWV, ISTAT
        CHARACTER*64 CCSD
	CHARACTER*68 PEDGRE, DESCRP
        CHARACTER*5 DET
        CHARACTER*3 FGWA
	DOUBLE PRECISION MJD, WAV(100), THRPT(100)
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
C Get IRAF MEM common into main program
C
      LOGICAL          MEMB(1)
      INTEGER*2        MEMS(1)
      INTEGER*4        MEMI(1)
      INTEGER*4        MEML(1)
      REAL             MEMR(1)
      DOUBLE PRECISION MEMD(1)
      COMPLEX          MEMX(1)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C
      INTEGER TYREAL
      PARAMETER (TYREAL=6)
C------------------------------------------------------------------------------
C
C local variables
C
        INTEGER IDIN,COLIDS(5),ROW,NROWS,I,IWV
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(5)
        CHARACTER*5 DET1
        CHARACTER*3 FGWA1
        LOGICAL FNDWV, NULL(5)
	REAL COEF(3)
	INTEGER TWAV, TMJD1, TMJD2, TTHR1, TTHR2
	DOUBLE PRECISION THR, C1(2), C2(2)
        DATA COLNAM/'DETECTOR','FGWA_ID','WAVELENGTH','MJD',
     *              'CORRECTION'/
C---------------------------------------------------------------------------
C
C Allocate dynamic memory
C
	CALL UDMGET(100, TYREAL, TWAV , ISTAT)
	CALL UDMGET(100, TYREAL, TMJD1, ISTAT)
	CALL UDMGET(100, TYREAL, TMJD2, ISTAT)
	CALL UDMGET(100, TYREAL, TTHR1, ISTAT)
	CALL UDMGET(100, TYREAL, TTHR2, ISTAT)
	IF(ISTAT.NE.0)THEN
	   CONTXT='Error allocating dynamic memory'
	   GO TO 998
	ENDIF
C
	DO 10 I=1,100
   	   MEMR(TWAV +I-1)=0.0
   	   MEMR(TMJD1+I-1)=0.0
   	   MEMR(TMJD2+I-1)=0.0
   	   MEMR(TTHR1+I-1)=0.0
   	   MEMR(TTHR2+I-1)=0.0
10	CONTINUE
C
C Open table
C
        CALL UTTOPN(CCSD,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR opening CCSD table '//CCSD
           GO TO 998
        ENDIF
C
C get PEDIGREE and DESCRIP header keywords
C
        CALL UTHGTT(IDIN,'PEDIGREE',PEDGRE,ISTAT)
        CALL UTHGTT(IDIN,'DESCRIP' ,DESCRP,ISTAT)
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR reading CCSD table '//CCSD
           GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,5,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR locating needed columns in CCSD table '//
     *             CCSD
           GO TO 999
        ENDIF
C
C Loop on rows
C
	NWV=0
        DO 500 ROW=1,NROWS
           CONTXT='ERROR reading CCSD table '//CCSD
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
C Found an entry; read coefficient values.
C
                 CALL UTRGTR(IDIN,COLIDS(3),3,ROW,COEF,NULL,ISTAT)
                 IF(ISTAT.NE.0) GO TO 999
C
C loop over wavelengths already found to determine where these data belong
C
		 FNDWV = .FALSE.
		 DO 100 IWV=1,100
		    IF(COEF(1).EQ.MEMR(TWAV+IWV-1))THEN
		       FNDWV = .TRUE.
		       IF(COEF(2).LE.MJD)THEN
			  IF(MEMR(TTHR1+IWV-1).EQ.0)THEN
			     MEMR(TMJD1+IWV-1)=COEF(2)
			     MEMR(TTHR1+IWV-1)=COEF(3)
			  ELSE IF ((MJD-COEF(2)) .LT.
     *				   (MJD-MEMR(TMJD1+IWV-1)))THEN
			     MEMR(TMJD1+IWV-1)=COEF(2)
			     MEMR(TTHR1+IWV-1)=COEF(3)
			  ENDIF
		       ELSE IF(COEF(2).GT.MJD)THEN
			  IF(MEMR(TTHR2+IWV-1).EQ.0)THEN
			     MEMR(TMJD2+IWV-1)=COEF(2)
			     MEMR(TTHR2+IWV-1)=COEF(3)
			  ELSE IF ((COEF(2)-MJD) .LT.
     *				   (MEMR(TMJD2+IWV-1)-MJD))THEN
			     MEMR(TMJD2+IWV-1)=COEF(2)
			     MEMR(TTHR2+IWV-1)=COEF(3)
			  ENDIF
		       ENDIF
		    ENDIF
100		 CONTINUE
C
C store these data under a new wavlength value
C
                 IF(.NOT.FNDWV)THEN
                    NWV = NWV + 1
                    IF (NWV .GT. 100) THEN
                        WRITE(CONTXT,199)DET,FGWA
199                     FORMAT(
     * 'ERROR: More than 100 wavelengths found in CCSD for ',A5,1X,A3)
                        GO TO 999
                    ENDIF
                    MEMR(TWAV+NWV-1) = COEF(1)
                    IF(COEF(2).LE.MJD)THEN
                       MEMR(TMJD1+NWV-1)=COEF(2)
                       MEMR(TTHR1+NWV-1)=COEF(3)
                    ELSE
                       MEMR(TMJD2+NWV-1)=COEF(2)
                       MEMR(TTHR2+NWV-1)=COEF(3)
                    ENDIF
                 ENDIF
	      ENDIF
	   ENDIF
500     CONTINUE
        CALL UTTCLO(IDIN,ISTAT)
C
C Check to see if at least one wavelength entry found
C
        IF (NWV.EQ.0) THEN
            WRITE(CONTXT,99)DET,FGWA
99          FORMAT('WARNING: No rows found in CCSD for ',A5,1X,A3)
            GO TO 996
        ENDIF
C
C Interpolate in time to desired value
C
	DO 200 I=1,NWV
C
C If date of obs is earlier than first table value, don't extrapolate
C
	   IF(MEMR(TTHR1+I-1).EQ.0)THEN
	       THR=MEMR(TTHR2+I-1)
	   ELSE IF(MEMR(TTHR2+I-1).EQ.0)THEN
	       THR=MEMR(TTHR1+I-1)
	   ELSE
	       C1(1)=MEMR(TMJD1+I-1)
	       C1(2)=MEMR(TMJD2+I-1)
	       C2(1)=MEMR(TTHR1+I-1)
	       C2(2)=MEMR(TTHR2+I-1)
	       CALL ZLINTP(C1,C2,2,MJD,THR,1,ISTAT)
	   ENDIF
	   WAV(I)=MEMR(TWAV+I-1)
	   THRPT(I)=THR
200	CONTINUE
C
C Deallocate dynamic memory
C
	IF(TWAV.NE.0) CALL UDMFRE(TWAV,  TYREAL, ISTAT)
	IF(TMJD1.NE.0)CALL UDMFRE(TMJD1, TYREAL, ISTAT)
	IF(TMJD2.NE.0)CALL UDMFRE(TMJD2, TYREAL, ISTAT)
	IF(TTHR1.NE.0)CALL UDMFRE(TTHR1, TYREAL, ISTAT)
	IF(TTHR2.NE.0)CALL UDMFRE(TTHR2, TYREAL, ISTAT)
	IF(ISTAT.NE.0)THEN
	   CONTXT='Error deallocating dynamic memory'
	   GO TO 998
	ENDIF
C
C That's all folks
C
        ISTAT=0
        GO TO 1000
 996    CALL UTTCLO(IDIN,ISTAT)
 997    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        DESCRP=CONTXT
        PEDGRE='DUMMY'
        ISTAT=0
        GO TO 1000
 999    CALL UTTCLO(IDIN,ISTAT)
 998    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
 1000   RETURN
        END
