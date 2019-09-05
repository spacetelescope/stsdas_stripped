        SUBROUTINE YRCCSC(CCSC,DET,APER,FOCUS,WAV,THRPT,NWV,PEDGRE,
     *			  DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCSC
*
*  Keyphrase:
*  ----------
*       read focus correction table
*
*  Description:
*  ------------
*       This routine reads the table that contains relative aperture
*	throughputs as a function of wavelength and focus.
*
*  FORTRAN name: YRCCSC.for
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
*       2       Sep 95   J. Eisenhamer   Modified so as to not error, but
*                                        set to DUMMY.
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCSC - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       APER - aperture
*	FOCUS - focus value for observation
*
* Output parameters
*	WAV   - wavelength array
*	THRPT - throughput array
*	NWV - number of wavelength entries found for DET/APER combo
*	PEDGRE - CCSC PEDIGREE keyword
*	DESCRP - CCSC DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
	IMPLICIT NONE
C
        INTEGER NWV, ISTAT
        CHARACTER*64 CCSC
	CHARACTER*68 PEDGRE, DESCRP
        CHARACTER*5 DET
        CHARACTER*3 APER
	DOUBLE PRECISION FOCUS, WAV(25), THRPT(25)
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
        CHARACTER*3 APER1
        LOGICAL FNDWV, NULL(5)
	REAL COEF(3)
	INTEGER TWAV, TFOC1, TFOC2, TTHR1, TTHR2
	DOUBLE PRECISION THR, C1(2), C2(2)
        DATA COLNAM/'DETECTOR','APER_ID','WAVELENGTH','FOCUS',
     *              'THROUGHPUT'/
C---------------------------------------------------------------------------
C
C Allocate dynamic memory
C
        CALL UDMGET(25, TYREAL, TWAV , ISTAT)
        CALL UDMGET(25, TYREAL, TFOC1, ISTAT)
        CALL UDMGET(25, TYREAL, TFOC2, ISTAT)
        CALL UDMGET(25, TYREAL, TTHR1, ISTAT)
        CALL UDMGET(25, TYREAL, TTHR2, ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='Error allocating dynamic memory'
           GO TO 998
        ENDIF
C
        DO 10 I=1,25
           MEMR(TWAV +I-1)=0.0
           MEMR(TFOC1+I-1)=0.0
           MEMR(TFOC2+I-1)=0.0
           MEMR(TTHR1+I-1)=0.0
           MEMR(TTHR2+I-1)=0.0
10      CONTINUE
C
C Open table
C
        CALL UTTOPN(CCSC,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCSC table '//CCSC
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
                CONTXT='ERROR reading CCSC table '//CCSC
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,5,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCSC table '//
     *                  CCSC
                GO TO 999
        ENDIF
C
C Loop on rows
C
	NWV=0
        DO 500 ROW=1,NROWS
           CONTXT='ERROR reading CCSC table '//CCSC
C
C check detector
C
           CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
	   IF(DET1.EQ.DET)THEN
C
C Check aper_id
C
              CALL UTRGTT(IDIN,COLIDS(2),1,ROW,APER1,NULL,ISTAT)
              IF(ISTAT.NE.0)GO TO 999
	      IF(APER1.EQ.APER)THEN
C
C Found an entry; read coefficient values.
C
                 CALL UTRGTR(IDIN,COLIDS(3),3,ROW,COEF,NULL,ISTAT)
                 IF(ISTAT.NE.0) GO TO 999
C
C loop over wavelengths already found to determine where these data belong
C
		 FNDWV = .FALSE.
		 DO 100 IWV=1,25
		    IF(COEF(1).EQ.MEMR(TWAV+IWV-1))THEN
		       FNDWV = .TRUE.
		       IF(COEF(2).LE.FOCUS)THEN
			  IF(MEMR(TTHR1+IWV-1).EQ.0)THEN
			     MEMR(TFOC1+IWV-1)=COEF(2)
			     MEMR(TTHR1+IWV-1)=COEF(3)
			  ELSE IF ((FOCUS-COEF(2)) .LT.
     *                             (FOCUS-MEMR(TFOC1+IWV-1)))THEN
			     MEMR(TFOC1+IWV-1)=COEF(2)
			     MEMR(TTHR1+IWV-1)=COEF(3)
			  ENDIF
		       ELSE IF(COEF(2).GT.FOCUS)THEN
			  IF(MEMR(TTHR2+IWV-1).EQ.0)THEN
			     MEMR(TFOC2+IWV-1)=COEF(2)
			     MEMR(TTHR2+IWV-1)=COEF(3)
			  ELSE IF ((COEF(2)-FOCUS) .LT.
     *                             (MEMR(TFOC2+IWV-1)-FOCUS))THEN
			     MEMR(TFOC2+IWV-1)=COEF(2)
			     MEMR(TTHR2+IWV-1)=COEF(3)
			  ENDIF
		       ENDIF
		    ENDIF
100		 CONTINUE
C
C store these data under a new wavelength value
C
		 IF(.NOT.FNDWV)THEN
		    NWV = NWV + 1
		    IF (NWV .GT. 25) THEN
            	        WRITE(CONTXT,199)DET,APER
199                     FORMAT(
     * 'ERROR: More than 25 wavelengths found in CCSC for ',A5,1X,A3)
            	        GO TO 999
        	    ENDIF
		    MEMR(TWAV+NWV-1) = COEF(1)
		    IF(COEF(2).LE.FOCUS)THEN
		       MEMR(TFOC1+NWV-1)=COEF(2)
		       MEMR(TTHR1+NWV-1)=COEF(3)
		    ELSE
		       MEMR(TFOC2+NWV-1)=COEF(2)
		       MEMR(TTHR2+NWV-1)=COEF(3)
		    ENDIF
		 ENDIF
	      ENDIF
	   ENDIF
500     CONTINUE
        CALL UTTCLO(IDIN,ISTAT)
C
C Check to see if at least one wavelength entry was found
C
        IF (NWV.EQ.0) THEN
            WRITE(CONTXT,99)DET,APER
99          FORMAT('WARNING: No rows found in CCSC for ',A5,1X,A3)
            GO TO 996
        ENDIF
C
C Interpolate in focus to desired value at each wavelength
C
	DO 200 I=1,NWV
	   IF(MEMR(TTHR1+I-1).EQ.0)THEN
	      C1(1)=MEMR(TFOC2+I-1)
	      C2(1)=MEMR(TTHR2+I-1)
	      CALL ZLINTP(C1,C2,1,FOCUS,THR,1,ISTAT)
	   ELSE IF(MEMR(TTHR2+I-1).EQ.0)THEN
	      C1(1)=MEMR(TFOC1+I-1)
	      C2(1)=MEMR(TTHR1+I-1)
	      CALL ZLINTP(C1,C2,1,FOCUS,THR,1,ISTAT)
	   ELSE
	      C1(1)=MEMR(TFOC1+I-1)
	      C1(2)=MEMR(TFOC2+I-1)
	      C2(1)=MEMR(TTHR1+I-1)
	      C2(2)=MEMR(TTHR2+I-1)
	      CALL ZLINTP(C1,C2,2,FOCUS,THR,1,ISTAT)
	   ENDIF
	   WAV(I)=MEMR(TWAV+I-1)
	   THRPT(I)=THR
200	CONTINUE
C
C Deallocate dynamic memory
C
        IF(TWAV.NE.0) CALL UDMFRE(TWAV,  TYREAL, ISTAT)
        IF(TFOC1.NE.0)CALL UDMFRE(TFOC1, TYREAL, ISTAT)
        IF(TFOC2.NE.0)CALL UDMFRE(TFOC2, TYREAL, ISTAT)
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
