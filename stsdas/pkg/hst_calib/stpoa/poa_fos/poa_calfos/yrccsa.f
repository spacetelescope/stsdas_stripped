C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRCCSA(CCSA,MJD,FOCUS,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCSA
*
*  Keyphrase:
*  ----------
*       read OTA focus history table
*
*  Description:
*  ------------
*       This routine reads the focus history table (CCSA) and
*	computes a focus value at the MJD of the observation
*	by linear interpolation within the tabulated values.
*
*  FORTRAN name: YRCCSA.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCSA                    I       table containing focus values
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
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCSA - table name
*	MJD  - date of observation
*
* Output parameters
*
*       FOCUS - interpolated focus value for MJD of observation
*	PEDGRE - CCSA PEDIGREE keyword
*	DESCRP - CCSA DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
	IMPLICIT NONE
C
        INTEGER ISTAT
        CHARACTER*64 CCSA
	CHARACTER*68 PEDGRE, DESCRP
	DOUBLE PRECISION MJD, FOCUS
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
C------------------------------------------------------------------------------
C Get IRAF MEM common into main program.
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
      INTEGER TYDOUB
      PARAMETER (TYDOUB=7)
C------------------------------------------------------------------------------
C
C local variables
C
        INTEGER IDIN,COLIDS(2),ROW,NROWS,NFNDA,NFNDB
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(2)
        LOGICAL NULL(2)
	DOUBLE PRECISION TMJD(2),TFOC(2),COEF(2)
        DATA COLNAM/'MJD','FOCUS'/
C---------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCSA,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCSA table '//CCSA
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
                CONTXT='ERROR reading CCSA table '//CCSA
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,2,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCSA table '//
     *                  CCSA
                GO TO 999
        ENDIF
C
	DO 400 ROW=1,2
	   TMJD(ROW)=0.0d0
	   TFOC(ROW)=0.0d0
400	CONTINUE
C
C Loop on rows
C
	NFNDB = 0
	NFNDA = 0
        DO 500 ROW=1,NROWS
C
           CONTXT='ERROR reading CCSA table '//CCSA
C
           CALL UTRGTD(IDIN,COLIDS,2,ROW,COEF,NULL,ISTAT)
           IF(ISTAT.NE.0)GO TO 999
C
C Find the two MJD/FOCUS entries that straddle the MJD of the observation
C
	   IF(COEF(1).LE.MJD)THEN
	      IF(NFNDB.EQ.0)THEN
		 TMJD(1)=COEF(1)
		 TFOC(1)=COEF(2)
		 NFNDB = 1
	      ELSE IF((MJD-COEF(1)).LT.(MJD-TMJD(1)))THEN
		 TMJD(1)=COEF(1)
		 TFOC(1)=COEF(2)
	      ENDIF
	   ELSE IF(COEF(1).GT.MJD)THEN
	      IF(NFNDA.EQ.0)THEN
		 TMJD(2)=COEF(1)
		 TFOC(2)=COEF(2)
		 NFNDA = 1
	      ELSE IF((COEF(1)-MJD).LT.(TMJD(2)-MJD))THEN
		 TMJD(2)=COEF(1)
		 TFOC(2)=COEF(2)
	      ENDIF
	   ENDIF
500	CONTINUE
        CALL UTTCLO(IDIN,ISTAT)
C
C Find focus for observation MJD by linear interpolation
C
	IF(NFNDB.EQ.0)THEN
	   FOCUS=TFOC(2)
	ELSE IF(NFNDA.EQ.0)THEN
	   FOCUS=TFOC(1)
	ELSE
	   CALL ZLINTP(TMJD,TFOC,2,MJD,FOCUS,1,ISTAT)
	ENDIF
C
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
