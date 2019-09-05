        SUBROUTINE YWRPED(PFLAGS,REFFIL,PEDGRE,DESCRP,GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: ywrped
*
*  Keyphrase:
*  ----------
*       Write pedigree information
*
*  Description:
*  ------------
*       This routine writes reference file names and PEDIGREE info
*	to the end of the processing log (trailer file).
*
*  FORTRAN name: ywrped.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*              
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Mar 94  H. Bushouse     Designed and coded
*	2	Oct 94	H. Bushouse	Added APR, AIS, TIM_CORR ref. files
*       2.1     Mar 97  M. De La Pena   Added KYDPLY and REFFIL(27) checks
*-------------------------------------------------------------------------------
*
* INPUTS:
*       PFLAGS - processing flags
*	REFFIL - reference file names
*	PEDGRE - reference file PEDIGREE keywords
*	DESCRP - reference file DESCRIP keywords
*	GRNDMD - ground mode of observation
*
* OUTPUTS:
*       ISTAT - error status
*-------------------------------------------------------------------------
        INTEGER ISTAT
        CHARACTER*8 PFLAGS(*)
	CHARACTER*64 REFFIL(*)
	CHARACTER*68 PEDGRE(*), DESCRP(*)
	CHARACTER*18 GRNDMD
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        LOGICAL     KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        INTEGER NX,NOBJ,NSKY,NBCK
        COMMON /CONFG3/NX,NOBJ,NSKY,NBCK
	LOGICAL PAIRED
	REAL YUPPER, YLOWER
	COMMON /CCS1CM/ PAIRED,YUPPER,YLOWER
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C local variables
C
	LOGICAL FOUND
        INTEGER I
        CHARACTER*8 STEPS(15)
	CHARACTER*8 REFNAM(27)
        CHARACTER*80 CONTXT
C 
        DATA STEPS/'CNT_CORR','OFF_CORR','PPC_CORR','BAC_CORR',
     *             'SCT_CORR','FLT_CORR','SKY_CORR','WAV_CORR',
     *             'FLX_CORR','APR_CORR','AIS_CORR','TIM_CORR',
     *		   'ERR_CORR','MOD_CORR','GMF_CORR' /
	DATA REFNAM /'BACHFILE', 'FL1HFILE', 'FL2HFILE', 'IV1HFILE',
     *               'IV2HFILE', 'RETHFILE', 'DDTHFILE', 'DQ1HFILE',
     *               'DQ2HFILE', 'CCG2', 'CCS0', 'CCS1', 'CCS2',
     *               'CCS3', 'CCS4', 'CCS5', 'CCS6', 'CCS7', 
     *               'OFFS_TAB', 'CCS8', 'CCS9', 'CCSA', 'CCSB',
     *		     'CCSC', 'CCSD', 'AISHFILE', 'PCPHFILE' /
C
C------------------------------------------------------------------------------
	FOUND = .FALSE.
	CONTXT = 'Reference file PEDIGREE information:'
	CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Data quality initialization files
C
	CONTXT(35:52) = 'DQI INITIALIZATION'
	IF (PEDGRE(8).NE.' ') THEN
	    FOUND = .TRUE.
	    CONTXT(1:34) = REFNAM(8) // '=' // REFFIL(8)
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
	    CALL YMSPUT(PEDGRE(8),STDOUT,0,ISTAT)
	    IF (DESCRP(8).NE.' ')
     *		CALL YMSPUT(DESCRP(8),STDOUT,0,ISTAT)
	END IF
	IF (PAIRED .OR. POLID.NE.'C') THEN
	    IF (PEDGRE(9).NE.' ') THEN
	        FOUND = .TRUE.
		CONTXT(1:34) = REFNAM(9) // '=' // REFFIL(9)
	        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		CALL YMSPUT(PEDGRE(9),STDOUT,0,ISTAT)
		IF (DESCRP(9).NE.' ')
     *		    CALL YMSPUT(DESCRP(9),STDOUT,0,ISTAT)
	    END IF
	END IF
	CONTXT = ' '
C
	DO 100 I = 1, 15
	   IF (PFLAGS(I).EQ.'OMIT') GO TO 100
C
	   CONTXT(35:51) = STEPS(I) // '=' // PFLAGS(I)
	   IF (PFLAGS(I).EQ.'PERFORM') CONTXT(44:51)='COMPLETE'
C
C	Dead diode table
C
	   IF (I.EQ.1 .AND. PEDGRE(7).NE.' ') THEN
		CONTXT(1:34) = REFNAM(7) // '=' // REFFIL(7)
		FOUND = .TRUE.
		CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		CALL YMSPUT(PEDGRE(7),STDOUT,0,ISTAT)
		IF (DESCRP(7).NE.' ')
     *		    CALL YMSPUT(DESCRP(7),STDOUT,0,ISTAT)
C
C	GIMP correction tables
C
	   ELSE IF (I.EQ.2) THEN
		IF (REFFIL(19)(1:3).NE.'n/a' .AND.
     *		    REFFIL(19)(1:3).NE.'N/A') THEN 
		    IF (PEDGRE(19).NE.' ') THEN
			FOUND = .TRUE.
			CONTXT(1:34) = REFNAM(19) // '=' // REFFIL(19)
		        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
			CALL YMSPUT(PEDGRE(19),STDOUT,0,ISTAT)
			IF (DESCRP(19).NE.' ')
     *                      CALL YMSPUT(DESCRP(19),STDOUT,0,ISTAT)
		    END IF
		ELSE IF (PEDGRE(18).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(18) // '=' // REFFIL(18)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(18),STDOUT,0,ISTAT)
		    IF (DESCRP(18).NE.' ')
     *			CALL YMSPUT(DESCRP(18),STDOUT,0,ISTAT)
		END IF
C
C	Paired pulse correction table
C
	   ELSE IF (I.EQ.3 .AND. PEDGRE(10).NE.' ') THEN
		FOUND = .TRUE.
		CONTXT(1:34) = REFNAM(10) // '=' // REFFIL(10)
		CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		CALL YMSPUT(PEDGRE(10),STDOUT,0,ISTAT)
		IF (DESCRP(10).NE.' ')
     *		    CALL YMSPUT(DESCRP(10),STDOUT,0,ISTAT)
C
C	Background subtraction and scaling files and table
C
	   ELSE IF (I.EQ.4) THEN
		IF (NBCK.EQ.0) THEN
		    IF (PEDGRE(1).NE.' ') THEN
			FOUND = .TRUE.
			CONTXT(1:34) = REFNAM(1) // '=' // REFFIL(1)
			CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
			CALL YMSPUT(PEDGRE(1),STDOUT,0,ISTAT)
			IF (DESCRP(1).NE.' ')
     *			    CALL YMSPUT(DESCRP(1),STDOUT,0,ISTAT)
		    END IF
		    IF (PFLAGS(15).NE.'OMIT' .AND. 
     *			PEDGRE(20).NE.' ') THEN
			FOUND = .TRUE.
			CONTXT(1:34) = REFNAM(20) // '=' // REFFIL(20)
			CONTXT(35:51) = STEPS(15) // '=' // PFLAGS(15)
			IF(PFLAGS(15).EQ.'PERFORM')
     *			   CONTXT(44:51)='COMPLETE'
			CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
			CALL YMSPUT(PEDGRE(20),STDOUT,0,ISTAT)
			IF (DESCRP(20).NE.' ')
     *			    CALL YMSPUT(DESCRP(20),STDOUT,0,ISTAT)
		    END IF
		ELSE IF (PEDGRE(14).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(14) // '=' // REFFIL(14)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(14),STDOUT,0,ISTAT)
		    IF (DESCRP(14).NE.' ')
     *			CALL YMSPUT(DESCRP(14),STDOUT,0,ISTAT)
		END IF
C
C	Scattered light correction table
C
	   ELSE IF (I.EQ.5 .AND. PEDGRE(21).NE.' ') THEN
		FOUND = .TRUE.
		CONTXT(1:34) = REFNAM(21) // '=' // REFFIL(21)
		CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		CALL YMSPUT(PEDGRE(21),STDOUT,0,ISTAT)
		IF (DESCRP(21).NE.' ')
     *		    CALL YMSPUT(DESCRP(21),STDOUT,0,ISTAT)
C
C	Flat field correction file(s)
C
	   ELSE IF (I.EQ.6) THEN
		IF (PEDGRE(2).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(2) // '=' // REFFIL(2)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(2),STDOUT,0,ISTAT)
		    IF (DESCRP(2).NE.' ')
     *			CALL YMSPUT(DESCRP(2),STDOUT,0,ISTAT)
		END IF
		IF (PAIRED .OR. POLID.NE.'C') THEN
		    IF (PEDGRE(3).NE.' ') THEN
			FOUND = .TRUE.
			CONTXT(1:34) = REFNAM(3) // '=' // REFFIL(3)
			CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
			CALL YMSPUT(PEDGRE(3),STDOUT,0,ISTAT)
			IF (DESCRP(3).NE.' ')
     *			    CALL YMSPUT(DESCRP(3),STDOUT,0,ISTAT)    
		    END IF
		END IF
C
C	Sky subtraction tables
C
	   ELSE IF (I.EQ.7) THEN
		IF (NSKY.EQ.0) GO TO 100
		IF (PEDGRE(11).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(11) // '=' // REFFIL(11)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(11),STDOUT,0,ISTAT)
		    IF (DESCRP(11).NE.' ')
     *			CALL YMSPUT(DESCRP(11),STDOUT,0,ISTAT)
		END IF
		IF (PEDGRE(13).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(13) // '=' // REFFIL(13)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(13),STDOUT,0,ISTAT)
		    IF (DESCRP(13).NE.' ')
     *			CALL YMSPUT(DESCRP(13),STDOUT,0,ISTAT)
		END IF
		IF (PEDGRE(14).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(14) // '=' // REFFIL(14)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(14),STDOUT,0,ISTAT)
		    IF (DESCRP(14).NE.' ')
     *			CALL YMSPUT(DESCRP(14),STDOUT,0,ISTAT)
		END IF
		IF (PEDGRE(16).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(16) // '=' // REFFIL(16)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(16),STDOUT,0,ISTAT)
		    IF (DESCRP(16).NE.' ')
     *			CALL YMSPUT(DESCRP(16),STDOUT,0,ISTAT)
		END IF
C
C	Wavelength coefficient table
C
	   ELSE IF (I.EQ.8 .AND. PEDGRE(17).NE.' ') THEN
		FOUND = .TRUE.
		CONTXT(1:34) = REFNAM(17) // '=' // REFFIL(17)
		CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		CALL YMSPUT(PEDGRE(17),STDOUT,0,ISTAT)
		IF (DESCRP(17).NE.' ')
     *		    CALL YMSPUT(DESCRP(17),STDOUT,0,ISTAT)
C
C	Inverse sensitivity file(s) (old method)
C
	   ELSE IF (I.EQ.9) THEN
		IF (PEDGRE(4).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(4) // '=' // REFFIL(4)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(4),STDOUT,0,ISTAT)
		    IF (DESCRP(4).NE.' ')
     *			CALL YMSPUT(DESCRP(4),STDOUT,0,ISTAT)
		END IF
		IF (PAIRED .OR. POLID.NE.'C') THEN
		    IF (PEDGRE(5).NE.' ') THEN
			FOUND = .TRUE.
			CONTXT(1:34) = REFNAM(5) // '=' // REFFIL(5)
			CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
			CALL YMSPUT(PEDGRE(5),STDOUT,0,ISTAT)
			IF (DESCRP(5).NE.' ')
     *			    CALL YMSPUT(DESCRP(5),STDOUT,0,ISTAT)
		    END IF
		END IF
C
C	Aperture throughput and focus correction tables
C
	   ELSE IF (I.EQ.10) THEN
		IF (PEDGRE(22).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(22) // '=' // REFFIL(22)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(22),STDOUT,0,ISTAT)
		    IF (DESCRP(22).NE.' ')
     *			CALL YMSPUT(DESCRP(22),STDOUT,0,ISTAT)
		END IF
		IF (PEDGRE(23).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(23) // '=' // REFFIL(23)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(23),STDOUT,0,ISTAT)
		    IF (DESCRP(23).NE.' ')
     *			CALL YMSPUT(DESCRP(23),STDOUT,0,ISTAT)
		END IF
		IF (PEDGRE(24).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(24) // '=' // REFFIL(24)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(24),STDOUT,0,ISTAT)
		    IF (DESCRP(24).NE.' ')
     *			CALL YMSPUT(DESCRP(24),STDOUT,0,ISTAT)
		END IF
C
C	Inverse sensitivity file (new method)
C
	   ELSE IF (I.EQ.11) THEN
		IF (PEDGRE(26).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(26) // '=' // REFFIL(26)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(26),STDOUT,0,ISTAT)
		    IF (DESCRP(26).NE.' ')
     *			CALL YMSPUT(DESCRP(26),STDOUT,0,ISTAT)
		END IF
C
C	Sensitivity degradation (time) correction table
C
	   ELSE IF (I.EQ.12) THEN
		IF (PEDGRE(25).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(25) // '=' // REFFIL(25)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(25),STDOUT,0,ISTAT)
		    IF (DESCRP(25).NE.' ')
     *			CALL YMSPUT(DESCRP(25),STDOUT,0,ISTAT)
		END IF
C
C	Statistical error calculation (no reference file/table)
C
C	   ELSE IF (I.EQ.13) THEN
C
C	Special mode processing files
C
	   ELSE IF (I.EQ.14 .AND. GRNDMD.EQ.'SPECTROPOLARIMETRY') THEN
		IF (PEDGRE(6).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(6) // '=' // REFFIL(6)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(6),STDOUT,0,ISTAT)
		    IF (DESCRP(6).NE.' ')
     *			CALL YMSPUT(DESCRP(6),STDOUT,0,ISTAT)
		END IF
		IF (PEDGRE(15).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(15) // '=' // REFFIL(15)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(15),STDOUT,0,ISTAT)
		    IF (DESCRP(15).NE.' ')
     *			CALL YMSPUT(DESCRP(15),STDOUT,0,ISTAT)
		END IF
		IF (PEDGRE(27).NE.' ') THEN
		    FOUND = .TRUE.
		    CONTXT(1:34) = REFNAM(27) // '=' // REFFIL(27)
		    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		    CALL YMSPUT(PEDGRE(27),STDOUT,0,ISTAT)
		    IF (DESCRP(27).NE.' ')
     *			CALL YMSPUT(DESCRP(27),STDOUT,0,ISTAT)
		END IF
	   END IF
100	CONTINUE
C
	IF (.NOT.FOUND) THEN
	    CONTXT = '   No information available'
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
	END IF
C
        ISTAT = 0
1000    RETURN
        END
