C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YUHEAD(ID,IDNUM,PFLAGS,ISTAT)
*
*  Module number:
*
*  Module name: yuhead
*
*  Keyphrase:
*  ----------
*       Update output file header
*
*  Description:
*  ------------
*       This routine marks completed calibration steps, the filetype,
*	and the brightness units in the output header.
*
*  FORTRAN name: yuhead.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       id                      O       output file
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uhdpst uhdpsr uhdahs
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Aug 89  D. Lindler      Designed and coded
*		May 90	S. Hulbert	PANGAPER (new/old data format)
*		Sep 90	S. Hulbert	update BUNIT
*	1.3	Mar 90	S. Hulbert	Check that UDL exists if updating
*					select header keywords
*	1.4	May 91	S. Hulbert	reprocessing header format
*	1.5	Sep 91	S. Hulbert	Update DEFDDT if necessary and GMF_CORR
*	1.6	Nov 91	S. Hulbert	Don't need to update DEFDDT anymore
*	1.7	Apr 93	H. Bushouse	Changed declaration of PFLAG(1) to (*);
*					Changed declaration of QUAL and QUALS
*					from char*3 to char*4.
*	2	Mar 94	H. Bushouse	Update PFLAGS indexes and STEPS and
*					LAST lists for SCT_CORR
*	3	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*	4	Oct 94	H. Bushouse	Update PFLAGS indexes and STEPS and
*					LAST lists for APR_CORR, AIS_CORR,
*					and TIM_CORR.
*       4.1     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* INPUTS:
*       ID - file id
*       IDNUM - number of the output file
*       PFLAGS - processing flags
*
* OUTPUTS:
*       ISTAT - error status
*-------------------------------------------------------------------------
        INTEGER ID,IDNUM,ISTAT
        CHARACTER*8 PFLAGS(*)
C
C Common block containing input/output file descriptions
C
C       IDS - file id numbers
C       GCOUNT - group counts
C       NAXIS - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL - Fill values
C
        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME,HEADER,TRAILR,
     *          DEFDDT
        INTEGER NX,NOBJ,NSKY,NBCK
        COMMON /CONFG3/NX,NOBJ,NSKY,NBCK
	LOGICAL PAIRED
	REAL YUPPER, YLOWER
	COMMON /CCS1CM/ PAIRED,YUPPER,YLOWER
	CHARACTER * 18 GRNDMD
	COMMON /GMODE/ GRNDMD
C
C brigtness units for output files
C
        CHARACTER * 20 BUNITS(10)
        COMMON /BUNITS/ BUNITS
C
C history keywords for output files
C
	CHARACTER*68 PEDGRE(27), DESCRP(27)
	CHARACTER*64 REFFIL(27)
	COMMON /HDKEYS/ PEDGRE, DESCRP, REFFIL
C
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C header i/o parameter
C
        INTEGER UHDPFN
        PARAMETER (UHDPFN = 40)
C
C common block containing minimum and maximum wavelength
C
        REAL MINWAV, MAXWAV
        COMMON /MNMXWV/ MINWAV, MAXWAV
C
C local variables
C
        INTEGER LAST(10),IFILE,I
        CHARACTER*3 FTYPES(10)
        CHARACTER*4 QUALS(10),QUAL
        CHARACTER*8 STEPS(15)
        CHARACTER*80 CONTXT
	CHARACTER*80 LINE
	CHARACTER*8 REFNAM(27)
C 
        DATA FTYPES/'WAV','FLX','ERR','CDQ','MOD',
     *              'SCI','OBJ','SKY','BCK','NET'/
        DATA STEPS/'CNT_CORR','OFF_CORR','PPC_CORR','BAC_CORR',
     *             'SCT_CORR','FLT_CORR','SKY_CORR','WAV_CORR',
     *             'FLX_CORR','APR_CORR','AIS_CORR','TIM_CORR',
     *		   'ERR_CORR','MOD_CORR','GMF_CORR' /
        DATA QUALS/'.C0H','.C1H','.C2H','.CQH','.C3H','.C4H','.C5H',
     *             '.C6H','.C7H','.C8H'/
        DATA LAST /13,13,13,13,14,1,6,6,3,7/
	DATA REFNAM /'BACHFILE', 'FL1HFILE', 'FL2HFILE', 'IV1HFILE',
     *               'IV2HFILE', 'RETHFILE', 'DDTHFILE', 'DQ1HFILE',
     *               'DQ2HFILE', 'CCG2', 'CCS0', 'CCS1', 'CCS2',
     *               'CCS3', 'CCS4', 'CCS5', 'CCS6', 'CCS7', 
     *               'OFFS_TAB', 'CCS8', 'CCS9', 'CCSA', 'CCSB',
     *		     'CCSC', 'CCSD', 'AISHFILE', 'PCPHFILE' /
C
C------------------------------------------------------------------------------
C234567
      IFILE = IDNUM - 10
      QUAL = QUALS(IFILE)
C
C Update file type
C
        CALL UHDPST(ID,'FILETYPE',FTYPES(IFILE),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR updating FILETYPE in '//QUAL//' file'
                GO TO 999
        ENDIF
C
C Update brightness units
C
        CALL UHDPST(ID,'BUNIT',BUNITS(IFILE),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR updating BUNIT in '//QUAL//' file'
                GO TO 999
        ENDIF
C
C flag completed processing steps
C
        DO 10 I=1,LAST(IFILE)
            IF (PFLAGS(I).EQ.'PERFORM') THEN
                CALL UHDPST(ID,STEPS(I),'COMPLETE',ISTAT)
	    ELSE IF (PFLAGS(I).EQ.'SKIPPED') THEN
                CALL UHDPST(ID,STEPS(I),'SKIPPED',ISTAT)
	    END IF
            IF(ISTAT.NE.0)THEN
               CONTXT='ERROR updating '//STEPS(I)//' in '//
     *                 QUAL//' file'
               GO TO 999
            ENDIF
10      CONTINUE
C
C check for scaling reference background since this doesn't follow the above 
C routine
C
	IF(LAST(IFILE).GT.1)THEN
	   IF (PFLAGS(15).EQ.'PERFORM') THEN
                CALL UHDPST(ID,'GMF_CORR','COMPLETE',ISTAT)
	   ELSE IF (PFLAGS(15).EQ.'SKIPPED') THEN
                CALL UHDPST(ID,'GMF_CORR','SKIPPED',ISTAT)
	   END IF
           IF(ISTAT.NE.0)THEN
              CONTXT='ERROR updating GMF_CORR in '//QUAL//' file'
              GO TO 999
	   END IF
	END IF
C
C update min and max wavelength
C
        IF ((IFILE .LE. 5) .AND. PFLAGS(8).EQ.'PERFORM') THEN
            CALL UHDPSR(ID,'MINWAVE',MINWAV,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR updating MINWAVE in '//QUAL//' file'
                GO TO 999
            ENDIF
            CALL UHDPSR(ID,'MAXWAVE',MAXWAV,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR updating MAXWAVE in '//QUAL//' file'
                GO TO 999
            ENDIF
        ENDIF
C
C Add history records containing PEDIGREE and DESCRIP keyword strings
C
C First, Data quality initialization files
C
	LINE(35:52) = 'DQI INITIALIZATION'
	IF (PEDGRE(8).NE.' ') THEN
	    LINE(1:34) = REFNAM(8) // '=' // REFFIL(8)
	    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
	    CALL UHDAHS(ID,'HISTORY',PEDGRE(8),ISTAT)
	    IF (DESCRP(8).NE.' ')
     *		CALL UHDAHS(ID,'HISTORY',DESCRP(8),ISTAT)
	END IF
	IF (PAIRED .OR. POLID.NE.'C') THEN
	    IF (PEDGRE(9).NE.' ') THEN
		LINE(1:34) = REFNAM(9) // '=' // REFFIL(9)
		CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		CALL UHDAHS(ID,'HISTORY',PEDGRE(9),ISTAT)
		IF (DESCRP(9).NE.' ')
     *		    CALL UHDAHS(ID,'HISTORY',DESCRP(9),ISTAT)
	    END IF
	END IF
	LINE(35:52) = '                  '
C
C Loop over the remaining ref files used in each processing step
C
	DO 100 I = 1, LAST(IFILE)
	   IF (PFLAGS(I).EQ.'OMIT') GO TO 100
C
	   LINE(35:51) = STEPS(I) // '=' // PFLAGS(I)
	   IF (PFLAGS(I).EQ.'PERFORM') LINE(44:51)='COMPLETE'
C
C       Dead diode table
C
	   IF (I.EQ.1 .AND. PEDGRE(7).NE.' ') THEN
		LINE(1:34) = REFNAM(7) // '=' // REFFIL(7)
		CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		CALL UHDAHS(ID,'HISTORY',PEDGRE(7),ISTAT)
		IF (DESCRP(7).NE.' ')
     *		    CALL UHDAHS(ID,'HISTORY',DESCRP(7),ISTAT)
C
C       GIMP correction tables
C
	   ELSE IF (I.EQ.2) THEN
		IF (REFFIL(19)(1:3).NE.'n/a' .AND.
     *		    REFFIL(19)(1:3).NE.'N/A') THEN 
		    IF (PEDGRE(19).NE.' ') THEN
			LINE(1:34) = REFNAM(19) // '=' // REFFIL(19)
			CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
			CALL UHDAHS(ID,'HISTORY',PEDGRE(19),ISTAT)
			IF (DESCRP(19).NE.' ')
     *                      CALL UHDAHS(ID,'HISTORY',DESCRP(19),ISTAT)
		    END IF
		ELSE IF (PEDGRE(18).NE.' ') THEN
		    LINE(1:34) = REFNAM(18) // '=' // REFFIL(18)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(18),ISTAT)
		    IF (DESCRP(18).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(18),ISTAT)
		END IF
C
C       Paired pulse correction table
C
	   ELSE IF (I.EQ.3 .AND. PEDGRE(10).NE.' ') THEN
		LINE(1:34) = REFNAM(10) // '=' // REFFIL(10)
		CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		CALL UHDAHS(ID,'HISTORY',PEDGRE(10),ISTAT)
		IF (DESCRP(10).NE.' ')
     *		    CALL UHDAHS(ID,'HISTORY',DESCRP(10),ISTAT)
C
C       Background subtraction and scaling files and table
C
	   ELSE IF (I.EQ.4) THEN
		IF (NBCK.EQ.0) THEN
		    IF (PEDGRE(1).NE.' ') THEN
			LINE(1:34) = REFNAM(1) // '=' // REFFIL(1)
			CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
			CALL UHDAHS(ID,'HISTORY',PEDGRE(1),ISTAT)
			IF (DESCRP(1).NE.' ')
     *			    CALL UHDAHS(ID,'HISTORY',DESCRP(1),ISTAT)
		    END IF
		    IF (PFLAGS(15).NE.'OMIT' .AND. 
     *			PEDGRE(20).NE.' ') THEN
			LINE(1:34) = REFNAM(20) // '=' // REFFIL(20)
			LINE(35:51) = STEPS(15) // '=' // PFLAGS(15)
			IF(PFLAGS(15).EQ.'PERFORM')
     *			   LINE(44:51)='COMPLETE'
			CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
			CALL UHDAHS(ID,'HISTORY',PEDGRE(20),ISTAT)
			IF (DESCRP(20).NE.' ')
     *			    CALL UHDAHS(ID,'HISTORY',DESCRP(20),ISTAT)
		    END IF
		ELSE IF (PEDGRE(14).NE.' ') THEN
		    LINE(1:34) = REFNAM(14) // '=' // REFFIL(14)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(14),ISTAT)
		    IF (DESCRP(14).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(14),ISTAT)
		END IF
C
C       Scattered light correction table
C
	   ELSE IF (I.EQ.5 .AND. PEDGRE(21).NE.' ') THEN
		LINE(1:34) = REFNAM(21) // '=' // REFFIL(21)
		CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		CALL UHDAHS(ID,'HISTORY',PEDGRE(21),ISTAT)
		IF (DESCRP(21).NE.' ')
     *		    CALL UHDAHS(ID,'HISTORY',DESCRP(21),ISTAT)
C
C       Flat field correction file(s)
C
	   ELSE IF (I.EQ.6) THEN
		IF (PEDGRE(2).NE.' ') THEN
		    LINE(1:34) = REFNAM(2) // '=' // REFFIL(2)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(2),ISTAT)
		    IF (DESCRP(2).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(2),ISTAT)
		END IF
		IF (PAIRED .OR. POLID.NE.'C') THEN
		    IF (PEDGRE(3).NE.' ') THEN
			LINE(1:34) = REFNAM(3) // '=' // REFFIL(3)
			CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
			CALL UHDAHS(ID,'HISTORY',PEDGRE(3),ISTAT)
			IF (DESCRP(3).NE.' ')
     *			    CALL UHDAHS(ID,'HISTORY',DESCRP(3),ISTAT)
		    END IF
		END IF
C
C       Sky subtraction tables
C
	   ELSE IF (I.EQ.7) THEN
		IF (NSKY.EQ.0) GO TO 100
		IF (PEDGRE(11).NE.' ') THEN
		    LINE(1:34) = REFNAM(11) // '=' // REFFIL(11)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(11),ISTAT)
		    IF (DESCRP(11).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(11),ISTAT)
		END IF
		IF (PEDGRE(13).NE.' ') THEN
		    LINE(1:34) = REFNAM(13) // '=' // REFFIL(13)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(13),ISTAT)
		    IF (DESCRP(13).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(13),ISTAT)
		END IF
		IF (PEDGRE(14).NE.' ') THEN
		    LINE(1:34) = REFNAM(14) // '=' // REFFIL(14)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(14),ISTAT)
		    IF (DESCRP(14).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(14),ISTAT)
		END IF
                IF (PEDGRE(16).NE.' ') THEN
                    LINE(1:34) = REFNAM(16) // '=' // REFFIL(16)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(16),ISTAT)
		    IF (DESCRP(16).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(16),ISTAT)
                END IF
C
C       Wavelength coefficient table
C
	   ELSE IF (I.EQ.8 .AND. PEDGRE(17).NE.' ') THEN
		LINE(1:34) = REFNAM(17) // '=' // REFFIL(17)
		CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		CALL UHDAHS(ID,'HISTORY',PEDGRE(17),ISTAT)
		IF (DESCRP(17).NE.' ')
     *		    CALL UHDAHS(ID,'HISTORY',DESCRP(17),ISTAT)
C
C       Inverse sensitivity file(s) (old method)
C
	   ELSE IF (I.EQ.9) THEN
		IF (PEDGRE(4).NE.' ') THEN
		    LINE(1:34) = REFNAM(4) // '=' // REFFIL(4)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(4),ISTAT)
		    IF (DESCRP(4).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(4),ISTAT)
		END IF
		IF (PAIRED .OR. POLID.NE.'C') THEN
		    IF (PEDGRE(5).NE.' ') THEN
			LINE(1:34) = REFNAM(5) // '=' // REFFIL(5)
			CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
			CALL UHDAHS(ID,'HISTORY',PEDGRE(5),ISTAT)
			IF (DESCRP(5).NE.' ')
     *			    CALL UHDAHS(ID,'HISTORY',DESCRP(5),ISTAT)
		    END IF
		END IF
C
C       Aperture throughput and focus correction tables
C
	   ELSE IF (I.EQ.10) THEN
		IF (PEDGRE(22).NE.' ') THEN
		    LINE(1:34) = REFNAM(22) // '=' // REFFIL(22)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(22),ISTAT)
		    IF (DESCRP(22).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(22),ISTAT)
		END IF
		IF (PEDGRE(23).NE.' ') THEN
		    LINE(1:34) = REFNAM(23) // '=' // REFFIL(23)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(23),ISTAT)
		    IF (DESCRP(23).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(23),ISTAT)
		END IF
		IF (PEDGRE(24).NE.' ') THEN
		    LINE(1:34) = REFNAM(24) // '=' // REFFIL(24)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(24),ISTAT)
		    IF (DESCRP(24).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(24),ISTAT)
		END IF
C
C       Inverse sensitivity file (new method)
C
	   ELSE IF (I.EQ.11) THEN
		IF (PEDGRE(26).NE.' ') THEN
		    LINE(1:34) = REFNAM(26) // '=' // REFFIL(26)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(26),ISTAT)
		    IF (DESCRP(26).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(26),ISTAT)
		END IF
C
C       Sensitivity degradation (time) correction table
C
	   ELSE IF (I.EQ.12) THEN
		IF (PEDGRE(25).NE.' ') THEN
		    LINE(1:34) = REFNAM(25) // '=' // REFFIL(25)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(25),ISTAT)
		    IF (DESCRP(25).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(25),ISTAT)
		END IF
C
C       Statistical error calculation (no reference file/table)
C
C	   ELSE IF (I.EQ.13) THEN
C
C       Special mode processing files
C
	   ELSE IF (I.EQ.14 .AND. GRNDMD.EQ.'SPECTROPOLARIMETRY') THEN
		IF (PEDGRE(6).NE.' ') THEN
		    LINE(1:34) = REFNAM(6) // '=' // REFFIL(6)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(6),ISTAT)
		    IF (DESCRP(6).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(6),ISTAT)
		END IF
		IF (PEDGRE(15).NE.' ') THEN
		    LINE(1:34) = REFNAM(15) // '=' // REFFIL(15)
		    CALL UHDAHS(ID,'HISTORY',LINE,ISTAT)
		    CALL UHDAHS(ID,'HISTORY',PEDGRE(15),ISTAT)
		    IF (DESCRP(15).NE.' ')
     *			CALL UHDAHS(ID,'HISTORY',DESCRP(15),ISTAT)
		END IF
	   END IF
C
           IF (ISTAT.NE.0) THEN
               CONTXT='ERROR adding HISTORY keyword in '
     *                //QUAL//' file'
               GO TO 999
           END IF
100	CONTINUE
C
        ISTAT = 0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT = 1
1000    RETURN
        END
