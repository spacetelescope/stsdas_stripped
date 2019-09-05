        SUBROUTINE SPCINQ(REFFIL,PFLAGS,GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: SPCINQ
*
*  Keyphrase:
*  ----------
*       Check that the reference relation tables and files exist
*
*  Description:
*  ------------
*       This routine inquires about the existence of the reference    
*       relation tables and reference files that have been previously   
*       read from the .c1h header. Only those files needed by the
*       calibration switches set to 'PERFORM' are checked.
*
*       If any of the files is not found then the routine returns an 
*       error status.
*
*                 pflags(1) - convert to count rates
*                       (2) - GIMP correction
*                       (3) - deadtime correction (paired pulse)
*                       (4) - background subtraction
*                       (5) - scattered light subtraction
*                       (6) - flat fielding
*                       (7) - sky subtraction
*                       (8) - wavelength assignments
*                       (9) - conversion to absolute flux units (old)
*			(10) - aperture throughput and focus correction
*			(11) - conversion to absolute flux units (new)
*			(12) - sensitivity degradation (time) correction
*                       (13) - output statistical error array
*                       (14) - special statistics processing
*                       (15) - scale reference background
*
*  FORTRAN name: SPCINQ.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  BACHFILE                    Input    default background
*  FL1HFILE                    Input    flat field #1
*  FL2HFILE                    Input    flat field #2
*  IV1HFILE                    Input    inverse sensitivity file #1
*  IV2HFILE                    Input    inverse sensitivity file #2
*  RETHFILE                    Input    retardation file
*  DDTHFILE                    Input    disabled diode file
*  DQ1HFILE                    Input    data quality initialization file #1
*  DQ2HFILE                    Input    data quality initialization file #2
*  CCG2                        Input    paired pulse coefficient table
*  CCS0                        Input    aperture size table
*  CCS1                        Input    upper/lower aperture position table
*  CCS2                        Input    sky emission line position table
*  CCS3                        Input    sky/background filter width table
*  CCS4                        Input    Wollaston/Waveplate parameter table
*  CCS5                        Input    sky shift table
*  CCS6                        Input    wavelength coefficient table
*  CCS7                        Input    GIMP correction scale factors
*  OFFS_TAB        	       Input	GIMP offsets (for use in post-pipeline)
*  CCS8                        Input    predicted background count rates
*  CCS9                        Input    scattered light diode ranges
*  CCSA			       Input	OTA focus history
*  CCSB			       Input	relative aperture throughputs
*  CCSC			       Input	aperture throughput vs focus
*  CCSD			       Input	sensitivity change vs time
*  AISHFILE     	       Input	average inverse sensitivity file
*  PCPHFILE                    Input    post-COSTAR polarimetry corrections
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       none
*  SDAS:
*       YCLACC
*
*  History:
*  --------
*  Version      Date        Author          Description
*	1	Sep 93	H. Bushouse	Modified version of YCLINQ
*	2	Jun 94	H. Bushouse	Added SCT_CORR step to PFLAGS;
*					Mods to handle PEDIGREE keywords
*					(changed PFLAGS from LOGICAL to 
*					 CHAR data type).
*	3	Nov 94	H. Bushouse	Mods to handle new flux cal REFFILE's
*       4       Feb 98  M. De La Pena   Only try to read PCPHFILE if not
*                                         'N/A', 'n/a', or blank
*-------------------------------------------------------------------------------
C
C INPUT
C
      CHARACTER*8 PFLAGS(*)
C
      CHARACTER*64 REFFIL(*)
C
      CHARACTER*18 GRNDMD
C
      INTEGER ISTAT
C
      CHARACTER*5 DET
      CHARACTER*3 FGWAID,APERID,YTYPE(3)
      CHARACTER*1 POLID
      INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *        INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME
      LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
      COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
      COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *        INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME,HEADER,TRAILR,
     *        DEFDDT
C
C LOCAL VARIABLES
C
      CHARACTER * 3 APPAIR(4), CHAR3
      CHARACTER * 8 NAMES(27)
      INTEGER I, ISTATS(27)
      LOGICAL PAIRED
C
      DATA APPAIR/'A-2','A-3','A-4','C-1'/
      DATA NAMES/'BACHFILE','FL1HFILE','FL2HFILE','IV1HFILE',
     *           'IV2HFILE','RETHFILE','DDTHFILE','DQ1HFILE',
     *		 'DQ2HFILE','CCG2','CCS0','CCS1','CCS2','CCS3',
     *		 'CCS4','CCS5','CCS6','CCS7','OFFS_TAB','CCS8',
     *		 'CCS9','CCSA','CCSB','CCSC','CCSD','AISHFILE',
     *           'PCPHFILE'/
C
C--------------------------------------------------------------------------
C
      DO 10 I=1,27
         ISTATS(I)= 0
   10 CONTINUE
C
C Is it a paired aperture?
C
      PAIRED=.FALSE.
      DO 100 I=1,4
         IF (APERID .EQ. APPAIR(I)) THEN
            PAIRED=.TRUE.
            GO TO 110
         ENDIF
  100 CONTINUE
  110 CONTINUE
C
C     Look for the FILES and TABLES that will be needed according
C     to the calibration switches that are set
C
C Ground software mode - SPECTROPOLARIMETRY
C       RETHFILE - retardation file
C       CCS4 - Wollaston/Waveplate parameter table
C       PCPHFILE - post-COSTAR polarimetry corrections
C It is not an error not to have a PCPHFILE.
C
      IF (PFLAGS(14).EQ.'PERFORM') THEN
	 IF (GRNDMD.EQ.'SPECTROPOLARIMETRY')THEN
            CALL YCLACC(REFFIL(6),NAMES(6),'FILE ',ISTATS(6))
            CALL YCLACC(REFFIL(15),NAMES(15),'TABLE',ISTATS(15))
            IF (KYDPLY) THEN
                CHAR3 = REFFIL(27)(1:3)
                IF (CHAR3 .NE. 'n/a' .AND. CHAR3 .NE. 'N/A' .AND.
     *              CHAR3 .NE. '   ') THEN
                    CALL YCLACC(REFFIL(27),NAMES(27),'FILE ',ISTATS(27))
                ENDIF
            ENDIF
	 ENDIF
      ENDIF
C
C Aperture positions:    
C       CCS1 - upper/lower aperture position table
C
      CALL YCLACC(REFFIL(12),NAMES(12),'TABLE',ISTATS(12))
C
      DO 130 I=1,27
         IF (ISTATS(I) .NE. 0) GO TO 999
  130 CONTINUE
C
      ISTAT = 0
      GOTO 1000
C
  999 ISTAT = 1
C
 1000 RETURN
      END
