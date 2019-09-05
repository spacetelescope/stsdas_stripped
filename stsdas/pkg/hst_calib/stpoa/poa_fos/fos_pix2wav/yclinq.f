C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLINQ(REFFIL,PFLAGS,GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: YCLINQ
*
*  Keyphrase:
*  ----------
*       Check that the reference relation tables and files exist
*
*  Description:
*  ------------
*       This routine inquires about the existence of the reference    
*       relation tables and reference files that have been previously   
*       read from the .d0h header. Only those files needed by the
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
*			(10) - aperture throughput and focus corrections
*			(11) - conversion to absolute flux units (new)
*			(12) - sensitivity degradation (time) correction
*                       (13) - output statistical error array
*                       (14) - special statistics processing
*                       (15) - scale reference background   
*
*  FORTRAN name: YCLINQ.FOR
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
*  CCS9                        Input	scattered light diode ranges
*  CCSA			       Input	OTA focus history table
*  CCSB			       Input	Aperture throughput vs wavelength
*  CCSC			       Input	Aperture throughput vs focus
*  CCSD			       Input	Time changes in sensitivity
*  AISHFILE		       Input	average inverse sensitivity file
*  PCPHFILE                    Input    Post-Costar Polarimetry corrections file
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
*       1       Oct 89      S J Hulbert     Designed and coded
*	2	May 91	    S. Hulbert	    Added GIMP correction table and
*					    calibration switch
*	2.1	Aug 91	    S. Hulbert	    Added scaling of reference 
*					    background
*	2.2	Apr 93	    H. Bushouse	    Added missing declarations for
*					    ISTAT, I and changed declaration of
*					    of passed arrays to (*).
*	2.3	Mar 94	    H. Bushouse     Added SCT_CORR step to PFLAGS
*	3	Mar 94	    H. Bushouse	    Mods to handle PEDIGREE keywords;
*					    (changed PFLAGS from LOGICAL to
*					     CHAR data type).
*	3.1	Oct 94	    H. Bushouse	    Mods to handle new aperture, focus,
*					    flux, and time correction files.
*       3.2     Sep 95      J. Eisenhamer   Allow missing AISHFILE.
*       3.3     Mar 97      M. De La Pena   Added KYDEPLOY to CONFG1 and 
*                                           PCPHFILE 
*       3.4     Feb 98      M. De La Pena   Only try to read PCPHFILE if not
*                                           'N/A', 'n/a', or blank
*-------------------------------------------------------------------------------
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
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
      INTEGER NX,NOBJ,NSKY,NBCK
      COMMON /CONFG3/NX,NOBJ,NSKY,NBCK
C
C LOCAL VARIABLES
C
      CHARACTER * 3 APPAIR(4)
      CHARACTER * 3 CHAR3
      CHARACTER * 8 NAMES(27)
      CHARACTER*80 CONTXT
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
C
C     Look for the FILES and TABLES that will be needed according
C     to the calibration switches that are set
C
C Count rate:
C             DDTHFILE - disabled diode file (if DEFDDT is 'FALSE')
C
  110 IF (PFLAGS(1).EQ.'PERFORM' .AND. (.NOT. DEFDDT)) THEN
         CALL YCLACC(REFFIL(7),NAMES(7),'FILE ',ISTATS(7))
      ENDIF
C
C GIMP correction:
C             CCS7- GIMP correction scale factors
C             OFFS_TAB GIMP table special case
C
      IF (PFLAGS(2).EQ.'PERFORM') THEN
         CALL YCLACC(REFFIL(18),NAMES(18),'TABLE',ISTATS(18))
         CHAR3 = REFFIL(19)(1:3)
         IF (CHAR3 .NE. 'n/a' .AND. CHAR3 .NE. 'N/A') THEN
             CALL YCLACC(REFFIL(19),NAMES(19),'TABLE',ISTATS(19))
         ENDIF
      ENDIF
C
C Paired pulse correction:
C             CCG2- paired pulse coefficient table
C
      IF (PFLAGS(3).EQ.'PERFORM') THEN
         CALL YCLACC(REFFIL(10),NAMES(10),'TABLE',ISTATS(10))
      ENDIF
C
C Background subtraction
C             BACHFILE - default background file
C	      CCS8 - predicted background count rates for scaling reference
C		     background
C
      IF (PFLAGS(4).EQ.'PERFORM' .AND. NBCK.EQ.0) THEN
         CALL YCLACC(REFFIL(1),NAMES(1),'FILE ',ISTATS(1))
         IF (PFLAGS(15).EQ.'PERFORM') THEN
            CALL YCLACC(REFFIL(20),NAMES(20),'TABLE',ISTATS(20))
         ENDIF
      ENDIF
C
C Scattered light subtraction
C	      CCS9 - scattered light measurement diode range values
C
      IF (PFLAGS(5).EQ.'PERFORM') THEN
         CALL YCLACC(REFFIL(21),NAMES(21),'TABLE',ISTATS(21))
      ENDIF
C
C Flat field
C       FL1HFILE, FL2HFILE - flat field files
C
      IF (PFLAGS(6).EQ.'PERFORM') THEN
         CALL YCLACC(REFFIL(2),NAMES(2),'FILE ',ISTATS(2))
         IF (PAIRED .OR. POLID .NE. 'C') THEN
            CALL YCLACC(REFFIL(3),NAMES(3),'FILE ',ISTATS(3))
         ENDIF
      ENDIF
C
C Sky subtraction:
C       CCS0 - aperture size table
C       CCS2 - sky emission line position table
C       CCS5 - sky shift table
C
      IF (PFLAGS(7).EQ.'PERFORM' .AND. NSKY.GT.0) THEN
         CALL YCLACC(REFFIL(11),NAMES(11),'TABLE',ISTATS(11))
         CALL YCLACC(REFFIL(13),NAMES(13),'TABLE',ISTATS(13))
         CALL YCLACC(REFFIL(16),NAMES(16),'TABLE',ISTATS(16))
      ENDIF
C
C Wavelength scale:
C       CCS6 - wavelength coefficient table
C       PIX2WAV tool must always read in the wave ref file
c      IF (PFLAGS(8).EQ.'PERFORM') THEN
         CALL YCLACC(REFFIL(17),NAMES(17),'TABLE',ISTATS(17))
c      ENDIF
C
C Inverse sensitivity (old method):
C       IV1HFILE, IV2HFILE - invserse sensitivity files.
C
      IF (PFLAGS(9).EQ.'PERFORM') THEN
         CALL YCLACC(REFFIL(4),NAMES(4),'FILE ',ISTATS(4))
         IF (PAIRED .OR. POLID .NE. 'C') THEN
            CALL YCLACC(REFFIL(5),NAMES(5),'FILE ',ISTATS(5))
         ENDIF
      ENDIF
C
C Inverse sensitivity (new method):
C     AISHFILE - average inverse sensitivity file
C     If missing, turn off all the steps depending on this.
C
      IF (PFLAGS(11).EQ.'PERFORM')THEN
         CALL YCLACC(REFFIL(26),NAMES(26),'FILE ',ISTATS(26))
         IF (ISTATS(26).NE.0)THEN
            ISTATS(26)=0
            PFLAGS(10)='OMIT'
            PFLAGS(11)='OMIT'
            PFLAGS(12)='OMIT'
            CONTXT='WARNING: No AISHFILE file found.'
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            WRITE(CONTXT,99)
 99         FORMAT('    APR_CORR, AIS_CORR, and TIM_CORR will',
     *           ' not be performed.')
            CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         ENDIF
      ENDIF
C
C Aperture throughput and focus corrections:
C	CCSA - OTA focus history table
C	CCSB - Aperture throughput vs wavelength
C	CCSC - Aperture throughput vs focus
C
      IF (PFLAGS(10).EQ.'PERFORM') THEN
	 CALL YCLACC(REFFIL(22),NAMES(22),'TABLE',ISTATS(22))
	 CALL YCLACC(REFFIL(23),NAMES(23),'TABLE',ISTATS(23))
	 CALL YCLACC(REFFIL(24),NAMES(24),'TABLE',ISTATS(24))
      END IF
C
C Sensitivity degradation (time) correction:
C	CCSD - time changes in sensitivity
C
      IF (PFLAGS(12).EQ.'PERFORM') THEN
	 CALL YCLACC(REFFIL(25),NAMES(25),'TABLE',ISTATS(25))
      END IF
C
C Ground software mode - SPECTROPOLARIMETRY:
C       RETHFILE - retardation file
C       CCS4 - Wollaston/Waveplate parameter table
C       PCPHFILE - post-COSTAR polarimetry corrections
C It is not an error not to have a PCPHFILE.
C
      IF (PFLAGS(14).EQ.'PERFORM') THEN
         IF (GRNDMD.EQ.'SPECTROPOLARIMETRY')THEN
            CALL YCLACC(REFFIL(6), NAMES(6), 'FILE ',ISTATS(6))
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
C Filter widths:
C       CCS3 - sky/background filter width table
C
      IF (PFLAGS(4).EQ.'PERFORM' .OR. PFLAGS(7).EQ.'PERFORM') THEN
	IF (NBCK.GT.0 .OR. NSKY.GT.0) 
     *   CALL YCLACC(REFFIL(14),NAMES(14),'TABLE',ISTATS(14))
      ENDIF
C
C Data quality initialization:
C       DQ1HFILE, DQ2HFILE - data quality initialization files
C
      CALL YCLACC(REFFIL(8),NAMES(8),'FILE ',ISTATS(8))
      IF (PAIRED .OR. POLID .NE. 'C') THEN 
          CALL YCLACC(REFFIL(9),NAMES(9),'FILE ',ISTATS(9))
      ENDIF
C
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
 999  CONTINUE
      CONTXT='ERROR: Required reference files/tables not found'
      CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT = 1
C
 1000 RETURN
      END
