        SUBROUTINE YGTREF(PFLAGS,GRNDMD,REFFIL,ISTAT)
*
*  Module number:
*
*  Module name: YGTREF.FOR
*
*  Keyphrase:
*  ----------
*       get reference file names
*
*  Description:
*  ------------
*       This routine reads the reference file and table names from
*       header parameters in the .d0h file.
*
*  FORTRAN name: ygtref.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.d0h          I       FOS science data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uhdgst
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*	2	Sep 90	S. Hulbert	Enhanced DQI
*	3	May 91	S. Hulbert	GIMP correction. Set up to use
*					reprocessing headers.
*	3.1	Aug 91	S. Hulbert	Added predicted background count rates
*					for  scaling of reference background
*	3.2	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*	4	Mar 94	H. Bushouse	Added scattered light ref. table CCS9,
*					and pass in PFLAGS array.
*	5	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords;
*					Complete rewrite to only read keywords
*					needed based on calibration switches.
*	6	Oct 94	H. Bushouse	Mods to handle new aper, focus, flux,
*					and time correction reference files
*       7       Mar 97  M. De La Pena   Mods for post-COSTAR polar calibrations
*-------------------------------------------------------------------------------
* inputs
*	pflags - processing flags
*	grndmd - ground mode
* outputs
*	reffil - reference file and table names
*	istat - error status
C
C-------------------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER      ISTAT
      CHARACTER*18 GRNDMD
      CHARACTER*64 REFFIL(*)
      CHARACTER*8  PFLAGS(*)
      CHARACTER*5  DET
      CHARACTER*3  FGWAID,APERID,YTYPE(3)
      CHARACTER*1  POLID
C
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
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C	HEADER I/O status message
C
      INTEGER USHPNF
      PARAMETER (USHPNF = 40)
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
C local variables
C
        INTEGER I, ISTATS(27)
        CHARACTER*80 CONTXT
        CHARACTER*8 NAMES(27)
        DATA NAMES/'BACHFILE','FL1HFILE','FL2HFILE','IV1HFILE',
     *             'IV2HFILE','RETHFILE','DDTHFILE','DQ1HFILE',
     *		   'DQ2HFILE','CCG2','CCS0','CCS1','CCS2','CCS3',
     *		   'CCS4','CCS5','CCS6','CCS7','OFFS_TAB','CCS8',
     *		   'CCS9','CCSA','CCSB','CCSC','CCSD','AISHFILE',
     *             'PCPHFILE'/
C---------------------------------------------------------------------
      DO 10 I=1,27
         ISTATS(I) = 0
10    CONTINUE
C
C Look for the FILE and TABLE names that will be needed according
C to the calibration switches that are set
C
C Count rate:
C             DDTHFILE - disabled diode file (if DEFDDT is 'FALSE')
C
      IF (PFLAGS(1).EQ.'PERFORM' .AND. (.NOT. DEFDDT)) THEN
          CALL UHDGST(IDS(1), NAMES(7), REFFIL(7), ISTATS(7))
      END IF
C
C GIMP correction:
C             CCS7- GIMP correction scale factors
C             OFFS_TAB GIMP table special case
C
      IF (PFLAGS(2).EQ.'PERFORM') THEN
          CALL UHDGST(IDS(1), NAMES(18), REFFIL(18), ISTATS(18))
          CALL UHDGST(IDS(1), NAMES(19), REFFIL(19), ISTATS(19))
      END IF
C
C Paired pulse correction:
C             CCG2- paired pulse coefficient table
C
      IF (PFLAGS(3).EQ.'PERFORM') THEN
          CALL UHDGST(IDS(1), NAMES(10), REFFIL(10), ISTATS(10))
      END IF
C
C
C Background subtraction
C             BACHFILE - default background file
C             CCS8 - predicted background count rates for scaling reference
C                    background
C
      IF (PFLAGS(4).EQ.'PERFORM' .AND. NBCK.EQ.0) THEN
          CALL UHDGST(IDS(1), NAMES(1), REFFIL(1), ISTATS(1))
          IF (PFLAGS(15).EQ.'PERFORM') THEN
              CALL UHDGST(IDS(1), NAMES(20), REFFIL(20), ISTATS(20))
          END IF
      END IF
C
C Scattered light subtraction
C             CCS9 - scattered light measurement diode range values
C
      IF (PFLAGS(5).EQ.'PERFORM') THEN
          CALL UHDGST(IDS(1), NAMES(21), REFFIL(21), ISTATS(21))
      END IF
C
C Flat field
C       FL1HFILE, FL2HFILE - flat field files
C
      IF (PFLAGS(6).EQ.'PERFORM') THEN
          CALL UHDGST(IDS(1), NAMES(2), REFFIL(2), ISTATS(2))
          CALL UHDGST(IDS(1), NAMES(3), REFFIL(3), ISTATS(3))
      END IF
C
C Sky subtraction:
C       CCS0 - aperture size table
C       CCS2 - sky emission line position table
C       CCS5 - sky shift table
C
      IF (PFLAGS(7).EQ.'PERFORM' .AND. NSKY.GT.0) THEN
          CALL UHDGST(IDS(1), NAMES(11), REFFIL(11), ISTATS(11))
          CALL UHDGST(IDS(1), NAMES(13), REFFIL(13), ISTATS(13))
          CALL UHDGST(IDS(1), NAMES(16), REFFIL(16), ISTATS(16))
      ENDIF
C
C
C Wavelength scale:
C       CCS6 - wavelength coefficient table
C
      IF (PFLAGS(8).EQ.'PERFORM') THEN
          CALL UHDGST(IDS(1), NAMES(17), REFFIL(17), ISTATS(17))
      END IF
C
C Inverse sensitivity (old method):
C       IV1HFILE, IV2HFILE - invserse sensitivity files.
C
      IF (PFLAGS(9).EQ.'PERFORM') THEN
          CALL UHDGST(IDS(1), NAMES(4), REFFIL(4), ISTATS(4))
          CALL UHDGST(IDS(1), NAMES(5), REFFIL(5), ISTATS(5))
      END IF
C
C Aperture throughput and focus corrections
C       CCSA - OTA focus history table
C	CCSB - aperture throughput vs wavelength
C	CCSC - aperture throughput vs focus
C
      IF (PFLAGS(10).EQ.'PERFORM') THEN
          CALL UHDGST(IDS(1), NAMES(22), REFFIL(22), ISTATS(22))
          CALL UHDGST(IDS(1), NAMES(23), REFFIL(23), ISTATS(23))
          CALL UHDGST(IDS(1), NAMES(24), REFFIL(24), ISTATS(24))
      END IF
C
C Inverse sensitivity (new method):
C	AISHFILE - average inverse sensitivity file
C
      IF (PFLAGS(11).EQ.'PERFORM') THEN
	  CALL UHDGST(IDS(1), NAMES(26), REFFIL(26), ISTATS(26))
      END IF
C
C Sensitivity degradation (time) correction:
C	CCSD - time changes in sensitivity
C
      IF (PFLAGS(12).EQ.'PERFORM') THEN
	  CALL UHDGST(IDS(1), NAMES(25), REFFIL(25), ISTATS(25))
      END IF
C
C Ground software mode - SPECTROPOLARIMETRY:
C       RETHFILE - retardation file
C       CCS4 - Wollaston/Waveplate parameter table
C       PCPHFILE - post-COSTAR polarimetry corrections
C
      IF (PFLAGS(14).EQ.'PERFORM') THEN
          IF (GRNDMD.EQ.'SPECTROPOLARIMETRY')THEN
              CALL UHDGST(IDS(1), NAMES(6),  REFFIL(6),  ISTATS(6))
              CALL UHDGST(IDS(1), NAMES(15), REFFIL(15), ISTATS(15))
              IF (KYDPLY) THEN
                  CALL UHDGST(IDS(1), NAMES(27), REFFIL(27), ISTATS(27))
              END IF
          END IF
      END IF
C
C Filter widths:
C       CCS3 - sky/background filter width table
C
      IF (PFLAGS(4).EQ.'PERFORM' .OR. PFLAGS(7).EQ.'PERFORM') THEN
          IF (NBCK.GT.0 .OR. NSKY.GT.0)
     *        CALL UHDGST(IDS(1), NAMES(14), REFFIL(14), ISTATS(14))
      END IF
C
C Data quality initialization:
C       DQ1HFILE, DQ2HFILE - data quality initialization files
C
      CALL UHDGST(IDS(1), NAMES(8), REFFIL(8), ISTATS(8))
      CALL UHDGST(IDS(1), NAMES(9), REFFIL(9), ISTATS(9))
C
C
C Aperture positions:
C       CCS1 - upper/lower aperture position table
C
      CALL UHDGST(IDS(1), NAMES(12), REFFIL(12), ISTATS(12))
C
C Check for errors encountered when reading keywords
C
      DO 101 I=1,27
         IF (ISTATS(I) .NE. 0) THEN
             CONTXT='ERROR: reading .d0h header keyword '//NAMES(I)
             GO TO 999
         END IF
101   CONTINUE
C
      ISTAT=0
      GO TO 1000
999   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT=1
1000  RETURN
      END
