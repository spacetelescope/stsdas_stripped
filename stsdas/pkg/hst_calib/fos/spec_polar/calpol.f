        SUBROUTINE CALPOL
*
*  Module number:
*
*  Module name: CALPOL
*
*  Keyphrase:
*  ----------
*       Calibrate FOS spectropolarimetry data
*
*  Description:
*  ------------
*       This routine performs the calibration of FOS spectropolarimetry 
*       data.  It has  two input cl parameters:
*               input - input filename including extension
*               output - output rootname
*
*
*  FORTRAN name:
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       The following calibrated data files are accessed:
*               <input>.c0h .c0d - wavelength data file
*               <input>.c1h .c1d - calibrated data file
*               <input>.c2h .c2d - statistical error file
*               <input>.cqh .cqd - data quality file
*
*
* The following reference files are used.  Files names are taken from the
* input .c1h file header.
*
*       RETHFILE - retardation file
*       PCPHFILE - Post-COSTAR polarimetry corrections file
*       CCS1 - upper/lower aperture position table
*       CCS4 - Wollaston/Waveplate parameter table
*       CCS6 - wavelength coefficient table
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       spcprc, ymsput, spoc1h
*  SDAS:
*       badwp, toroot, uuclgs, uerror
*  Others:
*
*
*  History:
*  --------
* Version   Date        Author          Description
*       1   Oct 92      D. Bazell       Modified version of YCLFOS
*	2   Sep 93	H. Bushouse	Removed dependence on raw data files
*	3   Jun 94	H. Bushouse	Mod's to handle new PFLAGS, PEDIGREE,
*					and NREAD > 1.
*	4   Nov 94	H. Bushouse	Mod's to handle new PFLAGS for new
*					flux cal steps.
*       5   Feb 98      M. De La Pena   Mods to incorporate the post-COSTAR
*                                       corrections.
*       5.1 Jun 98      M. De La Pena   Updated SPCNFG to remove unecessary
*                                       check on KYDPLY.
*       5.2 May 99      M. De La Pena   Updated SPCPOL for post-COSTAR data
*                                       POLSCAN=4; do NOT apply post-COSTAR
*                                       correction. Updated version and removed
*                                       STSDAS version this file.
*-------------------------------------------------------------------------------
C
C     Version number
C
      CHARACTER * 3 VERSN
      PARAMETER (VERSN = '3.2')
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)

C Array of flags indicating bad waveplate positions: BADFRM(2) means
C that waveplate position 2 is bad and should not be processed
C
      LOGICAL BADFRM(16)
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
C Common block containing rootname for YMSPUT.FOR
C
        CHARACTER*10 ROOTNM
        COMMON /YMSGCM/ROOTNM
C
C Common block containing ground mode
C
        CHARACTER * 18 GRNDMD
        COMMON /GMODE/ GRNDMD
C
C Common block containing input file name
C
        CHARACTER*64 INFILE, INEXT
        COMMON /CINFILE/INFILE, INEXT
C
C Local variables
C
        CHARACTER*64 ROOT,ROOTO, TMP
C                                    --->Input and output root names
        INTEGER ISTAT,ISTAT1,ISTAT2
C                                    --->error status
        CHARACTER*80 CONTXT
C                                    --->text message
        INTEGER I
C
C -----------------------------------------------------------------------
C
C CALPOLAR Version info
C
 	CONTXT='*** CALPOLAR - Version '//VERSN//' ***'
        CALL UMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C initialization
C
        ROOTNM=' '
        DO 10 I=1,30
                IDS(I)=-1
C                                    --->flag as not open
10      CONTINUE

C
C get rootnames of the input/output files
C
        CALL UUCLGS('input',TMP,ISTAT1)
        CALL UUCLGS('output',ROOTO,ISTAT2)
        IF((ISTAT1.NE.0).OR.(ISTAT2.NE.0))THEN
                CONTXT='ERROR getting value of CL parameter'
                GO TO 999
        ENDIF
        
        INFILE = TMP
        CALL TOROOT( INFILE, ROOT, INEXT, ISTAT)
        IF(ROOTO.EQ.' ')ROOTO=ROOT
C
C Get the list of waveplate positions that are bad
C
        CALL BADWP(BADFRM,ISTAT)
        IF (ISTAT.NE.0) THEN
           CONTXT='ERROR parsing bad waveplate positions'
           GOTO 999
        ENDIF
C
C open input .c1h file
C
c       CALL YOPD0H(ROOT,GRNDMD,ISTAT)
        CALL SPOC1H(ROOT,GRNDMD,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
        CONTXT='Begin CALPOLAR for input file rootname: '//ROOT
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        CONTXT='                  output file rootname: '//ROOTO
        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Only process spectropolarimetry data
C
        IF(GRNDMD.EQ.'SPECTROPOLARIMETRY')THEN
                CALL SPCPRC(ROOT,ROOTO,GRNDMD,BADFRM,ISTAT)
          ELSE
                CONTXT='Not Polarimetry data - no processing done'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
C
C Close any output files remaining open
C
c       DO 200 I=11,30
999     DO 200 I=15,30
            IF(IDS(I).GT.0)THEN
                CALL UIMCLO(IDS(I),ISTAT1)
                IF(ISTAT1.NE.0)THEN
                        CONTXT='ERROR closing output file(s)'
                        ISTAT=1
                ENDIF
            ENDIF
200     CONTINUE
C
C Close the spectropolarimetry input files
C
c       DO 300 I=1,2
c          IF (INID(I).GT.0) THEN
c             CALL UIMCLO(INID(I), ISTAT1)
        DO 300 I=1,14
           IF (IDS(I).GT.0) THEN
              CALL UIMCLO(IDS(I), ISTAT1)
              IF(ISTAT1.NE.0)THEN
                 CONTXT='ERROR closing spectropolarimetry input file(s)'
                 ISTAT=1
              ENDIF
           ENDIF
300     CONTINUE

C
C print completion message
C
        IF(ISTAT.EQ.0)THEN
                CONTXT='Reduction completed for input file '//ROOT
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
           ELSE
                CONTXT='Reduction NOT completed for input file '//
     *                           ROOT
                CALL UERROR(CONTXT)
        ENDIF
        RETURN
        END
