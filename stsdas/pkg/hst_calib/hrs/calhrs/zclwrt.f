        SUBROUTINE ZCLWRT(ROOT,PASS,NS,NSPEC,WAVE,DATA,EPS,ERR,
     *                    NBINS,ET,EPSET,BCK,BCKID,ISTAT)
*
*  Module number:
*
*  Module name: ZCLWRT
*
*  Keyphrase:
*  ----------
*       Write results of calibration
*
*  Description:
*  ------------
*       This routine writes the output data files containing the
*       calibrated data.  Up to seven files are written. They are
*               <rootname>.c0h  wavelengths
*               <rootname>.c1h  flux
*               <rootname>.cqh  data quality vectors
*               <rootname>.c2h  propagated statistical error
*               <rootname>.c3h  special diodes
*               <rootname>.c4h  special diodes data quality
*               <rootname>.c5h  background
*       On input to this routine the .C1H file is already opened to
*       the correct group if PASS=FIRST
*
*  FORTRAN name: zclwrt.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <root>.c1h              I/O     Output flux file, also used
*                                       as template for other files
*       <root>.c0h              O       wavelength file
*       <root>.cqh              O       data quality file
*       <root>.c2h              O       propagated statistical errors
*       <root>.c3h              O       special diodes 
*       <root>.c4h              O       special diodes data quality
*       <root>.c5h              O       background
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*	zogpar
*  SDAS:
*	uipl1*, uimopn, uimclo, uuimcp, ZMSPUT
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb 89  D. Lindler      Designed and coded
*	1.1	Sep 90	S. Hulbert	Put image creation into new 
*					routine (zimcpy) and change how 
*					DATAMIN/DATAMAX are updated
*	1.2	Feb 91	S. Hulbert	Changed datatype of c3h/c4h back to
*					real*4
*	1.3	Sep 91	S. Hulbert	Implemented PASS flag
*       1.4     Feb 94  J. Eisenhamer   Added background output.
*       1.5     Oct 96  M. De La Pena   Added FBMD
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       root - rootname of the observation (ch*64)
*       pass - integer variable = 1 for first call, -1 for last
*       ns - number of sample positions in each output spectrum
*       nspec - number of output spectra
*       fadc - processing flag for application of disp. coef.
*       wave - wavelength array (real*8)
*       data - flux array (real*4)
*       eps - data quality array (real*4)
*       err - statistical error array (real*4)
*       nbins - number of substep bins
*       et - extracted data array (real*4)
*       epset -  extracted data quality array (real*4)
*       bck - Background that was subtracted (real*4)
*       bckid - Bin to use to find group information.
*
* Output parameter
*
*       istat - error status (integer)
*
*******************************************************************************
        CHARACTER*64 ROOT
        INTEGER PASS
        INTEGER NS,NSPEC,ISTAT,NBINS,BCKID
        DOUBLE PRECISION WAVE(NS,NSPEC)
        REAL DATA(NS,NSPEC),EPS(NS,NSPEC),ERR(NS,NSPEC),BCK(NS)
        REAL ET(24,7),EPSET(24,7)
C--------------------------------------------------------------------------
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
      INTEGER   RDWRIT
      PARAMETER (RDWRIT = 2)
C
C     CODES FOR DATA TYPES
C
      INTEGER   TYINT
      PARAMETER (TYINT = 4)
      INTEGER   TYREAL
      PARAMETER (TYREAL = 6)
      INTEGER   TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C----------------------------------------------------------------------------
C
C                       /HRSIO/
C Common Block containting input/output parameters
C
C   IDS(20) - input file IDs
C               1 - .shh
C               2 - .ulh
C               3 - .d0h
C               4 - .q0h
C               5 - .x0h
C               6 - .xqh
C               7 - .c0h
C               8 - .c1h
C               9 - .cqh
C               10 - .c2h
C               11 - .c3h
C               12 - .c4h
C               13 - .c5h
C   GCOUNT(20) - group counts for input files
C   MERGE - Number of bins merged in output spectra
C   OBSRPT - observation repeats
C   NGOUT - number of output groups
C   NGSDT - number of output groups for the special diode files
C   READNO - readout number
C   NSOUT - number of samples in output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
C                       /HRSFLG/
C Common block containing processing flags
C
C Initial values = 'PERFORM' or 'OMIT'
C final values = 'COMPLETE', 'PERFORM' or 'OMIT'
C
        CHARACTER*12 FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *            FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *            FPLY,FGWC,FBMD
        COMMON /HRSFLG/FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *            FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *            FPLY,FGWC,FBMD
C
C-------------------------------------------------------------------------
C
C LOCAL VARIABLES
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        CHARACTER*30 BUNIT
        CHARACTER*80 CONTXT
        INTEGER IGOUT,ISPEC,IGSDT,IBIN, NSD
	INTEGER ISTAT1, ISTAT2
        REAL DMIN,DMAX
	DOUBLE PRECISION MINWAV,MAXWAV,MINWV0,MAXWV0
	LOGICAL WAVFLG
        DATA NSD /12/
C---------------------------------------------------------------------------
C
C if first call, open output files using the c1h file as a template
C
      IF (PASS.EQ.FIRST) THEN
C
C set up group pointers
C
         IGOUT=1
         IGSDT=1
C                                                             ---- open .c0h
C calculate minimum and maximum wavelength and put in c1h header
C before we create wavelength file (only if FADC = perform)
C
	 WAVFLG = .FALSE.
         IF (FADC.EQ.'PERFORM')THEN
	    CALL DMNMAX(WAVE,NS*NSPEC,MINWV0,IMIN,MAXWV0,IMAX)
            CALL UHDPSD(IDS(8),'MINWAVE',MINWV0,ISTAT1)
            CALL UHDPSD(IDS(8),'MAXWAVE',MAXWV0,ISTAT2)
            IF (ISTAT1.NE.0 .OR. ISTAT2.NE.0) THEN
	         CONTXT = 'ERROR updating MINWAVE/MAXWAVE keywords'
		 GO TO 1000
	    ENDIF
C
            CALL ZIMCPY(ROOT, 'c0h', IGOUT, NGOUT, TYDOUB, NS,
     $			IDS(8), 'WAV', 'ANGSTROMS', IDS(7), ISTAT)
            IF (ISTAT .NE. 0) GO TO 1000
         ENDIF
C                                                             ---- open .cqh
         CALL ZIMCPY(ROOT, 'cqh', IGOUT, NGOUT, TYINT, NS,
     $			IDS(8), 'FDQ', '        ', IDS(9), ISTAT)
         IF (ISTAT .NE. 0) GO TO 1000
C                                                             ---- open .c2h
C get bunits from c1h file
C
         CALL UHDGST(IDS(8),'BUNIT',BUNIT,ISTAT)
         CALL ZIMCPY(ROOT, 'c2h', IGOUT, NGOUT, TYREAL, NS,
     $			IDS(8), 'ERR', BUNIT, IDS(10), ISTAT)
         IF (ISTAT .NE. 0) GO TO 1000
C
C figure out bunits
C
	 IF (FEXP.EQ.'PERFORM')THEN
	    BUNIT = 'COUNTS/S'
	 ELSE
	    BUNIT = 'COUNTS  '
	 ENDIF
C                                                             ---- open .c5h
         IF(FBCK.EQ.'PERFORM')THEN
            CALL ZIMCPY(ROOT, 'c5h', IGOUT, NGOUT, TYREAL, NS,
     $           IDS(8), 'BCK', BUNIT, IDS(13), ISTAT)
            IF (ISTAT .NE. 0) GO TO 1000
            CALL ZNOFLG(IDS(13),ISTAT)
         ENDIF
C                                                             ---- open .c3h
         CALL ZIMCPY(ROOT, 'c3h', IGSDT, NGSDT, TYREAL, NSD,
     $	             IDS(8), 'SPD', BUNIT, IDS(11), ISTAT)
         IF (ISTAT .NE. 0) GO TO 1000
C        	                                             ---- open .c4h
         CALL ZIMCPY(ROOT, 'c4h', IGSDT, NGSDT, TYINT, NSD,
     $	             IDS(8), 'SPQ', '        ', IDS(12), ISTAT)
         IF (ISTAT .NE. 0) GO TO 1000
C
      ENDIF
C
C ******************** START LOOP ON SPECTRA **************************
C
      DO 500 ISPEC=1,NSPEC
C
C Point to the proper group 
C
         IF (IGOUT .EQ. 1) GO TO 600
C                                                             ---- .c1h
C calculate datamin/datamax for group igout-1
C
	 CALL UUMNMX(IDS(8),DMIN,DMAX,ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR calculating datamin/datamax for .c1h file'
            GO TO 999
         ENDIF
C
C point to group igout passing in the min/max values for group igout-1
C for closing/writing purposes
C
         CALL UUOPGR(IDS(8),IGOUT,DMIN,DMAX,IDS(3),ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR changing group within .c1h file'
            GO TO 999
         ENDIF
C                                                             ---- .c0h
         IF(FADC.EQ.'PERFORM')THEN
	    CALL UUMNMX(IDS(7),DMIN,DMAX,ISTAT)
            IF(ISTAT.NE.0)THEN
               CONTXT='ERROR calculating datamin/datamax for .c0h file'
               GO TO 999
            ENDIF
            CALL UUOPGR(IDS(7),IGOUT,DMIN,DMAX,IDS(8),ISTAT)
            IF(ISTAT.NE.0)THEN
               CONTXT='ERROR changing group within .c0h file'
               GO TO 999
            ENDIF
         ENDIF
C                                                             ---- .cqh
	 CALL UUMNMX(IDS(9),DMIN,DMAX,ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR calculating datamin/datamax for .cqh file'
            GO TO 999
         ENDIF
         CALL UUOPGR(IDS(9),IGOUT,DMIN,DMAX,IDS(8),ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR changing group within .cqh file'
            GO TO 999
         ENDIF
C                                                             ---- .c2h
	 CALL UUMNMX(IDS(10),DMIN,DMAX,ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR calculating datamin/datamax for .c2h file'
            GO TO 999
         ENDIF
         CALL UUOPGR(IDS(10),IGOUT,DMIN,DMAX,IDS(8),ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR changing group within .c2h file'
                GO TO 999
         ENDIF
C                                                             ---- .c5h
         IF(FBCK.EQ.'PERFORM')THEN
            CALL UUMNMX(IDS(13),DMIN,DMAX,ISTAT)
            IF(ISTAT.NE.0)THEN
               CONTXT='ERROR calculating datamin/datamax for .c5h file'
               GO TO 999
            ENDIF
            CALL UUOPGR(IDS(13),IGOUT,DMIN,DMAX,IDS(8),ISTAT)
            IF(ISTAT.NE.0)THEN
               CONTXT='ERROR changing group within .c5h file'
               GO TO 999
            ENDIF
         ENDIF
C
C Write the data 
C
  600    CALL UIPL1R(IDS(8),DATA(1,ISPEC),ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR: Error writing to .C1H file'
            GO TO 999
         ENDIF
C
         IF(FADC.EQ.'PERFORM')THEN
C
C keep track of the minimum and maximum wavelengths for the
C entire observation just to satisfy the ridiculous needs
C of the archive
C
	    CALL DMNMAX(WAVE,NS*NSPEC,MINWAV,IMIN,MAXWAV,IMAX)
	    IF (MINWAV.LT.MINWV0) MINWV0 = MINWAV
	    IF (MAXWAV.GT.MAXWV0) MAXWV0 = MAXWAV
	    IF (.NOT.WAVFLG) WAVFLG = .TRUE.
C
            CALL UIPL1D(IDS(7),WAVE(1,ISPEC),ISTAT)
            IF(ISTAT.NE.0)THEN
               CONTXT='ERROR: Error writing to .C0H file'
               GO TO 999
            ENDIF
         ENDIF
C
         CALL UIPL1R(IDS(9),EPS(1,ISPEC),ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR: Error writing to .CQH file'
            GO TO 999
         ENDIF
C
         CALL UIPL1R(IDS(10),ERR(1,ISPEC),ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR: Error writing to .C2H file'
            GO TO 999
         ENDIF
C
         IF(FBCK.EQ.'PERFORM')THEN
            CALL UIPL1R(IDS(13),BCK,ISTAT)
            IF(ISTAT.NE.0)THEN
               CONTXT='ERROR: Error writing to .C5H file'
               GO TO 999
            ENDIF
         ENDIF
C
C update group parameters
C
         CALL ZOGPAR(IDS(8),ISPEC,'c1h',ISTAT)
         IF(ISTAT.NE.0) GO TO 1000
C
         IF(FADC.EQ.'PERFORM')THEN
            CALL ZOGPAR(IDS(7),ISPEC,'c0h',ISTAT)
            IF(ISTAT.NE.0) GO TO 1000
         ENDIF
C
         CALL ZOGPAR(IDS(9),ISPEC,'cqh',ISTAT)
         IF(ISTAT.NE.0) GO TO 1000
C
         CALL ZOGPAR(IDS(10),ISPEC,'c2h',ISTAT)
         IF(ISTAT.NE.0) GO TO 1000
C
         IF(FBCK.EQ.'PERFORM')THEN
            IF(BCKID.EQ.0)BCKID=ISPEC
            CALL ZOGPAR(IDS(13),BCKID,'c5h',ISTAT)
            IF(ISTAT.NE.0) GO TO 1000
         ENDIF
C
C increment group pointer
C
         IGOUT=IGOUT+1
500   CONTINUE
C
C ******************** END LOOP ON SPECTRA ****************************
C
C ***************** START LOOP ON SUBSTEP BINS *************************
C
      DO 700 IBIN = 1, NBINS
C
C Point to the proper group
C
        IF (IGSDT .EQ. 1) GO TO 800
C                                                         ---- .c3h
	CALL UUMNMX(IDS(11),DMIN,DMAX,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR calculating datamin/datamax for .c3h file'
           GO TO 999
        ENDIF
        CALL UUOPGR(IDS(11),IGSDT,DMIN,DMAX,IDS(8),ISTAT)
        IF (ISTAT.NE.0)THEN
           CONTXT='ERROR changing group within .c3h file'
           GO TO 999
        ENDIF
C                                                             ---- .c4h
	CALL UUMNMX(IDS(12),DMIN,DMAX,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR calculating datamin/datamax for .c4h file'
           GO TO 999
        ENDIF
        CALL UUOPGR(IDS(12),IGSDT,DMIN,DMAX,IDS(8),ISTAT)
        IF (ISTAT.NE.0)THEN
           CONTXT='ERROR changing group within .c4h file'
           GO TO 999
        ENDIF
C
C Write the data 
C
  800   CALL UIPL1R(IDS(11),ET(1,IBIN),ISTAT)
        IF (ISTAT.NE.0) THEN
           CONTXT='ERROR: Error writing to .C3H file'
           GO TO 999
        ENDIF
C
        CALL UIPL1R(IDS(12),EPSET(1,IBIN),ISTAT)
        IF (ISTAT.NE.0) THEN
           CONTXT='ERROR: Error writing to .C4H file'
           GO TO 999
        ENDIF
C
C write group parameters
C
        CALL ZOGPAR(IDS(11),IBIN,'c3h',ISTAT)
        IF(ISTAT.NE.0) GO TO 1000
C
        CALL ZOGPAR(IDS(12),IBIN,'c4h',ISTAT)
        IF(ISTAT.NE.0) GO TO 1000
C
C increment group pointer
C
        IGSDT=IGSDT+1
700   CONTINUE
C
C Force the computation of datamin/datamax for the last group
C of each image and update MINWAVE and MAXWAVE, if necessary
C
      IF (IGOUT .GT. NGOUT .AND. IGSDT .GT. NGSDT) THEN
         DO 101 I = 7, 13
             IF (I.EQ.7 .AND. FADC.EQ.'OMIT') GO TO 101
C            IF (I.EQ.13 .AND. FBCK.EQ.'OMIT') GO TO 101
             IF (I.EQ.13 .AND. ((FBCK.EQ.'OMIT') .OR. 
     *                          (FBCK.EQ.'SKIPPED'))) GO TO 101
	     CALL UUMNMX(IDS(I),DMIN,DMAX,ISTAT)
             IF (ISTAT .NE. 0) GO TO 1000
	     IF (WAVFLG) THEN
                 CALL UHDPSD(IDS(I),'MINWAVE',MINWV0,ISTAT)
                 CALL UHDPSD(IDS(I),'MAXWAVE',MAXWV0,ISTAT)
                 IF (ISTAT1.NE.0 .OR. ISTAT2.NE.0) THEN
	             CONTXT = 'ERROR updating MINWAVE/MAXWAVE keywords'
		     GO TO 1000
		 ENDIF
	     ENDIF
101      CONTINUE
      ENDIF
C
C *********************************************************************
      ISTAT=0
      GO TO 1000
  999 CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT=1
 1000 RETURN
      END
