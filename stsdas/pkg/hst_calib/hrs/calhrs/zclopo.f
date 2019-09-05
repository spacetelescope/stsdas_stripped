        SUBROUTINE ZCLOPO(ROOT,ISTAT)
*
*  Module number:
*
*  Module name: ZCLOPO
*
*  Keyphrase:
*  ----------
*       Routine to open output data set (<root>.c1h)
*  Description:
*  ------------
*       This routine opens the output data set using the input
*       .d0h file as a template.  the size of the output data
*       set is NGOUT groups of vectors of legnth=NSOUT.
*       NSOUT and NGOUT are stored in common block HRSIO and
*       were previously set by subroutine ZCLOUT.
*
*  FORTRAN name: ZCLOPO.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*  <rootname>.c1h               Output  output flux file
*  <rootname>.d0h               Input   template file for .c1h
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zfname
*  SDAS:
*       uuimcp, ZMSPUT
*  Others:
*
*
*  History:
*  --------
*  Version      Date	Author          Description
*	1	Feb 89	D. Lindler	Designed and coded
*     1.1	Sep 90	S. Hulbert	Make compatable with new data format 
*					and update bunit
*     1.2       Jun 91  S. Hulbert      Reprocessing headers
*     1.3       Oct 96  M. De La Pena   Added FBMD flag
*-------------------------------------------------------------------------------
        CHARACTER*64 ROOT
C                                    --->root name of the input observation
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
      INTEGER   TYREAL
      PARAMETER (TYREAL = 6)
      INTEGER  RDWRIT
      PARAMETER (RDWRIT = 2)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C     END IRAF77.INC
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
C   NSOUT - number of samples in the output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
C                       /HRSMOD/
C Common block containing observing mode parameters
C
C   GRAT - grating mode
C   DET - detector
C   SCLAMP - spectral calibration lamp
C   CARPOS - carrousel position
C   OBSMOD - observation mode (DIR, ACC, TAR)
C   APER - aperture (LSA, SSA, SC1, SC2)
C
        CHARACTER*3 OBSMOD,APER
        CHARACTER*5 GRAT
        INTEGER DET,SCLAMP,CARPOS
        COMMON /HRSMOD/ DET,SCLAMP,CARPOS
        COMMON /HRSMD1/ GRAT,OBSMOD,APER
C
C                       /HRSFLG/
C Common block containing processing flags
C
C Initial values = 'PERFORM' or 'OMIT'
C final values = 'COMPLETE', 'PERFORM' or 'OMIT'
C
        CHARACTER*12 FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *                  FPLY,FGWC,FBMD
        COMMON /HRSFLG/FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *                  FPLY,FGWC,FBMD
C
C LOCAL VARIABLES
C
        CHARACTER*64 FNAME
        CHARACTER*80 CONTXT
        INTEGER ISTAT
        CHARACTER*30 BUNIT
C
C---------------------------------------------------------------------------
C
C OPEN .C1H FILE USING TEMPLATE ----------------------------------------
C
        CALL ZFNAME(ROOT,'c1h',1,NGOUT,FNAME)
C                                                   --->construct file name
        CALL UUIMCP(FNAME,TYREAL,1,NSOUT,IDS(3),IDS(8),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: Error opening output .C1H file'
                GO TO 999
        ENDIF
C
C Update file type
C
        CALL UHDPST(IDS(8),'FILETYPE','FLX',ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR updating FILETYPE in .c1h header'
                GO TO 999
        ENDIF
C
C update bunit keyword before processing
C
C
         IF (FFLX.EQ.'PERFORM')THEN
	    BUNIT = 'ERGS/CM**2/S/A'
	 ELSE IF (FEXP.EQ.'PERFORM')THEN
	    BUNIT = 'COUNTS/S'
	 ELSE
	    BUNIT = 'COUNTS  '
	 ENDIF
         CALL UHDPST(IDS(8),'BUNIT',BUNIT,ISTAT)
         IF (ISTAT .NE. 0) THEN
                CONTXT='ERROR updating BUNIT in .c1h header'
                GO TO 999
         ENDIF
C
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
