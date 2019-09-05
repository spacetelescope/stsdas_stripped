        SUBROUTINE SPCRD(FRAME,BADFRM,DATA,EPS,ERR,WAVE,ISTAT)
*
*  Module number:
*
*  Module name: SPCRD
*
*  Keyphrase:
*  ----------
*       Read input data for the spectropolarimetry reduction task
*
*  Description:
*  ------------
*       This routine reads the next frame of input data, including
*       data quality files.
*
*  FORTRAN name: spcrd.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.c0h          I       wavelength file
*       <rootname>.c1h          I       data file
*       <rootname>.c2h          I       error file
*       <rootname>.cqh          I       data quality
*
*  Subroutines called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uuogpr, uimgsr, uigl1r
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Oct 92      D. Bazell       Modified version of YCLRD
*	2	Sep 93	    H. Bushouse	    Removed loading of trailer data
*	2.1	Jun 94      H. Bushouse	    Mod's to handle NREAD > 1 and
*					    read wavelength (c0h) file.
*       2.2     Feb 98      M. De La Pena   Added KYDPLY to CONFG1.
*-------------------------------------------------------------------------------
*
* Inputs:
*       frame - frame number
*       badfrm - Array of flags indicating a frame is bad e.g. 
*                 badfrm(2)=.true. means frame 2 is bad and should not 
*                 be processed
*
* Outputs:
*       data - data vector
*       eps - data quality vector
*       err - error array
*	wave - wave array
*       istat - error status
*-----------------------------------------------------------------------------
        INTEGER ISTAT,FRAME
        REAL EPS(*),DATA(*),ERR(*),WAVE(*)
        LOGICAL BADFRM(*)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
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
        REAL EXPO
        COMMON /CONFG4/EXPO
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
        CHARACTER*80 CONTXT
        INTEGER IREAD,CLEARS,NREPS,I
	INTEGER WVPOS,INFRAME
        REAL DMIN,DMAX
C--------------------------------------------------------------------------
C
C figure out which waveplate position this data frame corresponds to
C	(added Jun-94 HAB)
C
	WVPOS = INT( (FRAME-1)/NREAD ) + 1
C
C read data and error -------------------- spectropolarimetry input file
C
C The data file (typically c1h) and the error file (typically c2h) have
C the two pass directions separated into consecutive groups (1,2; 3,4; ..)
C where each groups has only half as many points as in the raw data.  
C Thus, we read two groups at a time into a single data array
C
        INFRAME = 2*FRAME-1
C
C WAVE File
        CALL UUOPGR(IDS(11),INFRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR moving to next group of the WAVE file'
           GO TO 999
        ENDIF
C
C DATA File
c       CALL UUOPGR(INID(1),INFRAME,DMIN,DMAX,0,ISTAT)
        CALL UUOPGR(IDS(12),INFRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR moving to next group of the DATA file'
           GO TO 999
        ENDIF
C
C ERROR File
c       CALL UUOPGR(INID(2),INFRAME,DMIN,DMAX,0,ISTAT)
        CALL UUOPGR(IDS(13),INFRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR moving to next group of the ERROR file'
           GO TO 999
        ENDIF
C
C DATA QUALITY File
c       CALL UUOPGR(INID(3),INFRAME,DMIN,DMAX,0,ISTAT)
        CALL UUOPGR(IDS(14),INFRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR moving to next group of the DATA QUAL file'
           GO TO 999
        ENDIF
C
C Read data... current frame first, then next frame into the next 
C NX array positions.  Increment inframe to be ready to skip that the
C next frame
C
        INFRAME = INFRAME + 1
C
	CALL UIGL1R(IDS(11),WAVE,ISTAT)

C Next frame, WAVE File

        CALL UUOPGR(IDS(11),INFRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR moving to next group of the WAVE file'
           GO TO 999
        ENDIF

        CALL UIGL1R(IDS(11), WAVE(NX+1), ISTAT)
C
C Read DATA ... current frame then next frame into the next NX array
C positions.
C
        CALL UIGL1R(IDS(12),DATA,ISTAT)
           
C Next frame, DATA File

        CALL UUOPGR(IDS(12),INFRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR moving to next group of the DATA file'
           GO TO 999
        ENDIF

        CALL UIGL1R(IDS(12), DATA(NX+1), ISTAT)
C
C Read ERROR... current frame then next frame into the next NX array
C positions.  
C
        CALL UIGL1R(IDS(13),ERR,ISTAT)
           
C Next frame, ERROR File

        CALL UUOPGR(IDS(13),INFRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR moving to next group of the ERROR file'
           GO TO 999
        ENDIF

        CALL UIGL1R(IDS(13), ERR(NX+1), ISTAT)

        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR reading DATA or ERROR file'
           GO TO 999
        ENDIF
C
C read data quality  -----------------------------------------  cqh file
C
        CALL UIGL1R(IDS(14),EPS,ISTAT)

C Next frame, DQ File

	CALL UUOPGR(IDS(14),INFRAME,DMIN,DMAX,0,ISTAT)
	IF(ISTAT.NE.0)THEN
	   CONTXT='ERROR moving to next group of the .cqh file'
	   GO TO 999
	ENDIF

	CALL UIGL1R(IDS(14), EPS(NX+1), ISTAT)

        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR reading .cqh file'
           GO TO 999
        ENDIF
C
C If this frame is flaged as bad then set the eps array
C equal to 800 so frame is not processed. 
C
c       IF(BADFRM(FRAME)) THEN
        IF(BADFRM(WVPOS)) THEN
           DO 110 I=1,2*NX
              EPS(I) = 800
 110       CONTINUE
        ENDIF
C
C Compute exposure time for the frame
C
        CLEARS = (FRAME-1)/NREAD
        IREAD = FRAME - CLEARS*NREAD
        NREPS = IREAD*OVERSN*NPAT*INTS
        EXPO = (LVTIME*7.8125E-6)*NREPS
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
