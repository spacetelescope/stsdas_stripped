        SUBROUTINE SPCPRC(ROOT,ROOTO,GRNDMD,BADFRM,ISTAT)
*
*  Module number:
*
*  Module name: spcprc
*
*  Keyphrase:
*  ----------
*       process FOS spectropolarimetry data
*
*  Description:
*  ------------
*       This routine performs the calibration of FOS spectropolarimetry 
*       data.  Only processing for spectropolarimetry mode is done.  It
*       is assumed that PFLAGS(14) is set to yes, which specifies
*       special statistics processing.
*
*  FORTRAN name: spcprc.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       SEE CALPOL
*
*  Subroutines Called:
*  -------------------
*
*       ymsput, yclopn, yopd0h, yrccs3, yclrd, yclwrt, yosize
*       ycldqi, yclexp, yclflt, yclppc, yclbck, yclsky, yclivs
*       yclwav, ygtref, yconfg, ypflags, yclmod
*	yclinq, yrccs1, yclerr, ycloff, udmget, udmfre
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Oct 92      D. Bazell       Modified version of YCLPRC
*	2	Sep 93	    H. Bushouse	    Removed dependence on raw files
*	2.1	Jun 94	    H. Bushouse	    Mod's to handle new PFLAGS,
*					    PEDIGREE, and NREAD > 1.
*	2.2	Nov 94	    H. Bushouse	    Mod's to handle new PFLAGS for
*					    new flux cal steps.
*       2.3     Feb 98      M. De La Pena   Added KYDPLY to CONFG1.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       root - rootname of input files
*       rooto - rootname of the output files
*       grndmd - ground mode
*       badfrm - array of flags. badfrm(2)=.true. means the waveplate
*                  position is bad and should not be processed
*
* OUTPUTS:
*       istat - error status
*
        CHARACTER*18 GRNDMD
        CHARACTER*64 ROOT,ROOTO
        LOGICAL      BADFRM(*)
        INTEGER ISTAT
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
C------------------------------------------------------------------------------
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      INTEGER TYREAL
      PARAMETER (TYREAL = 6)
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
        INTEGER XOFFS, YOFFS, NSPEC
        COMMON /GMPOFF/ XOFFS, YOFFS, NSPEC
C
	CHARACTER*68 PEDGRE(27), DESCRP(27)
	COMMON /HDKEYS/ PEDGRE, DESCRP, REFFIL
C
C Local variables
C
C
        CHARACTER*64 REFFIL(27)
C                                    --->reference file names
        CHARACTER*80 CONTXT
C                                    --->input group counter
        CHARACTER*8 PFLAGS(15)
C                                    --->processing flags
        CHARACTER * 3  DTYPE
        INTEGER I,FRAME,NDATA,ISTATS(4)
        REAL BADEPS
        INTEGER DATA,EPS,ERR,WAVE
C
C FLAG for bad data points (if eps > or =badeps) point is not calibrated
C and treated as fill.
C
        DATA BADEPS/200.0/
C-----------------------------------------------------------------------------
	DO 10 I = 1, 27
	   PEDGRE(I) = ' '
	   DESCRP(I) = ' '
10	CONTINUE
C
C OPEN input files
C
        FRAME=1
c       CALL YCLOPN(ROOT,ISTAT)
c       IF(ISTAT.NE.0) GO TO 999
        CALL SPCOPN(ROOT, ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C GET configuration parameters
C
c       CALL YCONFG(ISTAT)
        CALL SPCNFG(ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C Initialize all PFLAGS to 'OMIT'. Then set PFLAGS(14) to 'PERFORM' if
C there are more than 4 groups.  We are ignoring the processing flags
C set in the input data header
C
        DO 50 I = 1,15
           PFLAGS(I) = 'OMIT' 
 50     CONTINUE
	gcount(1) = gcount(12)/2
        IF ((GRNDMD .EQ. 'SPECTROPOLARIMETRY') .AND. 
     .                          (GCOUNT(1)/NREAD .GE. 4)) THEN
           PFLAGS(14) = 'PERFORM'
        ELSE
              CONTXT = 'Not enough waveplate positions to process data'
              ISTAT = 1
              GOTO 999
        ENDIF

C
C GET reference file names
C
c       CALL YGTREF(REFFIL,ISTAT)
        CALL SPGREF(REFFIL,ISTAT)
        IF(ISTAT.NE.0) GO TO 999

C
C Check that all reference tables and files exist
C
c       CALL YCLINQ(REFFIL,PFLAGS,GRNDMD,ISTAT)
        CALL SPCINQ(REFFIL,PFLAGS,GRNDMD,ISTAT)
        IF(ISTAT.NE.0) GO TO 999

C
C Calculate number of pixels per frame and the number of spectra 
C per frame
C
        NDATA = (NCHNLS+OVERSN-1)*NXSTEP*YSTEPS*SLICES
        NSPEC = YSTEPS*SLICES
C
C Allocate memory for DATA,EPS,ERR,WAVE
C
        DO 101 I = 1, 4
            ISTATS(I) = 0
101     CONTINUE
        CALL UDMGET (NDATA, TYREAL, DATA, ISTATS(1))
        CALL UDMGET (NDATA, TYREAL, EPS, ISTATS(2))
        CALL UDMGET (NDATA, TYREAL, ERR, ISTATS(3))
        CALL UDMGET (NDATA, TYREAL, WAVE, ISTATS(4))
C
        DO 100 I = 1, 4
            IF (ISTATS(I) .NE. 0) THEN
                ISTAT = 1
                CONTXT = 'ERROR allocating dynamic memory'
                GO TO 999
            ENDIF
100     CONTINUE
C
C read aperture position table
C
        CALL YRCCS1(REFFIL(12),DET,FGWAID,APERID,
     $		    PEDGRE(12),DESCRP(12),ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C determine output data sets sizes
C
c       CALL YOSIZE(GRNDMD,PFLAGS,DTYPE,ISTAT)
        CALL SPOSIZ(GRNDMD,PFLAGS,DTYPE,ISTAT)
        IF(ISTAT.NE.0)GO TO 999

C
C Loop on input readouts

        DO 500 FRAME=1,GCOUNT(1)
C
C Read in input data.
C
           CALL SPCRD(FRAME,BADFRM,MEMR(DATA),MEMR(EPS),MEMR(ERR),
     .		MEMR(WAVE),ISTAT)
c          CALL SPCRD(FRAME,BADFRM,MEMR(DATA),MEMR(EPS),MEMR(ERR), 
c    .          MEMR(REJECT),MEMR(EPSREJ),ISTAT)
           IF(ISTAT.NE.0)GO TO 999

C
C Perform polarimetry mode processing
C
c          CALL SPCPOL(FRAME, ROOTO, PFLAGS, GCOUNT(12), REFFIL(6), 
           CALL SPCPOL(FRAME, ROOTO, PFLAGS, GCOUNT(1), REFFIL(6), 
     .          REFFIL(15), REFFIL(27), MEMR(WAVE), MEMR(DATA), 
     .          MEMR(ERR), MEMR(EPS), BADEPS, ISTAT)
           IF(ISTAT.NE.0)GO TO 999
 500    CONTINUE
C
C deallocate dynamic memory
C
999     IF (DATA .NE. 0) CALL UDMFRE (DATA, TYREAL, ISTATS(1))
        IF (EPS .NE. 0) CALL UDMFRE (EPS, TYREAL, ISTATS(2))
        IF (ERR .NE. 0) CALL UDMFRE (ERR, TYREAL, ISTATS(3))
        IF (WAVE .NE. 0) CALL UDMFRE (WAVE, TYREAL, ISTATS(4))
        DO 102, I = 1, 4
            IF (ISTATS(I) .NE. 0) THEN
                ISTAT = 1
                CONTXT = 'ERROR deallocating dynamic memory'
                GO TO 1000
            ENDIF
102     CONTINUE
C
        IF (ISTAT .EQ. 0) GO TO 1000
C
        WRITE(CONTXT,888)FRAME
888     FORMAT('ERROR occured during processing of readout ',I6)
        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
