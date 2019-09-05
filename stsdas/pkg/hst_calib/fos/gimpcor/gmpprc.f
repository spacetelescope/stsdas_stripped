        SUBROUTINE GMPPRC(ROOT,ROOTO,GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: gmpprc
*
*  Keyphrase:
*  ----------
*       process FOS data
*
*  Description:
*  ------------
*       NOTE: This routine now only basically peforms the GIMP
*       calculation.  This was ripped from CALFOS.  Hence, many of
*       the comments are still in tack.
*
*       Refer to the driver routine (gmpfos) for additional details.
*
*  FORTRAN name: gmpprc.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       SEE GMPFOS
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
*       1       Jul 89  D. Lindler      Designed and coded
*	2	Sep 90	S. Hulbert	Enhanced DQI
*	3	May 91	S. Hulbert	Added GIMP correction. Set up for
*					new headers. Fixed bug in determining
*					size of output files
*					Renumbered pflags
*					Added dymanic some memory allocation
*     3.1       May 91  S. Hulbert      Added y-offset to GIMP correction
*       4       Aug 91  S. Hulbert      Added scaling of reference background
*					based on geomagnetic position
*       5       Mar 92  S. Hulbert      Check istat from call to yclerr
*     4.0       Feb 93  J. Eisenhamer   Tore apart calfos just to get gimp
*     4.1     Dec 2000    M. De La Pena Added KYDPLY to CONFG1 common.
*-------------------------------------------------------------------------------
*
* INPUTS:
*       root - rootname of input files
*       rooto - rootname of the output files
*       grndmd - ground mode
*
* OUTPUTS:
*       istat - error status
*
        CHARACTER*18 GRNDMD
        CHARACTER*64 ROOT,ROOTO
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
        LOGICAL HEADER,TRAILR,DEFDDT
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
C        COMMON /GMPOFF/ XOFFS, YOFFS, NSPEC
C
C Local variables
C
        CHARACTER*64 REFFIL(20)
C                                    --->reference file names
        CHARACTER*80 CONTXT
C                                    --->input group counter
        CHARACTER*8 PFLAGS(15)
C                                    --->processing flags
C        INTEGER BCKMD,BCKMN,SKYMD,SKYMN
C                                    --->Filter widths
        INTEGER FRAME,NDATA,ISTATS(8)
        REAL BADEPS
        INTEGER DATA,EPS,ERR,REJECT,EPSREJ,WAVE
	CHARACTER*68 PEDGR1, PEDGR2, DESCR1, DESCR2
C
C FLAG for bad data points (if eps > or =badeps) point is not calibrated
C and treated as fill.
C
        DATA BADEPS/200.0/
C-----------------------------------------------------------------------------
C
C OPEN input files
C
        FRAME=1
        CALL YCLOPN(ROOT,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C GET configuration parameters
C
        CALL YCONFG(ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C GET processing flags
C
        CALL GMPFLG(GRNDMD,PFLAGS,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
c        IF((GRNDMD.EQ.'SPECTROPOLARIMETRY').AND.(GCOUNT(1).LT.4))
c     *                                  PFLAGS(10)="OMIT"
C
C GET reference file names
C
        CALL YGTREF(PFLAGS,GRNDMD,REFFIL,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C Check that all reference tables and files exist
C
        CALL YCLINQ(REFFIL,PFLAGS,GRNDMD,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C calculate number of pixels per frame
C and the number of spectra per frame
C
        NDATA = (NCHNLS+OVERSN-1)*NXSTEP*YSTEPS*SLICES
        NSPEC = YSTEPS*SLICES
C
C allocate memory for DATA,EPS,ERR,REJECT,EPSREJ,WAVE
C
        DO 101 I = 1, 8
            ISTATS(I) = 0
101     CONTINUE
        CALL UDMGET (NDATA, TYREAL, DATA, ISTATS(1))
        CALL UDMGET (NDATA, TYREAL, EPS, ISTATS(2))
        CALL UDMGET (NDATA, TYREAL, ERR, ISTATS(3))
        CALL UDMGET (NDATA, TYREAL, REJECT, ISTATS(4))
        CALL UDMGET (NDATA, TYREAL, EPSREJ, ISTATS(5))
c        IF (PFLAGS(7)) THEN
c            CALL UDMGET (NDATA, TYREAL, WAVE, ISTATS(6))
c        ELSE
            WAVE = 0
c        ENDIF
c        IF (PFLAGS(2)) THEN
            CALL UDMGET (NSPEC, TYREAL, XOFFS, ISTATS(7))
            CALL UDMGET (NSPEC, TYREAL, YOFFS, ISTATS(8))
c        ELSE
c            XOFFS = 0
c            YOFFS = 0
c        ENDIF
C
        DO 100 I = 1, 8
            IF (ISTATS(I) .NE. 0) THEN
                ISTAT = 1
                CONTXT = 'ERROR allocating dynamic memory'
                GO TO 999
            ENDIF
100     CONTINUE
C
C read aperture position table
C
        CALL YRCCS1(REFFIL(12),DET,FGWAID,APERID,PEDGR1,DESCR1,ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C read filter widths for background/sky smoothing
C
c        IF(PFLAGS(4).OR.PFLAGS(6))THEN
c            CALL YRCCS3(REFFIL(14),DET,BCKMD,BCKMN,SKYMD,SKYMN,ISTAT)
c            IF(ISTAT.NE.0)GO TO 999
c        ENDIF
C
C Loop on input readouts
C
        DO 500 FRAME=1,GCOUNT(1)
C
C read data
C
                CALL YCLRD(FRAME,MEMR(DATA),MEMR(EPS),MEMR(REJECT),
     $                        MEMR(EPSREJ),ISTAT)
                IF(ISTAT.NE.0)GO TO 999
C
C Compute statistical errors for raw data
C
                CALL YCLERR(MEMR(DATA),MEMR(EPS),NDATA,FILL(1),
     $                        MEMR(ERR),ISTAT)
                IF(ISTAT.NE.0)GO TO 999
C
C perform data quality initialization
C
                CALL YCLDQI(FRAME,REFFIL(8),REFFIL(9),FILL(1),
     $               MEMR(EPS),PEDGR1,PEDGR2,DESCR1,DESCR2,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
C
C GIMP correction
C
c                IF (PFLAGS(2)) THEN
                    CALL GPOFF(FRAME,REFFIL(18),REFFIL(19),MEMR(XOFFS),
     $                      MEMR(YOFFS),NSPEC,MEMR(DATA),
     $                      MEMR(ERR),MEMR(EPS),NDATA,ISTAT)
                    IF(ISTAT.NE.0)GO TO 999
c        	ENDIF
C
500     CONTINUE
C
C deallocate dynamic memory
C
999     IF (DATA .NE. 0) CALL UDMFRE (DATA, TYREAL, ISTATS(1))
        IF (EPS .NE. 0) CALL UDMFRE (EPS, TYREAL, ISTATS(2))
        IF (ERR .NE. 0) CALL UDMFRE (ERR, TYREAL, ISTATS(3))
        IF (REJECT .NE. 0) CALL UDMFRE (REJECT, TYREAL, ISTATS(4))
        IF (EPSREJ .NE. 0) CALL UDMFRE (EPSREJ, TYREAL, ISTATS(5))
        IF (WAVE .NE. 0) CALL UDMFRE (WAVE, TYREAL, ISTATS(6))
        IF (XOFFS .NE. 0) CALL UDMFRE (XOFFS, TYREAL, ISTATS(7))
        IF (YOFFS .NE. 0) CALL UDMFRE (YOFFS, TYREAL, ISTATS(8))
        DO 102, I = 1, 8
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
