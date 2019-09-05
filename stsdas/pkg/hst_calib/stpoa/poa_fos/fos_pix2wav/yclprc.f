C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLPRC(ROOT,ROOTO,PIXTAB,SUB_PIX,POAFILE,SPECNUM,
     *                    GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: yclprc
*
*  Keyphrase:
*  ----------
*       process FOS data
*
*  Description:
*  ------------
*       This routine performs the calibration of FOS data.
*       The processing steps are controled by the processing flags
*       found in the .d0h header.  They are read into the boolean array
*       pflags with the following elements:
*                 pflags(1) - convert to count rates
*			(2) - offset correction (GIMP, GIMPerror,...)
*                       (3) - deadtime correction (paired pulse)
*                       (4) - background subtraction
*			(5) - scattered light subtraction
*                       (6) - flat fielding
*                       (7) - sky subtraction
*                       (8) - wavelength assignments
*                       (9) - conversion to absolute flux units (old method)
*			(10) - aperture throughput and focus corrections
*			(11) - conversion to absolute flux units (new method)
*			(12) - sensitivity degradation correction
*                       (13) - output statistical error array
*                       (14) - special statistics processing
*                       (15) - scale reference background    
*
*       Refer to the driver routine (yclfos) for additional details.
*
*  FORTRAN name: yclprc.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       SEE YCLFOS
*
*  Subroutines Called:
*  -------------------
*
*       ymsput, yclopn, yopd0h, yrccs3, yclrd, yclwrt, yosize
*       ycldqi, yclexp, yclflt, yclppc, yclbck, yclsct, yclsky
*       yclivs, yclais, yclwav, ygtref, yconfg, ypflags, yclmod
*	yclinq, yrccs1, yclerr, ycloff, yclapr, yclfcs, ycltim,
*	udmget, udmfre
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
*					Added some dynamic memory allocation
*     3.1       May 91  S. Hulbert      Added y-offset to GIMP correction
*       4       Aug 91  S. Hulbert      Added scaling of reference background
*					based on geomagnetic position
*       5       Mar 92  S. Hulbert      Check istat from call to yclerr
*     5.1	Apr 93	H. Bushouse	Added declaration of local variable I
*       6	Mar 94	H. Bushouse	Added scattered light subtraction
*	7	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*	8	Oct 94	H. Bushouse	Add aperture throughput, focus, and
*					sens.degradation corrections and new
*					absolute flux conversion (YCLAPR,
*					YCLFCS, YCLAIS, YCLTIM)
*       9       Dec 95  J. Eisenhamer   Skip AIS stuff if file is not present.
*                                       Skip AIS stuff if WAV_CORR is not done.
*      10       Dec 95  J. Eisenhamer   Modified yclsct to get EPS array.
*      11       Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1 and mods for
*                                       post-COSTAR polarimetry corrections
*     /////////////////////////////////////////////////////////////////////
*            Start of Post Operational Archive versions (fos_pix2wav)
*     /////////////////////////////////////////////////////////////////////
*     1         Jun 01  A. Alexov         Created from poa_calfos code
*     2         Jun 01  A. Alexov         Took out uncessary steps for wavecal
*     3         Oct 01  A. Alexov         Added poafile, subpix and specnum as
*                                         new input params
*-------------------------------------------------------------------------------
*
* INPUTS:
*       root - rootname of input files
*       rooto - rootname of the output wavelength file
*       pixtab - output pixel table name
*       sub_pix - switch whether or not to perform the sub-pixel correction      
*       poafile - rootname of the poa file
*       specnum - the spectral number from which to use the poa sub-pix value
*       grndmd - ground mode
*
* OUTPUTS:
*       istat - error status
*
        CHARACTER*18 GRNDMD
        CHARACTER*64 ROOT,ROOTO,PIXTAB,POAFILE
        INTEGER SPECNUM
        LOGICAL SUB_PIX
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
	INTEGER SCTVAL, SCTERR
	COMMON /SCTCOR/ SCTVAL, SCTERR
C
	CHARACTER*68 PEDGRE(27), DESCRP(27)
	COMMON /HDKEYS/ PEDGRE, DESCRP, REFFIL

c the POA orbital computation yields new group parameters 
      REAL*8 MIDTIMP,POAXP,POAYP,POAZP,VXP,VYP,VZP,BNP,BEP,BDP,
     a       BV1P,BV2P,BV3P,BDXP,BDYP,BDZP,YGMPXSCL,YGMPYSCL,
     b       YOFFXP,YOFFYP,YYBASE0,YYBSXSCL,YMEANTMP,YTMPXSCL,YAPGRTX0,
     c       YXCEN,GMSTP,GLNGP,GLATP,MLNGP,MLATP,ALNGP,ALATP,LSHP,
     d       PVX,PVY,PVZ
      COMMON /POA_P/ MIDTIMP,POAXP,POAYP,POAZP,VXP,VYP,VZP,BNP,BEP,BDP,
     *       BV1P,BV2P,BV3P,BDXP,BDYP,BDZP,YGMPXSCL,YGMPYSCL,
     *       YOFFXP,YOFFYP,YYBASE0,YYBSXSCL,YMEANTMP,YTMPXSCL,YAPGRTX0,
     *       YXCEN,GMSTP,GLNGP,GLATP,MLNGP,MLATP,ALNGP,ALATP,LSHP,
     *       PVX,PVY,PVZ
c the POA header keys get placed into an ascii txt output file
      CHARACTER*64 POA_TXT   
      COMMON /POA_T/ POA_TXT
      INTEGER GLOBAL_COUNT
      COMMON /GCOUNT/ GLOBAL_COUNT
c
c the PIXTAB related global/common vars
      REAL*8 PIXPOS(3000)
      REAL*4 WAVPOS(3000)
      INTEGER PIXPOS_TOTNUM
      COMMON /PIX_POS/ PIXPOS, WAVPOS, PIXPOS_TOTNUM

C Local variables
        CHARACTER*80 CONTXT
C       --->reference file names
        CHARACTER*64 REFFIL(27)   
C       --->input group counter
C       --->processing flags
        CHARACTER*8 PFLAGS(15)
C       --->Filter widths
        CHARACTER*3 DTYPE
	CHARACTER*3 REFAPR
	INTEGER I
        INTEGER FRAME,NDATA,ISTATS(10)
        REAL BADEPS
        INTEGER DATA,EPS,ERR,REJECT,EPSREJ,WAVE
C
C FLAG for bad data points (if eps > or =badeps) point is not calibrated
C and treated as fill.
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
        CALL YCLOPN(ROOT,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
        CALL YCLOPNPIX(PIXTAB,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C GET configuration parameters
C
        CALL YCONFG(ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C GET processing flags
C
        CALL YPFLAG(GRNDMD,PFLAGS,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
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
C allocate memory for DATA, EPS, ERR, REJECT, EPSREJ, and WAVE
C
        DO 101 I = 1, 10
            ISTATS(I) = 0
101     CONTINUE
        CALL UDMGET (NDATA, TYREAL, DATA,   ISTATS(1))
        CALL UDMGET (NDATA, TYREAL, EPS,    ISTATS(2))
        CALL UDMGET (NDATA, TYREAL, ERR,    ISTATS(3))
        CALL UDMGET (NDATA, TYREAL, REJECT, ISTATS(4))
        CALL UDMGET (NDATA, TYREAL, EPSREJ, ISTATS(5))
        CALL UDMGET (NDATA, TYREAL, WAVE, ISTATS(6))

C
C allocate memory for XOFFS and YOFFS (OFF_CORR correction)
C *poa* allways allocate , since computation is always done, 
C       decision whether or not to apply correction is done in ycloff
         CALL UDMGET (NSPEC, TYREAL, XOFFS, ISTATS(7))
         CALL UDMGET (NSPEC, TYREAL, YOFFS, ISTATS(8))
C
C no need to allocate memory for SCTVAL and SCTERR 
C
            SCTVAL = 0
            SCTERR = 0
C
C check for errors in memory allocation
C
        DO 100 I = 1, 10
            IF (ISTATS(I) .NE. 0) THEN
                ISTAT = 1
                CONTXT = 'ERROR allocating dynamic memory'
                GO TO 999
            ENDIF
100     CONTINUE
C
C initialize memory
C
        DO 110 I = 1, NDATA
           MEMR(DATA+I-1) = 0.
           MEMR(EPS+I-1) = 0.
           MEMR(ERR+I-1) = 0.
           MEMR(REJECT+I-1) = 0.
           MEMR(EPSREJ+I-1) = 0.
           MEMR(WAVE+I-1) = 0.
 110    CONTINUE
        DO 111 I = 1, NSPEC
           IF (XOFFS.NE.0) MEMR(XOFFS+I-1) = 0.
           IF (YOFFS.NE.0) MEMR(YOFFS+I-1) = 0.
           IF (SCTVAL.NE.0) MEMR(SCTVAL+I-1) = 0.
           IF (SCTERR.NE.0) MEMR(SCTERR+I-1) = 0.
 111    CONTINUE
C
C read aperture position table
C
       CALL YRCCS1(REFFIL(12),DET,FGWAID,APERID,
     $              PEDGRE(12),DESCRP(12),ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C determine output data sets sizes
C
       CALL YOSIZE(GRNDMD,PFLAGS,DTYPE,ISTAT)
        IF(ISTAT.NE.0)GO TO 999

C Open the poafile, read offset values and determine sub-pix offset

       IF (SUB_PIX) THEN
          CALL YCLOPNPOA(POAFILE,SPECNUM,NXSTEP,ISTAT)
          IF(ISTAT.NE.0) GO TO 999
       ENDIF
C
C Loop on input readouts (= processing is done per group)
C
        GLOBAL_COUNT=0
        REFAPR='ANY'
        DO 500 FRAME=1,GCOUNT(1)
C
C read data
C
                CALL YCLRD(FRAME,MEMR(DATA),MEMR(EPS),MEMR(REJECT),
     $                        MEMR(EPSREJ),ISTAT)
                IF(ISTAT.NE.0)GO TO 999
C
C Compute wavelengths for first frame only, they remain the same
C for subsequent frames
C
                 CALL YCLWAV(REFFIL(17),NDATA,MEMR(WAVE),PEDGRE(17),
     *                       DESCRP(17),GRNDMD,ISTAT)
                 IF(ISTAT.NE.0)GO TO 999
C write the wavtab output file, now that the wavlengths have 
C been calulated
                 CALL WRTWAVTAB(ROOTO,PIXTAB,ISTAT)
                    IF(ISTAT.NE.0)GO TO 999  
C

500     CONTINUE
C
C write reference file pedigree info to processing log
C
CCC	CALL YWRPED (PFLAGS,REFFIL,PEDGRE,DESCRP,GRNDMD,ISTAT)
CCC	IF (ISTAT.NE.0) GO TO 999
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
        IF (SCTVAL .NE. 0) CALL UDMFRE (SCTVAL, TYREAL, ISTATS(9))
        IF (SCTERR .NE. 0) CALL UDMFRE (SCTERR, TYREAL, ISTATS(10))
        DO 102, I = 1, 10
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
