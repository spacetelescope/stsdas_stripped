      SUBROUTINE YSCLBK(FRAME,GMLAT,NLAT,GMLONG,NLONG,BRATES,NBRATE,
     $				RBACK,SBACK,NX,IS,IY,ISTAT)
*
*  Module number:
*
*  Module name: YSCLBK
*
*  Keyphrase:
*  ----------
*       Scale reference background according to geomagnetic position
*
*  Description:
*  ------------
*       This routine calculates and applies a scale correction to the
*	reference background based on the geomagnetic postion of the
*	spacecraft at the time of observation.
*
*  FORTRAN name: ysclbk.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments

*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Sep 91	S. Hulbert      Designed and coded
*	1.1     Jan 92	S. Hulbert      Change minimum allowed value for MEAN
*	1.1.1	Jan 92	S. Hulbert	Bug Fix--average predicted background
*					rates for an ACCUM mode observation
*					Add FRAME to calling sequence
*       1.1.2   Jun 92  D. Bazell       Check YSTEP and SLICE before 
*                                       deallocating buffers
*	1.1.3	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*	1.1.4	Oct 93  H. Bushouse	Bug Fix--allocate memory for SAVRAT
*					for dimension=LASTFR, not NREAD.
*	1.2	Mar 94	H. Bushouse	Bug Fix--fix calculation of average
*					predicted bkg rates for ACCUM mode obs
*					when NREAD.ne.NFRAME (e.g. polarimetry
*					data with NREAD > 1).
*       1.3     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* INPUTS:
*	frame - frame number
*	gmlat - array of geomagnetic latitudes from table ccs8
*	nlat - number of latitudes
*	gmlong - array of geomagnetic longitudes from table ccs8
*	nlong - number of longitudes
*	brates - array of geomagnetic predicted rates from table ccs8
*	nbrate - number of rates
*	rback - reference background spectrum
*	nx - number of pixels in background spectrum
*	is - slice number
*	iy - ystep number
*
* INPUT/OUTPUT:
*	sback - scaled background spectrum
*
* OUTPUT:
*       istat - error status
*
*----------------------------------------------------------------------------
        INTEGER FRAME,NX,ISTAT,IS,IY,NLONG,NLAT,NBRATE
        DOUBLE PRECISION GMLAT(*),GMLONG(*),BRATES(*)
	REAL SBACK(*),RBACK(*) 
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
      INTEGER TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.1415926535898)
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM,HEADER,TRAILR,
     *          DEFDDT
        INTEGER DEADTM
        COMMON /CONFG5/DEADTM
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
C LOCAL VARIABLES ------------------------------------------------------
        CHARACTER*80 CONTXT
        DOUBLE PRECISION FPKTIM,EXPYST,EXPRDT
        DOUBLE PRECISION MIDTIM,XPOS,YPOS,ZPOS,LAT,LNG,ST
	DOUBLE PRECISION INTLAT,INTLNG,INTRATE
	DOUBLE PRECISION SUM,MEAN,SCALEF,TEMP
	INTEGER BRAT2D,SAVRAT,I,LASTFR,ISTATS(2)
	INTEGER CLEARS, IREAD
C
C-----------------------------------------------------------------------
C
C for first slice/ystep
C
	IF(FRAME.EQ.1)THEN
C
C we will want to watch for the last frame
C
            LASTFR= GCOUNT(1)
C
C allocate memory for brat2d
C
            CALL UDMGET (NBRATE, TYDOUB, BRAT2D, ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR allocating dynamic memory'
                GO TO 999
            ENDIF
C
C allocate memory for predicted background rates
C We need to keep track of these to handle ACCUM mode observations correctly
C
	    IF(NREAD.GT.1)THEN
                CALL UDMGET (LASTFR, TYDOUB, SAVRAT, ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR allocating dynamic memory'
                    GO TO 999
                ENDIF
	    ENDIF
C
C set up to interpolate
C
	    CALL SPLIE2(GMLAT,GMLONG,BRATES,NLAT,NLONG,MEMD(BRAT2D))
C
C determine exposure time for each ystep and readout in days
C
            EXPYST = (LIVETM+DEADTM)*7.8125e-6*OVERSN*NXSTEP*
     $                NPAT*INTS/24./3600.
            EXPRDT = EXPYST * YSTEPS * SLICES
C
	ENDIF
C----------------- end of frame 1 only processing -----------------------------
C
C compute observation time midpoint of each ystep/slice
C
C get first packet time from the d0h file
C
        CALL UHDGSD(IDS(1),'FPKTTIME',FPKTIM,ISTAT)
C
C Don says: "if npat greater then 1 then all ysteps and slices
C for a readout will be at the midpoint of the readout. If npat=1 then
C the readout will be divided into YSTEPS*SLICES intervals"
C
        IF(NPAT.GT.1)THEN
            MIDTIM=FPKTIM-EXPRDT/2.
        ELSE
	    I=(IS-1)*YSTEPS+IY
            MIDTIM=FPKTIM-EXPRDT+(2.*I-1.)/2.*EXPYST
        ENDIF
C
C compute spacecraft position at midpoint 
C
        CALL HSTPOS(MIDTIM,1,XPOS,YPOS,ZPOS,LNG,LAT,ST,ISTAT)
	IF(ISTAT.NE.0)THEN
	    CONTXT='ERROR calculating geographic position'
	    GO TO 999
	ENDIF
C
C compute goemagnetic position
C
        CALL GEOPOS(LAT,LNG,INTLAT,INTLNG,ISTAT)
	IF(ISTAT.NE.0)THEN
	    CONTXT='ERROR calculating geomagnetic position'
	    GO TO 999
	ENDIF
C
C interpolate in predicted rates 
C 
	CALL SPLIN2(GMLAT,GMLONG,BRATES,MEMD(BRAT2D),NLAT,NLONG,INTLAT,
     $			INTLNG,INTRATE)
	IF(INTRATE.LT.0)INTRATE=0.
C
C scale background
C
	SUM=0.
	DO 110 I=1,NX
	    SUM=SUM+RBACK(I)
110	CONTINUE
	MEAN=SUM/NX
C
C compute average predicted countrate for ACCUM mode;
C modified Mar-94 by HAB to fix case where NREAD.ne.NFRAME.
C calculate readout index (IREAD) relative to last memory clear
C
	IF(NREAD.GT.1)THEN
	    CLEARS = (FRAME-1)/NREAD
	    IREAD  = FRAME - CLEARS*NREAD
C	    MEMD(SAVRAT+FRAME-1)=INTRATE
	    MEMD(SAVRAT+IREAD-1)=INTRATE
	    TEMP=0.
C	    DO 205 I=1,FRAME
	    DO 205 I=1,IREAD
		TEMP=TEMP+MEMD(SAVRAT+I-1)
205	    CONTINUE
C	    INTRATE=TEMP/FRAME
	    INTRATE=TEMP/IREAD
	ENDIF
C
C scale factor based on mean count rate
C
	IF (MEAN.GT.1.0D-7)THEN
	    SCALEF=INTRATE/MEAN
	ELSE
	    SCALEF=1.
	ENDIF
C
	DO 120 I=1,NX
	    SBACK(I)=RBACK(I)*SCALEF
120	CONTINUE
C
C
C clean up if all done.  This means we are on the last frame the last 
C ystep and the last slice.
C
        IF ((FRAME.EQ.LASTFR).AND.(IY.EQ.YSTEPS).AND.
     $       (IS.EQ.SLICES)) THEN
	    IF (BRAT2D.NE.0) CALL UDMFRE (BRAT2D, TYDOUB, ISTATS(1))
            IF (SAVRAT.NE.0) CALL UDMFRE (SAVRAT, TYDOUB, ISTATS(2))
	    DO 165 I = 1, 2
                IF (ISTATS(I).NE.0) THEN
                    CONTXT = 'Error deallocating dyanmic memory'
                    GO TO 999
                ENDIF
165         CONTINUE
        ENDIF

C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
1000    RETURN
        END
