C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLBCK(FRAME,NAME1,NAME2,ROOT,PFLAGS,FILL,
     $			  MED,MEAN,DATA,EPS,ERR,
     $                    PEDGR1,PEDGR2,DESCR1,DESCR2,GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: YCLBCK
*
*  Keyphrase:
*  ----------
*       subtract background.
*
*  Description:
*  ------------
*       This routine subtracts the background from SKY and OBJECT
*       spectra. If no background was taken, a default reference
*       background is used.  The default reference background may
*	be scaled to a mean expected count rate based on the 
*	geomagnetic position of the spacecraft at the time of 
*	observation. The observed background is smoothed
*       with a median, followed by a mean filter before subtraction.
*       No smoothing is done to the reference file background if used.
*
*  FORTRAN name: yclbck.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       NAME1                   I       Background reference file
*       NAME2                   I       Predicted background count rates
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       YMSPUT, YRDBAC, YRCCS8, YCLWRT, YGTORB, YCLREP, YMEDN, YMEAN, YSCLBK
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       AUG 89  D. LINDLER      DESIGNED AND CODED
*	2	Aug 91	S. Hulbert	Added scaling of reference background
*	2.1	Jan 92	S. Hulbert	Do not send informational messages to
*					STDERR
*	2.1.1	Jan 92	S. Hulbert	Bug Fix--Add FRAME to calling sequence
*					of YSCLBK to fix bug in YSCLBK
*	2.1.2	Apr 93	H. Bushouse	Bug Fix--Fixed typo: variable LASTFR
*					was mistyped as LASTRM.
*					Upgrade array declarations to conform
*					to F77 standards; change (1) to (*)
*					for arrays passed in/out.
*	2.1.3	Oct 93  H. Bushouse	Bug Fix--Check that allocation has
*					taken place before deallocating GMLAT,
*					GMLONG, and BRATES.
*	2.1.4 	Mar 94	H. Bushouse	Modify PFLAGS indexing for SCT_CORR
*	3	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*	3.1	Oct 94	H. Bushouse	Modify PFLAGS indexing for new APR, 
*					AIS, and TIM corrections.
*       3.2     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*     /////////////////////////////////////////////////////////////////////
*            Start of Post Operational Archive versions (poa_calfos)
*     /////////////////////////////////////////////////////////////////////
*     1.2       Jun 01  A. Alexov       Adding in POA common block 
*                                       Adding GRDMODE input for SPECPHOTO
*                                       special processing to replace DEACCUM
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       name1 - reference file name for default background
*       name2 - reference table name for predicted background count rates
*       root - root name of the output file
*       pflags - processing flags
*       fill - value of epsilon for data to not calibrate
*       med - median filter width
*       mean - mean filter width
*       grndmd - mode of the data
*
* INPUT/OUTPUT:
*       data - data array
*       eps - data quality array
*       err - propagated statistical errors
*
* OUTPUTS:
*	pedgr1 - BACHFILE PEDIGREE keyword
*	pedgr2 - CCS8 PEDIGREE keyword
*	descr1 - BACHFILE DESCRIP keyword
*	descr2 - CCS8 DESCRIP keyword
*       istat - error status
*

c the POA common block, for SHELL parameter in background scaling (LSHP)
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

c the POA dark correction-related common block
      INTEGER CURRENT_GPNUM
      COMMON  /DARK_GP/ CURRENT_GPNUM
      REAL*8  PREV_SCALE
      COMMON  /DARK_SC/ PREV_SCALE

        INTEGER FRAME,MED,MEAN,ISTAT
        CHARACTER*64 NAME1,NAME2,ROOT
	CHARACTER*68 PEDGR1,PEDGR2,DESCR1,DESCR2
	CHARACTER*8 PFLAGS(*)
        REAL FILL,DATA(*),EPS(*),ERR(*)
        CHARACTER*18 GRNDMD
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
        REAL XFILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,XFILL
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
C LOCAL VARIABLES
C
        INTEGER IYBACK,IPOS,IY,IS,I,IOFF,K,ISTATS(3)
        REAL RBACK(5000),SBACK(5000),C7H_SBACK(5000)
C                                    --->smooth background arrays
	INTEGER GMLAT,GMLONG,BRATES
C				     --->pointers to latitudes,longitudes,rates
        INTEGER MASK(5000)
C                                    --->mask for unsmoothed regions
        INTEGER FMASK(5000)
C                                    --->mask of background fill regions
	INTEGER MAXLEN,NBRATE,NLAT,NLONG
	INTEGER LASTFR
        DOUBLE PRECISION SUM
        REAL AVE
        CHARACTER*80 CONTXT
C
C FLAG for fill data (repaired) in background
        REAL EPSFIL
        DATA EPSFIL/120/
        DATA MASK/5000*0/
	DATA MAXLEN/5000/
C----------------------------------------------------------------------------
        IF((NOBJ.EQ.0).AND.(NSKY.EQ.0))GO TO 1000
C
C read reference files or prepare observed background on first call
C
        IF(FRAME.EQ.1)THEN
	   LASTFR = GCOUNT(1)
C				---> keep track of last frame
C
C read reference file background if none taken with the observation
C
            IF(NBCK.EQ.0)THEN
                CALL YRDBAC(NAME1,MAXLEN,SBACK,PEDGR1,DESCR1,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
                CONTXT='Background taken from reference file '//NAME1
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C if the reference file contains dummy data, then skip the correction
C
		IF (PEDGR1(1:5).EQ.'DUMMY') THEN
		   CONTXT='WARNING: PEDIGREE = DUMMY for '//NAME1
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		   CONTXT='         Background subtraction will be '//
     *                    'skipped'
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		   GO TO 1000
		END IF
C
C get predicted count rates used to scale reference background 
C
 		IF(PFLAGS(15).EQ.'PERFORM')THEN
		    CALL YRCCS8(NAME2,DET,GMLAT,NLAT,GMLONG,NLONG,
     $				BRATES,NBRATE,PEDGR2,DESCR2,ISTAT)
                    IF(ISTAT.NE.0)GO TO 1000
                    CONTXT='Reference background scaled using '//NAME2
                    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C if the reference table contains dummy data, then skip the scaling
C
		    IF (PEDGR2(1:5).EQ.'DUMMY') THEN
		       CONTXT='WARNING: PEDIGREE = DUMMY for '//NAME2
		       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		       CONTXT='         Background scaling will be '//
     *                    'skipped'
		       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		       PFLAGS(15)='SKIPPED'
C
C reset size of c7h output file and write (unscaled) ref background
C
		       GCOUNT(19)=1
		       CALL YCLWRT(ROOT,1,PFLAGS,SBACK,19,'RBK',ISTAT)
                       IF(ISTAT.NE.0)GO TO 1000
		       GO TO 310
		    END IF

C
C get orbital information -- we don't know if we have already done this
C for the gimp correction
C
		    CALL YGTORB(ISTAT)
                    IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR reading orbital data'
                	CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
			GO TO 1000
		    ENDIF
C
C store reference background in RBACK prior to scaling
C
		    DO 260 I=1,NX
			RBACK(I)=SBACK(I)
260		    CONTINUE
		ELSE 
C
C Write the unscaled reference background to c7h output file
C
300                 CALL YCLWRT(ROOT,1,PFLAGS,SBACK,19,'RBK',ISTAT)
                    IF(ISTAT.NE.0)GO TO 1000
		ENDIF
C
C flag reference background as having no fill data
C
310             DO 20 I=1,NX
                     FMASK(I)=0
20              CONTINUE
C
C--- Use the observed background spectrum:
C
            ELSE
C
C Determine the y-step of the observed background
C
                DO 30 I=1,3
                        IF(YTYPE(I).EQ.'BCK')IYBACK=I
30              CONTINUE
                WRITE(CONTXT,99)IYBACK
99              FORMAT('Background taken from ystep',I4)
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            ENDIF
        ENDIF
C----------------- end of frame 1 only processing -----------------------------
C
C loop on slices
C
        DO 500 IS=1,SLICES
C
C If not reference file background, smooth the observed background
C
            IF(NBCK.GT.0)THEN
                IPOS = (IS-1)*NX*YSTEPS + (IYBACK-1)*NX + 1
C
C repair the background
C
                CALL YCLREP('background',FILL,NX,FRAME,IS,DATA(IPOS),
     *                          EPS(IPOS),ISTAT)
C
C flag repaired points
C
                IPOS = IPOS-1
                DO 40 I=1,NX
                    FMASK(I)=0
                    IF(EPS(IPOS+I).GE.FILL)FMASK(I)=1
40              CONTINUE

C
C Don't subtract bad background
C
                IF(ISTAT.NE.0)GO TO 500
C
C Perform median filter
C
                IF(MED.GT.1)THEN
                    CALL YMEDN(DATA(IPOS+1),NX,MASK,MED,SBACK,ISTAT)
                  ELSE
                    DO 45 I=1,NX
                        SBACK(I)=DATA(I+IPOS)
45                  CONTINUE
                ENDIF
C
C perform mean filter
C
                IF(MEAN.GE.NX)THEN
C
C just average all points
C
                    SUM = 0.0
                    DO 50 I=1,NX
                        SUM = SUM + SBACK(I)
50                  CONTINUE
                    AVE = SUM/NX
                    DO 55 I=1,NX
                        SBACK(I)=AVE
55                  CONTINUE
                ELSE
C
C filter
C
                    IF(MEAN.GT.1)THEN
                        CALL YMEAN(SBACK,NX,MASK,MEAN,RBACK,ISTAT)
                        CALL YMEAN(RBACK,NX,MASK,MEAN,SBACK,ISTAT)
                    ENDIF
                ENDIF
            ENDIF
C ***************************
C
C Loop on ysteps
C
            DO 100 IY=1,YSTEPS
C
C if reference background, scale it. SBACK will have the scaled background;
C for spec mode, save a C7H_SBACK scaled copy, for writing to output file
C
		IF(NBCK.EQ.0.AND.PFLAGS(15).EQ.'PERFORM')THEN
		    CALL YSCLBK(FRAME,MEMD(GMLAT),NLAT,
     $				MEMD(GMLONG),NLONG,MEMD(BRATES),NBRATE,
     $				RBACK,SBACK,C7H_SBACK,NX,IS,IY,GRNDMD,ISTAT)
		    IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR scaling reference background '
                        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
		        GO TO 1000
		    ENDIF
C
C write the scaled reference background. Determine which one it is.
C
		    I=(FRAME-1)*SLICES*YSTEPS+(IS-1)*YSTEPS+IY
                    IF (GRNDMD(1:12).NE.'SPECTROSCOPY') THEN
                       CALL YCLWRT(ROOT,I,PFLAGS,SBACK,19,'RBK',ISTAT)
                       IF(ISTAT.NE.0)GO TO 1000
                    ELSE
C SPEC mode data has special background array, due to deaccume issue
                       CALL YCLWRT(ROOT,I,PFLAGS,C7H_SBACK,19,
     $                             'RBK',ISTAT)
                       IF(ISTAT.NE.0)GO TO 1000
                    ENDIF
		ENDIF
C
C determine if OBJECT or SKY
C
                IF(IY.LE.3)THEN
                    IF((YTYPE(IY).NE.'OBJ').AND.(YTYPE(IY).NE.'SKY'))
     *                                  GO TO 100
                ENDIF
C
C subtract it
C
                IOFF = (IS-1)*YSTEPS*NX + (IY-1)*NX
                DO 60 I=1,NX
                        K=IOFF + I
                        IF(EPS(K).LT.FILL)DATA(K)=DATA(K)-SBACK(I)
                        IF((FMASK(I).EQ.1).AND.(EPS(K).LT.EPSFIL))
     *                                     EPS(I) = EPSFIL
60              CONTINUE
100         CONTINUE
500     CONTINUE
C
C Print some more el junko
C
        IF((FRAME.EQ.1).AND.(NBCK.GT.0))THEN
            IF(MED.GT.1)THEN
                WRITE(CONTXT,799)MED
799             FORMAT('Background smoothed with ',I4,
     *                  ' point median filter')
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            ENDIF
            IF((MEAN.GT.1).AND.(MEAN.LT.NX))THEN
                WRITE(CONTXT,899)MEAN
899             FORMAT('Background smoothed with ',I4,
     *                  ' point mean filter done twice')
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            ENDIF
            IF(MEAN.GE.NX)THEN
                CONTXT='Average of all background points used as'//
     *                  ' global background'
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
        ENDIF
C
C clean up memory
C
	IF(FRAME.EQ.LASTFR)THEN
   	    IF (GMLAT  .NE. 0) CALL UDMFRE (GMLAT, TYDOUB, ISTATS(1))
	    IF (GMLONG .NE. 0) CALL UDMFRE (GMLONG, TYDOUB, ISTATS(2))
	    IF (BRATES .NE. 0) CALL UDMFRE (BRATES, TYDOUB, ISTATS(3))
            DO 160 I = 1, 3
                IF(ISTATS(I).NE.0)THEN
                    CONTXT='ERROR deallocating dynamic memory'
                    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    GO TO 1000
		ENDIF
160	    CONTINUE
	ENDIF
C
        ISTAT=0
1000    RETURN
        END
