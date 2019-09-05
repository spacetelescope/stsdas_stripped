        SUBROUTINE YCLPOL(FRAME,ROOT,PFLAGS,NFRAME,RETFIL,CCS4,
     *                    PCPFIL,WAVE,DATA,ERR,EPS,BAD,PEDGR1,PEDGR2,
     *                    PEDGR3,DESCR1,DESCR2,DESCR3,ISTAT)
*
*  Module number:
*
*  Module name: YCLPOL
*
*  Keyphrase:
*  ----------
*       Polarimetry processing
*
*  Description:
*  ------------
*       This routine performs the FOS polarimetry processing
*       On the first call the reference data is read.
*       On all calls the flux and errors are saved for
*       use on the last call in which the processing is done.
*
*  FORTRAN name: yclpol.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       retfil                  I       Retardation file
*       ccs4                    I       Wollaston/waveplate parameter table
*       pcpfil                  I       Post-COSTAR polarimetry file
*       <root>.c3h              O       output file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yfname, yrdret, yrccs4, ypolar, ymsput, ywshft, ypol2, ypol3,
*       yrdpcp, yclpcc, yclwrt
*
*  SDAS:
*	uhdgsd, uhdgsr
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Aug 89  D. Lindler      Designed and coded
*		May 90	S. Hulbert	PANGAPER (old/new data formats)
*		Sep 90	S. Hulbert	Added POLANG, update BUNITS
*     1.3	May 91	S. Hulbert	new headers: PANGAPER->PA_APER
*       2	Mar 92	S. Hulbert	Pass in EPS and use to check for 
*					fill data (BAD)
*     2.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*	3	Mar 94	H. Bushouse	Mods to handle NREAD > 1
*	4	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*     4.1	May 94	H. Bushouse	Changed error to warning for nwvpos<4
*     4.2	Oct 94	H. Bushouse	Modified PFLAGS indexes for new APR,
*					AIS, and TIM corrections.
*     5.0       Mar 97  M. De La Pena   Mods post-COSTAR polarimetry corrections
*     5.1       Feb 98  M. De La Pena   Changed way Pass 4 post-COSTAR computed,
*                                       & modified output messages.
*                                       Rotate pass 1-3 to proper frame before
*                                       post-COSTAR correction.
*     5.2       Apr 99  M. De La Pena   Check for post-COSTAR POLSCAN=4 data.
*                                       DO NOT apply post-COSTAR correction in
*                                       this case.  Just process as if it were
*                                       pre-COSTAR data.
*-------------------------------------------------------------------------------
*
* Inputs:
*       frame  - frame number
*       root   - root name of the output file
*       pflags - processing flags
*       nframe - number of frames
*       retfil - retardation reference file
*       ccs4   - Wollaston/waveplate parameter table
*       pcpfil - post-COSTAR polarimetry file
*       wave   - wavelength arrays
*       data   - data array
*       err    - propagated error array
*       eps    - data quality array
*       fill   - fill value above which we should reject the data
*
* Outputs:
*	pedgr1 - retardation file PEDIGREE keyword
*	pedgr2 - ccs4 PEDIGREE keyword
*       pedgr3 - post-Costar polarimetry PEDIGREE keyword
*	descr1 - retardation file DESCRIP keyword
*	descr2 - ccs4 DESCRIP keyword
*       descr3 - post-Costar polarimetry DESCRIP keyword
*       istat  - error status
*
*----------------------------------------------------------------------------
C
        CHARACTER*64 ROOT,RETFIL,CCS4,PCPFIL
	CHARACTER*68 PEDGR1,PEDGR2,PEDGR3,DESCR1,DESCR2,DESCR3
        INTEGER FRAME,NFRAME,ISTAT
        CHARACTER*8 PFLAGS(*)
        REAL DATA(*),ERR(*),WAVE(*),EPS(*)
	REAL BAD
C
C UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
        INTEGER STDOUT
        PARAMETER (STDOUT = 1)
        INTEGER STDERR
        PARAMETER (STDERR = 2)
C
C Header I/O status message: header parameter not found
C
        INTEGER UHDPNF
        PARAMETER (UHDPNF = 40)
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
C       IDS    - file id numbers
C       GCOUNT - group counts
C       NAXIS  - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL   - Fill values
C
        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL
C
C Number of quantities being modified (I, Q, U, V, I error, Q error,
C U error, V error, PL, PC, THETA, PL error, PC error, and THETA
C error)
        INTEGER NUPDAT 
        PARAMETER (NUPDAT = 14)
C
C scratch common area
C
        REAL FTAB(16,2070,2),ETAB(16,2070,2),RES1(2070,NUPDAT)
	REAL QTAB(16,2070,2)
        COMMON /YSCRTC/FTAB,ETAB,RES1,QTAB
C
C brightness units for output files
C
        CHARACTER * 20 BUNITS(10)
        COMMON /BUNITS/ BUNITS
C
C local variables
C
        REAL RES2(2070,NUPDAT),TMPRES(2070,NUPDAT)
        LOGICAL FOUND(2)
        INTEGER NPASS,I,J,K,OFFSET(2),OFF,GROUP,MAXLEN,COMBPX,NSHIFT
        INTEGER IPIX
	INTEGER WVPOS,NWVPOS
        CHARACTER*64 NAME
        REAL ALPHA(2),W1,RET(2070,3,2)
        REAL POLANG
        DOUBLE PRECISION COEF(7),PGAPER,CPGAPER
        CHARACTER*80 CONTXT
        CHARACTER*3 COLNAM(7), CHAR3
        DOUBLE PRECISION DPI, DGTORD
        DOUBLE PRECISION UCORR(2070,3),QCORR(2070,3),PHCORR(2070,3)
        DATA DPI/3.1415926536D0/
        DATA MAXLEN/2070/
        DATA COLNAM/'  A','  B',' C1',' C2',' C3',' C4',' C5'/
C-----------------------------------------------------------------------------
C
        DGTORD = DPI / 180.0D0
C
C On first pass perform error checking of configuration and read reference
C files
C
        IF(FRAME.EQ.1)THEN
C
C Check number of waveplate positions observed (number of unique waveplate
C positions is NFRAME/NREAD, since for NREAD>1, multiple frames have
C same waveplate position). Added Mar 94 by H.A.B.
C
	    NWVPOS = NFRAME / NREAD
	    WVPOS = 0
c           IF((NFRAME.LT.4).OR.(NFRAME.GT.16))THEN
	    IF((NWVPOS.LT.4).OR.(NWVPOS.GT.16))THEN
                CONTXT='WARNING: 4 to 16 waveplate positions required'//
     *                  ' for polarimetry'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		CONTXT='         processing; MOD_CORR will be skipped.'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		PFLAGS(14) = 'SKIPPED'
                GO TO 1000
            ENDIF
C
C determine y-step and position offset in DATA for each pass direction.
C
            IF(NOBJ.LT.1)THEN
                CONTXT='ERROR: Polarimetry processing specified with '//
     *                  'no object spectra avail.'
                GO TO 999
            ENDIF
            IF(NOBJ.GT.2)THEN
                CONTXT='ERROR in Polarimetry processing, more than 2'//
     *                  'object spectra'
                GO TO 999
            ENDIF
            NPASS = 0
            DO 10 I=1,3
                IF(YTYPE(I).EQ.'OBJ')THEN
                        NPASS=NPASS+1
                        OFFSET(NPASS)=(I-1)*NX
                ENDIF
10          CONTINUE
C
C Read ccs4 table
C
            CALL YRCCS4(CCS4,DET,FGWAID,POLID,ALPHA,W1,COMBPX,
     *                  COEF,PEDGR2,DESCR2,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
C
C If reference table contains dummy data, then skip processing step
C
	    IF (PEDGR2(1:5).EQ.'DUMMY') THEN
		CONTXT='WARNING: PEDIGREE = DUMMY for CCS4 '//CCS4
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		CONTXT='         Polarimetry processing will be '//
     *                 'skipped'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		GO TO 1000
	    END IF
C
C Determine if this is a post-COSTAR POLSCAN = 4 observation.  If it is,
C *DO NOT* apply the post-COSTAR correction.  Rather process this data as if
C it were a pre-COSTAR observation.  Issue a warning.
C
            IF (KYDPLY) THEN
                IF (NWVPOS .EQ. 4) THEN
                   KYDPLY = .FALSE.
                   CONTXT='This post-COSTAR FOS observation ' //
     *                    'employs POLSCAN = 4.  No correction'
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                   CONTXT='for COSTAR-induced instrumental ' //
     *                    'polarization can be made.  The '
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                   CONTXT='results contain significant instrumental ' //
     *                    'polarization.  Refer to '
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                   CONTXT='FOS Instrument Science Report 150 ' //
     *                    'for a discussion of the magnitude'
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                   CONTXT='and uncertainties of instrumental ' //
     *                    'polarization induced by COSTAR.'
		   CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                ENDIF
            ENDIF
C
C At this stage PCPFIL has been filled either with a valid file name,
C 'N/A', 'n/a', or blank.  Issue appropriate message for post-COSTAR situation.
C
            IF (KYDPLY) THEN
                CHAR3 = PCPFIL(1:3)
                IF (CHAR3 .EQ. 'n/a' .OR. CHAR3 .EQ. 'N/A' .OR. 
     *              CHAR3 .EQ. '   ') THEN
                    CONTXT='KYDEPLOY = T, but no COSTAR ' //
     *                     'polarimetry calibration specified '
		    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    CONTXT='or available for this ' //
     *                     'grating/waveplate/aperture combination.'
		    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    CONTXT='COSTAR induced instrumental '//
     *                     'polarization correction will not '
		    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		    CONTXT='be applied.'
		    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    KYDPLY = .FALSE.
                ELSE
C
C Read PCPFIL image
C If the input calibration file does not match the grating/waveplate/
C aperture combination of the input file, or there is a problem reading
C the file, or the reference image contains dummy data, then use only 
C pre-COSTAR corrections.  Reset KYDPLY to .FALSE.
C 
                    CALL YRDPCP(PCPFIL,MAXLEN,UCORR,QCORR,PHCORR,
     *                          PEDGR3,DESCR3,ISTAT)
                    IF(ISTAT.NE.0)THEN
                        CONTXT='COSTAR induced instrumental '//
     *                         'polarization correction will not '
		        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		        CONTXT='be applied.'
                        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                        KYDPLY = .FALSE.
                    ELSE IF (PEDGR3(1:5).EQ.'DUMMY') THEN
                        CONTXT='WARNING: PEDIGREE = DUMMY for ' //
     *                         'PCPFIL '// PCPFIL
                        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                        CONTXT='COSTAR induced instrumental '//
     *                         'polarization correction will not '
		        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		        CONTXT='be applied.'
                        CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                        KYDPLY = .FALSE.
                    ELSE
                        CONTXT='COSTAR induced instrumental '//
     *                         'polarization correction will be '
                        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                        CONTXT='applied for this observation.'
                        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                        CONTXT='Post-COSTAR polarization correction'//
     *                         ' file =' // PCPFIL
                        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                    ENDIF
                ENDIF
            ENDIF
C
C Report waveplate parameters
C
            CALL YMSPUT('Polarization Wollaston/Waveplate parameters:',
     *                          STDOUT,0,ISTAT)
            WRITE(CONTXT,99)ALPHA,W1
99          FORMAT('   Alpha1=',F8.5,'  Alpha2=',F8.5,'  W1=',F10.5)
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C read retardation arrays for each pass direction
C
            FOUND(1)=.FALSE.
            FOUND(2)=.FALSE.
            CALL YFNAME(RETFIL,' ',1,0,NAME)
            CALL YRDRET(NAME,MAXLEN,FOUND,RET,PEDGR1,DESCR1,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
            CALL YFNAME(RETFIL,' ',2,0,NAME)
            CALL YRDRET(NAME,MAXLEN,FOUND,RET,PEDGR1,DESCR1,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
            IF((.NOT.FOUND(1)).OR.(.NOT.FOUND(2)))THEN
                CONTXT='ERROR: Both groups of RETFILE same PASS_DIR'
                GO TO 999
            ENDIF
            CONTXT = 'Polarization retardation file ='//RETFIL
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C If reference table contains dummy data, then skip processing step
C
	    IF (PEDGR1(1:5).EQ.'DUMMY') THEN
		CONTXT='WARNING: PEDIGREE = DUMMY for '//RETFIL
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		CONTXT='         Polarimetry processing will be '//
     *                 'skipped'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		GO TO 1000
	    END IF
C
C Read PA_APER from .D0H file header
C
            CALL UHDGSD(IDS(1),'PA_APER',PGAPER,ISTAT)
            IF(ISTAT.NE.0)THEN
                 CONTXT='ERROR getting PA_APER from .d0h file'
                 GO TO 999
            ENDIF
            IF (PGAPER .LT. 0. .OR. PGAPER .GT. 360.) THEN
                 CONTXT='Invalid value for PA_APER found. '
        	 GO TO 999
            ENDIF
C
C Retain the original position angle of the aperture.
C
            CPGAPER = PGAPER
C
C If post-COSTAR observation, adjust the PGAPER angle to be in the
C instrument frame.
C
            IF (KYDPLY) PGAPER = 33.4
C
C Apply the HARTIG angle.
C
            IF(DET.EQ.'BLUE')THEN
                 PGAPER = PGAPER - 36.81
              ELSE
                 PGAPER = PGAPER + 36.81
            ENDIF
C
C Convert angle to radians.
C
            PGAPER  = PGAPER * DGTORD
C
C Read POLANG from .D0H file header
C
            CALL UHDGSR(IDS(1),'POLANG',POLANG,ISTAT)
            IF(ISTAT.NE.0)THEN
        	IF (ISTAT .EQ. UHDPNF) THEN
                        CONTXT='ERROR cannot find POLANG keyword.'//
     $        	        ' Value of 0 assumed.'
                        CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        	    POLANG = 0.0
        	ELSE 
                    CONTXT='ERROR getting POLANG from .d0h file'
                    GO TO 999
                ENDIF
            ENDIF
            IF (POLANG .LT. 0. .OR. POLANG .GT. 360.) THEN
                 CONTXT='Invalid value for POLANG found. '
        	 GO TO 999
            ENDIF
            WRITE (CONTXT,95) POLANG
 95         FORMAT('Initial angle of polarizer = ',F5.1)
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            POLANG = POLANG * DGTORD
C
C Determine wavelength shift between two pass directions
C
            IF(NPASS.GT.1)THEN
                IPIX = (COMBPX*NXSTEP)-FCHNL
                CALL YWSHFT(WAVE(OFFSET(1)+1),WAVE(OFFSET(2)+1),NX,
     *                          IPIX,NSHIFT,ISTAT)
                IF(ISTAT.NE.0)GO TO 1000
                WRITE(CONTXT,79)COMBPX,NSHIFT
79              FORMAT('Shift in wavelengths between pass dir. at diode'
     *                  ,I4,' =',I4,' pixels')
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                CONTXT = 'Coefficients for correction of interference'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                DO 90 I=1,7
                    WRITE(CONTXT,89)COLNAM(I),COEF(I)
89                  FORMAT('    ',A3,' =',G20.12)
                    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
90              CONTINUE
            ENDIF
C
C update brightness units
C
            BUNITS(5) = '        '
C
        ENDIF
C
C processing done on all calls -----------------------------------------------
C
C (only if frame is last accumulation for this waveplate position);
C  Mar 94, HAB.
C
        IF (MOD(FRAME,NREAD).EQ.0) THEN
            WVPOS = WVPOS + 1
            DO 100 I=1,NPASS
               OFF = OFFSET(I)
               DO 50 J=1,NX
                  K = OFF + J
c                 FTAB(FRAME,J,I)=DATA(K)
c                 ETAB(FRAME,J,I)=ERR(K)
c                 QTAB(FRAME,J,I)=EPS(K)
                  FTAB(WVPOS,J,I)=DATA(K)
                  ETAB(WVPOS,J,I)=ERR(K)
                  QTAB(WVPOS,J,I)=EPS(K)
50             CONTINUE
100         CONTINUE
        ENDIF
C
C Processing done on last call ------------------------------------------------
C
        IF(FRAME.EQ.NFRAME)THEN
C
C Pass 1
C
            CALL YPOLAR(NWVPOS,NX,1,FTAB(1,1,1),ETAB(1,1,1),
     *		    QTAB(1,1,1),BAD,MAXLEN,RET(1,1,1),ALPHA(1),W1,
     *		    POLANG,RES1)
C
C Copy the Pass 1 data into a temporary array
C
            DO 420 I = 1, MAXLEN
               DO 410 J = 1, NUPDAT
                  TMPRES(I,J) = RES1(I,J)
410            CONTINUE
420         CONTINUE
C
C Apply the Pass 1 post-COSTAR corrections to the temporary array
C First, rotate the data to the proper frame, then apply the
C post-COSTAR corrections.
C
            IF (KYDPLY) THEN
                CALL YPOL3(NX,MAXLEN,WAVE(OFFSET(1)+1),COEF,
     *                     PGAPER,TMPRES)
                CALL YCLPCC(TMPRES,NX,MAXLEN,NUPDAT,UCORR(1,1),
     *                      QCORR(1,1),PHCORR(1,1),ISTAT)
                IF (ISTAT .NE. 0) THEN
                    CONTXT='ERROR in applying post-COSTAR '//
     *                     'correction for Pass 1.'
                    GO TO 999
                ENDIF
            ENDIF
            DO 400 J=1, NUPDAT
               GROUP = J
               CALL YCLWRT(ROOT,GROUP,PFLAGS,TMPRES(1,J),15,'POL',
     *                     ISTAT)
               IF(ISTAT.NE.0)GO TO 1000
400         CONTINUE
C
C Pass 2
C
            IF(NPASS.GT.1)THEN
                CALL YPOLAR(NWVPOS,NX,2,FTAB(1,1,2),ETAB(1,1,2),
     *		      QTAB(1,1,2),BAD,MAXLEN,RET(1,1,2),ALPHA(2),W1,
     *		      POLANG,RES2)
C
C Copy the Pass 2 data into a temporary array
C
                DO 520 I = 1, MAXLEN
                   DO 510 J = 1, NUPDAT
                      TMPRES(I,J) = RES2(I,J)
510                CONTINUE
520             CONTINUE
C
C Apply the Pass 2 post-COSTAR corrections to the temporary array
C First, rotate the data to the proper frame, then apply the
C post-COSTAR corrections.
C
                IF (KYDPLY) THEN
                    CALL YPOL3(NX,MAXLEN,WAVE(OFFSET(1)+1),COEF,
     *                         PGAPER,TMPRES)
                    CALL YCLPCC(TMPRES,NX,MAXLEN,NUPDAT,UCORR(1,2),
     *                          QCORR(1,2),PHCORR(1,2),ISTAT)
                    IF (ISTAT .NE. 0) THEN
                        CONTXT='ERROR in applying post-COSTAR'//
     *                         ' correction for Pass 2.'
                        GO TO 999
                    ENDIF
                ENDIF
                DO 500 J=1, NUPDAT
                    GROUP = 14 + J
                    CALL YCLWRT(ROOT,GROUP,PFLAGS,TMPRES(1,J),15,'POL',
     *                          ISTAT)
                    IF(ISTAT.NE.0)GO TO 1000
500             CONTINUE
C
C Combine two pass directions
C
                CALL YPOL2(NX,MAXLEN,NSHIFT,RES1,RES2)
C
C Apply the combined post-COSTAR corrections if appropriate
C First, rotate the data to the proper frame, then apply the
C post-COSTAR corrections.
C
                IF (KYDPLY) THEN
                    CALL YPOL3(NX,MAXLEN,WAVE(OFFSET(1)+1),COEF,
     *                         PGAPER,RES1)
                    CALL YCLPCC(RES1,NX,MAXLEN,NUPDAT,UCORR(1,3),
     *                          QCORR(1,3),PHCORR(1,3),ISTAT)
                    IF (ISTAT .NE. 0) THEN
                        CONTXT='ERROR in applying post-COSTAR'//
     *                         ' correction for Combined pass.'
                        GO TO 999
                    ENDIF
                ENDIF
                CALL YMSPUT('The two pass directions combined',
     *                                   STDOUT,0,ISTAT)
                DO 600 J=1, NUPDAT
                   GROUP = 28 + J
                   CALL YCLWRT(ROOT,GROUP,PFLAGS,RES1(1,J),15,'POL',
     *                         ISTAT)
                   IF(ISTAT.NE.0)GO TO 1000
600             CONTINUE
C
C Correct for interference effect Pre-COSTAR only.  This has already been
C done for the post-COSTAR data.
C
                IF (.NOT.KYDPLY) THEN
                    CALL YPOL3(NX,MAXLEN,WAVE(OFFSET(1)+1),COEF,
     *                         PGAPER,RES1)
                ENDIF
                CONTXT='Instrumental Polarization correction performed'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C If post-COSTAR, rotate the angle of polarization counter-clockwise 
C into the sky frame.
C
                IF (KYDPLY) THEN
                    CPGAPER = (CPGAPER - 33.4) * DGTORD
                    CALL YCLROT(RES1, NX, MAXLEN, CPGAPER, ISTAT)
                    IF (ISTAT .NE. 0) THEN
                        CONTXT='ERROR in applying post-COSTAR'//
     *                         ' correction for '//
     *                         'Combined/Instrumental pass.'
                        GO TO 999
                    ENDIF
                    CONTXT='COSTAR induced polarization correction ' //
     *                     'has been applied.'
                    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                ENDIF
C
                DO 700 J=1, NUPDAT
                   GROUP = 42 + J
                   CALL YCLWRT(ROOT,GROUP,PFLAGS,RES1(1,J),15,'POL',
     *                         ISTAT)
                   IF(ISTAT.NE.0)GO TO 1000
700             CONTINUE
            ENDIF
        ENDIF
C
        ISTAT = 0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT = 1
1000    RETURN
        END
