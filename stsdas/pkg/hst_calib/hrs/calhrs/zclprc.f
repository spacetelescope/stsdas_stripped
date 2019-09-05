        SUBROUTINE ZCLPRC(ROOT,ROOTO,ISTAT)
*
*  Module number:
*
*  Module name: ZCLPRC
*
*  Keyphrase:
*  ----------
*       calibrate hrs data
*
*  Description:
*  ------------
*       See ZCLHRS
*
*  FORTRAN name: zclhrs.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       see zclhrs.for
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zcliac, zcldqi, zclexp, zcldio, zclppc, zcldop, zclphc, zclvig
*       zclrd, zcladc, zclair, zclhel, zclabs, zclrip, zclwrt
*       zrccr3, zgtorb
*
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*	1.1	Sep 90  S. Hulbert	change call to zclwrt
*	1.2	May 91	S. Hulbert	move background subtraction after
*					dispersion correction 
*	1.3	Sep 91	S. Hulbert	change how we flag first/last time
*					through	routines to accommodate dynamic
*					memory allocation
*       1.4     18May92 J. Eisenhamer   Added call to zexdef to properly
*                                       calibrate long scan data.
*       1.5     Feb94   J. Eisenhamer   Added the GWC_CORR step and
*                                       photocathode blemish marking.
*       1.6     Oct 96  M. De La Pena   Added FBMD,CCRE,additional parameter to
*                                       zclrd,new routines zclbmd & zgtorb,
*                                       STPTIM to HRSDEF
*       1.7     Nov 96  M. De La Pena   Added SAAFIL
*-------------------------------------------------------------------------------
*
* Inputs
*       root - root name of the data files
*	rooto - root name of output data files
* Outputs:
*       istat - error status
*-------------------------------------------------------------------------------
        CHARACTER*64 ROOT,ROOTO
        INTEGER ISTAT
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C                       /HRSDEF/
C Deflection pattern common block
C       NBINS - Number of supstep bins
C       NINIT - number of initial deflection pairs
C       IXDEF(5) - initial x-deflections
C       XDEF(7) - x-deflections for each bin
C       YDEF(7) - y-deflections for each bin
C       RCODES(7) - repeat codes for each bin
C       BINIDS(7) - substep bins ids for each bin
C       SAMPLE(7) - starting sample position for each spectra
C       LINE(7) - starting line position for each spectra
C       DELTAS(7) - sample position increment for each spectra
C       HOFF(7),VOFF(7) - horizontal and vertical offsets
C       DOPMAG, DOPZER - doppler magnitude and zero time
C       XDCAL, XDCALP - x-deflection calibration parameters
C       STPTIM - integration time at each step pattern position
C
        INTEGER NBINS,NINIT,IXDEF(5),XDEF(7),YDEF(7),RCODES(7)
        INTEGER BINIDS(7),HOFF(7),VOFF(7)
        REAL SAMPLE(7),LINE(7),DELTAS(7),DOPMAG,XDCAL,XDCALP
        DOUBLE PRECISION DOPZER,STPTIM
        COMMON /HRSDEF/ DOPZER,STPTIM,NBINS,NINIT,IXDEF,XDEF,YDEF,
     *                  RCODES,BINIDS,SAMPLE,LINE,DELTAS,HOFF,VOFF,
     *                  DOPMAG,XDCAL,XDCALP
C
C                       /HRSFLG/
C Common block containing processing flags
C
C Initial values = 'PERFORM' or 'OMIT'
C final values = 'PERFORMED' or 'OMITTED'
C
        CHARACTER*12 FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *                  FPLY,FGWC,FBMD
        COMMON /HRSFLG/FPPC,FDIO,FPHC,FVIG,FDOP,FADC,FIAC,FVAC,FHEL,
     *                  FBCK,FMNF,FMDF,FECH,FFLX,FEXP,FMAP,FDQI,FMER,
     *                  FPLY,FGWC,FBMD
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
C                       HRSREF
C  Common block containing reference file names and table relation
C  names
C
        CHARACTER*64 CCR1,CCR2,CCR3,CCR4,CCR5,CCR6,CCR7,CCR8,CCR9,
     *                  CCRA,CCRB,CCG2,DIOFIL,PHCFIL,VIGFIL,ABSFIL,
     *                  NETFIL,DQIFIL,CCRC,CCRD,CCRE,SAAFIL
        COMMON /HRSREF/ CCR1,CCR2,CCR3,CCR4,CCR5,CCR6,CCR7,CCR8,CCR9,
     *                  CCRA,CCRB,CCG2,DIOFIL,PHCFIL,VIGFIL,ABSFIL,
     *                  NETFIL,DQIFIL,CCRC,CCRD,CCRE,SAAFIL
C
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
C                         /HRSGPR/
C
C Common block for output group parameter storage
C
C        PKTIME - packet times in MJD
C        ERRCNT - error counts (.d0h)
C        FILLCN - fill counts  (.d0h)
C        SCOEF - sample coefficients
C        EXPO - exposure times
C
        INTEGER ERRCNT(7),FILLCN(7)
        REAL SCOEF(4,7),EXPO(7)
        DOUBLE PRECISION PKTIME(7)
        COMMON /HRSGPR/PKTIME,ERRCNT,FILLCN,SCOEF,EXPO
C
C                       /CCR3PR/
C
C Common block for HRS parameters from table ccr3
C
        INTEGER S0,C1,C2,SKYMNF,SKYMDF,INTMNF,INTMDF,SKYORD,INTORD
        REAL C,MINDIO,MINPHC,MINECH,MINABS,DELTAT,RATIO,PERIOD
        COMMON /CCR3PR/ PERIOD,S0,C,C1,C2,SKYMNF,SKYMDF,SKYORD,
     *                  INTMNF,INTMDF,INTORD,MINDIO,MINPHC,MINECH,
     *                  MINABS,DELTAT,RATIO

C
C HRS epsilons
C
C	EPSFIL = 800		Fill data
C	EPSDED = 400		Dead or disabled diode
C	EPSR50 = 300		Severe saturation (>50% error in ppc)
C	EPSR20 = 190		Large saturation (>20% error in ppc)
C	EPSR05 = 130		Moderate saturation (>5% error in ppc)
C	EPSRSE = 100		Reed-Solomon decoding error
C	EPSCMB =  30		Dead diode contributed to combadded data point
        INTEGER EPSFIL,EPSDED,EPSR50,EPSR20,EPS405,EPSRSE,EPSCMB
        PARAMETER(EPSFIL=800)
        PARAMETER(EPSDED=400)
        PARAMETER(EPSR50=300)
        PARAMETER(EPSR20=190)
        PARAMETER(EPS405=130)
        PARAMETER(EPSRSE=100)
        PARAMETER(EPSCMB=30)
C
C INPUT DATA BUFFERS
C
        REAL ET(24,7),EPSET(24,7)
C                                    --->eng. trailer file
        INTEGER UDL(80,2)
C                                    --->unique data log.
        REAL DATA(500,7),EPS(500,7),ERR(500,7)
C                                    --->data files
C
C OUTPUT DATA BUFFERS
C
        DOUBLE PRECISION WAVE(500,7)
        REAL EPSM(500,7),ERRM(500,7),FLUX(500,7),BCK(2000)
        INTEGER NSPEC,BCKID
C
C LOCAL VARIABLES
C
        INTEGER PASS
        INTEGER FIRST 
        PARAMETER (FIRST = 1) 
        INTEGER LAST  
        PARAMETER (LAST = -1) 
        INTEGER OTHER 
        PARAMETER (OTHER = 0) 
C
        CHARACTER*68 PEDGRE
        CHARACTER*80 CONTXT
        INTEGER I,NREADS
        INTEGER XOFF(5)
C                                    --->combaddition offsets in diodes
        INTEGER SPORD
C                                    --->spectral order
        DOUBLE PRECISION EXPPKT(2)   
C                                    --->PKTTIMEs bounding readout
C
C Percent of exposure for each inital deflection
C
        REAL PERCNT(5,7)
C
C mapping function coefficients
C
        REAL LCOEF(2)
C
C doppler wieghts
C
        REAL DOPLER(400)
C
C Extra X/Y deflections for long scans
C
        INTEGER EXXDEF, EXYDEF
C
        REAL TOTOBS
C                                    --->total observation time
        INTEGER J,K
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C Read CCR3 - detector parameters
C
        CALL ZRCCR3(CCR3,DET,ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C Get the orbital information
C
        CALL ZGTORB(ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading orbital data'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	    GO TO 999
        ENDIF
C
C Special circumstance- Need to check for X and Y Scans.  Problem is that
C there are two types of scans, short and long.  Short scans are fine.  Long
C scans require more than NBINS offsets, specifically NBINS * 33 offsets.
C However, the headers contain offsets for only the first NBINS.  Thus
C need to trap for this event and in the main loop below, adjust the X/YDEF
C for each NBINS so that the offsets are appropriate. 
C
        CALL ZEXDEF(EXXDEF,EXYDEF,ISTAT)
        IF(ISTAT.NE.0) GO TO 999
C
C Loop on readouts
C
C                            ------------------------------- Begin readout loop
        NREADS = GCOUNT(3)/NBINS
        IF(OBSMOD.EQ.'DIR') NREADS=NREADS-3
        DO 900 I=1,NREADS
C
C set PASS
C
	    IF(I.EQ.1)THEN
                PASS=FIRST
	    ELSEIF(I.EQ.NREADS)THEN
	        PASS=LAST
	    ELSE
	        PASS=OTHER
	    ENDIF
C
C initialize output arrays
C 
C
C read data
C
            CALL ZCLRD(ROOT,PASS,DATA,EPS,ET,EPSET,ERR,UDL,EXPPKT,ISTAT)
            IF(ISTAT.EQ.-1)THEN
               DO 100 K = 1, NBINS
                  DO 110 J = 1, 500
                     WAVE(J,K)=0.D0
                     FLUX(J,K)=0.
                     EPSM(J,K)=EPSFIL
                     ERRM(J,K)=0.
 110              CONTINUE
                  DO 120 J=1, 24
                     ET(J,K)=0.
                     EPSET(J,K)=EPSFIL
 120              CONTINUE
 100           CONTINUE
               DO 130 K = 1, 2000
                  BCK(K)=0.
 130           CONTINUE
               GO TO 890
            ENDIF
C                                    --->skip this readout
            IF(ISTAT.NE.0)GO TO 999
C
C if first group then create the .c1h file
C
            IF(PASS.EQ.FIRST)THEN
                CALL ZCLOPO(ROOTO,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
            ENDIF
C
C Compute exposure times and combaddition patterns for conversion
C to count rates and diode response correction
C
           IF((FEXP.EQ.'PERFORM').OR.(FDIO.EQ.'PERFORM').OR.
     *                  (FDQI.EQ.'PERFORM'))
     *          CALL ZCLPAT(READNO,ET,EPSET,XOFF,EXPO,PERCNT,
     *                                             TOTOBS,ISTAT)
C
C perform data quality initialization
C
            IF(FDQI.EQ.'PERFORM')THEN
                CALL ZCLDQI(PASS,FDQI,NINIT,XOFF,DQIFIL,DET,NBINS,
     *                  EPS,EPSET,ISTAT)
                IF(ISTAT.NE.0)FDQI='OMIT'
            ENDIF
C
C convert to count rates
C
            IF(FEXP.EQ.'PERFORM')THEN
                CALL ZCLEXP(I,NBINS,DATA,EPS,ET,EPSET,ERR,
     *                         EXPO)
            ENDIF
C
C Perform diode response correction
C
            IF(FDIO.EQ.'PERFORM')THEN
                CALL ZCLDIO(PASS,FDIO,DIOFIL,DET,MINDIO,NBINS,
     &              NINIT,XOFF,PERCNT,DATA,ET,ERR,ISTAT)
                IF(ISTAT.NE.0.AND.FDIO.EQ.'PERFORM') GO TO 999
            ENDIF
C
C perfrom paired pulse correction
C
            IF(FPPC.EQ.'PERFORM')THEN
                CALL ZCLPPC(PASS,CCG2,NBINS,DET,EPS,EPSET,DATA,ET,
     *                          ERR,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
            ENDIF
C
C perform mapping function
C
            IF(FMAP.EQ.'PERFORM')THEN
                CALL ZCLMAP(PASS,CCR1,CCR2,DET,SCOEF,LCOEF,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
            ENDIF
C
C create doppler compensation weight vector
C
            IF (DOPMAG.EQ.0.0) FDOP='OMIT'
            IF( (FDOP.EQ.'PERFORM').AND. ((FVIG.EQ.'PERFORM').OR.
     *                                    (FPHC.EQ.'PERFORM')))THEN
                CALL ZCLDOP(ROOT,I,PERIOD,TOTOBS,DELTAT,DOPLER,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
            ENDIF
C
C remove photocathode granularities
C
            IF(FPHC.EQ.'PERFORM')THEN
                CALL ZCLPHC(I,PHCFIL,FDOP,MINPHC,DOPLER,
     &              FPHC,DATA,ERR,EPS,ISTAT)
                IF(ISTAT.NE.0.AND.FPHC.EQ.'PERFORM')GO TO 999
            ENDIF
C
C remove vignetting
C
            IF(FVIG.EQ.'PERFORM')THEN
                CALL ZCLVIG(I,VIGFIL,FDOP,MINPHC,DOPLER,FVIG,
     &              EPSET,ET,DATA,ERR,EPS,ISTAT)
                IF(ISTAT.NE.0.AND.FVIG.EQ.'PERFORM')GO TO 999
            ENDIF
C
C merge data
C
            CALL ZCLMER(PASS,DATA,EPS,ERR,FLUX,EPSM,ERRM,NSOUT,NSPEC)
C
C flag photocathode blemishes
C
            IF((FDQI.EQ.'PERFORM').AND.(FMAP.EQ.'PERFORM'))THEN
               CALL ZCLBLM(PASS,CCRD,NSPEC,NSOUT,EPSM)
            ENDIF
C
C apply dispersion coefficients
C
            IF(FADC.EQ.'PERFORM')THEN
                CALL ZCLADC(I,PASS,CCR5,CCR6,CCR7,CCRC,FGWC,
     *              NSOUT,NSPEC,WAVE,SPORD,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
            ENDIF
C
C subtract background - use observed background (FMNF, FMDF, and/or
C FPLY options) or the background count rate model (FBMD)
C
            IF(FBCK.EQ.'PERFORM')THEN
                IF(FBMD.EQ.'PERFORM')THEN
                    BCKID  = 0
                    PEDGRE = ' '
                    CALL ZCLBMD(PASS,I,CCRE,SAAFIL,DET,EXPPKT,FBMD,
     *                          NSOUT,FLUX,BCK,PEDGRE,ISTAT) 
                    IF(ISTAT.NE.0)GO TO 999
                    IF(PEDGRE(1:5).EQ.'DUMMY')THEN
                        FBMD='SKIPPED'
                        FBCK='SKIPPED'
                    ENDIF
                ELSE
C
C reset filter widths if filter switches are turned off
C
                    IF(FMNF.EQ.'OMIT')THEN
                        SKYMNF=0
                        INTMNF=0
                    ENDIF
                    IF(FMDF.EQ.'OMIT')THEN
                        SKYMDF=0
                        INTMDF=0
                    ENDIF
		    IF(FPLY.EQ.'OMIT')THEN
		        SKYORD=-1
		        INTORD=-1
		    ENDIF
                    CALL ZCLBKG(I,CCRB,GRAT,CARPOS,APER,SPORD,DATA,
     $                  ET,EPS,EPSET,SKYMDF,SKYMNF,SKYORD,INTMDF,INTMNF,
     $                  INTORD,RATIO,NSOUT,EPSM,FLUX,BCK,BCKID,ISTAT)
                    IF(ISTAT.NE.0)GO TO 999
                ENDIF
            ENDIF
C
C apply incidence angle correction
C
            IF(FIAC.EQ.'PERFORM')THEN
                CALL ZCLIAC(PASS,CCR8,SPORD,NSOUT,NSPEC,WAVE,ISTAT)
                IF(ISTAT.NE.0)FIAC='OMIT'
            ENDIF
C
C perform echelle ripple correction
C
            IF(FECH.EQ.'PERFORM')THEN
                CALL ZCLRIP(PASS,CCR9,CCRA,GRAT,CARPOS,SPORD,MINECH,
     *                  NSOUT,NSPEC,FLUX,ERRM,EPSM,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
            ENDIF
C
C change to absolute flux units
C
            IF(FFLX.EQ.'PERFORM')THEN
                CALL ZCLABS(PASS,ABSFIL,NETFIL,GRAT,MINABS,NSPEC,
     *                  NSOUT,WAVE,FFLX,FLUX,ERRM,EPSM,ISTAT)
                IF(ISTAT.NE.0.AND.FFLX.EQ.'PERFORM')GO TO 999
            ENDIF
C
C heliocentric wavelength correction
C
           IF(FHEL.EQ.'PERFORM')THEN
                CALL ZCLHEL(PASS,NSOUT,NSPEC,WAVE,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
           ENDIF
C
C Perform vacuum to air corrections to the wavlengths
C
           IF(FVAC.EQ.'PERFORM')CALL ZCLAIR(PASS,NSOUT,NSPEC,WAVE)
C
C update processing flags in output header
C
           IF(PASS.EQ.FIRST)THEN
              CALL ZUDFLG(ISTAT)
              IF(ISTAT.NE.0)GO TO 999
           ENDIF
C
C write results
C
 890       CONTINUE
            CALL ZCLWRT(ROOTO,PASS,NSOUT,NSPEC,WAVE,FLUX,EPSM,
     *          ERRM,NBINS,ET,EPSET,BCK,BCKID,ISTAT)
            IF(ISTAT.NE.0)GO TO 999
C
C Increment the X/Y Deflections.
C
            DO 899 J = 1, NBINS
               XDEF(J) = XDEF(J) + EXXDEF
               YDEF(J) = YDEF(J) + EXYDEF
899         CONTINUE
C
900     CONTINUE
C                            ------------------------------ End loop on readouts
        ISTAT=0
        GO TO 1000
999     WRITE(CONTXT,9)I
9       FORMAT('ERROR: Processing aborted during readout ',I5)
        CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
