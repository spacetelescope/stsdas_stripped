        SUBROUTINE ZCLDOP(ROOT,IREAD,PERIOD,TOTEXP,DELTAT,
     *                          DOPLER,ISTAT)
*
*  Module number:
*
*  Module name: ZCLDOP
*
*  Keyphrase:
*  ----------
*	Compute doppler wieghts
*
*  Description:
*  ------------
*	This routine computes the percent of time spent at each doppler
*	offset.  These are computed by dividing the observation into
*	DELTAT time segments and computing the deflection offset for each
*	segment.  The SHP packet time is used as the start of the readout
*	and the packet time of the first science packet is used as the
*	ending time of the readout
*
*  FORTRAN name: zcldop.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*	Rootname.shh		I	standard header packet.
*					used to get zero spacecraft
*					time and the starting observation
*					time
*	Rootname.d0h		I	used to get the ending observation
*					time 
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*	ztimev, zsctim
*
*  SDAS:
*	ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*	1	May 89	D. Lindler	Designed and coded
*		Sep 90	S. Hulbert	Account for non-zero spacecraft time
*					at UTC0
*		Feb 91	S. Hulbert	Corrected computation of UTC0
*	2	May 91	S. Hulbert	All packet times in MJD
*       2.1     Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* INPUTS:
*       ROOT = root file name
*       IREAD = readout
*       PERIOD = orbital period in minutes
*       TOTEXP = total exposure time all bins (seconds)
*       DELTAT = deflection update interval in 1/8 second UNITS
*
* OUTPUTS:
*       DOPLER - doppler weights.  DOPLER(i) gives the percent of
*               time spent at deflection offset i-200.
*       ISTAT - error status
*_-------------------------------------------------------------------------
        INTEGER IREAD,ISTAT
        REAL PERIOD,TOTEXP,DELTAT,DOPLER(400)
        CHARACTER*64 ROOT
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C			/HRSMOD/
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
C       DOPMAG, DOPZER - dopler magnitude and zero time
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
C   NGSDT - number of output groups for special diode files
C   READNO - readout number
C   NSOUT - number of samples in the output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
	DOUBLE PRECISION PI
	PARAMETER (PI = 3.1415926535898D0)
C
C Local variables
C
        CHARACTER*80 CONTXT
        DOUBLE PRECISION TIME1,TIME2
C                                    --->last two SHP times
        DOUBLE PRECISION ETIME
C                                    --->end time of the observation
        DOUBLE PRECISION OTIME
C                                    --->total observation time
        DOUBLE PRECISION STIME
C                                    --->start time
        INTEGER NINT
C                                    --->number of deltat intervals
        CHARACTER*64 FNAME
C                                    --->File name
        INTEGER ISHP,NSHP
C                                    --->number and current group in SHP file
        REAL SCALE
C                                    --->2*pi/peroid
        INTEGER DX
C                                    --->deflection offset
        DOUBLE PRECISION PKTSHP
C				     --->packet time from shp
        INTEGER POS,I
        REAL DMIN,DMAX
        DOUBLE PRECISION DIFF
C
C-----------------------------------------------------------------------------
C
C Initialization on first read
C
        IF(DOPMAG.EQ.0.0) GO TO 1000
C
C IF DDLINK then no change in dopler deflection after first readout
C
        IF((IREAD.GT.1).AND.(OBSMOD.EQ.'DIR')) GO TO 1000
C
C Initialize dopler array
C
        DO 3 I=1,400
            DOPLER(I)=0.0
3	CONTINUE
C
C first time through
C
        IF(IREAD.EQ.1)THEN
C
C check limits 
C
                IF((DOPMAG.GT.199.0).OR.(DOPMAG.LT.0.0))THEN
                        CONTXT='ERROR: Invalid DOPMAG in .D0H file'
                        GO TO 999
                ENDIF
                IF((PERIOD.LT.50.0).OR.(PERIOD.GT.300.0))THEN
                        CONTXT='ERROR: Invalid orbital period in CCR3'
                        GO TO 999
                ENDIF
C
C convert deltat to seconds
C
                DELTAT = DELTAT/8.0
                IF(DELTAT.LT.1.0)THEN
                        CONTXT='ERROR: DELTA-T in CCR3 is too small'
                        GO TO 999
                ENDIF
C
C convert period to seconds
C
                PERIOD = PERIOD * 60.0
                SCALE = 2.0 * PI / PERIOD
C
C initialize a few things
C
                TIME1=0.0
                TIME2=0.0
                NSHP=GCOUNT(1)
                ISHP=1
        ENDIF
C
C set the end time of the observation to the packet time of 
C the first packet in the d0h file
C
	ETIME = PKTIME(1)
C
C Find begin time from .shh file
C
10	CALL ZFNAME(ROOT,'shh',ISHP,0,FNAME)
        CALL UUOPGR(IDS(1),ISHP,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: switching groups in the .shh file '
                GO TO 999
        ENDIF
C
C get the next begin time
C
        CALL UHDGSD(IDS(1),'PKTTIME',PKTSHP,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: getting PKTTIME from '//FNAME
                GO TO 999
        ENDIF
C
C set the last begin time 
C note: time1 and time2 hold the two shp times that end up bracketing
C the end time from the d0h. time1 should end up holding the
C necessary begin time
C
	TIME2 = PKTSHP
C
C now make sure time1 has something in it the first time through
C
        IF (TIME1 .EQ. 0.0) TIME1 = TIME2
C
C if time2(shp) is past etime(d0h) then time1 is the one we want
C
        IF (TIME2 .GT. ETIME) THEN
	    STIME = TIME1
	    GO TO 100
	ENDIF
C
C if we made it here we need to try again: 
C go point to next group in the shp to be read
C
        ISHP=ISHP+1
C
C if we've gone throught all groups of the shp save the latest begin time
C and go try to finish
C
        IF(ISHP.GT.NSHP)THEN
            STIME = TIME2
            GO TO 100
        ENDIF
C
C save the last time and go get the next
C
        TIME1=TIME2
        GO TO 10
C
C compute observation length in seconds
C
100     OTIME = (ETIME - STIME) * 86400.0D0
C
C Compare observation time with total exposure time
C
        DIFF = OTIME-TOTEXP
        IF((DIFF.LT.0).OR.(DIFF.GT.5000))THEN
           CONTXT='Invalid observation time computed for'//
     *                 ' doppler correction'
           GO TO 999
        ENDIF
C
C number of intervals
C

        NINT = OTIME/DELTAT + 1
C
C compute doppler weights (put them in the 'middle' of the array)
C deltim is the number of seconds since dopzer
C
	DELTIM = (STIME - DOPZER) * 86400.
        DO 200 I=1,NINT
            DX = DOPMAG * SIN(SCALE*(I*DELTAT+DELTIM)) + 0.5
            POS = DX + 200
            DOPLER(POS)=DOPLER(POS)+1
200     CONTINUE
C
C normalize doppler weights 
C
        DO 300 I=1,400
300             DOPLER(I)=DOPLER(I)/NINT
C
        ISTAT = 0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
