      SUBROUTINE ZCLRD(ROOT,PASS,DATA,EPS,ET,EPSET,ERR,
     &     UDL,EXPPKT,ISTAT)
*
*  Module number:
*
*  Module name: ZCLRD
*
*  Keyphrase:
*  ----------
*       Get next readout from input files
*  Description:
*  ------------
*       This routine reads the next readout of data from the
*       input data files .d0h, q0h, x0h, xqh, ulh.
*
*  FORTRAN name: ZCLRD.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*   <rootname>.d0h              I       data file
*   <rootname>.x0h              I       trailer file
*   <rootname>.q0h              I       data quality file
*   <rootname>.xqh              I       data quality file for .x0h
*   <rootname>,ulh              I       unique data log file
*   <rootname>.shh              I       standard header file.
*
*       All of these files must be previously opened with thier
*       image ids in common block HRSIO.
*
*  Subroutines Called:
*  -------------------
*  CDBS: zgetbt, zgrprd
*
*  SDAS: ZMSPUT
*
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1          Feb. 89 D. Lindler      Designed and coded
*	1.1		S. Hulbert	fix sclamp determination
*	2	May 91	S. Hulbert	Reprocessing headers:
*					crval1 no longer contains pkttime
*					added new data quality 
*	2.1	Sep 91	S. Hulbert	Implemented PASS flag
*       2.2     Mar 94  J. Eisenhamer   Added reading from the UDL file.
*       2.3     Oct 96  M. De La Pena   Added reading PKTTIME from UDL
*                                       for ACCUMs ;compute from .d0h for RAPID
*-------------------------------------------------------------------------------
*
* INPUT PARAMETERS
*
*       ROOT - root name of the data files
*       PASS - integer variable = 1 for first call, -1 for last
*
* OUTPUT PARAMETERS
*
*       DATA - data array (real 500x7)
*       EPS - data quality array (real 500x7)
*       ET - eng. trailer array (24x7 real)
*       EPSET - eng. trailer epsilon array (24x7 real)
*       ERR - statistical error array (500x7 real)
*       UDL - UDL log information (80x2 integer)
*       EXPPKT - PKTTIMEs (two double precision values) bounding the
*                readout
*       ISTAT - error status (integer)
*               ISTAT is set to -1 if the readout is incomplete and should
*                       be skipped
****************************************************************************
        CHARACTER*64 ROOT
        INTEGER PASS
        REAL    DATA(500,7),ET(24,7),ERR(500,7),EPS(500,7),EPSET(24,7)
        INTEGER UDL(80,2)
        INTEGER ISTAT
        DOUBLE PRECISION EXPPKT(2)
        DOUBLE PRECISION SCTODY
        PARAMETER        (SCTODY = 86400.0D0)
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
      INTEGER   RDWRIT
      PARAMETER (RDWRIT = 2)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C     END IRAF77.INC
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
C PODPS epsilons
C
        INTEGER PFILL
        PARAMETER (PFILL = 16)
        INTEGER PREED
        PARAMETER (PREED = 1)
C
C HRS epsilons
C
        INTEGER EPSFIL
        PARAMETER (EPSFIL = 800)
        INTEGER EPSRSE 
        PARAMETER (EPSRSE = 100)
C
C LOCAL VARIABLES
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        LOGICAL OK
C                                    ---> Flag if all bins found
        INTEGER YDEF0
C                                    --->Base y-deflection without offset added
        CHARACTER*80   CONTXT
        INTEGER GROUP
C                                    --->Group counter
        INTEGER I,J
C                                    --->Loop index
        INTEGER BIN
C                                    --->current bin number
        INTEGER BINNUM(7)
C                                    --->bin numbers from trailer file
        INTEGER LEFT
C                                    --->Number of groups left
        INTEGER INT
C                                    --->Just an integer values
        REAL VAL
        INTEGER ISTATS(3)
        CHARACTER*8 PNAMES(3)
        DATA PNAMES/'ERRCNT','FILLCNT','PKTTIME'/
C
C---------------------------------------------------------------------------
C
C INITIALIZATION ON FIRST CALL
C
        IF(PASS.EQ.FIRST)THEN
                GROUP=1
                READNO=1
                IF(OBSMOD.EQ.'DIR')GROUP=3
C                                     -----> Skip direct downlink pass def.
            ELSE
                READNO=READNO+1
        ENDIF
C
C LOOP ON BINS ==============================================================
C
C
C Enough groups left
C
        LEFT = GCOUNT(3)-GROUP+1
        IF(LEFT.LT.NBINS)THEN
           CALL ZMSPUT('Incomplete last readout(s), not calibrated',
     *                  STDOUT+STDERR,0,ISTAT)
           ISTAT=-1
           GO TO 1000
        ENDIF
        DO 100 BIN = 1,NBINS
C
C READ DATA
C
C       .D0H ------------------------
                CALL ZGRPRD(ROOT,3,GROUP,DATA(1,BIN),ISTAT)
                IF(ISTAT.NE.0) GO TO 1000
C
C read group parameters that change from one bin to the next
C
                CALL UHDGSI(IDS(3),PNAMES(1),ERRCNT(BIN),ISTATS(1))
                CALL UHDGSI(IDS(3),PNAMES(2),FILLCN(BIN),ISTATS(2))
                CALL UHDGSD(IDS(3),PNAMES(3),PKTIME(BIN),ISTATS(3))
                DO 7 I=1,3
                   IF(ISTATS(I).NE.0)THEN
                     CONTXT='ERROR: reading '//PNAMES(I)//
     *                          ' from the .d0h file'
                     GO TO 1000
                   ENDIF
7               CONTINUE
C       .X0H -------------------------
                CALL ZGRPRD(ROOT,5,GROUP,ET(1,BIN),ISTAT)
                IF(ISTAT.NE.0) GO TO 1000
C       .q0h -------------------------
                IF(IDS(4).GT.0)THEN
                    CALL ZGRPRD(ROOT,4,GROUP,EPS(1,BIN),ISTAT)
                    IF(ISTAT.NE.0) GO TO 1000
                 ELSE
                    DO 10 I=1,500
10                      EPS(I,BIN)=0
                ENDIF
C       .XQH --------------------------
                IF(IDS(6).GT.0)THEN
                    CALL ZGRPRD(ROOT,6,GROUP,EPSET(1,BIN),ISTAT)
                    IF(ISTAT.NE.0) GO TO 1000
                  ELSE
                    DO 20 I=1,24
20                      EPSET(I,BIN)=0
                ENDIF
                GROUP=GROUP+1
C                                    --->increment to next group
100     CONTINUE
C END READING DATA=========================================================
C
C EXTRACT BIN NUMBERS AND MAKE SURE ALL BINS ARE PRESENT
C
        OK = .TRUE.
        IF (NBINS.GT.1)THEN
            DO 200 I=1,NBINS
                IF(EPSET(24,I).NE.0)THEN
C                                           --->FILL DATA?
                        BINNUM(I)=I
                    ELSE
                        INT=ET(24,I)
                        CALL ZGETBT(INT,8,15,BINNUM(I))
                ENDIF
C
C If a bin number does not equal I then a packet must be missing
C
                IF(BINNUM(I).NE.I)THEN
C
C ERROR if invalid bin number
C
                    IF(BINNUM(I).GT.NBINS)THEN
                      WRITE(CONTXT,199)GROUP-NBINS+I
199                   FORMAT('ERROR: Invalid bin number in .X0H group',
     *                                  I8)
                      GO TO 999
                    ENDIF
                    OK = .FALSE.
                ENDIF
200         CONTINUE
        ENDIF
C
C if invalid bin found, look for first bin and reset group counter
C to point to it
C
        IF(.NOT.OK)THEN
              DO 250 I=2,NBINS
250                 IF(BINNUM(I).EQ.1)GROUP=GROUP-NBINS+I
              WRITE(CONTXT,299)READNO
299           FORMAT('ERROR: readout ',I5,
     *                  ' is missing data, it was not calibrated')
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ISTAT = -1
              GO TO 1000
         ENDIF
C
C Read next pair of groups from the UDL.
C For Rapid Readouts, however, there is not second group, so don't bother.
C 
C Read the PKTTIMEs bounding the readout from the UDLs for ACCUM mode.  
C 
C
         I=(READNO-1)*2+1
         IF(I.LE.GCOUNT(2))THEN
            CALL ZGRPRI(ROOT,2,I,UDL(1,1),ISTAT)
            IF(ISTAT.NE.0)THEN
                WRITE(CONTXT,309)I
 309            FORMAT('ERROR: Could not read group ',i3,' of UDL')
                GO TO 1000
            ENDIF
            IF(OBSMOD.EQ.'ACC')THEN
                CALL UHDGSD(IDS(2),PNAMES(3),EXPPKT(1),ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR: reading '//PNAMES(3)//
     *                     ' from the .ulh file'
                    GO TO 1000
                ENDIF
            ENDIF
            IF(I.LT.GCOUNT(2))THEN
               CALL ZGRPRI(ROOT,2,I+1,UDL(1,2),ISTAT)
               IF(ISTAT.NE.0)THEN
                   WRITE(CONTXT,309)I
                   GO TO 1000
               ENDIF
               IF(OBSMOD.EQ.'ACC')THEN
                   CALL UHDGSD(IDS(2),PNAMES(3),EXPPKT(2),ISTAT)
                   IF(ISTAT.NE.0)THEN
                       CONTXT='ERROR: reading '//PNAMES(3)//
     *                        ' from the .ulh file'
                       GO TO 1000
                   ENDIF
               ENDIF
            ENDIF
         ENDIF
C
C For Rapid Readouts use the PKTIME as the end of the exposure and subtract
C the integration time from the end exposure to obtain the start exposure.
C
         IF(OBSMOD.EQ.'DIR')THEN
             EXPPKT(1) = PKTIME(1) - (STPTIM / SCTODY)
             EXPPKT(2) = PKTIME(1)
         ENDIF
C
C get carrousel position
C
310     DO 350 I=1,NBINS
                IF(EPSET(20,I).EQ.0) THEN
			CARPOS=ET(20,I)
			GO TO 360
		ENDIF
350     CONTINUE
360     CONTINUE
C
C After 1 January 1994, side 1 carrousel positions are read from
C the side 2 encoder.  The carrousel position is still read from
C the side 1 encoder, but this value may not be accurate.  Hence, for
C side 1, we should use the command value for the side 2 encoder.
C This comes from the UDL.
C
        IF(DET.EQ.1)THEN
           CARPOS=UDL(3,1)
        ENDIF
C
C GET y-deflections using a single valid y-deflection in the eng.
C trailer and using the offsets from the UDL.  This allows some
C of the bins to have fill data in the trailer.
C
            YDEF0=-9999.0
            DO 400 I=1,NBINS
                 IF(EPSET(19,I).EQ.0)THEN
                       INT=ET(19,I)
                       CALL ZGETBT(INT,4,15,YDEF0)
                       YDEF0 = YDEF0 - VOFF(I)
		       GO TO 401
                 ENDIF
400         CONTINUE
401         IF(YDEF0.EQ.-9999.0)THEN
                 WRITE(CONTXT,499)READNO
499              FORMAT('ERROR: All bins have fill data in trailer',
     *                  ', readout',I6,' not processed')
                 CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                 ISTAT=-1
                 GO TO 1000
            ENDIF
            DO 500 I=1,NBINS
500              YDEF(I)=YDEF0+VOFF(I)
C
C reset data quality values using the HRS epsilon values
C
        DO 700 I=1,NBINS
            DO 700 J=1,500
                 IF (EPS(J,I) .EQ. PFILL) EPS(J,I) = EPSFIL
                 IF (EPS(J,I) .EQ. PREED) EPS(J,I) = EPSRSE
700     CONTINUE
C
C COMPUTE STATISTICAL ERROR ARRAY
C
        DO 600 I=1,NBINS
            DO 600 J=1,500
                 IF(EPS(J,I).NE.EPSFIL)THEN
                       VAL=DATA(J,I)
                       IF(VAL.EQ.0.0)THEN
                            ERR(J,I)=1.0
                       ELSE
                            ERR(J,I)=SQRT(VAL)
                       ENDIF
                 ELSE 
                       ERR(J,I)=0.0
                 ENDIF
600     CONTINUE
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
