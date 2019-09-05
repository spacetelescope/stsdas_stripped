        SUBROUTINE ZGETKW(ISTAT)
*
*  Module number:
*
*  Module name: zgetkw
*
*  Keyphrase:
*  ----------
*       Get header keyword header parameters from the science data file
*  Description:
*  ------------
*       This routine gets keyword header parameters from the science
*       data file (rootname.D0H), validates them and places thier values
*       in common block HRSDEF.  If an invalid value is found processing
*       will terminate or affected processing steps will be changed
*       to OMIT.
*
*  FORTRAN name: zgetkw.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*  <rootname>.D0H               I       Reads header keyword values
*                                       of previously opened file
*                                       with id = IDS(3) in common
*                                       block HRSIO
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       none
*  SDAS:
*       ZMSPUT, uhdgs*
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jan 88  D. Lindler      Designed and coded
*	2	May 91	S. Hulbert	Reprocessing headers
*	2.1	Feb 92	S. Hulbert	Bug Fix--use RPTOBS keyword instead
*					of group paramter OBSRPT
*       2.2     Jun 95  J. Eisenhamer   Report non-nominal FINCODEs.
*       2.3     Oct 96  M. De La Pena   Added FBMD and STPTIM
*-------------------------------------------------------------------------------
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
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
C-------------------------------------------------------------------------
C
        INTEGER ISTAT
C                                    ---> RETURN STATUS CODE
C
C Local variables
C
        INTEGER I,ISTAT1,ID,IRPT,FINCOD
        CHARACTER*100 CONTXT
        CHARACTER*8 KIXDEF(5),KHOFF(6),KVOFF(6),KRPTCD(6),
     *                  KBINID(7)
        DATA KIXDEF/'IXDEF1','IXDEF2','IXDEF3','IXDEF4',
     *                  'IXDEF5'/
        DATA KHOFF/'HOFF1','HOFF2','HOFF3','HOFF4','HOFF5',
     *                  'HOFF6'/
        DATA KVOFF/'VOFF1','VOFF2','VOFF3','VOFF4','VOFF5',
     *                  'VOFF6'/
        DATA KRPTCD/'RPTCD1','RPTCD2','RPTCD3','RPTCD4',
     *                  'RPTCD5','RPTCD6'/
        DATA KBINID/'BINID1','BINID2','BINID3','BINID4',
     *                  'BINID5','BINID6','BINID7'/
C
C----------------------------------------------------------------- FINCODE
C Note: FINCODE is not stored anywhere.  However, the user should
C be told when FINCODE is not nominal, i.e. 102.  Any other value
C indicates a problem.
        ID=IDS(3)
        CALL UHDGSI(ID,'FINCODE',FINCOD,ISTAT)
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR-- unable to get FINCODE value from '//
     *          '.D0H header'
           GO TO 999
        ENDIF
        IF(FINCOD.NE.102)THEN
           WRITE(CONTXT,201)FINCOD
 201       FORMAT('WARNING: FINCODE value of ',I4,' indicates',
     *          ' observation was shortened.')
           CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           WRITE(CONTXT,203)
 203       FORMAT ('    Check each group of CQH for quality',
     *          ' of calibrated data.')
           CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           WRITE(CONTXT,205)
 205       FORMAT ('    Check EXPOSURE keyword for each group',
     *          ' of C1H for the actual exposure time.')
           CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        IF(FINCOD.LT.102.OR.FINCOD.GT.106)THEN
           WRITE(CONTXT,207)
 207       FORMAT('    Completion status of observation unknown.')
        ENDIF
        IF(FINCOD.EQ.103)THEN
           WRITE(CONTXT,209)
 209       FORMAT('    Observation ended due to under-exposure.')
        ENDIF
        IF(FINCOD.EQ.104)THEN
           WRITE(CONTXT,211)
 211       FORMAT('    Observation ended due to over-exposure.')
        ENDIF
        IF(FINCOD.EQ.105)THEN
           WRITE(CONTXT,213)
 213       FORMAT('    Observation ended due to bad data quality.')
        ENDIF
        IF(FINCOD.EQ.106)THEN
           WRITE(CONTXT,215)
 215       FORMAT('    Observation ended due to observing time',
     *          ' expiring before nominal exposure completed.')
        ENDIF
        IF(FINCOD.NE.102) CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
C
C----------------------------------------------------------------- NBINS,NINIT
C
        CALL UHDGSI(ID,'NBINS',NBINS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR-- unable to get NBINS value from '//
     *                  '.D0H header'
                GO TO 999
        ENDIF
        IF((NBINS.LT.1).OR.(NBINS.GT.7))THEN
                CONTXT='ERROR-- invalid value of NBINS in .D0H'
                GO TO 999
        ENDIF
        CALL UHDGSI(ID,'NINIT',NINIT,ISTAT)
        IF(ISTAT.NE.0)THEN
             CONTXT='ERROR -- reading value of NINIT from header,'//
     *                  ' NINIT=1 used'
             CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT1)
             NINIT=1
        ENDIF
        IF((ISTAT.EQ.0).AND.((NINIT.LT.1).OR.(NINIT.GT.5)))THEN
                CONTXT='ERROR -- Invalid value for NINIT in header,'//
     *                  ' NINIT=1 used'
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT1)
                NINIT=1
        ENDIF
C
C ---------------------------------------------------------------- IXDEF
C
        DO 10 I=1,NINIT
            CALL UHDGSI(ID,KIXDEF(I),IXDEF(I),ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- unable to get header value '//KIXDEF(I)
                GO TO 999
            ENDIF
            IF((IXDEF(I).LE.0).OR.(IXDEF(I).GT.4095))THEN
                CONTXT='ERROR -- invalid header value for '//KIXDEF(I)
                GO TO 999
            ENDIF
10      CONTINUE
C
C
C ---------------------------------------------------------------- HOFF
C
C offset for first bin is always 0
C
        HOFF(1)=0
        IF(NBINS.GT.1)THEN
          DO 30 I=2,NBINS
            CALL UHDGSI(ID,KHOFF(I-1),HOFF(I),ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- unable to get header value '//
     *                          KHOFF(I-1)
                GO TO 999
            ENDIF
            IF((HOFF(I).LE.-4096).OR.(HOFF(I).GT.4095))THEN
                CONTXT='ERROR -- invalid header value for '//
     *                          KHOFF(I-1)
                GO TO 999
            ENDIF
30        CONTINUE
        ENDIF
C
C ---------------------------------------------------------------- VOFF
C
C offset for first bin is always 0
C
        VOFF(1)=0
        IF(NBINS.GT.1)THEN
          DO 40 I=2,NBINS
            CALL UHDGSI(ID,KVOFF(I-1),VOFF(I),ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- unable to get header value '//
     *                          KVOFF(I-1)
                GO TO 999
            ENDIF
            IF((VOFF(I).LE.-4096).OR.(VOFF(I).GT.4095))THEN
                CONTXT='ERROR -- invalid header value for '//
     *                          KVOFF(I-1)
                GO TO 999
            ENDIF
40        CONTINUE
        ENDIF
C ---------------------------------------------------------------- RPTCD
C
C repeat code for first bin is always 1
C
        RCODES(1)=1
        IF(NBINS.GT.1)THEN
          DO 50 I=2,NBINS
            CALL UHDGSI(ID,KRPTCD(I-1),RCODES(I),ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- unable to get header value '//
     *                          KRPTCD(I-1)
                GO TO 999
            ENDIF
            IF((RCODES(I).LT.1).OR.(RCODES(I).GT.8))THEN
                CONTXT='ERROR -- invalid header value for '//
     *                          KRPTCD(I-1)
                GO TO 999
            ENDIF
50        CONTINUE
        ENDIF
C ---------------------------------------------------------------- BINID
C
          DO 60 I=1,NBINS
            CALL UHDGSI(ID,KBINID(I),BINIDS(I),ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- unable to get header value '//
     *                          KBINID(I)
                GO TO 999
            ENDIF
            IF((BINIDS(I).LT.0).OR.(BINIDS(I).GT.15))THEN
                CONTXT='ERROR -- invalid header value for '//
     *                          KBINID(I)
                GO TO 999
            ENDIF
60        CONTINUE
C
C------------------------------------------------------------ DOPZER, DOPMAG
        CALL UHDGSD(ID,'DOPZER',DOPZER,ISTAT)
        CALL UHDGSR(ID,'DOPMAG',DOPMAG,ISTAT1)
        IF((ISTAT.NE.0).OR.(ISTAT1.NE.0))THEN
            CONTXT='ERROR -- getting value for DOPZER or DOPMAG'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            CONTXT='No dopler compensation processing will be done'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            FDOP='OMIT'
        ENDIF
C
C------------------------------------------------------------ XDCAL, XDCALP
        CALL UHDGSR(ID,'ZXDCALU',XDCAL,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- reading header value XDCALU,'//
     *                  ' will set it to 0'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
        CALL UHDGSR(ID,'ZXDCALP',XDCALP,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- reading header value XDCALP,'//
     *                  ' will set it to 0'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
C
C SCALE THEM
C
        XDCAL=XDCAL/50.0
        XDCALP=XDCALP/500.0
C
C----------------------------------------------------------------- STPTIM
        CALL UHDGSD(ID,'STEPTIME',STPTIM,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR -- reading header value STPTIM,'//
     *                  ' will set it to 1.0'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            STPTIM = 1.0D0
        ENDIF
C
C----------------------------------------------------------------- XDEF
C
C COMPUTE BASE X-DEFLECTION FOR EACH BIN (I.E. The deflection without
C       dopler compensation and for the set of initial deflections.
C
        DO 100 I=1,NBINS
                XDEF(I)=IXDEF(1) + HOFF(I) + XDCAL +
     *                          (IXDEF(1)-2048)*XDCALP
100     CONTINUE
C----------------------------------------------------------------- OBSRPT
C
C Compute number of observation repeats
C
        CALL UHDGSI(ID,'RPTOBS',OBSRPT,ISTAT)
        IF(ISTAT.NE.0)OBSRPT=0
        OBSRPT=OBSRPT+1
C                                    --->count first observation
C
C compute repeats from number of bins and gcount
C
        IRPT=(GCOUNT(3)+NBINS-1)/NBINS
        IF(IRPT.GT.OBSRPT)OBSRPT=IRPT
        ISTAT=0
        GO TO 1000
C
C ERROR
C
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
