        SUBROUTINE ZCLOUT(ROOT,ISTAT)
*
*  Module number:
*
*  Module name: zclout
*
*  Keyphrase:
*  ----------
*       Determine size of output data sets
*  Description:
*  ------------
*       This routine determines the size of the output data sets.
*       To determine the number of groups in the output the routine
*       must determine if data is to be merged and if so is the
*       data single, half, or quarter stepped.  The posibilities are:
*
*       1) Single stepped data
*               BINID(1) = 1 or 2
*               No other bins have a BINID of 1 or 2
*       2) half stepped data
*               BINID(1) = 1 or 2
*               BINID(2) = BINID(1)
*               VOFF(2)=VOFF(1)=0
*               HOFF(2)=4
*       3) quarter stepped data
*               BINID(1) = 1 or 2
*               BINID(4) = BINID(3) = BINID(2) = BINID(1)
*               VOFF(1)=VOFF(2)=VOFF(3)=VOFF(4)=0
*               HOFF(2)=2, HOFF(3)=4, HOFF(4)=6
*       4) OTHERWISE data will not be merged and no background
*               subtracted.
*
*  FORTRAN name: zclout.for
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*	ZMSPUT
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1        Feb 89     D. Lindler     Designed and coded
*    1.1      Oct 96     M. De La Pena  Added FBMD and STPTIM 
*       
*-------------------------------------------------------------------------------
        CHARACTER*64 ROOT
C                                    --->root name of the input observation
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
      INTEGER   TYREAL
      PARAMETER (TYREAL = 6)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C     END IRAF77.INC
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
C   NSOUT - number of samples in the output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        INTEGER ISTAT,I
C
C---------------------------------------------------------------------------
C
C DETERMINE NUMBER OF SPECTRA TO MERGE
C
        MERGE=0
        IF(FMER.EQ.'OMIT')GO TO 500
C
C Case 1 - single stepped ---------------------------------------------
C
100     IF((BINIDS(1).NE.1).AND.(BINIDS(1).NE.2))GO TO 500
        IF(NBINS.GT.1)THEN
                DO 110 I=2,NBINS
                      IF((BINIDS(I).EQ.1).OR.(BINIDS(I).EQ.2))GO TO 200
110             CONTINUE
        ENDIF
        MERGE=1
        GO TO 500
C
C Case 2 - half stepped -------------------------------------------------
C
200     IF(NBINS.LT.2) GO TO 500
        IF(BINIDS(2).NE.BINIDS(1)) GO TO 500
        IF(VOFF(2).NE.0) GO TO 500
        IF(NBINS.GT.2)THEN
                DO 210 I=3,NBINS
                      IF((BINIDS(I).EQ.1).OR.(BINIDS(I).EQ.2))GO TO 300
210             CONTINUE
        ENDIF
        IF(HOFF(2).EQ.4)MERGE=2
        GO TO 500
C
C Case 3 - quarter stepped ---------------------------------------------
C
300     IF(NBINS.LT.4) GO TO 500
        IF((BINIDS(3).NE.BINIDS(1)).OR.(BINIDS(4).NE.BINIDS(1)))
     *                                                     GO TO 500
        IF((VOFF(3).NE.0).OR.(VOFF(4).NE.0)) GO TO 500
        IF(NBINS.GT.4) THEN
                DO 310 I=5,NBINS
                      IF((BINIDS(I).EQ.1).OR.(BINIDS(I).EQ.2))GO TO 500
310             CONTINUE
        ENDIF
        IF((HOFF(2).NE.2).OR.(HOFF(3).NE.4).OR.(HOFF(4).NE.6))GO TO 500
        MERGE=4
500     CONTINUE
C
C-------------------------------------------------------------------------
C
C DETERMINE SIZE OF OUTPUT FILES
C
C   number of data points per group =
C               500 x number of bins merged
C
        NSOUT=500
        IF(MERGE.GT.1)NSOUT=500*MERGE
C
C Number of output groups
C
        NGOUT=GCOUNT(3)
        NGSDT = GCOUNT(3)
        IF(MERGE.GT.0)NGOUT=NGOUT/NBINS
C
C   Ignore first 2 and last readout of a direct-downlink observation
C
        IF(OBSMOD.EQ.'DIR') THEN
           NGOUT=NGOUT-3
           NGSDT = NGOUT
        ENDIF
        IF(NGOUT.LT.1)THEN
                CONTXT='ERROR: zero output data groups computed '
                GO TO 999
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
