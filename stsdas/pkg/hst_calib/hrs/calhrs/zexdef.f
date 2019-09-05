      SUBROUTINE ZEXDEF(EXXDEF,EXYDEF,ISTAT)
*
*  Module number:
*
*  Module name: ZEXDEF
*
*  Keyphrase:
*  ----------
*     determine extra x/y deflection offsets
*
*  Description:
*  ------------
*     Need to check for long X/Y Scans.  For these types of scans
*     not enough offsets are stored in the data file.  In fact,
*     only NBINS offsets are stored.  However, lonscans typically
*     have 33 * NBINS offsets.  The first NBINS are stored, but the
*     rest need to be calculated.  The total offsets for a 
*     NBINS increment is calculated and returned.  These offsets
*     are then added for each group of NBINS. See zlcprc.
*
*  FORTRAN name: zexdef.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*     PKTFMT from the .D0H (science data) file.
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*     uhdgsi
*
*  History:
*  --------
*  Version        Date       Author         Description
*  1.3.2.2        Apr 92     J. Eisenhamer  Designed and coded
*  1.3.2.3        Nov 96     M. De La Pena  Added STPTIM to HRSDEF
*---------------------------------------------------------------------------
*
* Inputs
*     <none>
* Outputs:
*     exxdef - Offsets to add to each group of NBINS XDEFI
*     exydef - Offsets to add to each group of NBINS YDEFI
*     istat  - Return status. 0 is OK.
*---------------------------------------------------------------------------
      INTEGER EXXDEF,EXYDEF,ISTAT
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
C LOCAL VARIABLES
C
C The packet format codes for X/Y Scans
C
        INTEGER XSCAN,YSCAN
        PARAMETER (XSCAN=134)
        PARAMETER (YSCAN=133)
C
C The packet format code.
C
        INTEGER PFCODE
C
C The packet format keyword.
        CHARACTER*8 PKTFMT
        DATA PKTFMT/'PKTFMT'/
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C By default, the offsets are zero.
C
        EXXDEF = 0
        EXYDEF = 0
C
C Get the Packet format code.
C
        CALL UHDGSI(IDS(3),PKTFMT,PFCODE,ISTAT)
        IF(ISTAT.NE.0) GO TO 1000

        IF( (PFCODE.EQ.XSCAN) .OR. (PFCODE.EQ.YSCAN) ) THEN
C
C Check number of groups.  If greater than NBINS, then need to
C determine the offsets.
C
           IF(GCOUNT(3).GT.NBINS) THEN
              EXXDEF = HOFF(2) + HOFF(7)
              EXYDEF = VOFF(2) + VOFF(7)
C
           ENDIF
        ENDIF
C
C Clean end.
C
        ISTAT = 0
C
C Exit
C
 1000   RETURN
        END
