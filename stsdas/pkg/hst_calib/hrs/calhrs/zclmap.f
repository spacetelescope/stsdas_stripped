        SUBROUTINE ZCLMAP(PASS,CCR1,CCR2,DET,SCOEF,LCOEF,ISTAT)
*
*  Module number:
*
*  Module name: ZCLMAP
*
*  Keyphrase:
*  ----------
*       Perform GHRS mapping function
*  Description:
*  ------------
*       This routine computes the mapping function for each
*       of the substep bins using the following formula:
*
*               SAMPLE(bin) = s0 + b*XD + c*XD**2
*               DELTAS(bin) = e
*               LINE(bin) = l0 + A*YD
*
*       where:
*               SAMPLE is the sample position of the first diode
*               DELTAS is the spacing between sample positions
*               LINE is the line position of the diodes
*               XD is the x-deflection minus 2048
*               YD is the y-deflection minus 2048
*               s0, b, c and e are coefficients in CCR2.  They
*                       are interpolated for the given y-def.
*               l0, and A are coefficients in CCR1
*
*  FORTRAN name: ZCLMAP
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccr1                    I       line mapping function coef. table
*       ccr2                    I       sample mapping function coef. table
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zrccr1, zrccr2
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       April 89   D. Lindler   Designed and coded
*     1.1       Sep 91     S. Hulbert   Implemented PASS flag
*     1.2       Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* INPUT parameters
*
*       PASS - integer variable set to 1 on first call, -1 on last
*       CCR1 - name for line mapping function table
*       CCR2 - name for the sample mapping function table
*       DET - detector number
*
* OUTPUT parameters
*       SCOEF - sample function coefficients for all bins
*               scoef(1,bin)=s0
*               scoef(2,bin)=b
*               scoef(3,bin)=c
*               scoef(4,bin)=e
*       LCOEF - line function coefficients
*               lcoef(1)=a
*               lcoef(2)=b
*
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER PASS
        CHARACTER*64 CCR1,CCR2
        INTEGER DET,ISTAT
        REAL SCOEF(4,7),LCOEF(2)
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
C Local variables
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        CHARACTER*80 CONTXT
        INTEGER I
        REAL XD
C------------------------------------------------------------------------------
C
C Get line mapping function coefficients if first call
C
        IF(PASS.EQ.FIRST)THEN
                CALL ZRCCR1(CCR1,DET,LCOEF(1),LCOEF(2),ISTAT)
                IF(ISTAT.NE.0) GO TO 1000
        ENDIF
C
C Get sample function coefficients
C
        CALL ZRCCR2(PASS,CCR2,DET,YDEF,NBINS,SCOEF,ISTAT)
        IF(ISTAT.NE.0)GO TO 1000
C
C compute mapping function for each bin
C
        DO 10 I=1,NBINS
                LINE(I) = LCOEF(1) + LCOEF(2)*(YDEF(I)-2048)
                XD = XDEF(I)-2048
                SAMPLE(I) = SCOEF(1,I) + SCOEF(2,I)*XD +
     *                         SCOEF(3,I)*XD*XD
                DELTAS(I)=SCOEF(4,I)
10      CONTINUE
C
C write line mapping function coef. to output header
C
        IF(PASS.EQ.FIRST)THEN
           CALL UHDPSR(IDS(8),'ZLCOEF1',LCOEF(1),ISTAT)
           IF(ISTAT.NE.0)THEN
               CONTXT='ERROR: writing header parameter ZLCOEF1'//
     *                  ' to .c1h file'
               GO TO 999
           ENDIF
           CALL UHDPSR(IDS(8),'ZLCOEF2',LCOEF(2),ISTAT)
           IF(ISTAT.NE.0)THEN
               CONTXT='ERROR: writing header parameter ZLCOEF2'//
     *                  ' to .c1h file'
               GO TO 999
           ENDIF
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
