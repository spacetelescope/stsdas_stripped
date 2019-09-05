        SUBROUTINE ZCLPAT(READNO,ET,EPSET,XOFF,EXPO,PERCNT,TOTOBS,
     *                                  ISTAT)
*
*  Module number:
*
*  Module name: zclpat
*
*  Keyphrase:
*  ----------
*       Compute pattern information
*
*  Description:
*  ------------
*       This routine computes the exposure times for each substep
*       bin and the combaddition pattern.
*
*  FORTRAN name: zclpat
*
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zgetbt
*  SDAS:
*       ZMSPUT
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb 89  D. Lindler      Designed and coded
*      1.1      Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* INPUT PARAMETERS
*       readno - readout number
*       et - engineering trailer REAL 24X7
*       epset - data quality for et REAL 24 X 7
*
* Output parameters
*       xoff - combaddition offsets (in diodes) integer
*       expo - exposure time (seconds) for each bin (real)
*       percnt - 5 x 7 array giving percent of the total exposure time
*               spent on each initial deflection running from 1-5 and
*               each substep bin running from 1-7
*	totobs - total observation time
*
        INTEGER READNO,XOFF(7)
        REAL ET(24,7),EPSET(24,7),EXPO(7),PERCNT(5,7),TOTOBS
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
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        INTEGER INT,INTEG,I,J,ISTAT
        INTEGER NC1,MAXRC,NTIMES,NLAST,NLEFT
        INTEGER NCOADD(7)
        REAL E
C
C------------------------------------------------------------------------------
C
C COMPUTE THE MAXIMUM REPEAT CODE
C
        MAXRC = 0
        DO 6 I=1,NBINS
6               IF(RCODES(I) .GT. MAXRC) MAXRC=RCODES(I)
C
C LOOP ON BINS ------------------------------------------
C
        TOTOBS = 0.0
        DO 100 I=1,NBINS
C
C compute exposure time for the bin
C
            IF((EPSET(21,I).GT.0.0).OR.(EPSET(23,I).GT.0.0))THEN
                WRITE(CONTXT,9)I,READNO
9               FORMAT('ERROR: unable to compute exposure time for',
     *                  ' bin:',I2,'  readout:',I6)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
C
C trailer contains fill data (no way to compute exposure)
C
                EXPO(I)=0.0
                NCOADD(I)=1
C
C we can compute an exposure time
C
             ELSE
                INT=ET(21,I)
                CALL ZGETBT(INT,8,15,INTEG)
C                                    --->INTEGRATION TIME
                NCOADD(I)=ET(23,I)
                IF(NCOADD(I).LT.1)NCOADD(I)=1
C                                    --->DDLINK HAS ZERO RECORDED
                E=(0.05*INTEG - 0.002)*NCOADD(I)
                EXPO(I)=E
                TOTOBS = TOTOBS + 0.05*INTEG*NCOADD(I)
            ENDIF
C
C NC1 = The number of coadds per initial deflection pair for each complete
C       pattern
C NTIMES = the number of complete patterns over all initial pairs performed
C NLEFT = the number of coadds in the incomplete pattern
C
            NC1 = MAXRC/RCODES(I)
            NTIMES = NCOADD(I)/NC1/NINIT
            NLEFT = NCOADD(I) - NTIMES*NC1*NINIT
C
C Place number of coadds from complete patterns into output PERCNT array
C
            DO 10 J=1,NINIT
10              PERCNT(J,I) = NTIMES*NC1
C
C Compute the number of address pairs in the last pattern that were
C completed (NLAST) and the number of coadds still not accounted for (NLEFT)
C
            NLAST = NLEFT/NC1
            NLEFT = NLEFT - NLAST*NC1
C
C add coadds for deflection pairs completed in last pattern (if any)
C
            IF (NLAST.GT.0) THEN
                DO 20 J=1,NLAST
20                      PERCNT(J,I) = PERCNT(J,I)+NC1
            ENDIF
C
C add the remaining coadds to an incompleted deflection pair
C
            IF (NLEFT .GT. 0)
     *                  PERCNT(NLAST+1,I)=PERCNT(NLAST+1,I)+NLEFT
C
C Normalize by total number of coadds
C
            DO 30 J=1,NINIT
30              PERCNT(J,I)=PERCNT(J,I)/NCOADD(I)
100     CONTINUE
C END LOOP ON BINS ------------------------------------------
C
C Compute combaddition offsets in diodes (1 diode = 8 deflection units)
C
        DO 200 I=1,NINIT
200             XOFF(I)=(IXDEF(I)-IXDEF(1))/8.0+0.5
        RETURN
        END
