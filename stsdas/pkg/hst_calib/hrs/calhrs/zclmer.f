        SUBROUTINE ZCLMER(PASS,DATA,EPS,ERR,FLUX,EPSM,ERRM,
     *                             NS,NSPEC)
*
*  Module number:
*
*  Module name: ZCLMER
*
*  Keyphrase:
*  ----------
*       Merge substep bins.
*  Description:
*  ------------
*       This routine merges the spectral data if MERGE is greater
*       then 0.  If merge equals 0, the output merged data is just
*       a copy of the input data.  Both the input and output data
*       arrays are two-dimensional arrays treated as 1-D arrays by
*       this routine.  The input is treated as 1-D to make copying
*       faster (2-D copying takes longer) and the output array
*       is treated as 1-D because this routine computes its dimensions.
*
*       To illustrate the merging of consider the input data having
*       values D<bin>.<diode> for bin number <bin> and diode number
*       <diode>.  The data would look like.
*
*               bin 1   D1.1  D1.2  D1.3  D1.4 ...
*               bin 2   D2.1  D2.2  D2.3  D2.4 ...
*                  ...         ...
*               bin 7   D7.1  D7.2  D7.3  D7.4 ...
*
*       The position of the data points in the 2-D data array mapped
*       into a 1-D data array are  500*<bin> + diode -1.
*
*       This routine maps the data into the output array for
*       MERGE = 2 as:
*               D1.1  D2.1  D1.2  D2.2  D1.3  D2.3 ...
*
*       and for MERGE = 4;
*               D1.1  D2.1  D3.1  D4.1  D1.2  D2.2  D3.2  D4.2  D1.3 ...
*
*
*  FORTRAN name: ZCLMER.FOR
*
*  Subroutines Called:
*  -------------------
*       NONE
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       FEB 89  D. LINDLER      DESIGNED AND CODED
*     1.1       Sep 91  S. Hulbert      Implemented PASS flag
*     1.2       Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* Input parameters
*
*	PASS - integer flag set to 1 on first call, -1 on last
*       DATA, EPS, ERR - unmerged data arrays
* Output parameters
*
*       FLUX, EPSM, ERRM - merged data arrays
*       NS, NSPEC - size of the output data arrays
*
******************************************************************************
        INTEGER PASS
        REAL DATA(1),EPS(1),ERR(1),FLUX(1),EPSM(1),ERRM(1)
        INTEGER NS, NSPEC
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
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
C----------------------------------------------------------------------------
C LOCAL VARIABLES
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        INTEGER I,J,N,INPOS,OUTPOS,ISTAT
        CHARACTER*80 CONTXT
C
C----------------------------------------------------------------------------
        IF(MERGE.EQ.0)THEN
C
C process unmerged data -----------------------------------------------------
C
                NS=500
                NSPEC=NBINS
                N=NS*NSPEC
                DO 100 I=1,N
                        FLUX(I)=DATA(I)
                        EPSM(I)=EPS(I)
                        ERRM(I)=ERR(I)
100             CONTINUE
           ELSE
C
C process merged data ------------------------------------------------------
C
                DELTAS(1)=DELTAS(1)/MERGE
                NS=MERGE*500
                NSPEC=1
                INPOS=1
                DO 200 J=1,MERGE
                        OUTPOS=J
                        DO 150 I=1,500
                                FLUX(OUTPOS)=DATA(INPOS)
                                EPSM(OUTPOS)=EPS(INPOS)
                                ERRM(OUTPOS)=ERR(INPOS)
                                INPOS=INPOS+1
                                OUTPOS=OUTPOS+MERGE
150                     CONTINUE
200             CONTINUE
                IF(PASS.EQ.FIRST)THEN
                   WRITE(CONTXT,99)MERGE
99                 FORMAT('Data merged from first',I2,' substep bins')
                   CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
                ENDIF
        ENDIF
        RETURN
        END
