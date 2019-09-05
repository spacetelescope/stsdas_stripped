        SUBROUTINE ZCLHEL(PASS,NS,NSPEC,WAVE,ISTAT)
*
*  Module number:
*
*  Module name: zclhel
*
*  Keyphrase:
*  ----------
*       Heliocentric wavelengths
*  Description:
*  ------------
*       This routine corrects for the earths motion around the Sun.
*
*  FORTRAN name: zclhel.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zhelio
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*     1.1       Sep 91  S. Hulbert      Implemented PASS flag
*-------------------------------------------------------------------------------
*
* INPUTS:
*       PASS - integer variable set to 1 on first call, -1 on last
*       NS - number of data points in spectra
*       NSPEC - number of spectra
*
* INPUT/OUTPUT:
*       WAVE - wavelength array
*
* OUTPUT:
*       ISTAT - error status
*
*-----------------------------------------------------------------------
        INTEGER PASS
        INTEGER NS,NSPEC,ISTAT
        DOUBLE PRECISION WAVE(NS,NSPEC)
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
C   NGSDT - number of output groups for special diode files
C   READNO - readout number
C   NSOUT - number of samples in the output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
C LOCAL VARIABLES
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        CHARACTER*80 CONTXT
        DOUBLE PRECISION V
C                                    --->Velocity of earth towards target (km/s)
        DOUBLE PRECISION FACTOR
C                                    --->correction factor
        DOUBLE PRECISION C
C                                    --->Speed of Light (Km/sec) IAU 1976
        PARAMETER (C = 2.99792458D5)
        INTEGER I,ISPEC
        DOUBLE PRECISION PKTTME
C				     --->
        REAL RA,DEC
C                                    --->RA,DEC IN J2000 coordinates
C
C-----------------------------------------------------------------------------
C
C If first call, compute earths velocity toward the target
C
        IF(PASS.EQ.FIRST)THEN
C
C read ra,dec from DG2 data and packet time in the SHP
C
            CALL UHDGSD(IDS(1),'PKTTIME',PKTTME,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: reading PKTTIME from the .SHH file'
                GO TO 999
            ENDIF
            CALL UHDGSR(IDS(1),'RA_TARG',RA,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: reading RA_TARG from the .SHH file'
                GO TO 999
            ENDIF
            CALL UHDGSR(IDS(1),'DEC_TARG',DEC,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: reading DEC_TARG from the .SHH file'
                GO TO 999
            ENDIF
C
C Compute velocity and correction factor
C
            CALL ZHELIO(PKTTME,RA,DEC,V,ISTAT)
            IF(ISTAT.NE.0) GO TO 1000
            FACTOR = 1.0D0 + V/C
            WRITE(CONTXT,99)V
99          FORMAT('Net velocity of earth toward target =',F6.1,
     *                   ' km/sec.')
            CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
C
C Correct wavelengths
C
        DO 100 ISPEC=1,NSPEC
                DO 100 I=1,NS
100                     WAVE(I,ISPEC)=WAVE(I,ISPEC)*FACTOR
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
