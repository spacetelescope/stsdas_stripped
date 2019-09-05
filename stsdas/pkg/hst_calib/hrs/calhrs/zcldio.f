        SUBROUTINE ZCLDIO(PASS,FLAG,DIOFIL,DET,MINDIO,NBINS,NINIT,
     *                  XOFF,PERCNT,DATA,ET,ERR,ISTAT)
*
*  Module number:
*
*  Module name: ZCLDIO
*
*  Keyphrase:
*  ----------
*       Perform GHRS diode response correction
*
*  Description:
*  ------------
*       This routine corrects for diode response by dividing
*       The input data by the diode response values.  When
*       comb-addition is used (i.e NINIT>1) then the a smoothed
*       diode response array is computed using by a wieghted
*       average of diode responses offset by XOFF and weighted
*       by PERCNT.  Data with a diode response value less than
*       MINDIO are set to 0.0.
*
*  FORTRAN name: ZCLDIO.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       DIOFIL                  I       Diode response file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zrddio
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*     1.1       Sep 91  S. Hulbert      Implemented PASS flag
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       PASS - integer variable set to 1 on first call, -1 on last
*       FLAG - calibration flag
*       DIOFIL - name of the diode response file
*       DET - detector number
*       MINDIO - minimum diode response value to use.
*       NBINS - number of substep bins
*       NINIT - number of initial deflection pairs
*       XOFF - offsets in diodes for each deflection pair
*       PERCNT - percent of time used for each deflection pair
*               PERCNT(j,i) is the percent of time for deflection
*               pair j, bin i.
*
* Input/Output parameters
*
*       DATA - data array for main diodes (500xnbins)
*       ET - trailer array containing special diodes (24xnbins)
*       ERR - propagated statistical errors
*
* Output parameter
*
*       ISTAT - error status
*
*------------------------------------------------------------------------
        INTEGER PASS
        CHARACTER*12 FLAG
        CHARACTER*64 DIOFIL
        INTEGER DET,NBINS,NINIT,XOFF(7),ISTAT
        REAL MINDIO,PERCNT(5,7),DATA(500,7),ET(24,7),ERR(500,7)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Local variables
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)

        REAL DRESP(512)
C                                    --->diode response array
        REAL SRESP(12)
C                                    --->diode response for special diodes
        REAL MRESP(500)
C                                    --->multiplexed response
        INTEGER BIN,I,J
C                                    --->loop indices
        REAL LASTP(5)
C                                    --->last set of percents processed
        REAL WEIGHT
        INTEGER IOFF,POS
        CHARACTER*80 CONTXT
C
C--------------------------------------------------------------------------
C
C On first call, read responses and set up output arrays
C
        IF(PASS.EQ.FIRST)THEN
C
C Read responses
C
                CALL ZRDDIO(DIOFIL,FLAG,DET,DRESP,ISTAT)
                IF(ISTAT.NE.0) GO TO 1000
C
C Separate into main array and special diodes
C
                DO 10 I=1,6
                        SRESP(I)=DRESP(I)
                        SRESP(I+6)=DRESP(506+I)
10              CONTINUE
                DO 20 I=1,500
                        DRESP(I)=DRESP(I+6)
                        MRESP(I)=DRESP(I)
20              CONTINUE
C
C Set up last set of percents for no multiplexing
C
                DO 30 I=1,5
30                      LASTP(I)=0.0
                LASTP(1)=1.0
        ENDIF
C END set up for first call --------------------------------------------------
C
C LOOP on substep bins
C
        DO 100 BIN=1,NBINS
C
C Last multiplexed response still usable
C
                DO 40 I=1,NINIT
                    IF(LASTP(I).NE.PERCNT(I,BIN))GO TO 50
C                                    --->different?
40              CONTINUE
C
C IF we made it here we can reuse last multiplexed response
C
                GO TO 80
C
C recompute multiplexed response
C
50              DO 55 I=1,500
55                      MRESP(I)=0.0
C
C Loop on initial deflection pairs
C
                DO 70 I=1,NINIT
                        IOFF = XOFF(I)
C                                    --->Diode offset
                        WEIGHT=PERCNT(I,BIN)
                        DO 60 J=1,500
                                POS=J-IOFF
                                IF((POS.GT.500).OR.(POS.LT.1))GO TO 60
                                MRESP(J)=MRESP(J)+WEIGHT*DRESP(POS)
60                      CONTINUE
70              CONTINUE
C
C save multiplexed percents presently computed
C
                DO 75 I=1,NINIT
75                      LASTP(I)=PERCNT(I,BIN)
C
C We are now ready to divide by the response
C
80              DO 85 I=1,500
                        IF(MRESP(I).GE.MINDIO)THEN
                                DATA(I,BIN)=DATA(I,BIN)/MRESP(I)
                                ERR(I,BIN)=ERR(I,BIN)/MRESP(I)
                           ELSE
                                DATA(I,BIN)=0.0
                                ERR(I,BIN)=0.0
                        ENDIF
85              CONTINUE
C
C now do special diodes
C
                DO 90 I=1,12
                        IF(SRESP(I).GE.MINDIO)THEN
                                ET(I,BIN)=ET(I,BIN)/SRESP(I)
                            ELSE
                                ET(I,BIN)=0.0
                        ENDIF
90              CONTINUE
100     CONTINUE
        IF(PASS.EQ.FIRST)THEN
           CONTXT='Diode non-uniformities removed using '//DIOFIL
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
        ISTAT=0
1000    RETURN
        END
