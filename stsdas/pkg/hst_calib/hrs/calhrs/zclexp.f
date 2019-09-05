        SUBROUTINE ZCLEXP(READNO,NBINS,DATA,EPS,ET,ETEPS,ERR,EXPO)
*
*  Module number:
*
*  Module name: zclexp
*
*  Keyphrase:
*  ----------
*       convert to count rates
*
*  Description:
*  ------------
*       This routine converts the input data to count rates by
*       dividing by the exposure times.  The exposure time is
*       computed for each bin as 0.05 x NCOADD x intper, where
*       NCOADD is the number of coadds to the bin and intper
*       is the integration period in 0.05 millisecond intervals.
*       Both intper and NCOADD are found in the eng. trailer
*       array ET.  If either value contains fill, no exposure time
*       can be computed and the entire bin is flagged as unusable.
*
*  FORTRAN name: zclexp
*
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       ZMSPUT
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb. 89   D. Lindler    Designed and coded
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       READNO - Readout number
*       NBINS - number of substep bins
*
* Input/Output parameters
*
*       DATA - data array for main diodes
*       EPS - data quality array for data
*       ET - eng. trailer and special diodes
*       ETEPS - data quality for ET
*       ERR - error array
*
* Output parameters
*
*       EXPO - vector of exposure times for each bin
*
********************************************************************************
        INTEGER NBINS,READNO
        REAL DATA(500,NBINS),EPS(500,NBINS),ET(24,NBINS),
     *          ERR(500,NBINS),ETEPS(24,NBINS),EXPO(7)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C HRS epsilons
C
        INTEGER EPSFIL
        PARAMETER (EPSFIL = 800)
C
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        INTEGER INT,NCOADD,INTEG,I,J,ISTAT
        REAL E
C
C------------------------------------------------------------------------------
C
C LOOP ON BINS
C
        DO 100 I=1,NBINS
C
C compute exposure time for the bin
C
            IF((ETEPS(21,I).GT.0.0).OR.(ETEPS(23,I).GT.0.0))THEN
                WRITE(CONTXT,9)I,READNO
9               FORMAT('ERROR: unable to compute exposure time for',
     *                  ' bin:',I2,'  readout:',I6)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CALL ZMSPUT('     Data flagged as fill',
     *                  STDOUT+STDERR,0,ISTAT)
C
C trailer contains fill data (no way to compute exposure)
C
                DO 10 J=1,500
                        EPS(J,I)=EPSFIL
                        DATA(J,I)=0.0
                        ERR(J,I)=0.0
10              CONTINUE
                DO 20 J=1,12
20                      ETEPS(J,I)=EPSFIL
                EXPO(I)=0.0
C
C we can compute an exposure time
C
             ELSE
                INT=ET(21,I)
                CALL ZGETBT(INT,8,15,INTEG)
C                                    --->INTEGRATION TIME
                NCOADD=ET(23,I)
                IF(NCOADD.EQ.0)NCOADD=1
C                                    --->DDLINK HAS ZERO RECORDED
                E=(0.05*INTEG-0.002)*NCOADD
                EXPO(I)=E
C
C divide by the exposure time
C
                DO 30 J=1,500
                    IF(EPS(J,I).NE.EPSFIL)THEN
                        DATA(J,I)=DATA(J,I)/E
                        ERR(J,I)=ERR(J,I)/E
                    ENDIF
30              CONTINUE
C
C special diodes
C
                DO 40 J=1,12
40                  IF(ETEPS(J,I).NE.EPSFIL)ET(J,I)=ET(J,I)/E
            ENDIF
100     CONTINUE
        IF(READNO.EQ.1)CALL ZMSPUT('Data converted to count rates',
     *           STDOUT,0,ISTAT)
        RETURN
        END
