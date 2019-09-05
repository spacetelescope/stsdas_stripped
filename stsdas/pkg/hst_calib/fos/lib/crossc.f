        SUBROUTINE CROSSC(WAVE,S1,S2,NPOINT,NBINS,MAXD,WCENT,ICENT,
     *                    DELW,DELI,TOTS1,TOTS2,NGOOD)
*
* Module Number: HRS/FOS utility
*
* Module Name: CROSSC
*
* Keyphrase:
* ----------
*       Compute offset by cross-correlation
*
* Description:
* ------------
*       This routine divides the input spectra into nbins and computes
*       an offset for each bin using cross correlation of the two
*       spectra.
*
* Fortran Name: crossc.for
*
* Keywords of accessed files and tables:
* --------------------------------------
*       none
*
* Subroutines Called:
* -------------------
* CDBS:
*       none
* SDAS:
*       none
*
* History:
* --------
* version         date            Author          Description
*    1          3/26/87         D. Lindler      Designed and coded
*    2          9/25/87         D. Lindler      New SDAS standards
*-------------------------------------------------------------------------
*
* Input paramters
*
*       wave - wavelength vector for the first spectra (real*8)
*       s1 - the first spectra (real*8)
*       s2 - the second spectra (real*8)
*       npoint - number of points in wave, s1, and s2 (integer)
*       nbins - number of bins to devide data into
*       maxd - max distance to search for offset
*
* Output paramters
*
*       wcent - central wavelength for each offset computed (real*8)
*       icent - central index number of each offset computed (real*8)
*       delw - offsets in wavelength units (real*8)
*       dels - offsets in sample units (real*8)
*       tots1 - total counts in each bin of s1 (real*8)
*       tots2 - total counts in each bin of spectrum s2 (real*8)
*       ngood - number of offsets computed (integer)
*
	IMPLICIT NONE
        DOUBLE PRECISION WAVE(*),S1(*),S2(*),WCENT(*),ICENT(*),
     *       DELW(*),DELI(*),TOTS2(*),TOTS1(*)
        INTEGER NPOINT,NBINS,MAXD,NGOOD
C
C LOCAL VARIABLES
C
        DOUBLE PRECISION C(100),SUM,MAXC,RK,SUM12,AVE1,AVE2,
     *        SUM2SQ,SUM1SQ,RS1,RS2,DENOM,TOTAL1,TOTAL2
        INTEGER I,J,K,IOFF,I1OFF,IC,WIDTH,N
C
C-----------------------------------------------------------------------------
C
C COMPUTE SIZE OF BINS AND SEARCH WIDTH
C
        WIDTH=MAXD*2+1
        N=(NPOINT-WIDTH)/NBINS
C
C LOOP ON EACH BIN
C
        NGOOD=0
        DO 100 I=1,NBINS
C
C COMPUTE OFFSET FOR EACH BIN
C
                IOFF=(I-1)*N+WIDTH/2
C
C COMPUTE AVERAGE OF SECOND SPECTRA FOR THE BIN
C
                SUM=0.0
                DO 5 K=1,N
                        SUM=SUM+S2(IOFF+K)
5               CONTINUE
                AVE2=SUM/N
C
C LOOP ON EACH OFFSET IN SEARCH WIDTH
C
                DO 50 J=1,WIDTH
C
C OFFSET FOR FIRST SPECTRA
C
                        I1OFF=IOFF+J-MAXD-1
C
C COMPUTE AVERAGE FOR BIN IN FIRST SPECTRUM
C
                        SUM=0.0
                        DO 10 K=1,N
                                SUM=SUM+S1(I1OFF+K)
10                      CONTINUE
                        AVE1=SUM/N
C
C COMPUTE TOTALS OF: (S1-AVE1)*(S2-AVE2)
C                    (S1-AVE1)**2
C                    (S2-AVE2)**2
C
                        SUM12=0.0
                        SUM1SQ=0.0
                        SUM2SQ=0.0
                        DO 20 K=1,N
                                RS1=S1(I1OFF+K)-AVE1
                                RS2=S2(IOFF+K)-AVE2
                                SUM1SQ=SUM1SQ+RS1*RS1
                                SUM2SQ=SUM2SQ+RS2*RS2
                                SUM12=SUM12+RS2*RS1
20                      CONTINUE
C
C PLACE SUM IN CORRELATION MATRIX
C
                        DENOM=SQRT(SUM1SQ)*SQRT(SUM2SQ)
                        IF(DENOM.EQ.0) GO TO 100
                        C(J)=SUM12/DENOM
50              CONTINUE
C
C FIND MAXIMUM OF CORRELATION MATRIX, MAXC, AND POSITION OF MAXIMUM, K
C
                K=0
                MAXC=0.0
                DO 60 J=1,WIDTH
                        IF(C(J).GT.MAXC)THEN
                                MAXC=C(J)
                                K=J
                        ENDIF
60              CONTINUE
C
C DON'T USE IF MAXIMUM IS ON EDGE OF CORRELTAION MATRIX
C
                IF((K.LT.2).OR.(K.EQ.WIDTH))GO TO 100
C
C USE QUADRATIC REFINE TO COMPUTE EXACT OFFSET (MAX. OF QUADRATIC FIT TO
C       C(K-1), C(K) AND C(K+1)
C
                RK=(C(K-1)-C(K))/(C(K-1)+C(K+1)-2.0*C(K))-0.5
C
C UPDATE OUTPUT VECTORS
C
                NGOOD=NGOOD+1
                DELI(NGOOD)=MAXD-K-RK+1.0
                IC=IOFF+N/2+1
                ICENT(NGOOD)=IC
                WCENT(NGOOD)=WAVE(IC)
C
C COMPUTE TOTAL IN SPECTRA FOR BIN
C
                TOTAL1=0.0
                TOTAL2=0.0
                DO 70 J=1,N
                        TOTAL1=TOTAL1+S1(IOFF+J)
                        TOTAL2=TOTAL2+S2(IOFF+J)
70              CONTINUE
                TOTS1(NGOOD)=TOTAL1
                TOTS2(NGOOD)=TOTAL2
C
C COMPUTE WAVELENGTH OFFSET FROM INDEX OFFSET
C
                DELW(NGOOD)=DELI(NGOOD)*(WAVE(IC+1)-WAVE(IC-1))/2.0
100     CONTINUE
        RETURN
        END
