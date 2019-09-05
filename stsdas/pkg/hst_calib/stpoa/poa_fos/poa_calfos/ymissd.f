C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YMISSD(NXSTEP,ND,NCHNLS,YSTEPS,SLICES,OVERSN,NREPS,
     *          REJECT,DDT,TRAILR,MISSED)
*
*  Module number:
*
*  Module name: ymissd
*
*  Keyphrase:
*  ----------
*       Compute missed repitions
*
*  Description:
*  ------------
*       This routine computes the missed repetitions when data
*       is taken with the reject mode
*
*  FORTRAN name: ymissd.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*       none
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jul 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* inputs:
*       nxstep - number of xsteps
*       nd - number of channels + overscan - 1 (data points per xstep)
*       nchnls - number of channels
*       ysteps - number of y steps
*       slices - number of slices
*       oversn - overscan
*       nreps - number of repititions
*       reject - reject array
*       ddt - dead diode table
*	trailr - boolean varible set to true if trailer (reject array)
*		is used
* outputs:
*       missed - number of missed repetition array
*
        INTEGER NXSTEP,ND,NCHNLS,YSTEPS,SLICES,OVERSN,NREPS
        LOGICAL TRAILR
        REAL REJECT(NXSTEP,OVERSN,YSTEPS,SLICES)
        REAL DDT(*)
        REAL MISSED(NXSTEP,ND,YSTEPS,SLICES)
C
C Local variables
C
        INTEGER I,J,K,L,M,DIODE
        REAL MISS
C------------------------------------------------------------------------
C
C loop on everything
C
        DO 100 L=1,SLICES
            DO 90 K=1,YSTEPS
                DO 80 I=1,NXSTEP

C
C compute number of misses for each data point within the xstep
C
                    DO 50 J=1,ND
                      MISS = 0
                      DO 40 M=1,OVERSN
                        DIODE = J-M+1
                        IF((DIODE.LT.1).OR.(DIODE.GT.NCHNLS))THEN
                                MISS = MISS + NREPS
                            ELSE
                                IF(DDT(DIODE).EQ.0.0) THEN
                                        MISS = MISS + NREPS
                                    ELSE
                                      IF(TRAILR)
     *                                   MISS = MISS + REJECT(I,M,K,L)
                                ENDIF
                        ENDIF
40                    CONTINUE
                      MISSED(I,J,K,L)=MISS
50                  CONTINUE
80              CONTINUE
90          CONTINUE
100     CONTINUE
        RETURN
        END
