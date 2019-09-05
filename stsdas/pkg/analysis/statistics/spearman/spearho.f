C**************************************************************************
C************************* SUBROUTINE SPERHO ******************************
C**************************************************************************

       SUBROUTINE SPERHO (XF, NTOT, RHO, PROB)

C
C     *     THIS SUBROUTINE COMPUTES THE SPEARMAN'S RHO STATISTIC AND   *
C     *     ITS PROBABILITY UNDER THE NULL HYPOTHESIS.                  *
C     *                                                                 *
C     *   INPUT                                                         *
C     *      XF(J, I) : RANKS OF TWO VARIABLES J = 1, 2                 *
C     *      NTOT     : NUMBER OF DATA POINTS                           *
C     *                                                                 *
C     *   OUTPUT                                                        *
C     *       RHO     : SPEARMAN'S RHO                                  *
C     *       PROB    : PROBABILITY                                     *
C     *                                                                 *
C     *   SUBROUTINE AGAUSS                                             *
C
C
       IMPLICIT REAL*8 (A-H, O-Z), INTEGER (I-N)
       DIMENSION XF(2, NTOT)
       
       XSUM = 0.0
       X2SUM = 0.0
       YSUM = 0.0
       Y2SUM = 0.0
       XYSUM = 0.0
       RN = NTOT
       
       DO 100 I = 1, NTOT
          XSUM = XSUM + XF(1,I)
          X2SUM = X2SUM + XF(1,I) ** 2
          YSUM = YSUM + XF(2,I)
          Y2SUM = Y2SUM + XF(2,I) ** 2
          XYSUM = XYSUM + XF(1,I) * XF(2,I)
  100  CONTINUE
  
       XBAR = XSUM / RN
       YBAR = YSUM / RN

       SXX = X2SUM - RN * XBAR ** 2
       SYY = Y2SUM - RN * YBAR ** 2
       SXY = XYSUM - RN * XBAR * YBAR

       RHO = SXY / SQRT (SXX * SYY)
C
C******  THE NEXT APPROXIMATION IS GOOD ONLY WHEN N IS LARGE (E.G. >30)  *
C
       
       Z   = RHO * SQRT (RN - 1.0)
       
       PROB = 1.0 - AGAUSS (Z)
       
       RETURN
       END
