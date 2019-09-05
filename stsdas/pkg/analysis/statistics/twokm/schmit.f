C***************************************************************************
C************************ SUBROUTINE SCHMIT  *******************************
C***************************************************************************
C
C
       SUBROUTINE SCHMIT(X, Y, NP, NC, DELX, DELY, XORG, YORG, TOL, 
     +                   MAX, NTOT, MX, MY, MXY, NM, NU, NL, FC, F, A, 
     +                   ALPHA, BETA)
C
C
C      *                                                                    *
C      *          THIS SUBROUTINE COMPUTES LINEAR REGRESSION COEFFICIENTS   *
C      *      ALPHA AND BETA (INTERCEPT AND SLOPE) BY SCHMITT'S BINNED      *
C      *      METHOD. BECAUSE OF THE BINNING METHOD, FINER BINNING GIVES    *
C      *      BETTER RESULTS, THOUGH THE CPU TIME MAY INCREASE VERY         *
C      *      MUCH. ALSO IF THE BINS ARE TOO FINE, THE CENSORED POINTS      *
C      *      MAY NOT FALL ON THE DETECTIONS.                               *
C      *                                                                    *
C      *                                                                    *
C      *      INPUT                                                         *
C      *            X(I): INDEPENDENT VARIABLE                              *
C      *            Y(I): DEPENDENT VARIABLE                                *
C      *           NP(I): INDICATOR OF CENSORED STATUS                      *
C      *                  IF NP(I)=0  : DETECTION                           *
C      *                          =1  : Y(I) IS LOWER LIMIT                 *
C      *                          =2  : X(I) IS LOWER LIMIT                 *
C      *                          =3  : BOTH ARE LOWER LIMITS               *
C      *                          =4  : Y(I) IS LOWER AND X(I) IS UPPER     *
C      *                  FOR THE UPPER LIMITS, CHANGE THE SIGNS            *
C      *           NC(K): NUMBER OF CENSORED POINTS OF EACH TYPE            *
C      *           DELX : BIN SIZE OF X AXIS                                *
C      *           DELY : BIN SIZE OF Y AXIS                                *
C      *           XORG : ORIGIN OF X                                       *
C      *           YORG : ORIGIN OF Y                                       *
C      *           TOL  : TOLERANCE FOR COMPUTAION F(I,J)                   *
C      *           MAX  : MAXIMUM ITERATION                                 *
C      *           NTOT : NUMBER OF DATA POINTS                             *
C      *            MX  : NUMBER OF BINS IN THE INDEPENDENT VARIABLE        *
C      *            MY  : NUMBER OF BINS IN THE DEPENDENT VARIABLE          *
C      *            MXY : TOTAL NUMBER OF BINS                              *
C      *                                                                    *
C      *     OUTPUT                                                         *
C      *       NM(K)    : NUMBER OF LIMITS CHANGED TO DETECTIONS            *
C      *       NU(I,J)  : NUMBER OF UNCENSORED IN THE BIN(I,J)              *
C      *       NL(I,J,K): NUMBER OF Y LOWER LIMITS IN THE BIN(I,J)          *
C      *       FC(I,J)  : COPY OF F(I,J) (TO CHECK THE CONVERGENCE)         *
C      *        F(I,J)  : NUMBER OF DATA POINTS IN BIN(I,J)                 *
C      *        A(I,J)  : MATRIX WHICH CONTAINS INFORMATION OF BIN          *
C      *                  POSITION  I=1,4 AND J=1,MX*MY                     *
C      *          ALPHA : INTERCEPT COEFFICIENT                             *
C      *           BETA : SLOPE COEFFICIENT                                 *
C      *                                                                    *
C      *     WORK                                                           *
C      *       TH(I)    : VECTOR " A(I,J)*F(I,J), I=1,4                     *
C      *        SUM     : TOTAL NUMBER OF POINTS.  THIS IS APPROXIMATELY    *
C      *                  EQUAL TO NTOT.  THE DIFFERENCE BETWEEN THE TWO    *
C      *                  VALUES DEPENDS ON THE TOLERANCE LEVEL.            *
C      *                                                                    *
C      *     SUBROUTINES:                                                   *
C      *         GRDPRB : THE SUBROUTINE WHICH COMPUTES THE TWO-DIMENSIONAL * 
C      *                  KAPLAN-MEIER PROBABILITY OF THE BINS              *
C      *                                                                    *

       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION X(NTOT),Y(NTOT),NP(NTOT),NC(8),NM(8),NU(MX,MY)
       DIMENSION NL(MX,MY,8),FC(MX,MY),F(MX,MY),A(5,MXY),TH(5) 
C
C      *                CALL SUBROUTINE GRDPRB                              *
C
       CALL GRDPRB (X, Y, NP, NC, DELX, DELY, XORG, YORG, TOL, MAX,
     +              NTOT, MX, MY, NM, NU, NL, FC, F)
C
C
C      *               MAKE MATRIX A(I,J)                                   *
C
       IJ = 0
       DO 1120 J = 1,MY
          DO 1110 I = 1,MX
             IJ = IJ + 1
             XB = XORG + DELX / 2.0 + DELX * (I - 1)
             YB = YORG + DELY / 2.0 + DELY * (J - 1)
C
             A(1,IJ) = XB
             A(2,IJ) = XB ** 2
             A(3,IJ) = XB * YB
             A(4,IJ) = YB
             A(5,IJ) = YB ** 2
 1110     CONTINUE
 1120  CONTINUE
C
C      *             COMPUTE THE VECTOR THETA : TH(I)                       *
C
       DO 1400 I = 1,5
          TH(I) = 0.0
 1400  CONTINUE

       IJ = 0
       DO 1430 J = 1,MY
          DO 1420 I = 1,MX
             IJ = IJ + 1
             DO 1410 K = 1,5
                TH(K) = TH(K) + A(K,IJ) * F(I,J) * NTOT
 1410        CONTINUE
 1420     CONTINUE
 1430  CONTINUE

       SUM = 0.0
       DO 1460 I = 1, MX
          DO 1450 J = 1, MY
             SUM = SUM + F(I,J) * NTOT
 1450     CONTINUE
 1460  CONTINUE
 
C
C      *     COMPUTE THE SLOPE COEFFICIENT : BETA, AND THE INTERCEPT        *
C      *     COEFFICIENT : ALPHA.                                           *
C
       DEN = SUM * TH(2) - TH(1) ** 2
       BETA = (SUM * TH(3) - TH(1) * TH(4)) / DEN
       ALPHA = (TH(2) * TH(4) - TH(1) * TH(3)) / DEN
C
       RETURN
       END
