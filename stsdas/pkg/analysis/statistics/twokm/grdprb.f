C      **********************************************************************
C      ******************** SUBROUTINE GRDPROB  *****************************
C      **********************************************************************
C
       SUBROUTINE GRDPRB(X, Y, NP, NC, DELX, DELY, XORG, YORG, TOL, MAX,
     +                   NTOT, MX, MY, NM, NU, NL, FC, F)
C
C
C      *                                                                    *
C      *     THIS SUBPROGRAM COMPUTES THE PROBABILITY OF BIN(I,J)           *
C      *     IN WHICH DETECTED POINTS EXIST. ONLY BINS WHICH HAVE           *
C      *     DETECTED POINTS CAN HAVE NON-ZERO PROBABILITY.                 *
C      *                                                                    *
C      *     INPUT                                                          *
C      *              X(I)  : INDEPENDENT VARIABLE                          *
C      *              Y(I)  : DEPENDENT VARIABLE                            *
C      *             NP(I)  : INDICATOR OF CENSORED STATUS                  *
C      *             NC(K)  : NUMBER OF CENSORED POINTS OF EACH TYPE        *
C      *             DELX   : BIN SIZE OF X AXIS                            *
C      *             DELY   : BIN SIZE OF Y AXIS                            *
C      *             XORG   : ORIGIN OF X                                   *
C      *             YORG   : ORIGIN OF Y                                   *
C      *             TOL    : TOLERANCE FOR COMPUTAION F(I,J)               *
C      *             MAX    : MAXIMUM ITERATION                             *
C      *              NTOT  : TOTAL NUMBER OF DATA                          *
C      *              MX    : BIN NUMBER OF X                               *
C      *              MY    : BIN NUMBER OF Y                               *
C      *     OUTPUT                                                         *
C      *           NM(K)    : NUMBER OF LIMITS CHANGED TO DETECTIONS        *
C      *           NU(I,J)  : NUMBER OF UNCENSORED IN THE BIN(I,J)          *
C      *           NL(I,J,K): NUMBER OF Y LOWER LIMITS IN THE BIN(I,J)      *
C      *           FC(I,J)  : COPY OF F(I,J) (TO CHECK THE CONVERGENCE)     *
C      *           F(I,J)   : NUMBER OF DATA POINTS IN BIN(I,J)             *
C      *      WORK                                                          *
C      *              SUM1  : WEIGHT ON BIN(I,J) FROM Y LOWER LIMIT         *
C      *              SUM2  : WEIGHT ON BIN(I,J) FROM X LOWER LIMITS        *
C      *              SUM3  : WEIGHT ON BIN(I,J) FROM DOUBLE LOWER          *
C      *                      LIMITS                                        *
C      *              SUM4  : WEIGHT ON BIN(I,J) FROM Y LOWER, X UPPER      *
C      *                      LIMITS                                        *
C      *              SUM5  : WEIGHT ON BIN(I,J) FROM Y UPPER LIMITS        *
C      *              SUM6  : WEIGHT ON BIN(I,J) FROM X UPPER LIMITS        *
C      *              SUM7  : WEIGHT ON BIN(I,J) FROM DOUBLE UPPER          *
C      *                      LIMITS                                        *
C      *              SUM8  : WEIGHT ON BIN(I,J) FROM Y UPPER, X LOWER      *
C      *                      LIMITS                                        *
C      *              ITE   : NUMBER OF ITERATIONS                          *
C      *              DEL   : TOLERANCE [SUM (F(I,J)-FC(I,J))**2]           *
C      *                                                                    *
C      *     SUBROUTINE : BIN                                               *
C      *                                                                    *
C
       IMPLICIT REAL*8(A-H,O-Z), INTEGER (I-N)
       DIMENSION X(NTOT),Y(NTOT),NP(NTOT),NC(8),NM(8),NU(MX,MY)
       DIMENSION NL(MX,MY,8),FC(MX,MY),F(MX,MY)
C
C      *             COMPUTE NUMBER OF POINTS IN EACH BIN                   *
C
       CALL BIN (X, Y, NP, NC, DELX, DELY, XORG, YORG, NTOT, MX, MY, 
     +           NM, NU, NL)
C
C      *             INITIAL SETTING OF FC(I,J)                             *
C
       DO 300 I = 1,MX
          DO 200 J = 1,MY
             IF (NU(I,J) .EQ. 0) THEN
                F(I,J) = 0.0
             ELSE
                F(I,J) = FLOAT(NU(I,J)) / FLOAT(NTOT)
             ENDIF
             FC(I,J) = F(I,J)
  200     CONTINUE
  300  CONTINUE
C
C
C      *             START ITERATIONS TO FIND F(I,J) LOOP 500               *
C
       SNT = NTOT
       ITE = 1

  500  DEL = 0.0
       DO 1600 I = 1,MX
          DO 1500 J = 1,MY
             IF (F(I,J) .NE. 0.0) THEN
C
C      *      COMPUTE THE INFLUENCE OF CENSORED DATA POINTS ON THE DETECTED *
C      *      POINT AT I,J.                                                 *
C
C
C   
C      *              Y LOWER LIMITS                                        *
C
                SUM1 = 0
                IF (NC(1) .NE. 0) THEN
                   DO 600 L = 1,J
                      IF (NL(I,L,1) .NE. 0) THEN
                         SUMF1 = 0.0
                         DO 550 L1 = L,MY
                            SUMF1 = SUMF1 + F(I,L1)
  550                    CONTINUE

                SUM1 = SUM1 +(FLOAT(NL(I,L,1)) / SNT)*(F(I,J) / SUMF1)

                      ENDIF
  600              CONTINUE
                ENDIF
C
C
C      *             X LOWER LIMITS                                         *
C
                SUM2 = 0
                IF (NC(2) .NE. 0) THEN
                   DO 700 K = 1,I
                      IF (NL(K,J,2) .NE. 0) THEN
                         SUMF2 = 0.0
                         DO 650 K1 = K,MX
                            SUMF2 = SUMF2 + F(K1,J)
  650                    CONTINUE

                SUM2 = SUM2 +(FLOAT(NL(K,J,2)) / SNT)*(F(I,J) / SUMF2)

                      ENDIF
  700              CONTINUE
                ENDIF
C
C
C      *            DOUBLE LOWER LIMITS                                     *
C
                SUM3 = 0
                IF (NC(3) .NE. 0) THEN
                   DO 800 K = 1,I
                      DO 790 L = 1,J
                         IF (NL(K,L,3) .NE. 0) THEN
                            SUMF3 = 0.0
                            DO 780 K1 = K,MX
                               DO 770 L1 = L,MY
                                  SUMF3 = SUMF3 + F(K1,L1)
  770                          CONTINUE
  780                       CONTINUE

                SUM3 = SUM3 +(FLOAT(NL(K,L,3)) / SNT)*(F(I,J) / SUMF3)

                         ENDIF
  790                 CONTINUE
  800              CONTINUE
                ENDIF
C
C
C      *            Y LOWER, X UPPER LIMITS                                 *
C
                SUM4 = 0
                IF (NC(4) .NE. 0) THEN
                   DO 900 K = 1,MX
                      KK = MX - K + 1
                      IF (KK.LT.I) GOTO 910
                      DO 890 L = 1,J
                         IF (NL(KK,L,4) .NE. 0) THEN
                            SUMF4 = 0.0
                            DO 880 K1 = 1,KK
                               DO 870 L1 = L,MY
                                  SUMF4 = SUMF4 + F(K1,L1)
  870                          CONTINUE
  880                       CONTINUE

                SUM4 = SUM4 +(FLOAT(NL(K,L,4)) / SNT)*(F(I,J) / SUMF4)

                         ENDIF
  890                 CONTINUE
  900              CONTINUE
                ENDIF
C
C
C      *            Y UPPER LIMITS                                          *
C
                SUM5 = 0
  910           IF (NC(5) .NE. 0) THEN
                   DO 1000 L = 1,MY
                      LL = MY - L + 1
                      IF (LL.LT.J) GOTO 1010
                      IF (NL(I,LL,5) .NE. 0) THEN
                         SUMF5 = 0.0
                         DO 950 L1 = 1,LL
                            SUMF5 = SUMF5 + F(I,L1)
  950                    CONTINUE

                SUM5 = SUM5 +(FLOAT(NL(I,LL,5)) / SNT)*(F(I,J) / SUMF5)

                      ENDIF
 1000              CONTINUE
                ENDIF
C
C
C      *               X UPPER LIMITS                                       *
C
                SUM6 = 0
 1010           IF (NC(6) .NE. 0) THEN
                   DO 1100 K = 1,MX
                      KK = MX - K + 1
                      IF (KK.LT.I) GOTO 1110
                      IF (NL(KK,J,6) .NE. 0) THEN
                         SUMF6 = 0.0
                         DO 1050 K1 = 1,KK
                            SUMF6 = SUMF6 + F(K1,J)
 1050                    CONTINUE

                SUM6 = SUM6 +(FLOAT(NL(KK,J,6)) / SNT)*(F(I,J) / SUMF6)

                      ENDIF
 1100              CONTINUE
                ENDIF
C
C
C      *            DOUBLE UPPER LIMITS                                     *
C
                SUM7 = 0
 1110           IF (NC(7) .NE. 0) THEN
                   DO 1200 K = 1,MX
                      KK = MX - K + 1
                      IF (KK.LT.I) GOTO 1210
                      DO 1190 L = 1,MY
                         LL = MY - L + 1
                         IF (LL.LT.J) GOTO 1200
                         IF (NL(KK,LL,7) .NE. 0) THEN
                            SUMF7 = 0.0
                            DO 1180 K1 = 1,KK
                               DO 1170 L1 = 1,LL
                                  SUMF7 = SUMF7 + F(K1,L1)
 1170                          CONTINUE
 1180                       CONTINUE

                SUM7 = SUM7 +(FLOAT(NL(KK,LL,7)) / SNT)*(F(I,J) / SUMF7)

                         ENDIF
 1190                 CONTINUE
 1200              CONTINUE
                ENDIF
C
C
C      *               Y UPPER, X LOWER LIMITS                              *
C
                SUM8 = 0
 1210           IF (NC(8) .NE. 0) THEN
                   DO 1300 K = 1,I
                      DO 1290 L = 1,MY
                         LL = MY - L + 1
                         IF (LL.LT.J) GOTO 1300
                         IF (NL(K,LL,8) .NE. 0) THEN
                            SUMF8 = 0.0
                            DO 1280 K1 = K,MX
                               DO 1270 L1 = 1,LL
                                  SUMF8 = SUMF8 + F(K1,L1)
 1270                          CONTINUE
 1280                       CONTINUE

                SUM8 = SUM8 +(FLOAT(NL(KK,LL,8)) / SNT)*(F(I,J) / SUMF8)

                         ENDIF
 1290                 CONTINUE
 1300              CONTINUE
                ENDIF
C
C      *            COMPUTE A NEW ESTIMATE OF F(I,J).                       *
C
 1400           SUM = SUM1 + SUM2 + SUM3 + SUM4 + 
     +                SUM5 + SUM6 + SUM7 + SUM8

                F(I,J) = FLOAT(NU(I,J)) / SNT + SUM
                DEL = DEL + (F(I,J) - FC(I,J)) ** 2
                FC(I,J) = F(I,J)
             ENDIF
 1500     CONTINUE
 1600  CONTINUE
C
C      *             CHECK CONVERGENCE                                      *
C
       ITE = ITE + 1

       IF (((DEL) .GT. TOL) .AND. (ITE.LE.MAX)) GOTO 500
       RETURN
       END
