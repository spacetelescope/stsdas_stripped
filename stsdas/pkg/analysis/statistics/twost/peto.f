C     
C
C***************************************************************************
C**************************  SUBROUTINE PETO   *****************************
C***************************************************************************
C
C
       SUBROUTINE PETO (ID1, ID2, XY, N1, N2, NCOMP, NTOT, 
     +                  TEST, PROB, D, E, R, D1, E1, R1, D2,
     +                  E2, R2, F, A, FT, AT, XA, SCORE, VAR)
C     *
C     * THIS SUBROUTINE COMPUTES THE PETO-PRENTICE STATISTIC USING THE      *
C     * FORMULATION IN LATTA, 'A MONTE-CARLO STUDY OF SOME TWO-SAMPLE RANK  *
C     * TESTS WITH CENSORED DATA', 1981, JOURNAL OF THE AMERICAN STATISTICAL*
C     * ASSOCIATION, VOL 76, PP 713-719.  THE FORM USED IS FROM EQUATION    *
C     * 2.2 AND THE ASYMPTOTIC VARIANCE ESTIMATE GIVEN IN THE ARTICLE IS    *
C     * USED FOR THE VARIANCE.                                              *
C     *                                                                     *
C     * INPUT                                                               *
C     *      ID1(I) : INDICATOR OF CENSORSHIP OF XY(I)                      *
C     *      ID2(I) : INDICATOR OF GROUP; 1 OR 2                            *
C     *      XY(I)  : DATA POINTS (SORTED TO SMALLEST TO LARGEST)           *
C     *      N1     : NUMBER OF DATA POINTS IN GROUP 1                      *
C     *      N2     : NUMBER OF DATA POINTS IN GROUP 2                      *
C     *      NCOMP  : TOTAL NUMBER OF DATA POINTS = N1 + N2                 *
C     *                                                                     *
C     * OUTPUT                                                              *
C     *     TEST    : STANDARDIZED PETO-PRENTICE STATISTIC                  *
C     *     PROB    : PROBABILITY                                           *
C     *                                                                     *
C     * OTHERS                                                              *
C     *      D1(I)  : THE NUMBER OF DETECTIONS OF GROUP 1 AT XY(I)          *
C     *      D2(I)  : THE NUMBER OF DETECTIONS OF GROUP 2 AT XY(I)          *
C     *      D(I)   : THE NUMBER OF DETECTIONS AT XY(I)                     *
C     *      R1(I)  : RISK SET OF GROUP 1 AT XY(I)                          *
C     *      R2(I)  : RISK SET OF GROUP 2 AT XY(I)                          *
C     *      R(I)   : RISK SET AT XY(I)                                     *
C     *      E1(I)  : THE NUMBER OF CENSORED POINTS OF GROUP 1 BETWEEN      *
C     *                                                     XY(I) & XY(I+1) *
C     *      E2(I)  : THE NUMBER OF CENSORED POINTS OF GROUP 2 BETWEEN      *
C     *                                                     XY(I) & XY(I+1) *
C     *      E(I)   : THE NUMBER OF CENSORED POINTS BETWEEN XY(I) & XY(I+1) *
C     *      F(I)   : THE ESTIMATE OF THE SURVIVAL FUNCTION AT XY(I)        *
C     *      A(I)   : COEFFICIENT AT XY(I)                                  *
C     *      XA(I)  : SUM OF 2 X D2(I) AND E2(I)                            *
C     *      SCORE  : SCORE OF THE DATA                                     *
C     *      VAR    : VARIANCE OF THE DATA                                  *



       IMPLICIT REAL*8 (A-H, O-Z), INTEGER (I-N)

       DIMENSION ID1(NTOT),ID2(NTOT),XY(NTOT)
       DIMENSION F(NTOT),A(NTOT),FT(NTOT),AT(NTOT)
       DIMENSION D(NTOT),E(NTOT),R(NTOT),XA(NTOT)
       DIMENSION D1(NTOT),E1(NTOT),R1(NTOT),D2(NTOT)
       DIMENSION E2(NTOT),R2(NTOT)

       J = 0
       I = 1
       L = 1
       F(I)  = 1.0
       A(I)  = 1.0
       R1(L) = REAL(N1)
       R2(L) = REAL(N2)
       R(L)  = REAL(NCOMP)
       ET1 = 0.0
       ET2 = 0.0

C
C     *  IF THE SMALLEST VALUE IS CENSORED, THIS LOOP WILL GO THROUGH THE   *
C     *  DATA UNTIL THE FIRST DETECTION IS REACHED.                         *
C
   10  IF (ID1(I) .NE. 0) THEN
          IF (ID2(I) .EQ. 1) THEN
             ET1 = ET1 + 1.0
          ELSE
             ET2 = ET2 + 1.0
          ENDIF
          I = I + 1
          F(I) = 1.0
          A(I) = 1.0
          GOTO 10
       ENDIF
C
C     *     START LOOP; THIS LOOP CONTINUES UNTIL THE COMPUTATION IS       *
C     *     FINISHED.                                                      *
C
   20  D(L)  = 0.0
       D1(L) = 0.0
       D2(L) = 0.0
       E(L)  = 0.0
       E1(L) = 0.0
       E2(L) = 0.0
       TEMP  = XY(I)
       K = 0
C
C     * CHECK IF THE DATA POINT IS DETECTED OR NOT. IF DETECTED, CONTINUE. *
C     * THEN CHECK WHICH GROUP THE DATA POINT BELONGS TO.                  *
C     * COMPUTE THE SURVIVAL FUNCTION AND THE COEFFICIENT FOR THE          *
C     *  APPROPRIATE GROUP.                                                *
C     * HERE, FT AND AT ARE USED, SINCE WE ASSUME THAT THERE ARE NO TIES.  *
C     * IF THERE ARE TIES IN THE DATA, FT AND AT WILL BE APPROPRIATELY     *
C     *  CONVERTED INTO THE FORM FOR TIED DATA AND PUT IN F AND A.         *
C

  30   IF (ID1(I) .EQ. 0) THEN
         IF (ID2(I) .EQ. 1) THEN
            D1(L) = D1(L) + 1.0
         ELSE
            D2(L) = D2(L) + 1.0
         ENDIF

         D(L) = D1(L) + D2(L)
         J = J + 1
         K = K + 1

         IF (L .EQ. 1) THEN
           RISK = R(L) - (ET1+ET2) - (D(L) - 1.0)
           IF (J .EQ. 1) THEN
              FT(J) = RISK / (RISK + 1.0)
              AT(J) = (RISK + 1.0) / (RISK + 2.0)
           ELSE
              FT(J) = FT(J-1) * RISK / (RISK + 1.0)
              AT(J) = AT(J-1) * (RISK + 1.0) / (RISK + 2.0)
           ENDIF
         ELSE 
           RISK = (R(L-1) - D(L-1)) - E(L-1) - (D(L) - 1.0)
           FT(J) = FT(J-1) * RISK / (RISK + 1.0)
           AT(J) = AT(J-1) * (RISK + 1.0) / (RISK + 2.0)
         ENDIF

C
C     * IF THE DATA POINT IS CENSORED, START CHECKING HOW MANY CENSORED    *
C     * DATA POINTS THERE ARE BETWEEN XY(I) AND XY(I+1).                   *
C
        ELSE
          IF (ID2(I) .EQ. 1) THEN
             E1(L) = E1(L) + 1.0
          ELSE
             E2(L) = E2(L) + 1.0
          ENDIF
          E(L) = E1(L) + E2(L)
        ENDIF

        IF (I .LT. NCOMP) THEN
        I = I + 1
C
C     * IF THE DATA POINT XY(I) IS TIED WITH PREVIOUS POINTS, GO BACK      *
C     * TO ADDRESS 30, AND COUNT THE NUMBER OF TIED DATA POINTS.           *
C     * ALSO, IF XY(I) IS NOT DETECTED GO BACK TO ADDRESS 30, AND COUNT    *
C     * THE NUMBER OF THE CENSORED DATA POINTS                             *
C
        IF (TEMP .EQ. XY(I)) GOTO 30
        IF (ID1(I) .NE. 0) GOTO 30

C
C     * IF THE DATA POINTS WERE TIED, K > 1.  COMPUTE THE AVERAGE OF       *
C     * FT AND AT BETWEEN JJ= 1, K AND USE THE AVERAGES FOR F AND A OF THE *
C     * DATA POINT.                                                        *
C
         SUM1 = 0.0
         SUM2 = 0.0
         JSTART = J - K + 1
         DO 50 JJ = JSTART, J
            SUM1 = SUM1 + FT(JJ)
            SUM2 = SUM2 + AT(JJ)
   50    CONTINUE

         F(L)  = SUM1 / FLOAT(K)
         A(L)  = SUM2 / FLOAT(K)
         XA(L) = 2.0 * D2(L) + E2(L)
C
C     *            COMPUTE NEW RISK SETS FOR THE NEXT STEP.                *
C
         IF (L .EQ. 1) THEN
             R1(L) = R1(L) - ET1
             R2(L) = R2(L) - ET2
             R(L)  = R1(L) + R2(L)
         ELSE
             R1(L) = R1(L-1) - D1(L-1) - E1(L-1)
             R2(L) = R2(L-1) - D2(L-1) - E2(L-1)
             R(L)  = R1(L) + R2(L)
         ENDIF
         L = L + 1
         GOTO 20
      ENDIF
C
C     *       COMPUTE THE SCORE AND VARIANCE                         *
C

      SCORE = 0.0
      VAR   = 0.0
      L1 = L - 1
      DO 200 I = 1, L1

         SCORE = SCORE + (2.0 * F(I) - 1.0) * D2(I) + 
     +           (F(I) - 1.0) * E2(I)

         SUM = 0.0
         JSTART = I + 1
         DO 100 J = JSTART, L1
            SUM = SUM + F(J) * XA(J)
  100    CONTINUE

         VAR = VAR + F(I) * (1.0 - A(I)) * XA(I) - (A(I) - F(I)) * 
     +               XA(I) * (F(I) * XA(I) + 2.0 * SUM)

  200    CONTINUE

C
C     *        NOW COMPUTE THE PETO-PRENTICE STATISTIC                   *
C
        TEST = SCORE / DSQRT (VAR)
        PROB = 1.0 - AGAUSS (TEST)

        RETURN
        END
