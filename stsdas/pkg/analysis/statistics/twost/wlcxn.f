C
C***************************************************************************
C**************************  SUBROUTINE WLCXN  *****************************
C***************************************************************************
C
C
       SUBROUTINE WLCXN(ID1, ID2, XY, N1, N2, NCOMP, NTOT, TEST, PROB, 
     +                  D, E, R, D1, E1, R1, D2, E2, R2, SCORE, VAR)
C     *
C     * THIS SUBROUTINE COMPUTES THE GEHAN GENERALIZED WILCOXON STATISTIC   *
C     * WITH CONDITIONAL PERMUTATION VARIANCE (HYPERGEOMETRIC VARIANCE)     *
C     * FROM EQUATIONS (2.2) AND (2.3) IN LATTA, 'A MONTE-CARLO STUDY OF    *
C     * SOME TWO-SAMPLE RANK TESTS WITH CENSORED DATA', 1981, JOURNAL OF    *
C     * THE AMERICAN STATISTICAL ASSOCIATION, VOL 76, PP 713-719.           *
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
C     *     TEST    : STANDARDIZED GEHAN STATISTIC                          *
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
C     *      SCORE  : SCORE OF THE DATA                                     *
C     *      VAR    : VARIANCE OF THE DATA                                  *


       IMPLICIT REAL*8 (A-H, O-Z), INTEGER (I-N)

       DIMENSION ID1(NTOT),ID2(NTOT),XY(NTOT)
       DIMENSION D(NTOT),E(NTOT),R(NTOT)
       DIMENSION D1(NTOT),E1(NTOT),R1(NTOT),D2(NTOT)
       DIMENSION E2(NTOT),R2(NTOT)

       I = 1
       L = 1
       R1(L) = REAL(N1)
       R2(L) = REAL(N2)
       R(L)  = REAL(NCOMP)
       ET1 = 0.0
       ET2 = 0.0

C
C     *  IF THE SMALLEST VALUE IS CENSORED, THIS LOOP WILL GO THROUGH THE   *
C     *  DATA UNTIL THE FIRST DETECTION IS REACHED.                         *
C
   10  IF(ID1(I) .NE. 0) THEN
          IF(ID2(I) .EQ. 1) THEN
             ET1 = ET1 + 1.0
          ELSE
             ET2 = ET2 + 1.0
          ENDIF
          I = I + 1
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
C
C     * CHECK IF THE DATA POINT IS DETECTED OR NOT. IF DETECTED, CONTINUE. *
C     * THEN CHECK WHICH GROUP THE DATA POINT BELONGS TO.                  *
C     * COMPUTE THE SURVIVAL FUNCTION AND THE COEFFICIENT FOR THE          *
C     * APPROPRIATE GROUP.                                                *
C

  30   IF(ID1(I) .EQ. 0) THEN
          IF(ID2(I) .EQ. 1) THEN
             D1(L) = D1(L) + 1.0
          ELSE
             D2(L) = D2(L) + 1.0
          ENDIF

          D(L) = D1(L) + D2(L)

C
C     * IF THE DATA POINT IS CENSORED, START CHECKING HOW MANY CENSORED    *
C     * DATA POINTS THERE ARE BETWEEN XY(I) AND XY(I+1).                   *
C
       ELSE
         IF(ID2(I) .EQ. 1) THEN
            E1(L) = E1(L) + 1.0
         ELSE
            E2(L) = E2(L) + 1.0
         ENDIF
            E(L) = E1(L) + E2(L)
         ENDIF

         IF(I .LT. NCOMP) THEN
           I = I + 1
C
C     * IF THE DATA POINT XY(I) IS TIED WITH PREVIOUS POINTS, GO BACK      *
C     * TO ADDRESS 30, AND COUNT THE NUMBER OF TIED DATA POINTS.           *
C     * ALSO, IF XY(I) IS NOT DETECTED GO BACK TO ADDRESS 30, AND COUNT    *
C     * THE NUMBER OF THE CENSORED DATA POINTS                             *
C
           IF(TEMP .EQ. XY(I)) GOTO 30
           IF(ID1(I) .NE. 0) GOTO 30

C
C     *            COMPUTE NEW RISK SETS FOR THE NEXT STEP.                *
C
           IF(L .EQ. 1) THEN
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

           SCORE = SCORE+ R(I) * (D2(I) - (R2(I) * D(I) / R(I)))

           IF (R(I) .GT. 1.0) THEN
              VAR = VAR + D(I) * (R(I) ** 2) * (R2(I) / R(I)) *
     +              (1.0 - (R2(I) / R(I))) * ((R(I) - D(I)) / 
     +              (R(I) - 1.0))
           ENDIF

C           VAR = VAR + D(I) * ((R(I) - REAL(I)) ** 2) + E(I) * 
C    +            (REAL(I) ** 2)

  200   CONTINUE

C        VAR = VAR * REAL(N1 * N2) / REAL(NCOMP * (NCOMP - 1))

CC
C      *        NOW COMPUTE THE GEHAN STATISTIC                          *
C
        TEST = SCORE / DSQRT(VAR)
        PROB = 1.0 - AGAUSS(TEST)
 
        RETURN
        END
