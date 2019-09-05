C
C      **********************************************************************
C      ********************** SUBROUTINE BIN  *******************************
C      **********************************************************************
C
       SUBROUTINE BIN(X, Y, NP, NC, DELX, DELY, XORG, YORG, NTOT, 
     +                MX, MY, NM, NU, NL)
C
C
C      *                                                                    *
C      *    THIS SUBROUTINE DOES BINNING AND CHANGES CENSORED POINTS        *
C      *    WHICH DO NOT HAVE DETECTED POINTS ABOVE (OR BELOW)              *
C      *    TO DETECTED POINTS.                                             *
C      *                                                                    *
C      *             WARNING   WARNING   WARNING   WARNING                  *
C      *                                                                    *
C      *    THE USER SHOULD BE WARNED THAT THIS SUBROUTINE ACTUALLY         *
C      *    CHANGES THE DATA!!  FIRST, IT REDEFINES SOME LIMITS TO          *
C      *    DETECTIONS.  IF THE BINS ARE CHOSEN TO BE TOO NARROW, THEN      *
C      *    VIRTUALLY ALL LIMITS COULD BE CHANGED.  SECOND, IT PUSHES       *
C      *    EACH LIMIT INTO THE ADJACENT BIN.  IF THE BINS ARE CHOSEN TO    *
C      *    TO BE TOO WIDE, THIS SUBSTANTIALLY ALTERS THE MEASURED VALUES.  *
C      *    THUS, THE USER MUST TREAD A FINE LINE IN CHOSING BIN SIZES.     *
C      *                                                                    * 
C      *                                                                    *
C      *    INPUT                                                           *
C      *           X(I)  : INDEPENDENT VARIABLE                             *
C      *           Y(I)  : DEPENDENT VARIABLE                               *
C      *          NP(I)  : INDICATOR OF CENSORING                           *
C      *          NC(K)  : NUMBER OF CENSORED POINTS OF EACH TYPE           *
C      *          DELX   : BIN SIZE OF X AXIS                               *
C      *          DELY   : BIN SIZE OF Y AXIS                               *
C      *          XORG   : ORIGIN OF X                                      *
C      *          YORG   : ORIGIN OF Y                                      *
C      *          NTOT   : TOTAL NUMBER OF DATA                             *
C      *            MX   : NUMBER OF BINS IN X                              *
C      *            MY   : NUMBER OF BINS IN Y                              *
C      *    OUTPUT                                                          *
C      *        NM(K)    : NUMBER OF LIMITS CHANGED TO DETECTIONS           *
C      *        NU(I,J)  : NUMBER OF UNCENSORED IN THE BIN(I,J)             *
C      *        NL(I,J,K): NUMBER OF Y LOWER LIMITS IN THE BIN(I,J)         *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION X(NTOT),Y(NTOT),NP(NTOT),NC(8)
       DIMENSION NM(8),NU(MX,MY),NL(MX,MY,8)
C
       DO 300 I = 1, MX
          DO 200 J = 1, MY
             NU(I,J) = 0
C
             DO 100 K = 1, 8
                NL(I,J,K) = 0
  100        CONTINUE
  200     CONTINUE
  300  CONTINUE
C
       DO 350 K = 1, 8
          NM(K) = 0
  350  CONTINUE
C
       DO 390 I = 1, NTOT
C
C      *    FIND POSITION OF I-TH DATA POINT IN THE GRID AND COUNT          *
C      *    NUMBERS OF N,N1,N2,.....,N8.                                    *
C
          IP = INT ((X(I) - XORG) / DELX) + 1
          JP = INT ((Y(I) - YORG) / DELY) + 1

C
C      *      FOR CONVENIENCE CENSORED POINTS ARE ASSIGNED TO THE NEXT BIN  *
C
C
C      *              DETECTIONS                                            *
C
          IF (NP(I) .EQ. 0) THEN
             NU(IP,JP) = NU(IP,JP) + 1
      
C
C      *              Y LOWER LIMITS                                        *
C
          ELSEIF (NP(I) .EQ. 1) THEN
             NL(IP,JP+1,1) = NL(IP,JP+1,1) + 1
          
C
C      *              X LOWER LIMITS                                        *
C
          ELSEIF (NP(I) .EQ. 2) THEN
             NL(IP+1,JP,2) = NL(IP+1,JP,2) + 1
          
C
C      *              DOUBLE LOWER LIMITS                                   *
C
          ELSEIF (NP(I) .EQ. 3) THEN
             NL(IP+1,JP+1,3) = NL(IP+1,JP+1,3) + 1
          
C
C      *              Y LOWER LIMITS, X UPPER LIMITS                        *
C
          ELSEIF (NP(I) .EQ. 4) THEN
             NL(IP+1,JP-1,4) = NL(IP+1,JP-1,4) + 1
          
C
C      *              Y UPPER LIMITS                                        *
C
          ELSEIF (NP(I) .EQ. -1) THEN
             NL(IP,JP-1,5) = NL(IP,JP-1,5) + 1
          
C
C      *              X UPPER LIMITS                                        *
C
          ELSEIF (NP(I) .EQ. -2) THEN
             NL(IP-1,JP,6)=NL(IP-1,JP,6) + 1
          
C
C      *              DOUBLE  UPPER LIMITS                                  *
C
          ELSEIF (NP(I) .EQ. -3) THEN
             NL(IP-1,JP-1,7) = NL(IP-1,JP-1,7) + 1
          
C
C      *              Y UPPER LIMITS, X LOWER LIMITS                        *
C
          ELSEIF (NP(I) .EQ. -4) THEN
             NL(IP-1,JP+1,8) = NL(IP-1,JP+1,8) + 1

       ENDIF
  390  CONTINUE
C
C      *    START CHECKING THE RELATION BETWEEN CENSORED POINTS AND         *
C      *    DETECTED POINTS. IF THE CENSORED POINTS ARE LOCATED  SO         *
C      *    THAT THEY CANNOT GIVE WEIGHT TO DETECTED POINTS, THE            *
C      *    CENSORED POINTS ARE CHANGED TO DETECTIONS.                      *
C

C
C      *              Y LOWER LIMITS                                        *
C

       IF (NC(1) .NE. 0) THEN
          DO 600 I = 1, MX
             DO 500 J = 1, MY
                JJ = MY - J + 1
                IF (NL(I,JJ,1) .NE. 0) THEN
                   K = JJ
  450              IF (NU(I,K) .EQ. 0) THEN
                      K = K + 1
                      IF (K .LE. MY) GOTO 450
                      NM(1) = NM(1) + NL(I,JJ,1)
                      NU(I,JJ) = NU(I,JJ) + NL(I,JJ,1)
                      NL(I,JJ,1) = 0
                   ENDIF
                ENDIF
  500        CONTINUE
  600     CONTINUE
       ENDIF

C
C
C      *              X LOWER LIMITS                                        *
C
       IF (NC(2) .NE. 0) THEN
          DO 800 J = 1,MY
             DO 700 I = 1,MX
                II = MX - I + 1
                IF (NL(II,J,2) .NE. 0) THEN
                   L = II
  650              IF (NU(L,J) .EQ. 0) THEN
                      L = L + 1
                      IF (L .LE. MX) GOTO 650
                      NM(2) = NM(2) + NL(II,J,2)
                      NU(II,J) = NU(II,J) + NL(II,J,2)
                      NL(II,J,2) = 0
                   ENDIF
                ENDIF
  700        CONTINUE
  800     CONTINUE
       ENDIF

C
C
C      *              DOUBLE LOWER LIMITS                                   *
C
       IF (NC(3) .NE. 0) THEN
          DO 1000 I = 1,MX
             II = MX - I + 1
             DO 950  J = 1,MY
                JJ = MY - J + 1
                IF (NL(II,JJ,3) .NE. 0) THEN
                   L = II
  850              K = JJ
  900              IF (NU(II,JJ) .EQ. 0) THEN
                      K = K + 1
                      IF (K .LE. MY) GOTO 900
                      L = L + 1
                      IF (L .LE. MX) GOTO 850
                      NM(3) = NM(3) + NL(II,JJ,3)
                      NU(II,JJ) = NU(II,JJ) + NL(II,JJ,3)
                      NL(II,JJ,3) = 0
                   ENDIF
                ENDIF
  950        CONTINUE
 1000     CONTINUE
       ENDIF

C
C
C      *              Y LOWER LIMITS, X UPPER LIMITS                        *
C
       IF (NC(4) .NE. 0) THEN
          DO 1300 I = 1,MX
             II = MX - I + 1
             DO 1200 J = 1,MY
                IF (NL(II,J,4) .NE. 0) THEN
                   L = II
 1050              K = J
 1100              IF (NU(L,K) .EQ. 0) THEN
                      K = K - 1
                      IF (K .GE. 1) GOTO 1100
                      L = L + 1
                      IF (L .LE. MX) GOTO 1050
                      NM(4) = NM(4) + NL(II,J,4)
                      NU(II,J) = NU(II,J) + NL(II,J,4)
                      NL(II,J,4) = 0
                   ENDIF
                ENDIF
 1200        CONTINUE
 1300     CONTINUE
       ENDIF

C
C
C      *              Y UPPER LIMITS                                        *
C
       IF (NC(5) .NE. 0) THEN
          DO 1600 I = 1,MX
             DO 1500 J = 1,MY
                IF (NL(I,J,5) .NE. 0) THEN
                   K = J
 1450              IF (NU(I,K) .EQ. 0) THEN
                      K = K - 1
                      IF (K .GE. 1) GOTO 1450
                      NM(5) = NM(5) + NL(I,J,5)
                      NU(I,J)  =  NU(I,J)  +  NL(I,J,5)
                      NL(I,J,5) = 0
                   ENDIF
                ENDIF
 1500        CONTINUE
 1600     CONTINUE
       ENDIF

C
C
C      *              X UPPER LIMITS                                        *
C
       IF (NC(6) .NE. 0) THEN
          DO 1800 J = 1,MY
             DO 1700 I = 1,MX
                IF (NL(I,J,6) .NE. 0) THEN
                   L = I
 1650              IF (NU(L,J) .EQ. 0) THEN
                      L = L - 1
                      IF (L .GE. 1) GOTO 1650
                      NM(6) = NM(6) + NL(I,J,6)
                      NU(I,J) = NU(I,J) + NL(I,J,6)
                      NL(I,J,6) = 0
                   ENDIF
                ENDIF
 1700        CONTINUE
 1800     CONTINUE
       ENDIF

C
C
C      *              DOUBLE UPPER LIMITS                                   *
C
       IF (NC(7) .NE. 0) THEN
          DO 2000 I = 1,MX
             DO 1950 J = 1,MY
                IF (NL(I,J,7) .NE. 0) THEN
                   L = I
 1850              K = J
 1900              IF (NU(L,K) .EQ. 0) THEN
                      K = K - 1
                      IF (K .GE. 1) GOTO 1900
                      L = L - 1
                      IF (L .GE. 1) GOTO 1850
                      NM(7) = NM(7) + NL(I,J,7)
                      NU(I,J) = NU(I,J) + NL(I,J,7)
                      NL(I,J,7) = 0
                   ENDIF
                ENDIF
 1950        CONTINUE
 2000     CONTINUE
       ENDIF

C
C
C      *              Y UPPER LIMITS, X LOWER LIMITS                        *
C
       IF (NC(8) .NE. 0) THEN
          DO 2300 I = 1,MX
             DO 2200 J = 1,MY
                JJ = MY - J + 1
                IF (NL(I,JJ,8) .NE. 0) THEN
                   L = I
 2050              K = JJ
 2100              IF (NU(L,K) .EQ. 0) THEN
                      K = K + 1
                      IF (K .LE. MY) GOTO 2100
                      L = L - 1
                      IF (L .GE. 1) GOTO 2050
                      NM(8) = NM(8) + NL(I,JJ,8)
                      NU(I,JJ)  =  NU(I,JJ) + NL(I,JJ,8)
                      NL(I,JJ,8) = 0
                   ENDIF
                ENDIF
 2200        CONTINUE
 2300     CONTINUE
       ENDIF

       RETURN
       END
