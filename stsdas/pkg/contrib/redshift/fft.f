      SUBROUTINE FFTR   (A,GAMN,N,IWK) 
C 
C-FFTR----------S-------LIBRARY 3--------------------------------------- 
C 
C   FUNCTION            - TO COMPUTE THE FAST FOURIER TRANSFORM OF A 
C                           REAL DATA SEQUENCE. 
C   USAGE               - CALL FFTR(A,GAMN,N,IWK) 
C   PARAMETERS   A      - REAL VECTOR OF LENGTH N WHICH, ON INPUT, 
C                           CONTAINS THE DATA TO BE TRANSFORMED. ON 
C                           OUTPUT, THE FIRST N/2 COMPLEX FOURIER COEF- 
C                           FICIENTS ARE STORED IN THE N LOCATIONS OF 
C                           A. THE (N/2+1)TH FOURIER COEFFICIENT IS 
C                           CONTAINED IN GAMN (DESCRIBED BELOW). THE 
C                           REMAINING COEFFICIENTS MAY BE DETERMINED BY 
C                             A(N+2-I)=COMPLEX CONJUGATE OF A(I) 
C                           FOR I = 2,3,4,...,N/2. 
C                GAMN   - COMPLEX VARIABLE WHICH, ON OUTPUT, CONTAINS 
C                           THE (N/2+1)TH FOURIER COEFFICIENT. 
C                N      - LENGTH OF DATA VECTOR A. N MUST BE A 
C                           POSITIVE EVEN INTEGER. 
C                IWK    - WORK STORAGE. IF N IS ANY INTEGRAL POWER OF 2, 
C                           SAY N=2**M, THEN IWK MUST HAVE LENGTH M. IF 
C                           N IS NOT A POWER OF 2, THEN IWK MUST HAVE 
C                           LENGTH 3(F+N)+26, WHERE F IS THE NUMBER OF 
C                           PRIME FACTORS IN N/2. 
C   PRECISION           - SINGLE 
C   REQD. IMSL ROUTINES - FFRDR2,FFTP,FFT2 
C   LANGUAGE            - FORTRAN 
C----------------------------------------------------------------------- 
C   LATEST REVISION     - OCTOBER 5,1973 
C 
      DIMENSION          IWK(1),A(1),G(2),B(2),Z(2) 
      COMPLEX            A,GAMN,XIMAG,ALPH,BETA,GAM,S1,ZD 
      EQUIVALENCE        (GAM,G(1)),(ALPH,B(1)),(Z,AR),(Z(2),AI),(ZD,Z) 
      DATA               ZERO/0.0/,HALF/0.5/,ONE/1.0/ 
      DATA               RPI/3.1415926535898/,IMAX/24/ 
      IF (N .NE. 2) GO TO 5 
      ZD = A(1) 
      THETA = AR 
      TP = AI 
      GAMN = CMPLX(THETA-TP,ZERO) 
      A(1) = CMPLX(THETA+TP,ZERO) 
      GO TO 9005 
    5 CONTINUE 
      ND2 = N/2 
C                                  COMPUTE THE CENTER COEFFICIENT 
      GAM = CMPLX(ZERO,ZERO) 
      DO 10 I=1,ND2 
         GAM = GAM + A(I) 
   10 CONTINUE 
      TP = G(1)-G(2) 
      GAM = CMPLX(TP,ZERO) 
C                                  DETERMINE THE SMALLEST M SUCH THAT 
C                                  N IS LESS THAN OR EQUAL TO 2**M 
      MTWO = 2 
      M = 1 
      DO 15 I=1,IMAX 
         IF (ND2 .LE. MTWO) GO TO 20 
         MTWO = MTWO+MTWO 
         M = M+1 
   15 CONTINUE 
   20 IF (ND2 .EQ. MTWO) GO TO 25 
C                                  N IS NOT A POWER OF TWO, CALL THE 
C                                  PRIME FACTOR FFT. 
      CALL FFTP(A,ND2,IWK,IWK,IWK) 
      GO TO 30 
C                                  N IS A POWER OF TWO, CALL FFT2. 
   25 CALL FFT2(A,M,IWK) 
      CALL FFRDR2(A,M,IWK) 
   30 ALPH = A(1) 
      A(1) = B(1) + B(2) 
      ND4 = (ND2+1)/2 
      IF (ND4 .LT. 2) GO TO 40 
      NP2 = ND2 + 2 
      THETA = RPI/ND2 
      TP = THETA 
      XIMAG = CMPLX(ZERO,ONE) 
C                                  DECOMPOSE THE COMPLEX VECTOR A 
C                                  INTO THE COMPONENTS OF THE TRANSFORM 
C                                  OF THE INPUT DATA. 
      DO 35 K = 2,ND4 
         NMK = NP2 - K 
         S1 = CONJG(A(NMK)) 
         ALPH = A(K) + S1 
         BETA = XIMAG*(S1-A(K)) 
         S1 = CMPLX(COS(THETA),SIN(THETA)) 
         A(K) = (ALPH+BETA*S1)*HALF 
         A(NMK) = CONJG(ALPH-BETA*S1)*HALF 
         THETA = THETA + TP 
   35 CONTINUE 
   40 CONTINUE 
      GAMN = GAM 
 9005 RETURN 
      END 
      SUBROUTINE FFRDR2 (A,M,IWK) 
C 
C-FFRDR2--------S-------LIBRARY 3--------------------------------------- 
C 
C   FUNCTION            - THIS SUBROUTINE PERMUTES A COMPLEX DATA VECTOR 
C                           IN REVERSE BINARY ORDER TO NORMAL ORDER. THE 
C                           ROUTINE CAN ALSO BE USED TO PERMUTE A COM- 
C                           PLEX DATA VECTOR IN NORMAL ORDER TO REVERSE 
C                           BINARY ORDER SINCE THE PERMUTATION IS SYM- 
C                           METRIC. 
C   USAGE               - CALL FFRDR2(A,M,IWK) 
C   PARAMETERS   A      - COMPLEX VECTOR OF LENGTH N=2**M WHICH CONTAINS 
C                           ON INPUT THE DATA TO BE PERMUTED. ON OUTPUT, 
C                           A CONTAINS THE PERMUTED DATA VECTOR. 
C                M      - N=2**M IS THE NUMBER OF DATA POINTS. 
C                IWK    - WORK AREA VECTOR OF LENGTH M+1 
C   PRECISION           - SINGLE 
C   LANGUAGE            - FORTRAN 
C----------------------------------------------------------------------- 
C   LATEST REVISION     - MARCH 16, 1973 
C 
      DIMENSION          IWK(1),A(1) 
      COMPLEX            A,TEMP 
C 
      IF(M .LE. 1) GO TO 9005 
      MP = M+1 
      JJ = 1 
C                                  INITIALIZE WORK VECTOR 
      IWK(1) = 1 
      DO 5  I = 2,MP 
         IWK(I) = IWK(I-1) * 2 
    5 CONTINUE 
      N4 = IWK(MP-2) 
      IF (M .GT. 2) N8 = IWK(MP-3) 
      N2 = IWK(MP-1) 
      LM = N2 
      NN = IWK(MP)+1 
      MP = MP-4 
C                                  DETERMINE INDICES AND SWITCH A*S 
      J = 2 
   10 JK = JJ + N2 
      TEMP = A(J) 
      A(J) = A(JK) 
      A(JK) = TEMP 
      J = J+1 
      IF (JJ .GT. N4) GO TO 15 
      JJ = JJ + N4 
      GO TO 35 
   15 JJ = JJ - N4 
      IF (JJ .GT. N8) GO TO 20 
      JJ = JJ + N8 
      GO TO 35 
   20 JJ = JJ - N8 
      K = MP 
   25 IF (IWK(K) .GE. JJ) GO TO 30 
      JJ = JJ - IWK(K) 
      K = K - 1 
      GO TO 25 
   30 JJ = IWK(K) + JJ 
   35 IF (JJ .LE. J) GO TO 40 
      K = NN - J 
      JK = NN - JJ 
      TEMP = A(J) 
      A(J) = A(JJ) 
      A(JJ) = TEMP 
      TEMP = A(K) 
      A(K) = A(JK) 
      A(JK) = TEMP 
   40 J = J + 1 
C                                  CYCLE REPEATED UNTIL LIMITING NUMBER 
C                                    OF CHANGES IS ACHIEVED 
      IF (J .LE. LM) GO TO 10 
 9005 RETURN 
      END 
      SUBROUTINE FFT2   (A,M,IWK) 
C 
C-FFT2----------S-------LIBRARY 3--------------------------------------- 
C 
C   FUNCTION            - COMPUTE THE FAST FOURIER TRANSFORM, GIVEN A 
C                           COMPLEX VECTOR OF LENGTH EQUAL TO A POWER 
C                           OF TWO 
C   USAGE               - CALL FFT2(A,M,IWK) 
C   PARAMETERS   A      - COMPLEX VECTOR OF LENGTH N=2**M WHICH CONTAINS 
C                           ON INPUT THE DATA TO BE TRANSFORMED (ASSUMED 
C                           TO BE IN NORMAL ORDER). ON OUTPUT, A CON- 
C                           TAINS THE FOURIER COEFFICIENTS STORED IN 
C                           REVERSE BINARY ORDER. 
C                M      - N = 2**M IS THE NUMBER OF DATA POINTS. 
C                IWK    - WORK AREA VECTOR OF LENGTH M+1. 
C   PRECISION           - SINGLE 
C   LANGUAGE            - FORTRAN 
C----------------------------------------------------------------------- 
C   LATEST REVISION     - OCTOBER 5,1973 
C 
      DIMENSION          IWK(1),A(1),Z0(2),Z1(2),Z2(2),Z3(2) 
      COMPLEX            A,ZA0,ZA1,ZA2,ZA3,AK2 
      EQUIVALENCE        (ZA0,Z0),(ZA1,Z1),(ZA2,Z2),(ZA3,Z3), 
     *                   (A0,Z0(1)),(B0,Z0(2)),(A1,Z1(1)),(B1,Z1(2)), 
     *                   (A2,Z2(1)),(B2,Z2(2)),(A3,Z3(1)),(B3,Z3(2)) 
      DATA               SQ,SK,CK /.70710678118655,.38268343236509, 
     *                            .92387953251129/ 
      DATA               TWOPI/6.2831853071796/,ZERO/0.0/,ONE/1.0/ 
C                                  SQ=SQRT2/2,SK=SIN(PI/8),CK=COS(PI/8) 
C                                  TWOPI=2*PI 
      MP = M+1 
      N = 2**M 
      IWK(1) = 1 
      MM = (M/2)*2 
      KN = N+1 
C                                  INITIALIZE WORK VECTOR 
      DO 5  I=2,MP 
         IWK(I) = IWK(I-1)+IWK(I-1) 
    5 CONTINUE 
      RAD = TWOPI/N 
      MK = M - 4 
      KB = 1 
      IF (MM .EQ. M) GO TO 15 
      K2 = KN 
      K0 = IWK(MM+1) + KB 
   10 K2 = K2 - 1 
      K0 = K0 - 1 
      AK2 = A(K2) 
      A(K2) = A(K0)-AK2 
      A(K0) = A(K0)+AK2 
      IF (K0 .GT. KB) GO TO 10 
   15 C1 = ONE 
      S1 = ZERO 
      JJ = 0 
      K = MM - 1 
      J = 4 
      IF (K .GE. 1) GO TO 30 
      GO TO 9005 
   20 IF (IWK(J) .GT. JJ) GO TO 25 
      JJ = JJ - IWK(J) 
      J = J-1 
      IF (IWK(J) .GT. JJ) GO TO 25 
      JJ = JJ - IWK(J) 
      J = J - 1 
      K = K + 2 
      GO TO 20 
   25 JJ = IWK(J) + JJ 
      J = 4 
   30 ISP = IWK(K) 
      IF (JJ .EQ. 0) GO TO 40 
C                                  RESET TRIGONOMETRIC PARAMETERS 
      C2 = JJ * ISP * RAD 
      C1 = COS(C2) 
      S1 = SIN(C2) 
   35 C2 = C1 * C1 - S1 * S1 
      S2 = C1 * (S1 + S1) 
      C3 = C2 * C1 - S2 * S1 
      S3 = C2 * S1 + S2 * C1 
   40 JSP = ISP + KB 
C                                  DETERMINE FOURIER COEFFICIENTS 
C                                    IN GROUPS OF 4 
      DO 50 I=1,ISP 
         K0 = JSP - I 
         K1 = K0 + ISP 
         K2 = K1 + ISP 
         K3 = K2 + ISP 
         ZA0 = A(K0) 
         ZA1 = A(K1) 
         ZA2 = A(K2) 
         ZA3 = A(K3) 
         IF (S1 .EQ. ZERO) GO TO 45 
         TEMP = A1 
         A1 = A1 * C1 - B1 * S1 
         B1 = TEMP * S1 + B1 * C1 
         TEMP = A2 
         A2 = A2 * C2 - B2 * S2 
         B2 = TEMP * S2 + B2 * C2 
         TEMP = A3 
         A3 = A3 * C3 - B3 * S3 
         B3 = TEMP * S3 + B3 * C3 
   45    TEMP = A0 + A2 
         A2 = A0 - A2 
         A0 = TEMP 
         TEMP = A1 + A3 
         A3 = A1 - A3 
         A1 = TEMP 
         TEMP = B0 + B2 
         B2 = B0 - B2 
         B0 = TEMP 
         TEMP = B1 + B3 
         B3 = B1 - B3 
         B1 = TEMP 
         A(K0) = CMPLX(A0+A1,B0+B1) 
         A(K1) = CMPLX(A0-A1,B0-B1) 
         A(K2) = CMPLX(A2-B3,B2+A3) 
         A(K3) = CMPLX(A2+B3,B2-A3) 
   50 CONTINUE 
      IF (K .LE. 1) GO TO 55 
      K = K - 2 
      GO TO 30 
   55 KB = K3 + ISP 
C                                  CHECK FOR COMPLETION OF FINAL 
C                                    ITERATION 
      IF (KN .LE. KB) GO TO 9005 
      IF (J .NE. 1) GO TO 60 
      K = 3 
      J = MK 
      GO TO 20 
   60 J = J - 1 
      C2 = C1 
      IF (J .NE. 2) GO TO 65 
      C1 = C1 * CK + S1 * SK 
      S1 = S1 * CK - C2 * SK 
      GO TO 35 
   65 C1 = (C1 - S1) * SQ 
      S1 = (C2 + S1) * SQ 
      GO TO 35 
 9005 RETURN 
      END 
      SUBROUTINE FFTP(A,N,I,J,K) 
      RETURN 
      END 
