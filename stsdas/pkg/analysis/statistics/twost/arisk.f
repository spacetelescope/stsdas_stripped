C
C      **********************************************************************
C      ******************** SUBROUTINE ARISK  *******************************
C      **********************************************************************
C
       SUBROUTINE ARISK(XY, ID1, NCOMP, NTOT, R, XM, X, E1, NG, H)
C
C  
C      *       THIS SUBROUTINE COMPUTES THE FOLLOWING FOUR                  *
C      *       ARRAYS FOR SUBROUTINE COX, LRANK, AND PWLCXN.                *
C      *         R(I) : NO. OF OBSERVATIONS IN RISK SET AT THE              *
C      *                I-TH DISTINCT FAILURE TIME.                         *
C      *        XM(I) : MULTIPLICITY OF THE I-TH DISTINCT                   *
C      *                FAILURE TIME.                                       *
C      *        E1(I) : XM(I)/R(I)                                          *
C      *         H(I) : KAPLAN AND MEIER'S ESTIMATES OF THE                 *
C      *                SURVIVOR FUNCTION                                   *
C      *                                                                    *
C      *         X(I) : THE ARRAY OF DISTINCT FAILURE TIMES                 *
C      *         NG   : NO OF X                                             *
C      *  THIS SUBROUTINE IS OBTAINED FROM ELISA T. LEE, "STATISTICAL       *
C      *  METHODS FOR SURVIVAL DATA ANALYSIS", 1980, LIFETIME LEARNING      *
C      *  PUBLICATIONS (BELMONT:CA); BUT HAS BEEN SIGNIFICANTLY MODIFIED.   *
C      *                                                                    *
C

       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION R(NTOT),XM(NTOT),X(NTOT),H(NTOT),E1(NTOT)
       DIMENSION XY(NTOT),ID1(NTOT)
C
       L = 1
       I = 1
       R(L) = REAL(NCOMP)
C
C      *             COMPUTE RISK SETS, AND OTHER QUANTITIES                *
C
 24    IF (ID1(I) .NE. 0) THEN
          R(L) = R(L) - 1.0
          I = I + 1
          GOTO 24
       ENDIF
C
  25   XM(L) = 1.0
       XNC = 0.0
       TEMP = XY(I)
       X(L) = TEMP

  21   IF (I .NE. NCOMP) THEN
          I = I + 1
C
          IF (ID1(I) .NE. 1) THEN
             IF (TEMP .NE. XY(I)) GOTO 20
             XM(L) = XM(L) + 1.0
             GOTO 21
          ENDIF

  26      XNC = XNC + 1.0
          X(L) = TEMP
          GOTO 21

  20      L = L + 1
          R(L) = R(L-1) - XM(L-1) - XNC
          GOTO 25
       ENDIF

  23   X(L) = TEMP
       NG = L
C    
C      *          COMPUTE KM ESTIMATOR                                      *

       DO 30 I = 1, NG
          E1(I) = XM(I) / R(I)
  30   CONTINUE

       H(1) = 1.0
       NG1 = NG + 1

       DO 31 I = 2, NG1
          H(I) = H(I-1) * (1.0 - E1(I-1))
  31   CONTINUE

       RETURN
       END
