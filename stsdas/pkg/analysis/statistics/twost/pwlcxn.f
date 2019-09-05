C
C      **********************************************************************
C      ******************** SUBROUTINE PWLCXN  ******************************
C      **********************************************************************
C
       SUBROUTINE PWLCXN (VERB, XY, ID1, ID2, IC, N1, N2, NCOMP, NTOT, 
     +                    H, XM, SCORE, TEST, PROB, IWLCX)
C
C      *           THIS SUBROUTINE COMPUTES PETO AND PETO'S                 *
C      *           GENERALIZED WILCOXON STATISTIC.                          *
C      *                                                                    *
C      *           OBTAINED FROM ELISA T. LEE, "STATISTICAL METHODS FOR     *
C      *           SURVIVAL DATA ANALYSIS", 1980, LIFETIME LEARNING         *
C      *           PUBLICATIONS (BELMONT:CA)                                *
C      *                                                                    *
C      * SUBROUTINES                                                        *
C      *           STAT                                                     *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       LOGICAL  VERB

       DIMENSION H(NTOT),XM(NTOT),SCORE(NTOT)
       DIMENSION XY(NTOT),ID1(NTOT),ID2(NTOT)
C
       IWLCX = 0
       IF (IC .EQ. 0) THEN
          IWLCX = 1
          RETURN    
       ENDIF
C
       L = 1
       I = 1
       IJK = 0
C
C*******       THE NEXT LINE IS CHANGED FROM "EQ.1" TO "EQ.0".              *
C
  63   IF (ID1(I) .NE. 0) THEN
          IF (IJK .EQ. 1) GOTO 65
          SCORE(I) = H(1) - 1.0
          IF (I .EQ. NCOMP) GOTO 200
          I = I + 1
          GOTO 63
       ENDIF
  62   M = INT(XM(L))

       DO 64 J = 1,M
          SCORE(I) = H(L) + H(L+1) - 1.0
          IF (I .EQ. NCOMP) GOTO 200
          I = I + 1
  64   CONTINUE

       IJK = 1
       L = L + 1
       GOTO 63

  65   SCORE(I) = H(L) - 1.0
       IF (I .EQ. NCOMP) GOTO 200
       I = I + 1
       GOTO 63
C
C*******      THE NEXT LINE IS ADDED. ALSO THE PRINTING COMMANDS            *
C*******      ARE CHANGED.                                                  *
C
 200   CALL STAT (VERB, ID1, ID2, N1, N2, NCOMP, NTOT, SCORE, TEST)
       PROB = 1.0 - AGAUSS (TEST)
       RETURN
       END
