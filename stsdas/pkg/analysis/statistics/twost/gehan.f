C
C      **********************************************************************
C      ********************* SUBROUTINE GEHAN  ******************************
C      **********************************************************************
C
       SUBROUTINE GEHAN (VERB, XY, ID1, ID2, IC, N1, N2, NCOMP, NTOT, 
     +                   R1, R2, TEST, PROB)
C
C 
C      * THIS SUBROUTINE COMPUTES GEHAN'S GENERALIZED WILCOXON TEST         *
C      * STATISTIC.  THE COMPUTATIONAL FORM IS FROM E.T. LEE , STATISTICAL  *
C      * METHODS FOR SURVIVAL DATA ANALYSIS, 1980, LIFETIME LEARNING        *
C      * PUBLICATIONS, BELM0NT, CA. THE FORM USED IS THE MANTEL METHOD OF   *
C      * ASSIGNING A SCORE TO EACH OBSERVATION BASED ON ITS RELATIVE RANK,  *
C      * FROM EQUATION 5.5 AND EXAMPLE 5.1                                  *
C      *                                                                    *
C      *         SUBROUTINES                                                *
C      *                   STAT                                             *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       LOGICAL  VERB

       DIMENSION R1(NTOT),R2(NTOT)
       DIMENSION XY(NTOT),ID1(NTOT),ID2(NTOT)

C
C      *        COMPUTATION OF R1                                           *
C      *        STEP 1 AND 2 : RANK FROM LEFT TO RIGHT, OMITTING            *
C      *        RIGHT CENSORED VALUES. ASSIGN NEXT HIGHER RANK              *
C      *        TO RIGHT CENSORED VALUES                                    *
C
C
       IRANK = 0
       DO 90 I = 1,NCOMP
          IF (ID1(I) .EQ. 1) THEN
             R1(I) = IRANK + 1
          ELSE
             IRANK = IRANK + 1
             R1(I) = REAL(IRANK)
          ENDIF
  90   CONTINUE
C
C      *        STEP3 : REDUCE THE RANK OF TIED OBSERVATIONS TO             *
C      *        THE LOWEST RANK FOR THE VALUE                               *
C
       K1 = NCOMP - 1
       L1 = 1
  12   IF (XY(L1) .EQ. XY(L1+1)) THEN
C
          JEMP = IABS (ID1(L1) - 1) * IABS (ID1(L1+1) - 1)
          IF (JEMP .NE. 0) THEN
             R1(L1+1) = R1(L1)
             IF (L1 .EQ. K1) GOTO 13
             L1 = L1 + 1
             GOTO 12
          ENDIF
       ENDIF
       IF (L1 .NE. K1) THEN
          L1 = L1 + 1
          GOTO 12
       ENDIF
C
C      *             COMPUTATION OF R2                                      *
C      *             STEP 1 : RANK FROM RIGHT TO LEFT                       *
C
  13   DO 14 I = 1, NCOMP
          R2(I) = REAL(NCOMP - I + 1)
  14   CONTINUE
C
C      *          STEP2 : REDUCE THE RANK OF TIED OBSERVATIONS              *
C      *          TO THE LOWEST FOR THE VALUE                               *
C
       L1 = NCOMP
  22   IF (XY(L1) .EQ. XY(L1-1)) THEN
C
          JEMP = IABS (ID1(L1) - 1) * IABS (ID1(L1-1) - 1)
          IF (JEMP .NE. 0) THEN
             R2(L1-1) = R2(L1)
             IF (L1 .EQ. 2) GOTO 23
             L1 = L1 - 1
             GOTO 22
          ENDIF
       ENDIF
       IF (L1 .NE. 2) THEN
          L1 = L1 - 1
          GOTO 22
       ENDIF

  23   IF (IC .NE. 0) THEN
C
C      *         STEP 3 : REDUCE THE RANK OF RIGHT CENSORED                 *
C      *         OBSERVATION TO UNITY                                       *
C
          DO 24 I = 1,NCOMP
             IF (ID1(I) .NE. 0)  R2(I) = 1.0
  24      CONTINUE
       ENDIF
C
C      *               COMPUTE FINAL SCORES - R1(I)                         *
C
       DO 25 I = 1,NCOMP
          R1(I) = R1(I) - R2(I)
  25   CONTINUE

       CALL STAT (VERB, ID1, ID2, N1, N2, NCOMP, NTOT, R1, TEST)
       PROB = 1.0 - AGAUSS (TEST)

       RETURN
       END
