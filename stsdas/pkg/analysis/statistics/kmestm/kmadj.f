C
C      **********************************************************************
C      ******************** SUBROUTINE KMADJ  *******************************
C      **********************************************************************
C
       SUBROUTINE KMADJ (ZU, ZC, NTOT, IU, IC, S, ISIGN, NTEMP, F, V)
C
C      *       THIS SUBROUTINE RESTORES THE DATA AND THE PRODUCT-LIMIT      *
C      *       ESTIMATOR TO UPPER-LIMITS FORM, IF THE DATA WAS IN THAT FORM *
C      *       INITIALLY.  TIES AT CENSORED POINTS ARE ALSO REMOVED TO      *
C      *       MAKE THE PRINTOUT CONSISTENT.                                *
C      *                                                                    *
C      *       INPUT    ZU(I)  :  UNCENSORED DATA POINTS                    *
C      *                ZC(I)  :  CENSORED DATA POINTS                      *
C      *                NTOT   :  TOTAL NUMBER OF DATA POINTS,=IU+IC.       *
C      *                 IU    :  NUMBER OF UNCENSORED DATA POINTS          *
C      *                 IC    :  NUMBER OF CENSORED DATA POINTS            *
C      *                 S(L)  :  PL ESTIMATOR                              *
C      *       OUTPUT  NTEMP   :  VALUE OF NTOT AFTER ADJUSTMENT FOR TIES   *
C      *       OTHER   F       :  PROBABILITY MASS ASSIGNED TO EACH         *
C      *                             UNCENSORED POINT(=JUMP IN S AT THE     *
C      *                                                  POINT)            *
C      *                                                                    *

       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION ZU(NTOT),ZC(NTOT),S(NTOT),F(NTOT),V(NTOT)

C
C      *  FOR LEFT-CENSORED DATASETS (I.E. UPPER LIMITS),                  *
C      *  CALCULATE JUMP IN SURVIVOR FUNCTION AT UNCENSORED POINTS         *
C
       IF (ISIGN .LT. 0) THEN
          F(1) = 1.0 - S(1)
          DO 120 I = 2, IU
             F(I) = S(I-1) - S(I)
 120      CONTINUE

C
C      *  REVERSE SEQUENCE OF POINTS, JUMPS AND ERRORS         *
C
            CALL NEGATE (ZU, IU)
            CALL REVRSD (ZU, IU)
C
            CALL NEGATE (ZC, IC)
            CALL REVRSD (ZC, IC)
C
            CALL REVRSD (F, IU)
            CALL REVRSD (V, IU-1)
C
C      *  COMPUTE SURVIVOR FUNCTION FOR REVERSED DATA                     *
C
            DO 170 I = 1, IU
               S(I) = 1.0
               DO 160 J = 1, I
                  S(I) = S(I) - F(J)
160            CONTINUE
170         CONTINUE
         ENDIF   

C      *   CORRECTS FOR TIES AT THE UNCENSORED POINTS                      *
C      *   NOTICE THAT IF THERE ARE TIES AT THESE POINTS, THEN BOTH        *
C      *   IU AND NTEMP ARE REDUCED.                                       *

       NTEMP = NTOT
       K = 1
190    IF (ZU(K) .EQ. ZU(K+1)) THEN
          DO 195 I = K, IU-1
             ZU(I) = ZU(I+1)
             S(I) = S(I+1)
             V(I) = V(I+1)               
195       CONTINUE
          IU = IU -1
          NTEMP = NTEMP - 1
       ELSE
          K = K + 1
       ENDIF
       IF(K.LT.IU) GOTO 190

       RETURN
       END
