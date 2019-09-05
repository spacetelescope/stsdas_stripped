C
C      **********************************************************************
C      ********************* SUBROUTINE SUMRY  ******************************
C      **********************************************************************
C
       SUBROUTINE SUMRY(U,IU,S,NTOT,FINT)
C
C      *       THIS SUBROUTINE CALCULATES AND PRINTS 75, 50, AND            *
C      *       25, PERCENTILES  OF SURVIVAL FOR A SURVIVAL CURVE.           *
C      *       S AND U ARE ARRAYS CONTAINING POINTS FOR WHICH VALUES OF THE *
C      *       SURVIVAL CURVE WERE CALCULATED, IU IS THE NUMBER OF          *
C      *       UNCENSORED POINTS.  ADOPTED FROM ELISA T. LEE, "STATISTICAL  *
C      *       METHODS FOR SURVIVAL DATA ANALYSIS", 1980, LIFETIME LEARNING *
C      *       PUBLICATIONS (BELMONT:CA).                                   *
C      *                                                                    *
C      *       INPUT       U : UNCENSORED DATA                              *
C      *                   S : PL ESTIMATOR                                 *
C      *       WORK       TY : PERCENTILE INDICATOR AT 75, 50, 25           *
C      *       OUTPUT    FINT: VALUES AT 75, 50, 25 PERCENTILES             *
C      *                                                                    *
C      *     THIS SUBROUTINE IS FROM SMSDA, EXCEPT PRINTING COMMAND.        *
C      *                                                                    *
C
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       DIMENSION U(IU),S(NTOT),TY(3),FINT(3)
C
C
       TY(1)=0.75
       TY(2)=0.50
       TY(3)=0.25

C      *       IF THE NUMBER OF DATA POINTS IS SMALLER THAN 4, NO          *
C      *       PERCENTILES CAN BE OBTAINED.                                *
C
       DO 40 I=1,3
          FINT(I)=0.0
  40   CONTINUE

       L=1
       IF(IU.LE.3) RETURN
       NN=IU-1

       DO 100 I=1,3
          IF(S(1).LT.TY(I)) THEN
             FINT(I) = U(1)-(TY(I)-S(1))/(1-S(1))*(U(1)-0)
          ELSE
             DO  90 J=L,NN
                IF((S(J).GE.TY(I)) .AND. (S(J+1).LE.TY(I))) THEN 
                   FINT(I)=U(J)-(S(J)-TY(I))/(S(J)-S(J+1))*(U(J)-U(J+1))
                   L=J+1
                   GOTO 100
                ENDIF
  90         CONTINUE
          ENDIF
 100   CONTINUE

       RETURN
       END
