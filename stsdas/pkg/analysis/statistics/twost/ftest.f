C
C      **********************************************************************
C      ********************* SUBROUTINE FTEST  ******************************
C      **********************************************************************
C
       SUBROUTINE FTEST(T1,T2,F,JD1,JD2)
C
C      *         THIS SUBROUTINE COMPUTES  THE TEST STATISTIC OF            *
C      *         THE F-TEST.                                                *
C
C*******     TO USE THE COMMON STATEMENT, THE FIRST HALF OF THE PROGRAM     *
C*******     IS CHANGED.                                                    *
C
C*******      COMMON STATEMENT IS DIFFERENT FROM SMSDA.                     *
C
       IMPLICIT REAL*8 (A-H,O-Z)
       COMMON /F/ XY(50),ID1(50),ID2(50)
       COMMON /G/ N,N1,N2,NCEN,ISIGN,IFULL,LO
C
       MU=0
       NU=0
       SUM1=0.0
       SUM2=0.0
       DO 200 I=1,N
       IF(ID2(I).NE.1) GOTO 100
       SUM1=SUM1+XY(I)
C
C*******      THE NEXT LINE IS CHANGED FROM ".EQ.1" TO ".EQ.0"              *
C
       IF(ID1(I).EQ.0) MU=MU+1
       GOTO 200
  100  SUM2=SUM2+XY(I)
       IF(ID1(I).EQ.0) NU=NU+1
  200  CONTINUE
       T1=SUM1/FLOAT(MU)
       T2=SUM2/FLOAT(NU)
C
C*******      UP TO THIS POINT, THE CODE IS DIFFERENT FROM SMSDA.           *
C
       F=T1/T2
       JD2=2*NU
       JD1=2*MU
       RETURN
       END
