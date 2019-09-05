C
C      **********************************************************************
C      ********************* SUBROUTINE COX *********************************
C      **********************************************************************
C
       SUBROUTINE COX(X,R,XM,NG,RISK,A,VAR,SUM,COXA,PROB)
C
C      *        THIS SUBROUTINE COMPUTES THE COX-MANTEL STATISTIC.          *
C
C*******        COMMON STATEMENT IS DIFFERENT FROM SMSDA.                   *
C
       IMPLICIT REAL*8 (A-H,O-Z)
       DIMENSION R(200),XM(200),X(200),A(200),RISK(200),S2(200)
       COMMON /F/ XY(200),ID1(200),ID2(200)
       COMMON /G/ N,N1,N2,NCEN,ISIGN,IFULL,LO
C      *  THE NEXT FEW LINES ARE ADDED TO COMPENSATE THE CHANGE IN          *
C      *  SUBROUTINE AARRAY                                                 *
C
       NU=0
       DO 20 I=1,N
       IF(ID2(I).EQ.1) GOTO 20
C
C      *      THE NEXT LINE IS CHANGED FROM "EQ.1" TO "EQ.0"                *
C
       IF(ID1(I).EQ.0) NU=NU+1
   20  CONTINUE
C
       L=0
       DO 100 I=1,N
       IF(ID2(I).EQ.1) GOTO 100
       L=L+1
       S2(L)=XY(I)
 100   CONTINUE
       IR=L
       K=1
       DO 91 I=1,NG
 129   IF(X(I).LE.S2(K)) GOTO 139
       IF(IR.EQ.0) GOTO 139
       IR=IR-1
       IF(K.EQ.L) GOTO 139
       K=K+1
       GOTO 129
 139   RISK(I)=IR
  91   CONTINUE
C
C      *               CALCULATE ARRAY A                                    *
C
       DO 150 J=1,NG
 150   A(J)=RISK(J)/R(J)
 270   XNU=NU
       SUM=0.0
       DO 300 J=1,NG
       SUM=SUM+XM(J)*A(J)
 300   CONTINUE
       SUM=XNU-SUM
       VAR=0.0
       DO 161 J=1,NG
       IF(R(J).EQ.1) GOTO 161
       VAR=VAR+(XM(J)*(R(J)-XM(J))*A(J)*(1.0-A(J)))/(R(J)-1.0)
 161   CONTINUE
       COXA=SUM/SQRT(VAR)
C
C*******         COMPUTE PROBABILITY OF TEST STATISTIC                      *
C
       PROB=1.0-AGAUSS(COXA)
       RETURN
       END
