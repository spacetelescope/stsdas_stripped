C
C      **********************************************************************
C      ******************** SUBROUTINE SORT2 ********************************
C      **********************************************************************
C
       SUBROUTINE SORT2
C
C      *                BUBBLE SORT PRGRAM.                                 *
C
C*******       THE COMMON STATEMENT IS DIFFERENT FROM SMSDA.                *
C
       IMPLICIT REAL*8 (A-H,O-Z)
       COMMON /F/ XY(200),ID1(200),ID2(200)
       COMMON /G/ N,N1,N2,NCEN,ISIGN,IFULL,LO
       DO 1 I=1,N
       J=N-I+1
       JJ=J-1
       IF(JJ.LT.1) GOTO 1
       DO 2 K=1,JJ
       IF(XY(K).NE.XY(J)) GOTO 3
       IF(ID1(J)-ID1(K)) 4,2,2
   3   IF(XY(K).LE.XY(J)) GOTO 2
   4   X1=XY(J)
       ITEM=ID1(J)
       ICTE=ID2(J)
       XY(J)=XY(K)
       ID1(J)=ID1(K)
       ID2(J)=ID2(K)
       XY(K)=X1
       ID1(K)=ITEM
       ID2(K)=ICTE
   2   CONTINUE
   1   CONTINUE
       RETURN
       END
