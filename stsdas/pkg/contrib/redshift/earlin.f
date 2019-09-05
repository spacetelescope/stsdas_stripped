      SUBROUTINE EARLIN(X,Y,N,V,MEG) 
      DIMENSION C(11,11),V(11),X(N),Y(N),XX(11),E(11) 
      M=IABS(MEG) 
      MPL=M+1 
      DO 10 I=1,M 
      DO 10 J=1,MPL 
      C(I,J)=0 
10    CONTINUE 
      DO 20 I=1,N 
      XXX=1 
      DO 20 J=1,M 
      IF(J.NE.1)XXX=XXX*X(I) 
      XX(J)=XXX 
      C(J,M+1)=C(J,M+1)+XX(J)*Y(I) 
      DO 20 K=1,J 
      C(J,K)=C(J,K)+XX(J)*XX(K) 
20    CONTINUE 
      DO 30 J=2,M 
      JMI=J-1 
      DO 30 K=1,JMI 
      C(K,J)=C(J,K) 
30    CONTINUE 
      DET=SIMUL(M,C,V,0.,0,11)
      IF(MEG.LT.0)RETURN 
      DO 40 I=1,M 
      E(I)=SQRT(C(I,I)) 
      DO 40 J=1,I 
      C(I,J)=C(I,J)/(E(I)*E(J)) 
40    CONTINUE 
C      PRINT 1000 
C      PRINT 1000,(V(I),I=1,M) 
C      PRINT 1000,(E(I),I=1,M) 
C      PRINT 1000 
C      DO 50 I=1,M 
C      PRINT 1000,(C(I,J),J=1,I) 
C50    CONTINUE 
      RETURN 
C1000  FORMAT(1H 10F12.5) 
      END 
