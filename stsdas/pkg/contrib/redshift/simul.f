      FUNCTION SIMUL(N,A,X,EPS,INDIC,NRC)
C 
C             FUNCTION SIMUL 
C 
C        THIS FUNCTION RETURNS THE VALUE OF THE DETERMINANT OF A 
C        MATRIX.  IN ADDITION, THE INVERSE MATRIX MAY BE CALCULATED 
C        IN PLACE, AND THE SOLUTION VECTOR OF THE CORRESPONDING LINEAR 
C        SYSTEM COMPUTED.  GAUSS-JORDAN ELIMINATION WITH  MAXIMUM 
C        PIVOT STRATEGY IS EMPLOYED, USING DOUBLE PRECISION ARITH- 
C        METIC.  IF THE MATRIX EXCEEDS THE MAXIMUM SIZE (50 BY 50), 
C        OR IF IT IS SINGULAR, A TRUE ZERO IS RETURNED. 
C 
C        CALLING SEQUENCE0 
C        SIMUL(N,A,X,EPS,INDIC,NRC) 
C             N IS THE SIZE OF THE MATRIX (N BY N) 
C             A IS THE MATRIX 
C             X IS THE SOLUTION VECTOR 
C             EPS IS A SMALL NUMBER TO BE USED AS A TEST FOR SINGULARITY 
C             INDIC IS THE CONTROL PARAMETER0 
C                  IF IT IS NEGATIVE, THE INVERSE IS COMPUTED IN PLACE 
C                  IF IT IS ZERO,THE MATRIX IS ASSUMED TO BE AUGMENTED, 
C                  AND THE SOLUTION AND INVERSE ARE COMPUTED 
C                  IF IT IS POSITIVE, ONLY THE SOLUTION IS COMPUTED 
C             NRC IS THE DIMENSION OF A IN THE CALLING PROGRAM 
C 
C        SUBPROGRAMS REQUIRED0 NONE 
C 
      DIMENSION IROW(50 ),JCOL(50 ),JORD(50 ),Y(50 ),A(NRC,NRC),X(N) 
      MAX=N 
      IF(INDIC.GE.0) MAX=N+1 
      IF(N.LE.50) GO TO 5 
      SIMUL=0.0 
      RETURN 
    5 DETER=1.0 
      DO 18 K=1,N 
      KM1=K-1 
      PIVOT=0.0 
      DO 11 I=1,N 
      DO 11 J=1,N 
      IF(K.EQ.1) GO TO 9 
      DO 8 ISCAN=1,KM1 
      DO 8 JSCAN=1,KM1 
      IF(I.EQ.IROW(ISCAN)) GO TO 11 
      IF(J.EQ.JCOL(JSCAN)) GO TO 11 
    8 CONTINUE 
    9 IF( ABS(A(I,J)).LE. ABS(PIVOT)) GO TO 11 
      PIVOT=A(I,J) 
      IROW(K)=I 
      JCOL(K)=J 
   11 CONTINUE 
      IF( ABS(PIVOT).GT.EPS) GO TO 13 
      SIMUL=0.0 
      RETURN 
   13 IROWK=IROW(K) 
      JCOLK=JCOL(K) 
      DETER=DETER*PIVOT 
      DO 14 J=1,MAX 
   14 A(IROWK,J)=A(IROWK,J)/PIVOT 
      A(IROWK,JCOLK)=1.0/PIVOT 
      DO 18 I=1,N 
      AIJCK=A(I,JCOLK) 
      IF(I.EQ.IROWK) GO TO 18 
      A(I,JCOLK)=-AIJCK/PIVOT 
      DO 17 J=1,MAX 
   17 IF(J.NE.JCOLK) A(I,J)=A(I,J)-AIJCK*A(IROWK,J) 
   18 CONTINUE 
      DO 20 I=1,N 
      IROWI=IROW(I) 
      JCOLI=JCOL(I) 
      JORD(IROWI)=JCOLI 
   20 IF(INDIC.GE.0) X(JCOLI)=A(IROWI,MAX) 
      INTCH=0 
      NM1=N-1 
      DO 22 I=1,NM1 
      IP1=I+1 
      DO 22 J=IP1,N 
      IF(JORD(J).GE.JORD(I)) GO TO 22 
      JTEMP=JORD(J) 
      JORD(J)=JORD(I) 
      JORD(I)=JTEMP 
      INTCH=INTCH+1 
   22 CONTINUE 
      IF(INTCH/2*2.LE.INTCH) DETER=-DETER 
   24 IF(INDIC.LE.0) GO TO 26 
      SIMUL=DETER 
      RETURN 
   26 DO 28 J=1,N 
      DO 27 I=1,N 
      IROWI=IROW(I) 
      JCOLI=JCOL(I) 
   27 Y(JCOLI)=A(IROWI,J) 
      DO 28 I=1,N 
   28 A(I,J)=Y(I) 
      DO 30 I=1,N 
      DO 29 J=1,N 
      IROWJ=IROW(J) 
      JCOLJ=JCOL(J) 
   29 Y(IROWJ)=A(I,JCOLJ) 
      DO 30 J=1,N 
   30 A(I,J)=Y(J) 
      SIMUL=DETER 
      RETURN 
      END
