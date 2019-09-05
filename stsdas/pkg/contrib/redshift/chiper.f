      FUNCTION CHIPER(X,Y,YE,UNC,LO,LUP,A,AE,NPARAM,NITERA) 
      COMPLEX Y(1),S(4096),CEXP1,CMPLX,F,FA(3) 
      COMPLEX SARG
      REAL X(1),YE(1),A(3),V(11),C(4,4),B(4,4) 
      REAL AE(NPARAM) 
      REAL SIG2
      INTEGER IEXC(4096)
      LOGICAL   CONV
      NPTS=0
      DO 109 J=LO,LUP
        NPTS=NPTS+1
        IEXC(J)=0
109     CONTINUE
      CHIOLD=0 
      SIG2 = EXP(2. * A(2) )
      DO 110 J=LO,LUP 
      S(J)=A(3)*CEXP1(X(J)**2/2.*SIG2,-X(J)*A(1)) 
110   CHIOLD=CHIOLD 
     $      +((REAL(S(J))-REAL(Y(J)))**2
     $      +(AIMAG(S(J))-AIMAG(Y(J)))**2)/YE(J)**2

      I = 0
      IFACT=0 
      call info( i, ifact, a, chiold, npts)

      DO 10 I=1,NITERA 
C  AFTER TWO ITERATIONS, EXCLUDE EXTREME DATA POINTS.
C  Use I .LE. 2 for that option.
      IF(I.LE.2) GOTO 220
        DO 200 J=LO,LUP
          PROJ=REAL(Y(J)*CEXP1(0.,X(J)*A(1)))
          IEXC(J)=0
          IF(PROJ.GT.1.5) IEXC(J)=1
          IF(PROJ.LT.-0.5) IEXC(J)=1
200       CONTINUE
C  RE-EVALUATE CHIOLD
        CHIOLD=0.0
        NPTS=0
        DO 210 J=LO,LUP
          IF(IEXC(J).EQ.1) GOTO 210
          S(J)=A(3)*CEXP1(X(J)**2/2.*SIG2,-X(J)*A(1))
          CHIOLD=CHIOLD
     $          +((REAL(S(J))-REAL(Y(J)))**2
     $          +(AIMAG(S(J))-AIMAG(Y(J)))**2)/YE(J)**2
          NPTS=NPTS+1
210       CONTINUE
220   CONTINUE
      DO 20 J=1,NPARAM 
      C(J,NPARAM+1)=0 
      DO 20 K=1,NPARAM 
      C(J,K)=0 
20    CONTINUE 
      DO 30 J=LO,LUP 
      IF(IEXC(J).EQ.1) GOTO 30
      F=(S(J)-Y(J))/YE(J)
      SARG=S(J)/YE(J)
      FA(1)=X(J)*CMPLX(AIMAG(SARG),-REAL(SARG)) 
      FA(2)=-X(J)**2 * SIG2 * SARG 
      FA(3)=SARG/A(3) 
      C(1,4)=C(1,4)+REAL(F)*REAL(FA(1))+AIMAG(F)*AIMAG(FA(1)) 
      C(2,4)=C(2,4)+REAL(F)*REAL(FA(2))+AIMAG(F)*AIMAG(FA(2)) 
      C(3,4)=C(3,4)+REAL(F)*REAL(FA(3))+AIMAG(F)*AIMAG(FA(3)) 
      C(3,3)=C(3,3)+REAL(FA(3))*REAL(FA(3))+AIMAG(FA(3))*AIMAG(FA(3)) 
      C(2,3)=C(2,3)+REAL(FA(2))*REAL(FA(3))+AIMAG(FA(2))*AIMAG(FA(3)) 
      C(1,3)=C(1,3)+REAL(FA(1))*REAL(FA(3))+AIMAG(FA(1))*AIMAG(FA(3)) 
      C(2,2)=C(2,2)+REAL(FA(2))*REAL(FA(2))+AIMAG(FA(2))*AIMAG(FA(2)) 
      C(1,2)=C(1,2)+REAL(FA(1))*REAL(FA(2))+AIMAG(FA(1))*AIMAG(FA(2)) 
      C(1,1)=C(1,1)+REAL(FA(1))*REAL(FA(1))+AIMAG(FA(1))*AIMAG(FA(1)) 
30    CONTINUE 
      DO 65 K=1,10 
      CONV=(K.EQ.1) 
      IF(K.EQ.1)FACT=0 
      IF(K.NE.1)FACT=2.**IFACT 
      DO 70 J=1,NPARAM 
      DO 75 L=1,J 
      B(L,J)=C(L,J) 
      B(J,L)=C(L,J) 
75    CONTINUE 
      B(J,NPARAM+1)=C(J,NPARAM+1) 
      B(J,J)=(1+FACT)*C(J,J) 
70    CONTINUE 
      DET=SIMUL(NPARAM,B,V,1E-10,0,4) 
      DO 40 J=1,NPARAM 
      A(J)=A(J)-V(J) 
      IF ( A(J) .EQ. 0. ) A(J) = 1.E-10
      CONV=CONV.AND.(ABS(V(J)/A(J)).LE.UNC) 
40    CONTINUE 
      IF ( A(2) .GT. 35. )  A(2) = 35.
      IF ( A(2) .LT. -35. )  A(2) = -35.
      IF ( A(3) .LT. 0. ) A(3) = 0.01
      SIG2 = EXP( 2. * A(2) )
      CHI=0 
      IF(CONV)GO TO 120 
      DO 100 J=LO,LUP 
      IF(IEXC(J).EQ.1) GOTO 100
      S(J)=A(3)*CEXP1(X(J)**2/2.*SIG2,-X(J)*A(1))
      CHI=CHI 
     $      +((REAL(S(J))-REAL(Y(J)))**2
     $      +(AIMAG(S(J))-AIMAG(Y(J)))**2)/YE(J)**2
100   CONTINUE
120   CONTINUE 

      call info(i, ifact, a, chi, npts)

      IF(K.NE.1)IFACT=IFACT-1 
      IF ( IFACT .LT. -15 )  IFACT = -15
      IF(CHI.LT.(1.0001*CHIOLD))GO TO 80
      IF(K.NE.1)IFACT=IFACT+2 
      IF ( IFACT .GT. 15 ) IFACT = 15
      DO 65 J=1,NPARAM 
      A(J)=A(J)+V(J) 
      IF ( A(J) .EQ. 0.)  A(J) = 1.E-10
65    CONTINUE 
      IF ( A(2) .GT. 35. ) A(2) = 35.
      IF ( A(2) .LT. -35. ) A(2) = -35.
      IF ( A(3) .LT. 0. ) A(3) = 0.01
      SIG2 = EXP( 2. * A(2) )
      IF(IFACT.GT.10)GO TO 50 
80    CONTINUE 
      IF(CONV)GO TO 50 
      IF(CHI.LT.CHIOLD)CHIOLD=CHI 
10    CONTINUE 
50    CONTINUE 
      NFREE=NPTS-NPARAM 
      PERDEG=CHIOLD/NFREE 
C      WRITE(7,1000)
      DO 60 I=1,NPARAM 
      AE(I)=SQRT(PERDEG*B(I,I)) 
      IF ( AE(I) .EQ. 0. )  AE(I) = 1.E-10
      DO 90 J=1,I 
      C(I,J)=PERDEG*B(I,J)/AE(I)/AE(J) 
90    CONTINUE 

60    CONTINUE 

      call info(i, ifact, a, perdeg, npts)

      CHIPER=PERDEG 
      RETURN 
      END 
