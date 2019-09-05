       SUBROUTINE aini(NX,NY,MT,IN,H0,HG,HL,EG,EL,SG,SL,YMT)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      initializes the algorithm:
C      EG,SG,EL,SL: exspectation value and standard deviation
C      of gradients and Laplace-terms resp. by noise (out)
C      other parameters as in afido
C
C------------------------------------------------------------------------
*       IMPLICIT NONE
C
       LOGICALL
       CHARACTER*1 YMT
       INTEGER     NX,NY,I1,J1,N1X,N1Y,I,J,T
       REAL        IN(NX,NY),H0(NX,NY),HG(NX,NY),HL(NX,NY),
     >             EG,EL,SG,SL,A,C,B,D,HX,HY,H,QG,QL,NP,NPP,
     >             MT(NX,NY)
C
C *** 1.order:smoothing
      DO 40 J=1,NY-1
        J1=J+1
        DO 30 I=1,NX-1
          I1=I+1
          H0(I1,J1)=(IN(I,J)+IN(I1,J)+IN(I,J1)+IN(I1,J1))/4.
30      CONTINUE
40    CONTINUE
C
C *** set edge lines: up and left and upper left corner pixel
*     ! corner
      H0(1,1)=H0(2,2)
C
      DO 50 I=2,NX
*        ! 1.row
         H0(I,1)=H0(I,2)
50    CONTINUE
C
      DO 60 J=2,NY
*        ! 1.column
         H0(1,J)=H0(2,J)
60    CONTINUE
C
      L=.FALSE.
      NP=0.
      EL=NP
      EG=NP
      QL=NP
      QG=NP
C
C *** 2.order: centering
      DO 80 J=1,NY-1
         J1=J+1
         DO 70 I=1,NX-1
            I1=I+1
            A=H0(I,J)
            B=H0(I1,J)
            C=H0(I,J1)
            D=H0(I1,J1)
            H=(A+B+C+D)/4.
*           ! smooth
            H0(I,J)=H
            H=IN(I,J)-H
*           ! Laplace
            HL(I,J)=H
*           ! no mask for statistics
            IF (YMT.EQ.'N') GOTO 15
            T=MT(I,J)
            IF (T.NE.0) L=.TRUE.
*           ! pixel not used for statistics
            IF (L) GOTO 16
   15       NPP=NP
            NP=NP+1
            NPP=NPP/NP
            H=H-EL
*           ! estimate statistics
            EL=EL+H/NP
*           ! for Laplace
            QL=QL+H*H*NPP
   16       HX=(A-B+C-D)/4.
            HY=(A+B-C-D)/4.
            H=SQRT(HX*HX+HY*HY)
*           ! gradient
            HG(I,J)=H
*           ! no mask for statistics
            IF (YMT.EQ.'N') GOTO 17
*           ! pixel not used for statistics
            IF (L) GOTO 20
   17       H=H-EG
*           ! estimate statistics
            EG=EG+H/NP
*           ! for gradient
            QG=QG+H*H*NPP
   20       L=.FALSE.
70       CONTINUE
80    CONTINUE

C correct statistics
      NP=NP-1
      SL=SQRT(QL/NP)
      SG=SQRT(QG/NP)
C
C *** set edge lines down and right and lower right corner pixel
      N1X=NX-1
      N1Y=NY-1
*     ! corner
      H0(NX,NY)=H0(N1X,N1Y)
      HG(NX,NY)=HG(N1X,N1Y)
      HL(NX,NY)=HL(N1X,N1Y)

*     ! last row
      DO 90 I=1,N1X
         H0(I,NY)=H0(I,N1Y)
         HG(I,NY)=HG(I,N1Y)
         HL(I,NY)=HL(I,N1Y)
90    CONTINUE

*     ! last column
      DO 100 J=1,N1Y
         H0(NX,J)=H0(N1X,J)
         HG(NX,J)=HG(N1X,J)
         HL(NX,J)=HL(N1X,J)
100   CONTINUE
C
      RETURN
      END
