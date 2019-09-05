      SUBROUTINE abox(NX,NY,MT,IN,H0,HG,HL,EG,EL,SG,SL,V,YMT)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     executes one order of the algorithm
C     V : shift parametre for H-transform; minimum=2 (in)
C     other parameters as in afido
C
C--------------------------------------------------------------------------
*      IMPLICIT    NONE
      LOGICAL     L
      CHARACTER*1 YMT
      INTEGER     NX,NY,V,V1,V2,V21,NXV,NYV,NXV1,NYV1,JV,IV,
     >            J2,I2,I,J,T
      REAL        IN(NX,NY),H0(NX,NY),HG(NX,NY),HL(NX,NY),MT(NX,NY),
     >            EG,EL,SG,SL,A,B,C,D,HX,HY,H,QG,QL,NP,NPP
C
      V1=V
      V2=V1/2
      L=.FALSE.
      NP=0.
      EL=NP
      EG=NP
      QL=NP
      QG=NP

      DO 30 J=1,NY-V1
         JV=J+V1
         J2=J+V2
         DO 20 I=1,NX-V1
            IV=I+V1
            I2=I+V2
            A=IN(I,J)
            B=IN(IV,J)
            C=IN(I,JV)
            D=IN(IV,JV)
            H=(A+B+C+D)/4.
*           ! smooth
            H0(I2,J2)=H
            H=IN(I2,J2)-H
*           ! Laplace
            HL(I2,J2)=H
*           ! no mask for statistics
            IF (YMT.EQ.'N') GOTO 5
            T=MT(I2,J2)
            IF (T.NE.0) L=.TRUE.
*           ! pixel not used for statistics
            IF (L) GOTO 6
    5       NPP=NP
            NP=NP+1
            NPP=NPP/NP
            H=H-EL
*           ! estimate statistics
            EL=EL+H/NP
*           ! for Laplace
            QL=QL+H*H*NPP
    6       HX=(A-B+C-D)/4.
            HY=(A+B-C-D)/4.
            H=SQRT(HX*HX+HY*HY)
*           ! gradient
            HG(I2,J2)=H
*           ! no mask for statistics
            IF (YMT.EQ.'N') GOTO 7
*           ! pixel not used for statistics
            IF (L) GOTO 10
    7       H=H-EG
*           ! estimate statistics
            EG=EG+H/NP
*           ! for gradient
            QG=QG+H*H*NPP
   10       L = .FALSE.
20       CONTINUE
30    CONTINUE
C
C *** correct statistics
      NP=NP-1
      SL=SQRT(QL/NP)
      SG=SQRT(QG/NP)
C
C *** set edge lines and corners
      V21=V2+1
      NXV=NX-V2
      NYV=NY-V2
      NXV1=NXV+1
      NYV1=NYV+1

*     ! edge rows
      DO 60 I=V21,NXV
         A=H0(I,V21)
         B=HG(I,V21)
         C=HL(I,V21)
*        ! up
         DO 40 J=1,V2
            H0(I,J)=A
            HG(I,J)=B
            HL(I,J)=C
40       CONTINUE
         A=H0(I,NYV)
         B=HG(I,NYV)
         C=HL(I,NYV)
*        ! down
         DO 50 J=NYV1,NY
            H0(I,J)=A
            HG(I,J)=B
            HL(I,J)=C
50      CONTINUE
60    CONTINUE

*     ! edge columns
      DO 90 J=V21,NYV
         A=H0(V21,J)
         B=HG(V21,J)
         C=HL(V21,J)
*        ! left
         DO 70 I=1,V2
            H0(I,J)=A
            HG(I,J)=B
            HL(I,J)=C
70       CONTINUE
         A=H0(NXV,J)
         B=HG(NXV,J)
         C=HL(NXV,J)
*        ! right
         DO 80 I=NXV1,NX
            H0(I,J)=A
            HG(I,J)=B
            HL(I,J)=C
80       CONTINUE
90    CONTINUE

      A=H0(V21,V21)
      B=HG(V21,V21)
      C=HL(V21,V21)
*     ! upper left corner
      DO 110 I=1,V2
         DO 100 J=1,V2
            H0(I,J)=A
            HG(I,J)=B
            HL(I,J)=C
100      CONTINUE
110   CONTINUE

      A=H0(NXV,V21)
      B=HG(NXV,V21)
      C=HL(NXV,V21)
*     ! upper right corner
      DO 130 I=NXV1,NX
         DO 120 J=1,V2
            H0(I,J)=A
            HG(I,J)=B
            HL(I,J)=C
120      CONTINUE
130   CONTINUE

      A=H0(V21,NYV)
      B=HG(V21,NYV)
      C=HL(V21,NYV)
*     ! lower left corner
      DO 150 I=1,V2
         DO 140 J=NYV1,NY
            H0(I,J)=A
            HG(I,J)=B
            HL(I,J)=C
140      CONTINUE
150   CONTINUE

      A=H0(NXV,NYV)
      B=HG(NXV,NYV)
      C=HL(NXV,NYV)
*     ! lower right corner
      DO 170 I=NXV1,NX
         DO 160 J=NYV1,NY
            H0(I,J)=A
            HG(I,J)=B
            HL(I,J)=C
160      CONTINUE
170   CONTINUE

      RETURN
      END
