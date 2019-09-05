      SUBROUTINE AFIDO(NX,NY,MT,IN,H0,H1,HG,HL,OUT,M,ORD,TYP,
     >                 FORM,NOI,K,YMT)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  execution of the algorithm.
C
C  NX,NY       : number of pixels in line and column resp.
C  IN          : input frame (IN_A)
C  MT          : mask frame for noise statistics (IN_B)
C  OUT         : output frame (OUT_A)
C  H0,H1,HG,HL : internal work frames
C  M           : internal work frame; holds the order at wich
C                a pixel becomes significant
C  ORD         : highest order to be applied (determined by the
C                largest filtersize AFISIZ)
C  TYPE        : filter type (AFITYP)
C  FORM        : filter shape (AFISHA)
C  NOI         : noise model (AFINOI)
C  K           : threshold for significance (AFIK)
C  YMT         : swich if mask for statistics is given (='Y') 
C                or not (='N')
C---------------------------------------------------------------------------
*      IMPLICIT NONE
C
      CHARACTER*80    B
      CHARACTER*1     TYP,FORM,YMT,NOI
      REAL            IN(1),H0(1),H1(1),HG(1),HL(1),OUT(1),A,
     >                EG,SG,EL,SL,K,M(1),MT(1),C,OG,OL,UL,MOG,MOL,MUL
      INTEGER         NSG,NSL,NREST,NX,NY,ORD,OE,V,N,I,NN
      INTEGER         ISTAT
C
      COMMON          /TEST/EG(16),SG(16),EL(16),SL(16),
     >                      NSG(16),NSL(16),NREST,MOG,MOL,MUL
C
 100  FORMAT(' *** WARNING:',I6,' PIXELS NEGATIVE ! ***')

      NN=0
      OE=0
      NREST=0
      N=NX*NY
      C=3./8.
C
      DO 10 I=1,N
         OUT(I)=0
         M(I)=0
10    CONTINUE
C
      IF (NOI.EQ.'A') THEN
         CALL AINI(NX,NY,MT,IN,H0,HG,HL,EG(2),EL(2),SG(2),SL(2),YMT)
      ELSE
*        ! Anscombe-transform
         DO 20 I=1,N
            A=IN(I)
            IF (A.LE.0.) THEN
               H1(I)=0.
               IF (A.LT.0.) NN=NN+1
            ELSE
               H1(I)=SQRT(A+C)
            ENDIF
20       CONTINUE
         IF (NN.NE.0) THEN
            WRITE(B,100) NN
            CALL UMSPUT (B, 1, 0, ISTAT)
         ENDIF
         CALL AINI(NX,NY,MT,H1,H0,HG,HL,EG(2),EL(2),SG(2),SL(2),YMT)
      ENDIF

*     ! threshold for gradient
      OG=EG(2)+K*SG(2)
*     ! upper threshold for Laplace
      OL=EL(2)+K*SL(2)
*     ! lower threshold for Laplace
      UL=EL(2)-K*SL(2)
      MOG=OG
      MOL=OL
      MUL=UL

C
      IF (NOI.EQ.'A') THEN
        CALL ATEST(NX,NY,IN,HG,HL,OUT,M,OG,UL,OL,
     >          NSG(2),NSL(2),2,TYP)
      ELSE
        CALL ATEST(NX,NY,H1,HG,HL,OUT,M,OG,UL,OL,
     >          NSG(2),NSL(2),2,TYP)
      ENDIF

      IF (ORD.LE.2) GOTO 90

C
      DO 30 I=3,16
         IF (FORM.EQ.'B') V=(2**I)/4
         IF (FORM.EQ.'P') V=(2**((I+1)/2))/2
         OE=I-(I/2)*2
         IF (OE.EQ.1) CALL ABOX(NX,NY,MT,H0,H1,HG,HL,EG(I),EL(I),
     >                          SG(I),SL(I),V,YMT)
         IF (OE.EQ.0) CALL ABOX(NX,NY,MT,H1,H0,HG,HL,EG(I),EL(I),
     >                          SG(I),SL(I),V,YMT)
         OG=EG(I)+K*SG(I)
         OL=EL(I)+K*SL(I)
         UL=EL(I)-K*SL(I)
         MOG=AMAX1(MOG,OG)
         MOL=AMAX1(MOL,OL)
         MUL=AMIN1(MUL,UL)
         IF (OE.EQ.1) CALL ATEST(NX,NY,H0,HG,HL,OUT,M,OG,UL,
     >                           OL,NSG(I),NSL(I),I,TYP)
         IF (OE.EQ.0) CALL ATEST(NX,NY,H1,HG,HL,OUT,M,OG,UL,
     >                           OL,NSG(I),NSL(I),I,TYP)
         IF (ORD.EQ.I) GOTO 90
30    CONTINUE
C
C *** fill the not jet significant pixels in the output frame
   90 DO 40 I=1,N
         IF (M(I).EQ.0) THEN
            IF (TYP.EQ.'S') THEN
               IF (OE.EQ.1) OUT(I)=H1(I)
               IF (OE.EQ.0) OUT(I)=H0(I)
            ELSE
               IF (TYP.EQ.'G') OUT(I)=HG(I)
               IF (TYP.EQ.'L') OUT(I)=HL(I)
            ENDIF
            NREST=NREST+1
         ENDIF
40    CONTINUE

C
      IF (NOI.EQ.'A' .OR. TYP.NE.'S') RETURN

      DO 50 I=1,N
*        ! inverse Anscombe-transform
         OUT(I)=OUT(I)**2-C
50    CONTINUE

      RETURN
      END
