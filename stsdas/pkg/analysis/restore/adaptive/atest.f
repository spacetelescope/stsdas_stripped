      SUBROUTINE ATEST(NX,NY,IN,HG,HL,OUT,M,OG,UL,OL,
     >                 NSG,NSL,ORD,TYP)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  compares actual gradient and Laplaces with noise statistics and decides
C  if a pixel is significant on the actual order
C  OG,UL,OL : upper limit for gradients and lower and upper limits for 
C  Laplace-terms respectively; pixel values exceeding these
C  limits make the pixel significant. (in)
C  NSG,NSL  : number of pixels wich became significant at the actual 
C  order by gradient or Laplace respectively. (out)
C
C----------------------------------------------------------------------------
*      IMPLICIT NONE
C
      CHARACTER*1 S,TYP
      INTEGER     NX,NY,ORD,NSG,NSL,I
      REAL       IN(1),HG(1),HL(1),OUT(1),OG,UL,OL,HHL,ORDR,M(1)
C
      ORDR=ORD
      S='L'
      NSG=0
      NSL=0
C
      DO 10 I=1,NX*NY
*     ! pixel already significant on lower order
      IF (M(I).NE.0) GOTO 10
      IF (TYP.NE.'S') GOTO 5
C
C *** test for filter type SMOOTH
*     ! gradient significant
      IF (HG(I).GE.OG) GOTO 2
      HHL=HL(I)
*     ! Laplace significant
      IF ((HHL.GE.OL).OR.(HHL.LE.UL)) GOTO 3
      GOTO 10
C
    2 S = 'G'
    3 IF (S.EQ.'G') NSG=NSG+1
      IF (S.EQ.'L') NSL=NSL+1
      S='L'
      OUT(I)=IN(I)
      GOTO 9
C
    5 IF (TYP.NE.'G') GOTO 7

C *** test for filter type GRADIENT
      HHL=HG(I)
*     ! gradient not significant
      IF(HHL.LT.OG) GOTO 10
      OUT(I)=HHL
      NSG=NSG+1
      GOTO 9
C
C *** test for filter type LAPLACE
    7 HHL=HL(I)
*     ! Laplace not significant
      IF ((HHL.LT.OL).AND.(HHL.GT.UL)) GOTO 10
      OUT(I)=HHL
      NSL=NSL+1
*     ! mark significant pixel
    9 M(I)=ORDR
   10 CONTINUE
C
      RETURN
      END
