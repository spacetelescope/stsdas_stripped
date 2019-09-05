C
C      **********************************************************************
C      ********************* SUBROUTINE PLESTM  *****************************
C      **********************************************************************
C
       SUBROUTINE PLESTM(U,C,NU,NC,S,V,NTOT,SMEAN,SIGMA,ICHANGE,
     +                   NCHANGE,L)
C       
C      *      THIS SUBROUTINE COMPUTES PL ESTIMATOR AND THE MEAN            *
C      *      AND ITS ERROR.                                                *
C      *                                                                    *
C      *       INPUT     U : UNCENSORED DATA POINTS                         *
C      *                 C : CENSORED DATA POINTS                           *
C      *                NU : NO. OF UNCENSORED DATA POINTS                  *
C      *                NC : NO. OF CENSORED DATA POINTS                    *
C      *               NTOT: TOTAL NUMBER OF DATA POINTS                    *
C      *                                                                    *
C      *       WORK      L : RANK OF THE UNCENSORED DATA                    *
C      *               VAR : VARIANCE OF THE MEAN                           *
C      *                KD : NUMBER OF TIED DATA POINTS                     *
C      *                                                                    *
C      *       OUTPUT    S : PL ESTIMATOR                                   *
C      *                 V : ERROR FOR THE PL ESTIMATOR                     *
C      *             SMEAN : MEAN OF THE DATA                               *
C      *             SIGMA : ERROR OF THE MEAN                              *
C      *            ICHANGE: IF THE LAST VALUE IS CENSORED, WE NEED TO      *
C      *                     CHANGE IT TO A DETECTION. THEN ICHANGE=-1,     *
C      *                     OTHERWISE ICHANGE=1.                           *
C      *            NCHANGE: IF ICHANGE = -1 AND THE LAST VALUE IS TIED     *
C      *                     WITH OTHER CENSORED VALUES, THIS RECORDS THE   *
C      *                     NUMBER OF TIED OBSERVATIONS (ALL OF THEM NEED  *
C      *                     TO BE CHANGED TO DETECTIONS).                  *
C      *                                                                    *
C      *       FIRST HALF OF THE PROGRAM IS FROM ELISA T. LEE, "STATISTICAL *
C      *       METHODS FOR SURVIVAL DATA ANALYSIS", 1980, LIFETIME LEARNING *
C      *       PUBLICATIONS (BELMONT:CA); WITH THE GRAPHIC ROUTINES REMOVED.*
C      *       FORMULAS USED FOR COMPUTATION OF THE MEAN AND THE ERROR ARE  *
C      *       FROM RUPERT G. MILLER, "SURVIVAL ANALYSIS", 1981,            *
C      *       JOHN WILEY & SONS (NY:NY)                                    *
C      *                                                                    *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION U(NTOT),C(NTOT),S(NTOT),V(NTOT),L(NTOT)
C
C      *          COMPUTE THE RANK (L) OF THE UNCENSORED POINTS             *
C
C*******     IF THE LAST VALUE IS CENSORED, CHANGE IT TO A DETECTION        *
C

C      THE FOLLOWING LOOP HAS BEEN MODIFIED AND NCHANGE ADDED TO THE 
C      PROGRAM TO COVER THE CASE WHEN TIED NONDETECTIONS ARE THE LARGEST
C      VALUE.  MODIFIED 4/92

       ICHANGE=1
       NCHANGE = 0
 13    IF(NC .NE. 0)THEN 
          IF(U(NU) .LE. C(NC))THEN 
             U(NU+1)=C(NC)
             NU=NU+1
             NC=NC-1
             NCHANGE = NCHANGE + 1
             ICHANGE=-1
          ELSE
             GOTO 15
          ENDIF
       ELSE
          GOTO 15
       ENDIF
       GOTO 13
C
 15    K=1
       KK=0
       NT=NU+NC
       IF(NC .NE. 0) THEN 
          DO 10 I=1,NU
             IF(KK .NE. NC) THEN
                DO 20 J=K,NC
                   K1=J
                   IF(C(J) .GE. U(I)) GOTO 1
                   KK=KK+1
  20            CONTINUE
             ENDIF
   1         K=K1
             L(I)=I+KK
  10      CONTINUE
       ELSE
          DO 19 I=1,NU
             L(I)=I
  19      CONTINUE
       ENDIF
C
C      *       COMPUTE P(T) FOR ALL UNCENSORED POINTS BASED ON RANK (L)     *
C
       V1=0.0
       PT=1.0
       XNT=NT
       DO 12 I=1,NU
          XL=L(I)
          PT=PT*((XNT-XL)/(XNT-XL+1.0))
          S(I)=PT
          IF((XNT-XL) .LE. 0.0) THEN
             VV=0.0
          ELSE
             V1=V1+1.0/((XNT-XL)*(XNT-XL+1.0))
             VV=DSQRT((PT**2)*V1)
          ENDIF
          V(I)=VV
  12   CONTINUE

C
C      *        COMPUTE THE MEAN                                            *
C      *        REF. FOR THE MEAN AND ERROR :                               *
C      *          MILLER, R. G. JR. 1981, "SURVIVAL ANALYSIS"               *
C      *          PP. 70-71 AND 195-198.                                    *
C
       SMEAN=U(1)
       I=2
  30   K=0
  40   IF((U(I+K).NE.U(I-1)).AND.(I+K.LE.NU)) THEN
          SMEAN=SMEAN+S(I+K-1)*(U(I+K)-U(I-1))
          IF(I+K.LT.NU) THEN
             I=I+K+1
             GOTO 30
          ENDIF
       ELSEIF(U(I+K).EQ.U(I-1)) THEN
          K=K+1
          GOTO 40
       ENDIF
C
C      *              COMPUTE THE ERROR OF THE MEAN                         *
C
       J=2    
       VAR=0.0
  120  I=J
       SSUM=0
  130  K=0
  140  IF((U(I+K).EQ.U(I-1)).AND.(I+K.LE.NU)) GOTO 145
          IF(U(I+K).EQ.U(I-1)) THEN
             K=K+1
             GOTO 140
          ENDIF
  145     SSUM=SSUM+S(I+K-1)*(U(I+K)-U(I-1))
          IF(I+K.LT.NU) THEN
             I=I+K+1
             GOTO 130
          ENDIF
C
C      *          KD IS NO. OF TIED OBSERVATIONS AT THAT POINT              *
C
       KD=1
  180  IF(U(J-1+KD).LE.U(J-1)) THEN
          KD=KD+1
          GOTO 180
       ENDIF
       XL=L(J-1)
       D=KD
       B=XNT-XL-D+1.0
C
C      *       IF THE LAST FEW DATA POINTS ARE UNCENSORED AND TIED, SKIP    *
C      *       THE NEXT LINES TO AVOID DIVISION BY 0.                       *
C
       IF(B .NE. 0.0) THEN
          VAR=VAR+SSUM*SSUM*D/((XNT-XL+1)*B)
          J=J+KD
          IF(J.LE.NU) GOTO 120
       ENDIF
  200  SIGMA=DSQRT(VAR)
   
       RETURN
       END
