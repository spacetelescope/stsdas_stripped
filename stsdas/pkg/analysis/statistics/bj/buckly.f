C
C      **********************************************************************
C      *********************** SUBROUTINE BUCKLY  ***************************
C      **********************************************************************
C
       SUBROUTINE BUCKLY(X,Y,IND,TOL,NTOT,NVAR,NVAR1,NVAR2,NU,NC,MAX,
     +                   ALPHA,SIGMAA,ITE,ARRAY,V,BU,TEST,TEST2,
     +                   IND2,XX,Z,W,WX,T,TY,ZY,IPT,ND,NO)

C
C
C      *     THIS IS A SUBPROGRAM WHICH PERFORMS THE BUCKLEY-JAMES          *
C      *     METHOD. THIS SUBROUTINE WAS ADAPTED FROM CODE BY J. HALPERN    *
C      *     (STANFORD UNIVERSITY SCHOOL OF MEDICINE, DEPARTMENT            *
C      *        OF FAMILY, COMMUNITY AND PREVENTIVE MEDICINE.)              *
C      *                                                                    *
C      *  INPUT                                                             *
C      *         X(J,I)   : INDEPENDENT VARIABLES                           *
C      *         Y(I)     : DEPENDENT VARIABLE                              *
C      *         IND(I)   : INDICATOR OF CENSORING                          *
C      *         TOL      : TOLERANCE LEVEL                                 *
C      *         NTOT     : NUMBER OF DATA POINTS                           *
C      *         NVAR     : NUMBER OF INDEPENDENT VARIABLES                 *
C      *         NVAR1    : NUMBER OF INDEPENDENT VARIABLES PLUS ONE        *
C      *         NVAR2    : NUMBER OF INDEPENDENT VARIABLES PLUS TWO        *
C      *         NU       : NUMBER OF DETECTED POINTS                       *
C      *         NC       : NUMBER OF CENSORED POINTS                       *
C      *         MAX      : MAXIMUM ITERATION                               *
C      *                                                                    *
C      *   WORK                                                             *
C      *        ARRAY(J,J): REGRESSION MATRIX                               *
C      *          V(J)    : AVERAGE OF J-TH DETECTED INDEPENDENT VARIABLE   *
C      *          BU(J)   : VARIANCE OF J-TH DETECTED INDEPENDENT VARIABLE  *
C      *         TEST(J)  : STORE OF THE PREVIOUS STEP ESTIMATIONS OF       *
C      *                    ALPHA(J)                                        *
C      *          Z(I)    : RESIDUALS                                       *
C      *          W(I)    : KM ESTIMATOR                                    *
C      *          WX(I)   : WEIGHT                                          *
C      *                                                                    *
C      *  OUTPUT                                                            *
C      *       ALPHA(J)   : REGRESSION COEFFICIENTS                         *
C      *       SIGMAA(J)  : ERROR                                           *
C      *       ITE        : ITERATION NUMBER                                *
C      *                                                                    *
C      *    SUBROUTINES                                                     *
C      *                    BJSORT, REGRES                                  *
C
C
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER(I-N)

       DIMENSION X(NVAR,NTOT),Y(NTOT),IND(NTOT),ALPHA(NVAR2)
       DIMENSION SIGMAA(NVAR1),ARRAY(NVAR,NVAR),V(NVAR),BU(NVAR)
       DIMENSION TEST(NVAR1),TEST2(NVAR1),IND2(NTOT),XX(NVAR,NTOT)
       DIMENSION Z(NTOT),W(NTOT),WX(NTOT),T(NTOT),TY(NTOT),ZY(NTOT)
       DIMENSION IPT(NTOT),ND(NTOT),NO(NTOT)
C
C      *               INITIALIZATION                                       *
C
       ITE=0
       DO 343 J=1,NVAR
          V(J) =0.0
          BU(J)=0.0
  343  CONTINUE

       DO 392 IN=1,NVAR1
          TEST(IN)=0.0
          TEST2(IN) =0.0
  392  CONTINUE
C
C      *       CALCULATE SOME VALUES FOR THE STANDARD DEVIATION             *
C
       DO 5 I=1,NTOT
          IF(IND(I).EQ.0) THEN
             DO 63 J=1,NVAR
                V(J)=V(J)+X(J,I)
   63        CONTINUE
          ENDIF
    5  CONTINUE

       DO 68 J=1,NVAR
          V(J)=V(J)/REAL(NU)
   68  CONTINUE

       DO 51 I=1,NTOT
          IF(IND(I).EQ.0) THEN
             DO 53 J=1,NVAR
                BU(J)=BU(J)+(X(J,I)-V(J))**2
   53        CONTINUE
       ENDIF
   51  CONTINUE
C
C      *      REGRES : SUBPROGRAM FOR LINEAR REGRESSION WITHOUT             *
C      *               CONSIDERING CENSORING STATUS                         *
C
       CALL REGRES(X,Y,NTOT,NVAR,ALPHA,SIGMAA,ARRAY)
C
C      *              GET RESIDUALS Z(I)                                    *
C
C      *       START ITERATION : 2000 LOOP.                                 *
C
C
 2000  DO 31 I=1,NTOT
          T(I)=-400.0
          IND2(I)=IND(I)
C       
          ZS=0.0
          DO 61 J=1,NVAR
             JJ=J+1
             ZS=ZS+ALPHA(JJ)*X(J,I)
             XX(J,I)=X(J,I)
   61     CONTINUE
          Z(I) =Y(I)-ZS
          TY(I)=Y(I)
   31  CONTINUE
C
C      *            SORTING .... INCREASING ORDER                           *
C
       CALL BJSORT(IND2,XX,TY,Z,NVAR,NTOT)
C
       DO 311 I=1,NTOT
          ZY(I)=Z(I)
  311  CONTINUE
C
C      *       THE LARGEST RESIDUAL MUST BE UNCENSORED.                     *
C
       IND2(NTOT)=0
C
C      *      ESTIMATE  VALUES FOR CENSORED DATA                            *
C      *                                                                    *
C      *    TY(I)=YY(I)*DEL+((ALPHA*X+SUM(WXX(K)*Z(K))/(1-W(I)))*(1-DEL)    *
C      *     WHERE                                                          *
C      *          TY   : ESTIMATED DEPENDENT VALUE                          *
C      *          DEL  : IF THE DATA IS UNCENSORED :DEL=1.0                 *
C      *                 IF THE DATA IS CENSORED  :DEL=0.0                  *
C      *          SUM  : SUM OVER UNCENSORED DATA Z(K)<Z(I)                 *
C      *          WX   : WEIGHT ... W(I-1)-W(I)                             *
C      *          W    : KAPLAN-MEIER PRODUCT LIMIT ESTIMATOR               *
C
C
       K=0
       DO 21 I=1,NTOT
          IF((IND2(I).NE.0).OR.(K.NE.0)) THEN
             IF(IND2(I).NE.0) THEN
                IPT(I)=K
                GOTO 21
             ENDIF
             IF((IND2(I).EQ.0).AND.(ZY(I).EQ.T(K))) THEN
                ND(K)=ND(K)+1
                IPT(I)=K
                GOTO 21
             ENDIF
          ENDIF
          ND(K+1)=1
          T(K+1)=ZY(I)
          IPT(I)=K+1
          K=K+1
   21  CONTINUE
C
       NI=K
       DO 28 I=1,NI
          NO(I)=0
          IDZ=0
   28  CONTINUE
C
       DO 29 I=1,NTOT
C
C      *      IF THE FIRST POINT IS A CENSORED VALUE, DROP THE POINT.       *
C      *      PT RUNS FROM 1 TO NU.                                         *
C
          IF(IPT(I).EQ.0) IDZ=IDZ+1
          IF(IPT(I).GT.0) NO(IPT(I))=NO(IPT(I))+1
   29  CONTINUE

       DENOM=REAL(NTOT-IDZ)
       W(1)=1.0-ND(1)/DENOM
       WJ=NTOT-NO(1)-IDZ
C
       DO 30 I=2,NI
          W(I)=W(I-1)*(1.0-ND(I)/WJ)
          WJ=WJ-NO(I)
   30  CONTINUE

       WX(1)=1.0-W(1)
C
       DO 41 I=2,NI
          WX(I)=W(I-1)-W(I)
   41  CONTINUE
C
       DO 83 I=1,NI
          WX(I)=WX(I)/REAL(ND(I))
   83  CONTINUE
C
       Z(NTOT)=0.0
       DO 36 JJ=2,NTOT
          I=NTOT-JJ+1
          Z(I)=Z(I+1)
          IF((ZY(I+1).GT.ZY(I)).AND.(IND2(I+1).EQ.0)) 
     +                           Z(I)=Z(I+1)+WX(IPT(I+1))*ZY(I+1)
   36  CONTINUE
C
       III=NTOT-1
       DO 32 I=1,III
          IF(IPT(I).GT.0) Z(I)=Z(I)/W(IPT(I))
          IF(IND2(I).NE.0) THEN
             ZS=0.0
             DO 62 J=1,NVAR
                JJ=J+1
                ZS=ZS+ALPHA(JJ)*XX(J,I)
   62        CONTINUE
             TY(I)=Z(I)+ZS
          ENDIF
   32  CONTINUE
C
       CALL REGRES(XX,TY,NTOT,NVAR,ALPHA,SIGMAA,ARRAY)
       ITE=ITE+1
C
C       *        TEST FOR CONVERGENCE                                       *
C       *     TEST(I) CONTAINS A PREVIOUS VALUE OF ALPHA(I).                *
C       *     IF NUMBER OF ITERATION EXCEEDS MAXITS, THE PROGRAM STOPS      *
C       *     TO CONTINUE ITERATION, EVEN IF TOLERANCE IS LARGER THAN       *
C       *     THE ASSIGNED VALUE. SINCE THE FINAL VALUES ARE OFTEN          *
C       *     TRAPPED IN OSCILLATION, THE PROGRAM TAKES AVERAGE OF THE      *
C       *     LAST TWO VALUES FOR THE FINAL OUTPUT.                         *
C
       IF(ITE.LE.MAX) THEN

          SUM=0.0
          DO 154 K=1,NVAR1
             SUM=SUM+(ALPHA(K)-TEST2(K))**2
             TEST2(K)=TEST(K)
             TEST(K)=ALPHA(K)
  154     CONTINUE
C
C       *  IF THE DIFFERENCE IS LARGER THAN THE TOLERANCE, REPEAT ITERATION *
C
  234     IF(DSQRT(SUM).GT.TOL) GOTO 2000
       ENDIF
C
C       *  THE CONVERSION IS OBTAINED OR THE ITERATION REACHED THE MAX.     *
C
       DO 270 K=1,NVAR1
          ALPHA(K)=(ALPHA(K)+TEST2(K))/2.0
  270  CONTINUE
C
C      *             CALCULATION OF VARIANCE ETC.                           *
C
       STD=0.0
       EM =0.0
       K=NTOT
       DO 35 I=1,NTOT
          IF(IND(I).EQ.0) THEN

             ZS=0.0
             DO 84 J=1,NVAR
                JJ=J+1
                ZS=ZS+ALPHA(JJ)*X(J,I)
   84        CONTINUE

             Z(I)=Y(I)-ZS
             EM=EM+Z(I)
          ENDIF
   35  CONTINUE

       EM=EM/REAL(NU)
       DO 37 I = 1,NTOT
          IF(IND(I) .EQ. 0) THEN
             EM2 = Z(I) - EM
             STD = STD+EM2*EM2
          ENDIF
   37  CONTINUE

       STD=STD/FLOAT(NU-NVAR1)

       DO 76 I=1,NVAR
          SIGMAA(I+1)=DSQRT(STD/BU(I))
   76  CONTINUE

       ALPHA(NVAR2)=DSQRT(STD)

       RETURN
       END
