C
C      **********************************************************************
C      ********************* SUBROUTINE COXREG  *****************************
C      **********************************************************************
C
       SUBROUTINE COXREG(IND,X,Y,NVAR,NTOT,ICENS,CHI,PROB,
     +                      RINFO,SCORE,FINFO,IL,IM)
C
C      *      THIS PROGRAM COMPUTES A CORRELATION PROBABILITY ACCORDING     *
C      *      TO COX'S (1972) PROPORTIONAL HAZARDS MODEL.                   *
C      *      ONLY ONE TYPE OF CENSORING (I.E. LOWER OR UPPER)              *
C      *      IS ALLOWED IN Y, BUT UP TO NVAR INDEPENDENT VARIABLES CAN     *
C      *      BE USED. THE HYPOTHESIS TESTED IS THE ABSENCE OF CORRELATION  *
C      *      BETWEEN THE DEPENDENT VARIABLE AND INDEPENDENT VARIABLES.     *
C      *      THEREFORE, THE  REGRESSION COEFFICIENT IN COX MODEL BETA      *
C      *      IS SET TO ZERO.                                               *
C
C      * NOTE NOTE NOTE:   THE PROBABILITY CALCULATED MAY NOT BE ACCURATE   *
C      *      WHEN THERE ARE A LARGE NUMBER OF TIED OBSERVATIONS (CF.       *
C      *      R. G. MILLER, SURVIVAL ANALYSIS, 1981, PP. 136-7).            *
C
C      *                                                                    *
C      *      INPUT    IND(I)  :  INDICATOR OF CENSORING                    *
C      *                           0 : UNCENSORED DATA POINT                *
C      *                           1 : Y(I) IS LOWER LIMIT                  *
C      *                          -1 : Y(I) IS UPPER LIMIT                  *
C      *              X(J,I)   :  INDEPENDENT VARIABLES (J=1,..NVAR)        *
C      *              Y(I)     :  DEPENDENT VARIABLE                        *
C      *               NVAR    :  NO. OF INDEPENDENT VARIABLES              *
C      *               NTOT    :  TOTAL NO. OF OBSERVATIONS                 *
C      *                                                                    *
C      *       WORK      DF    :  DEGREE OF FREEDOM                         *
C      *                IL(I)  :  INDICATOR OF TIES (#  OF TIES)            *
C      *                IM(I)  :  INDICATOR OF TIES (POSITION)              *
C      *               RINFO(I):  INFORMATION MATRIX AND ITS INVERSE        *
C      *                          MATRIX AFTER CALLING SUBROUTINE           *
C      *                          MATINV.                                   *
C      *               SCORE(I):  SCORE VECTOR                              *
C      *                                                                    *
C      *     OUTPUT       CHI  :  GLOBAL CHI-SQUARE                         *
C      *                 PROB  :  PROBABILITY OF CORRELATION                *
C      *                                                                    *
C      *     SUBROUTINES                                                    *
C      *                          CXSORT, TIE, MATINV, PCHISQ               *
C      *                                                                    *
C      *     REFERENCE:    RUPERT G. MILLER JR., "SURVIVAL ANALYSIS", 1981, *
C      *                         JOHN WILEY & SONS (NY:NY)                  *
C      *                                                                    *
C
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION IND(NTOT),Y(NTOT),X(NVAR,NTOT),IL(NTOT),IM(NTOT)
       DIMENSION RINFO(NVAR,NVAR),SCORE(NVAR),FINFO(NVAR)
C
       DF=NVAR
C
       DO 20 I=1,NTOT
          IF(ICENS.EQ.-1) Y(I)=-Y(I)
C
C      *  IF THE OBSERVATION IS CENSORED, ADD A SMALL NUMBER TO AVOID TIES  *
C      *  WITH DETECTED VALUE.                                              *
C
          IF(IND(I).NE.0) Y(I)=Y(I)*(1.0+FLOAT(ICENS)*0.0000001)
C
          DO 10 J=1,NVAR
             IF(ICENS.EQ.-1) X(J,I)=-X(J,I)
   10     CONTINUE
   20  CONTINUE
C
C      *           SORT Y IN ASCENDING ORDER                                *
C
       CALL CXSORT(IND,X,Y,NVAR,NTOT)
C 
C      *          CHECK TIED DATA POINTS AND GIVE THEM A SPECIAL FLAG.      *
C
       CALL TIE(IND,X,Y,NVAR,NTOT,IL,IM)
C
C      *         COMPUTE SCORE VECTOR. DIMENSION IS NVAR                    *
C
       DO 600 I=1,NVAR
          SCORE(I)=0.0
C
          DO 500 J=1,NTOT 
             IF(IND(J).EQ.0) THEN
                IF(IL(J).EQ.1) THEN
                   SUM=0.0
C
                   DO 400 K=J,NTOT
                      SUM=SUM+X(I,K)
  400              CONTINUE
C
                   JJ=J+IM(J)-1
                   XSUM=0.0
                   DO 450 KL=J,JJ
                      XSUM=XSUM+X(I,KL)
  450              CONTINUE
C
                   DEN=REAL(NTOT+1-J)
                   SCORE(I)=SCORE(I)+XSUM-IM(J)*SUM/DEN
                ENDIF
             ENDIF
  500     CONTINUE
  600  CONTINUE
C
C      *    COMPUTE THE INFORMATION MATRIX. DIMENSION IS NVAR BY NVAR       *
C
       DO 1000 I=1,NVAR
          DO  900 J=I,NVAR
             RINFO(I,J)=0.0
C
             DO 800 K=1,NTOT 
                IF(IND(K).EQ.0) THEN
                   IF(IL(K).EQ.1) THEN
                      SUM1=0.0
                      SUM2=0.0
                      SUM3=0.0
C
                      DO 700 L=K,NTOT
                         SUM1=SUM1+X(I,L)
                         SUM2=SUM2+X(J,L)
                         SUM3=SUM3+X(I,L)*X(J,L)
  700                 CONTINUE
                      DEN=NTOT+1-K
                      RINFO(I,J)=RINFO(I,J)-REAL(IM(K))
     +                     *(SUM1*SUM2/DEN**2-SUM3/DEN)
                   ENDIF
                ENDIF
  800        CONTINUE
             RINFO(J,I)=RINFO(I,J)
  900     CONTINUE
 1000  CONTINUE
C
C      *     INVERT INFORMATION MATRX RINFO(I,J). THE INVERTED MATRIX       *
C      *     IS STORED IN RINFO(I,J).                                       *
C
       CALL MATINV(RINFO,NVAR,DET)
C
C      *      COMPUTE GLOBAL CHI-SQUARE:                                    *
C      *                                                                    *
C      *       CHI = [SCORE(I)**T] X [RINFO(I,J)**-1] X [SCORE(J)]          *
C      *         WHERE T IS TRANSVERSE.                                     *
C
       DO 1200 I=1,NVAR
          FINFO(I)=0.0
          DO 1100 K=1,NVAR
             FINFO(I)=FINFO(I)+RINFO(I,K)*SCORE(K)
 1100     CONTINUE
 1200  CONTINUE
       CHI=0.0
C
       DO 1300 L=1,NVAR
          CHI=CHI+FINFO(L)*SCORE(L)
 1300  CONTINUE

C
C      *              GET THE REDUCED CHI-SQUARE                            *
C
       RCHI=CHI/DF
C
C      *           COMPUTE CHI-SQUARE PROBABILITY                           *
C
       PROB=PCHISQ(RCHI,NVAR)
C
       RETURN
       END
