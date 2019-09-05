C
C      **********************************************************************
C      ********************** SUBROUTINE BHK  *******************************
C      **********************************************************************
C
       SUBROUTINE BHK(IND,X,Y,NTOT,Z,PROB,TAU,IAA,IBB)
C
C      *             GENERALIZED KENDALL'S TAU CORRELATION COEFFICIENT      *
C      *                         FOR CENSORED DATA                          *
C
C      *      THIS PROGRAM COMPUTES KENDALL'S TAU FOR BIVARIATE DATA        *
C      *      SETS. THE DATA SETS CAN CONTAIN CENSORED POINTS IN THE        *
C      *      INDEPENDENT VARIABLE AND/OR THE DEPENDENT VARIABLE.           *
C      *      ALTHOUGH THIS PROGRAM GIVES ANSWERS FOR DATA SETS WHICH       *
C      *      CONTAIN TIES, IT MAY NOT BE ACCURATE.                         *
C      *    PARAMETERS :                                                    *
C      *     INPUT                                                          *
C      *     NTOT          : NUMBER OF OBSERVATIONS                         *
C      *     X(I)          : INDEPENDENT PARAMETER OF I-TH OBSERVATION      *
C      *     Y(I)          : DEPENDENT PARAMETER OF I-TH OBSERVATION        *
C      *     IND(I)        : INDICATOR OF CENSORED STATUS                   *
C      *      EACH POINT MUST BE SPECIFIED ITS CENSORED STATUS :            *
C      *        FOR THE LOWER LIMITS                                        *
C      *                0   :   DETECTED POINT                              *
C      *                1   :   ONLY DEPENDENT VARIABLE IS LOWER LIMIT      *
C      *                2   :   ONLY INDEPENDENT VARIABLE IS LOWER LIMIT    *
C      *                3   :   BOTH VARIABLES ARE LOWER LIMIT              *
C      *                4   :   INDEPENDENT VARIABLE IS LOWER LIMIT AND     *
C      *                        DEPENDENT VARIABLE IS UPPER LIMIT           *
C      *      FOR THE UPPER LIMITS, CHANGE THE SIGN OF ABOVE INDICATORS.    *
C      *                                                                    *
C      *     WORK                                                           *
C      *     IAA(I)         : CONCORDANCE INFORMATION FOR X                 *
C      *     IBB(I)         : CONCORDANCE INFORMATION FOR Y                 *
C      *                                                                    *
C      *     OUTPUT                                                         *
C      *     Z             : Z-VALUE                                        *
C      *     PROB          : SIGNIFICANCE LEVEL FOR THE HYPOTHESIS THAT     *
C      *                     X AND Y ARE NOT CORRELATED UNDER THE           *
C      *                     GAUSSIAN DISTRIBUTION                          *
C      *     TAU           : TAU; USE CRC STATISTICAL TABLES TO INTERPRET   *
C      *                                                                    *
C      *     SUBROUTINES                                                    *
C      *               CENS, COEFF                                          *
C      *                                                                    *
C      *      REF. BROWN, HOLLANDER, AND KORWAR 1974, IN RELIABILITY        *
C      *           AND BIOMETRY P.327, EQNS 1 TO 8, PROSCHAN AND            *
C      *           SERFLING EDS (SIAM)                                      *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION X(NTOT),Y(NTOT),IND(NTOT)
       DIMENSION IAA(NTOT),IBB(NTOT)
C
       SIS =0.0
       ASUM =0.0
       BSUM =0.0
       AASUM=0.0
       BBSUM=0.0
C
C
C      *      THE SUBROUTINE CENS ADDS OR SUBTRACTS A SMALL NUMBER          *
C      *      FROM EACH CENSORED POINT SO THAT NO TIES WITH DETECTED        *
C      *      POINTS OCCUR.                                                 *
C
C
       CALL CENS(X,Y,IND,NTOT)
C
C
C      *      START MAKING INFORMATION FOR CONCORDANCE                      *
C
C
       DO 1900 I=1,NTOT
C
C      *      INFORMATION OF CONCORDANCE  FOR THE INDEPENDENT VAR.          *
C
          IA=2
          IB=3
          IC=4
          ID=-2
          IE=-3
          IG=-4
          IH=1
          IJ=-1
C
C      *       SUBROUTINE WHICH FINDS CONCORDANCE INFORMATION               *
C
          CALL COEFF(I,X,IND,NTOT,IAA,IA,IB,IC,ID,IE,IG,IH,IJ)
C
C      *       INFORMATION OF CONCORDANCE FOR THE DEPENDENT VAR.            *
C
          IA=1
          IB=3
          IC=-4
          ID=-1
          IE=-3
          IG=4
          IH=2
          IJ=-2

          CALL COEFF(I,Y,IND,NTOT,IBB,IA,IB,IC,ID,IE,IG,IH,IJ)
C
C      *        START COMPUTING QUANTITIES IS, IASUM, IBSUM,                *
C      *        IAASUM, AND IBBSUM.                                         * 
C
          DO 1800 J=1,NTOT
             IF((IAA(J).EQ.0).AND.(IBB(J).EQ.0)) GOTO 1800
             SIS=SIS+IAA(J)*IBB(J)
             ASUM=ASUM+IAA(J)**2
             BSUM=BSUM+IBB(J)**2

 1650        DO 1700 K=1,NTOT
                IF(IAA(J).NE.0) THEN
                   IF(IAA(K).NE.0) THEN
                      AASUM=AASUM+IAA(J)*IAA(K)
                   ENDIF
                ENDIF
 1670           IF(IBB(J).NE.0) THEN
                   IF(IBB(K).NE.0) THEN
                      BBSUM=BBSUM+IBB(J)*IBB(K)
                   ENDIF
                ENDIF
 1700        CONTINUE
 1800     CONTINUE
 1900  CONTINUE
C
C      *    NOW COMPUTE THE STATISTIC AND THE PROBABILITY                   *
C 
       D1=NTOT*(NTOT-1)
       D2=D1*(NTOT-2)
       TAU=2.0*SIS/D1
       ALP=2.0*(ASUM*BSUM)/D1
       GAM=4.0*((AASUM-ASUM)*(BBSUM-BSUM))/D2
       VAR=ALP+GAM
       SIGMA=DSQRT(VAR)
       Z=SIS/SIGMA
       PROB=1.0-AGAUSS(Z)
C
       RETURN
       END
