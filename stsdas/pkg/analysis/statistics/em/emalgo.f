C
C      **********************************************************************
C      ********************* SUBROUTINE EMALGO  *****************************
C      **********************************************************************
C
       SUBROUTINE EMALGO(NTOT,Y1,Y2,P,MPLONE,X,W,WCEN,LENW,
     +          VCOV,WORK,LENWRK,ALPHA,TOL,MAXITS,IFAULT,ICHECK,NC)
C
C
C      *       ALGORITHM AS 139 APPL.STATIST. (1979) VOL.28., NO2           *
C      *                                                                    *
C      *       COMPUTES MAXIMUM LIKELIHOOD ESTIMATES                        *
C      *       FROM A LINEAR MODEL WITH NORMAL HETEROGENEOUS                *
C      *       VARIANCE. THE DESIGN MATRIX MUST BE NON-SINGULAR.            *
C      *       THE DEPENDENT VARIABLE MAY INCLUDE OBSERVATIONS              *
C      *       CENSORED IN EITHER TAIL AND/OR OBSERVATIONS CONFINED         *
C      *       BETWEEN FINITE LIMITS.                                       *
C      *                                                                    *
C      *  SUBROUTINES                                                       *
C      *             SYMINV, UNPACK, RMILLS                                 *
C
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       INTEGER P

       DIMENSION X(NTOT,MPLONE),TOL(MPLONE),ALPHA(MPLONE)
       DIMENSION Y1(NTOT),Y2(NTOT),P(NTOT)
       DIMENSION VCOV(LENWRK),WORK(LENWRK),W(LENW),WCEN(LENW)
       DATA QLIMIT /0.00001/, RLIMIT /0.00001/
       DATA C /0.39894228/
C
C      *                    INITIALIZATION                                  *
C      *       THE NEXT LINE IS LOCATED IN A DIFFERENT PLACE IN THE         *
C      *       ORIGINAL PROGRAM BY WOLYNETZ                                 *
C
       M=MPLONE-1
C
C      *                 CHECK ARRAY SIZES, ETC                             *
C
       IFAULT=-7
       IF(LENW.LT.(MPLONE+NTOT)) RETURN
       IF(LENWRK.LT.(M*NTOT)) RETURN
C
C      *          COMPUTE X'X IN LOWER TRIANGULAR FORM                      *
C
       II=0
       DO 53 I=1,M
          DO 50 J=1,I
             TEMP=0.0
             DO 40 K=1,NTOT
                TEMP=TEMP+X(K,I)*X(K,J)
   40        CONTINUE
             II=II+1
             VCOV(II)=TEMP
   50     CONTINUE
   53  CONTINUE

       CALL SYMINV(VCOV,M,WORK,W,NUL,LENWRK,LENWRK,LENW,IFAULT)

       IF((IFAULT.NE.0) .OR. (NUL.NE.0)) THEN
          VCOV(2)=REAL(IFAULT)
          VCOV(1)=REAL(NUL)
          IFAULT=-5
          RETURN
       ENDIF

C
C      *      THE MATRIX IS NON-SINGULAR AND WE HAVE ITS INVERSE.  NOW WE   *
C      *      COMPUTE (X'X) INVERSE*X.                                      *
C      *      THE FOLLOWING SCHEME IS USED TO REDUCE THE NUMBER OF STORAGE  *
C      *      ARRAYS NEEDED, BY EXPANDING FROM THE TRIANGULAR TO A SQUARE   *
C      *      MATRIX.                                                       *
C
C      *         THE NEXT LINE IS NOT IN WOLYNETZ.                          *
C
       IF(M.NE.1) THEN
          CALL UNPACK(WORK,M,LENWRK)
       ENDIF
C
C      *      DO MULTIPLICATION--ONE ROW AT A TIME--STARTING WITH           *
C      *      THE LAST ROW                                                  *
C
       JJ=NTOT*M
       II=M*M
       DO 220 I=1,M
          II=II-M

          DO 200 J=1,NTOT
             TEMP=0.0

             DO 170 K=1,M
                IIK=II+K
                TEMP=TEMP+WORK(IIK)*X(J,K)
  170        CONTINUE
             W(J)=TEMP
  200     CONTINUE

          DO 210 J=1,NTOT
             IJ=NTOT+1-J
             WORK(JJ)=W(IJ)
             JJ=JJ-1
  210     CONTINUE
  220  CONTINUE
C
       XSIG=ALPHA(MPLONE)
       IF(XSIG.LE.0.0) THEN
C
C      *       NO ACCEPTABLE INITIAL VALUE FOR SIGMA HAS BEEN INPUT,        *
C      *       OBTAIN INITIAL ESTIMATES FROM EXACTLY SPECIFIED              *
C      *       OBSERVATIONS ONLY (ALTHOUGH THE MATRIX IS BASED ON ALL       *
C      *       OBSERVATIONS) AND CONFINED OBSERVATIONS.                     *
C
          II=-NTOT
          DO 300 I=1,M
             II=II+NTOT
             TEMP=0.0
             DO 280 J=1,NTOT
                IIJ=II+J
                IPT=P(J)
                IF(IPT.EQ.0) THEN
                   TEMP=TEMP+WORK(IIJ)*Y1(J)
                ELSEIF(IPT.EQ.5) THEN 
                   TEMP=TEMP+WORK(IIJ)*(Y1(J)+Y2(J))*0.5
                ENDIF
  280        CONTINUE
             ALPHA(I)=TEMP
  300     CONTINUE
C
C      *           CALCULATE THE INITIAL ESTIMATE OF SIGMA                  *
C
          SUM2=0.0
          TEMP=0.0
          DO 350 I=1,NTOT
             IPT=P(I)
             IF(IABS(IPT).NE.1) THEN
                DEMP=Y1(I)
                IF(IPT.EQ.5) DEMP=(DEMP+Y2(I))*0.5

                DO 320 J=1,M
                   DEMP=DEMP-ALPHA(J)*X(I,J)
  320           CONTINUE

                SUM2=SUM2+DEMP**2
                TEMP=TEMP+1.0
             ENDIF
  350     CONTINUE

          XSIG=DSQRT(SUM2/TEMP)
       ENDIF
C
C      *         COMPUTE SOME CONSTANTS NEEDED THROUGHOUT THE SUBROUTINE    *
C
       R=0.0
       R2=0.0
       IFAULT=-2
       DO 600 I=1,NTOT
          IPT=P(I)
          IF(IPT.EQ.0)  THEN
             R=R+1.0
             W(I)=Y1(I)
C
C      *       THE NEXT LINE IS LOCATED IN A DIFFERENT PLACE IN THE         *
C      *       ORIGINAL PROGRAM BY WOLYNETZ                                 *
C
          ELSEIF(IPT.EQ.5) THEN
             IF(DABS(Y1(I)-Y2(I)) .GT. QLIMIT*DABS(Y1(I))) THEN
                R2=R2+1.0
                IF(Y1(I).LT.Y2(I)) GOTO 600
                RETURN
             ENDIF
             P(I)=0
             R=R+1.0
             W(I)=Y1(I)
          ENDIF
  600  CONTINUE

       I=INT(R+R2+0.01)
       IFAULT=-4
       IF(I.LT.MPLONE) RETURN
       IFAULT=0
C
C      *             START OF THE ITERATION PROCEDURE                       *
C
  620  TD=R
       SUM2=0.0
C
C      *             COMPLETE W-VECTOR                                      *
C
       DO 1000 I=1,NTOT
          IPT=P(I)
          YMEAN=0.0

          DO 650 J=1,M
             YMEAN=YMEAN+ALPHA(J)*X(I,J)
  650     CONTINUE
C
C      *       OBSERVATION IS NOT EXACTLY SPECIFIED: START FROM LINE 990    *
C
          IF(IPT.NE.0) THEN
             TEMP=(Y1(I)-YMEAN)/XSIG
C
C      *        OBSERVATION CENSORED FROM ABOVE:  LOWER BOUND IS KNOWN      *
C
             IF((IPT-1) .EQ. 0) THEN
  700           CALL RMILLS(TEMP,F,RLIMIT)
                W(I)=YMEAN+XSIG*F
                TD=TD+F*(F-TEMP)
C
C      *         OBSERVATON CENSORED FROM BELOW:  UPPER BOUND IS KNOWN      *
C
             ELSEIF((IPT-1) .LT. 0) THEN
  750           CALL RMILLS(-TEMP,F,RLIMIT)
                W(I)=YMEAN-XSIG*F
                TD=TD+F*(F+TEMP)
C
C      *       OBSERVATION CONFINED TO LIE BETWEEN TWO FINITE LIMITS        *
C
             ELSEIF((IPT-1) .GT. 0) THEN
  800           YN=DEXP(-0.5*TEMP**2)*C
                CALL RMILLS(TEMP,F,RLIMIT)
                YQ=YN/F
                TMPU=(Y2(I)-YMEAN)/XSIG
                YNU=DEXP(-0.5*TMPU**2)*C
                CALL RMILLS(TMPU,FU,RLIMIT)
                YQU=YNU/FU
                TINT=YQ-YQU

                IF(TINT.LT.QLIMIT) THEN
                   IFAULT=-3
                   RETURN
                ENDIF
C
C      *       AFTER STANDARDIZING, UPPER AND LOWER LIMITS RESULT IN        *
C      *       THE SAME PROBABILITY INTEGRAL                                *
C
  820           A=(YN-YNU)/TINT
                W(I)=YMEAN+XSIG*A
                TD=TD+(A**2+(TMPU*YNU-TEMP*YN)/TINT)
             ENDIF
          ENDIF
C
C      *        CALCULATE RESIDUAL SUM OF SQUARES                           *
C
  990     SUM2=SUM2+(W(I)-YMEAN)**2
 1000  CONTINUE
C
C      *    UPDATE PARAMETER ESTIMATES-STORE IN THE END OF THE W-VECTOR     *
C
       JJ=-NTOT
       DO 1200 J=1,M
          JJ=JJ+NTOT
          TEMP=0.0

          DO 1100 I=1,NTOT
             JJI=JJ+I
             TEMP=TEMP+WORK(JJI)*W(I)
 1100     CONTINUE
          NJ=NTOT+J
          W(NJ)=TEMP
 1200  CONTINUE

       NJ=NTOT+MPLONE
       W(NJ)=DSQRT(SUM2/TD)
C
C      *       STORE THE ESTIMATES FOR THE CENSORED POINTS                  *
C
       KC=0
       DO 1250 I=1,NTOT
          IF(P(I).EQ.0) GOTO 1250
          KC=KC+1
          WCEN(KC)=W(I)
 1250  CONTINUE
C
C      *             TEST FOR CONVERGENCE                                   *
C
       DO 1300 J=1,MPLONE
          NJ=NTOT+J
          IF(DABS(ALPHA(J)-W(NJ)).GE.TOL(J)) GOTO 1400
 1300  CONTINUE
C
C      *          IF WE REACH HERE, CONVERGENCE IS OBTAINED                 *
C
       IJ=IFAULT
       IFAULT=-1
C
C      *                UPDATE VALUES                                       *
C
 1400  DO 1450 J=1,MPLONE
          NJ=NTOT+J
          ALPHA(J)=W(NJ)
 1450  CONTINUE

       XSIG=ALPHA(MPLONE)
       IFAULT=IFAULT+1
       IF(IFAULT.NE.0) THEN
C
C      *      IF THE NUMBER OF ITERATIONS HAS NOT REACHED THE MAX., TRY     *
C      *      ANOTHER ITERATION.                                            *
C
          IF(IFAULT.LE.MAXITS) GOTO 620
          IFAULT=-1
          RETURN
       ENDIF
C
C      *        CONVERGENCE OBTAINED.  COMPUTE VARIANCE--COVARIANCE         *
C      *        MATRIX, AND INITIALIZE THE WORK ARRAY                       *
C
 1600  II=MPLONE*(MPLONE+1)/2
       DO 1650 I=1,II
          WORK(I)=0.0
 1650  CONTINUE

       DO 2500 I=1,NTOT
          IPT=P(I)
          YS=Y1(I)

          DO 1680 J=1,M
             YS=YS-ALPHA(J)*X(I,J)
 1680     CONTINUE

          YS=YS/XSIG
          JJ=0
          IF(IPT.EQ.0) THEN

C
C      *             EXACTLY SPECIFIED OBSERVATION                          *
C

             DO 1750 K=1,M
                DO 1720 J=1,K
                   JJ=JJ+1
                   WORK(JJ)=WORK(JJ)+X(I,K)*X(I,J)
 1720           CONTINUE
C
C
C      *      THE NEXT LINE IS CORRECTED ACCORDING TO REMARK AS R 32        *
C      *      APPL.STAT. VOL 30, P. 105 (1981).                             *
C
C
                KK=II-MPLONE+K
                WORK(KK)=WORK(KK)+YS*X(I,K)
 1750        CONTINUE
             WORK(II)=WORK(II)+1.0+YS**2

C
C      *      OBSERVATION CENSORED FROM ABOVE:  LOWER BOUND IS KNOWN        *
C

          ELSEIF((IPT-1) .LE. 0) THEN
             IF((IPT-1) .EQ. 0) THEN
                CALL RMILLS(YS,F,RLIMIT)
                TEMP=F*(F-YS)

C
C      *      OBSERVATION CENSORED FROM BELOW:  UPPER BOUND IS KNOWN        *
C

             ELSE
                CALL RMILLS(-YS,F,RLIMIT)
                TEMP=F*(F+YS)
             ENDIF
C
C      *         ROUTINE FOR CENSORED OBSERVATIONS                          *
C
             DO 2190 K=1,M
                DO 2170 J=1,K
                   JJ=JJ+1
                   WORK(JJ)=WORK(JJ)+X(I,J)*X(I,K)*TEMP
 2170           CONTINUE
C
C      *      THE NEXT LINE IS CORRECTED ACCORDING TO REMARK AS R 32        *
C      *      APPL.STAT. VOL 30, P. 105 (1981).                             *
C
                KK=II-MPLONE+K
                WORK(KK)=WORK(KK)+YS*X(I,K)*TEMP
 2190        CONTINUE
             WORK(II)=WORK(II)+YS**2*TEMP

C
C      *       OBSERVATION CONFINED BETWEEN TWO FINITE LIMITS               *
C

          ELSEIF((IPT-1) .GT. 0) THEN
             YN=DEXP(-0.5*YS**2)*C
             CALL RMILLS(YS,F,RLIMIT)
             YQ=YN/F
             YSU=YS+(Y2(I)-Y1(I))/XSIG
             CALL RMILLS(YSU,FU,RLIMIT)
             YNU=DEXP(-0.5*YSU**2)*C
             YQU=YNU/FU
             TINT=YQ-YQU
             A=(YN-YNU)/TINT
             B=(YNU*YSU-YN*YS)/TINT
             TEMP=A**2+B

             DO 2350 K=1,M

                DO 2330 J=1,K
                   JJ=JJ+1
                   WORK(JJ)=WORK(JJ)+X(I,J)*X(I,K)*TEMP
 2330           CONTINUE
                TEMP=(YS**2*YN-YSU**2*YNU)/TINT
C
C
C      *     THE NEXT LINE IS CORRECTED ACCORDING TO REMARK AS R 32         *
C      *     APPL.STAT. VOL 30, P. 105 (1981).                              *
C
C
                KK=II-MPLONE+K
                WORK(KK)=WORK(KK)-(TEMP+A*B)*X(I,K)
 2350        CONTINUE

             TEMP=(YS**3*YN-YSU**3*YNU)/TINT
             WORK(II)=WORK(II)-TEMP+B**2
           ENDIF
 2500  CONTINUE
C
C      *                   INVERT THE MATRIX                                *
C
       CALL SYMINV(WORK,MPLONE,VCOV,W,NUL,LENWRK,LENWRK,LENW,IFAULT)

       IF((IFAULT.NE.0).OR.(NUL.NE.0)) THEN
          VCOV(2)=REAL(IFAULT)
          VCOV(1)=REAL(NUL)
          IFAULT=-6
          ICHECK=IJ
          RETURN
       ENDIF
C
C      *               RESTORE THE ITERATION COUNTER                         *
C
       IFAULT=IJ
C
C      *               MULTIPLY BY SIGMA-SQUARED                            *
C
       TEMP=XSIG**2
       DO 2580 I=1,II
          VCOV(I)=VCOV(I)*TEMP
 2580  CONTINUE

C
C      *                UNPACK THE MATRIX                                   *
C
       CALL UNPACK(VCOV,MPLONE,LENWRK)

       RETURN
       END
