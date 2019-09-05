C
C      **********************************************************************
C      ********************* SUBROUTINE CHOL  *******************************
C      **********************************************************************
C
       SUBROUTINE CHOL(A,N,U,NULLTY,NA,NU,IFAULT)
C
C      *      ALGORITHM AS 6 J.R.STATIST.SOC.C.(1968) VOL.17, NO.2          *
C      *                                                                    *
C      *      GIVEN A SYMMETRIC MATRIX OF ORDER N AS A LOWER TRIANGLE       *
C      *      IN A( ),  CALCULATE AN UPPER TRIANGLE, U( ), SUCH THAT        *
C      *      UPRIME*U=A. U( ) MAY COINCIDE WITH A( ). A( ) MUST BE         *
C      *      POSITIVE SEMIDEFINITE.                                        *
C      *      ETA IS SET TO MULTIPLYING FACTOR DETERMINING THE              *
C      *      EFFECTIVE  ZERO FOR PIVOT.                                    *
C      *      NULLTY IS RETURNED AS NO. OF EFFECTIVE ZERO PIVOTS.           *
C      *      IFAULT IS RETURNED AS 1,IF N.LE.0, 2,IF A( ) IS NOT           *
C      *      POSITIVE SEMI-DEFINITE WITHIN THE TOLERANCE BY ETA.           *
C      *      OTHERWISE ZERO.                                               *
C
C      *        NOTE : VARIABLES NA,NU, HAVE BEEN ADDED TO THE              *
C      *               ARGUMENT LIST AND USED TO DIMENSION TO ARRAYS        *
C      *               A AND U, RESPECTIVELY. (BY WOLYNETZ (1979))          *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION A(NA),U(NU)
C
       DATA ETA /1.0E-9/
C
C      *       THE VALUE OF ETA WILL DEPEND ON THE WORD LENGTH OF           *
C      *       THE COMPUTER BEING USED.                                     *
C
       IFAULT=1
       IF(N.GT.0) THEN
          IFAULT=2
          NULLTY=0
          J=1
          K=0

          DO 10 ICOL=1,N
             L=0

             DO 11 IROW=1,ICOL
                K=K+1
                W=A(K)
                M=J

                DO 12 I=1,IROW
                   L=L+1
                   IF(I.EQ.IROW) GOTO 13
                   W=W-U(L)*U(M)
                   M=M+1
   12           CONTINUE

   13           IF(IROW.EQ.ICOL) GOTO 14
                IF(U(L).EQ.0.0) THEN
                   U(K) = 0.0
                ELSE
                   U(K)=W/U(L)
                ENDIF
   11        CONTINUE

   14        IF(DABS(W).GE.DABS(ETA*A(K))) THEN
                IF(W.LT.0.0) GOTO 100
                U(K)=DSQRT(W)
             ELSE
                U(K)=0.0
                NULLTY=NULLTY+1
             ENDIF
             J=J+ICOL
   10     CONTINUE

          IFAULT=0

       ENDIF
  100  RETURN
       END
