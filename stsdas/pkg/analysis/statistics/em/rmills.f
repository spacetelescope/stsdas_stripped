C
C      **********************************************************************
C      ********************* SUBROUTINE RMILLS  *****************************
C      **********************************************************************
C
       SUBROUTINE RMILLS(X,FUNC,TOL)
C
C      *      ALGORITHM AS 138.1 APPL.STATST. (1979) VOL.28. NO.2           *
C      *                                                                    *
C      *         COMPUTE THE RECIPROCAL OF MILLS RATIO                      *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       DATA FPI /1.2533141/, FPII /0.7978846/
C
       FUNC=0.0
       IF(X .LT. -10.0) RETURN
       FUNC=FPII
       Y=DABS(X)
       IF(Y .LT. 0.000001) RETURN
       SGN=1.0
       IF(X.LT.0.0) SGN=-1.0

       IF(Y.LE.2.0) THEN
          S=0.0
          A=1.0
          T=Y
          R=Y
          B=Y**2
   40     A=A+2.0
          S=T
          R=R*B/A
          T=T+R
          IF(R.GT.TOL) GOTO 40
          FUNC=1.0/(FPI*DEXP(0.5*B)-SGN*T)
          RETURN
       ENDIF

  100  A=2.0
       B1=Y
       S=Y
       A1=Y**2+1.0
       A2=Y*(A1+2.0)
       B2=A1+1.0
       T=A2/B2

  140  A=A+1.0
       A0=A1
       A1=A2
       A2=Y*A1+A*A0
       B0=B1
       B1=B2
       B2=Y*B1+A*B0
       R=S
       S=T
       T=A2/B2

       IF((T-R.GT.TOL) .OR.(T-S.GT.TOL)) GOTO 140
       FUNC=T

       IF(SGN.LT.0.0) FUNC=T/(2.0*FPI*DEXP(0.5*Y**2)*T-1.0)

       RETURN
       END
