C
C      **********************************************************************
C      ******************** SUBROUTINE UNPACK *******************************
C      **********************************************************************
C
       SUBROUTINE UNPACK(X,N,LENX)
C
C      *      ALGORITHM AS 139.1 APPL.STATIST. (1979) VOL.28., NO.2         *
C      *                                                                    *
C      *      THIS SUBROUTINE EXPANDS A SYMMETRIC MATRIX STORED IN          *
C      *      LOWER TRIANGLAR FORM IN THE FIRST N*(N+1)/2 POSITIONS         *
C      *      OF X INTO A MATRIX USING THE FIRST N*N POSITIONS              *
C
C      *      LENX--THE LENGTH OF VECTOR--MUST BE NOT LESS THAN N*N         *
C      *         (I.E. MUST NOT BE LESS THAN (NVAR+1)**2                    *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       DIMENSION X(LENX)

       NSQ=N*N
       II=NSQ
       JJ=N*(N+1)/2
C
C      *                     STORE LAST ROW                                 *
C
       DO 10 I=1,N
          X(II)=X(JJ)
          II=II-1
          JJ=JJ-1
   10  CONTINUE

       DO 80 I=2,N
C
C      *      OBTAIN UPPER PART OF MATRIX FROM PART ALREADY SHIFTED         *
C
          IJ=I-1
          KK=NSQ+1-I
          DO 50 J=1,IJ
             X(II)=X(KK)
             II=II-1
             KK=KK-N
   50     CONTINUE
C
C      *      OBTAIN LOWER PART OF MATRIX FROM ORIGINAL TRIANGULAR          *
C      *      STORAGE                                                       *
C
          IJ=N-IJ
          DO 70 J=1,IJ
             X(II)=X(JJ)
             II=II-1
             JJ=JJ-1
   70     CONTINUE
   80  CONTINUE

       RETURN
       END
