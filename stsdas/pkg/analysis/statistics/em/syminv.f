C
C      **********************************************************************
C      ******************* SUBROUTINE SYMINV  *******************************
C      **********************************************************************
C
       SUBROUTINE SYMINV(A,N,C,W,NULLTY,NA,NC,NW,IFAULT)
C
C      *      ALGORITHM AS 7 J.R.STATIST.SOC.C. (1968) VOL.17, NO.2         *
C      *                                                                    *
C      *        FORMS IN C( ) A LOWER TRIANGULAR MATRIX, WHICH IS A         *
C      *        GENERALISED INVERSE OF THE POSITIVE SEMI-DEFINITE SYMMETRIC *
C      *        MATRIX A( ) OF ORDER N.                                     *
C      *        C( ) MAY COINCIDE WITH A( ). NULLTY IS RETURNED AS          *
C      *        THE NULLITY OF A( ). IFAULT IS RETURNED AS 1 IF             *
C      *        N.LT.1,OTHERWISE ZERO. W( ) IS A WORK ARRAY OF              *
C      *        LENGTH AT LEAST N THAT IS ALLOCATED BY THE CALLING          *
C      *        ROUTINE.                                                    *
C
C      *        NOTE : THE VARIABLES NA,NC,AND,NW,HAVE BEEN ADDED           *
C      *               TO THE ARGUMENT LIST AND ARE USED TO                 *
C      *               DIMENSION TO ARRAYS A,C,AND W, RESPECTIVELY.         *
C      *               (BY WOLYNETZ (1979))                                 *
C      *                                                                    *
C      *        SUBROUTINES                                                 *
C      *               CHOL                                                 *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       DIMENSION A(NA),C(NC),W(NW)
C
       NROW=N
       IFAULT=1
       IF(NROW.GT.0) THEN
          IFAULT=0

          CALL CHOL(A,NROW,C,NULLTY,NA,NC,IFAULT)

          IF(IFAULT.EQ.0) THEN
             NN=(NROW*(NROW+1))/2
             IROW=NROW
             NDIAG=NN
   16        IF(C(NDIAG).NE.0.0) THEN
                L=NDIAG

                DO 10 I=IROW,NROW
                   W(I)=C(L)
                   L=L+I
   10           CONTINUE

                ICOL=NROW
                JCOL=NN
                MDIAG=NN
   15           L=JCOL
                X=0.0
                IF(ICOL.EQ.IROW) X=1.0/W(IROW)

                K=NROW
   13           IF(K.NE.IROW) THEN
                   X=X-W(K)*C(L)
                   K=K-1
                   L=L-1
                   IF(L.GT.MDIAG) L=L-K+1
                   GOTO 13
                ENDIF

                C(L)=X/W(IROW)
                IF(ICOL.EQ.IROW) GOTO 14
                MDIAG=MDIAG-ICOL
                ICOL=ICOL-1
                JCOL=JCOL-1
                GOTO 15
             ENDIF

             L=NDIAG
             DO 17 J=IROW,NROW
                C(L)=0.0
                L=L+J
   17        CONTINUE

   14        NDIAG=NDIAG-IROW
             IROW=IROW-1
             IF(IROW.NE.0) GOTO 16
          ENDIF
       ENDIF
       
       RETURN
       END
