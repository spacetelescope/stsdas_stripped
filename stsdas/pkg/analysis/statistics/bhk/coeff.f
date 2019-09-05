C
C      **********************************************************************
C      ************************ SUBROUTINE COEFF  ***************************
C      **********************************************************************
C
       SUBROUTINE COEFF(I,X,IND,NTOT,ICOEFF,IA,IB,IC,ID,IE,IG,IH,IJ)
C
C      *        SUBROUTINE WHICH FINDS CONCORDANCE INFORMATION  OF          *
C      *        THE QUANTITY X(I).                                          *
C      *                                                                    *
C      *      INPUT  :   X(I)      : THE QUANTITIY TO BE EXAMINED           *
C      *                IND(I)     : CENSORED STATUS OF X(I)                *
C      *                NTOT       : NUMBER OF DATA                         *
C      *      OUTPUT : ICOEFF(I)   : CONCORDANCE INFORMATION:               *
C      *                             FOR X(I) AND X(J) WITH I<J,            *
C      *                             IF X(I)<X(J), ICOEFF= 1                *
C      *                             IF X(I)>X(J), ICOEFF=-1                *
C      *                             OTHERWISE,    ICOEFF= 0                *
C
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       DIMENSION X(NTOT),IND(NTOT),ICOEFF(NTOT)
C
       DO 100 J=1,NTOT
          ICOEFF(J)=0
          IF(X(I).LT.X(J)) THEN
             IF(IND(I).EQ.IA) GOTO 100
             IF(IND(J).EQ.ID) GOTO 100
             IF(IND(I).EQ.IB) GOTO 100
             IF(IND(J).EQ.IE) GOTO 100
             IF(IND(I).EQ.IC) GOTO 100
             IF(IND(J).EQ.IG) GOTO 100

             ICOEFF(J)=1

          ELSEIF(X(I).GT.X(J)) THEN
   50        IF(IND(I).EQ.ID) GOTO 100
             IF(IND(J).EQ.IA) GOTO 100
             IF(IND(I).EQ.IE) GOTO 100
             IF(IND(J).EQ.IB) GOTO 100
             IF(IND(I).EQ.IG) GOTO 100
             IF(IND(J).EQ.IC) GOTO 100

             ICOEFF(J)=-1
          ENDIF
  100  CONTINUE
       RETURN
       END
