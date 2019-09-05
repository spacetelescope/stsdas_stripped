C
C      **********************************************************************
C      ************************ SUBROUTINE TIE  *****************************
C      **********************************************************************
C
       SUBROUTINE TIE(ID,X,Y,NVAR,NTOT,IL,IM)
C
C      *       CHECKS FOR THE EXISTENCE OF TIED DATA POINTS. IF A POINT     *
C      *       IS NOT TIED THE IT SETS IL(I)=1 AND IM(I)=1.                 *
C      * INPUT                                                              *
C      *       ID(I)    :  INDICATOR OF CENSORING                           *
C      *       X(J,I)   :  INDEPENDENT VARIABLES                            *
C      *       Y(I)     :  DEPENDENT VARIABLE                               *
C      *       NVAR     :  NUMBER OF INDEPENDENT VARIABLES                  *
C      *       NTOT     :  NUMBER OF DATA POINTS                            *
C      *                                                                    *
C      * OUTPUT   :                                                         *
C      *       ID, X, AND Y IN  ORDER SO THAT DETECTED POINTS ARE           *
C      *       LOCATED BEFORE CENSORED POINTS IF THEY ARE TIED.             *
C      *       IL(I)    :  INDICATOR OF TIES.                               *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
       DIMENSION ID(NTOT),X(NVAR,NTOT),Y(NTOT)
       DIMENSION IL(NTOT),IM(NTOT)
C
       IL(1)=1
       IM(1)=1
       IL(2)=1
       IM(2)=1
       I=2
       J=1
  200  IF(Y(I).EQ.Y(I-1)) THEN
          IL(I)=IL(I-1)+1
          J=J+1
          DO 300 K=1,J
             L=I+1-K
             IM(L)=J
  300     CONTINUE
       ELSE
          J=1
       ENDIF
       IF(I.LT.NTOT) THEN
          I=I+1
          IL(I)=1
          IM(I)=1
          GOTO 200
       ENDIF
C
C      *     IF TIED DATA CONTAINS CENSORED POINTS, ORDER THEM SO THAT      *
C      *     A DETECTED POINT COMES FIRST.                                  *
C
       I=1
  550  J=1
       IF(IM(I).NE.1) THEN
  600     IF(ID(I+J-1).NE.0) THEN
             IF(J.GE.IM(I)) GOTO 800
             J=J+1
             GOTO 600
          ENDIF
          IF(J.NE.1) THEN
C
C      *       EXCHANGE THE DETECTED POINT AND THE CENSORED POINT           *
C                                       
             ID1=ID(I+J-1)
             ID(I+J-1)=ID(I)
             ID(I)=ID1

             CALL SWAPX(X,I,I+J-1,NVAR,NTOT)
          ENDIF
       ENDIF
  800  IF(I.LT.NTOT) THEN
          I=I+J
          GOTO 550
       ENDIF

       RETURN
       END
