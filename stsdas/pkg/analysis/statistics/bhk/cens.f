C
C      **********************************************************************
C      ********************** SUBROUTINE CENS  ******************************
C      **********************************************************************
C
        SUBROUTINE CENS(X,Y,IND,NTOT)
C
C      *         THIS SUBROUTINE ADDS OR SUBTRACTS 0.00001 TIMES THE        *
C      *         VALUE FROM CENSORED POINTS SO THAT IF THERE ARE TIES WITH  *
C      *         DETECTED POINTS, THE CENSORED POINTS CAN BE                *
C      *         DISTINGUISHED FROM THE DETECTIONS.                         *
C      *         IF THE SMALLEST DIGIT IS LESS THAN OR EQUAL TO             *
C      *         0.0001, THEN 'CONST' SHOULD BE CHANGED TO A                *
C      *         SMALLER VALUE.                                             *
C      *                                                                    *
C      *         INPUT AND OUTPUT:                                          *
C      *                   X(I)    : INDEPENDENT VARIABLE                   *
C      *                   Y(I)    : DEPENDENT VARIABLE                     *
C      *                  IND(I)   : CENSORED STATUS INDICATOR              *
C
        IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)
        DIMENSION X(NTOT),Y(NTOT),IND(NTOT)
C
C
        CONST=1.000001
        DO 500 I=1,NTOT
C
C      *                 UPPER LIMITS CASES                                 *
C
        IF(IND(I).EQ.-1) THEN 
              Y(I)=Y(I)/CONST

          ELSEIF(IND(I).EQ.-2) THEN
              X(I)=X(I)/CONST

          ELSEIF(IND(I).EQ.-3) THEN
              X(I)=X(I)/CONST
              Y(I)=Y(I)/CONST

          ELSEIF(IND(I).EQ.-4) THEN
              X(I)=X(I)/CONST
              Y(I)=Y(I)*CONST

C
C      *                  LOWER LIMIT CASES                                 *
C
          ELSEIF(IND(I).EQ.1)  THEN
              Y(I)=Y(I)*CONST

          ELSEIF(IND(I).EQ.2)  THEN
              X(I)=X(I)*CONST

          ELSEIF(IND(I).EQ.3)  THEN
              X(I)=X(I)*CONST 
              Y(I)=Y(I)*CONST

          ELSEIF(IND(I).EQ.4)  THEN
              X(I)=X(I)*CONST
              Y(I)=Y(I)/CONST

        ENDIF
  500   CONTINUE
        RETURN
        END
