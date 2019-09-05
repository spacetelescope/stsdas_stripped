C
C      **********************************************************************
C      ********************* SUBROUTTINE XVAR  ******************************
C      **********************************************************************
C
       SUBROUTINE XVAR(IND,X,J,NVAR,NTOT,
     &                 ISIGN,ZU,ZC,IU,IC,RISK,ATIE,LTOT)
C
C      *       THIS SUBROUTINE DISTINGUISHES UNCENSORED AND CENSORED        *
C      *       DATA IN THE X VARIABLE AND SORTS IT INTO ZU AND ZC. ALSO,    *
C      *       IF THE DATA CONTAIN UPPER LIMITS, THE SIGN OF THE            *
C      *       VALUES ARE CHANGED SO THAT THE PROGRAM FOR THE LOWER         *
C      *       LIMITS CAN BE USED. ADOPTED FROM ELISA T. LEE, "STATISTICAL  *
C      *       METHODS FOR SURVIVAL DATA ANALYSIS", 1980, LIFETIME          *
C      *       LEARNING PUBLICATIONS (BELMONT:CA).                          *
C      *                                                                    *
C      *       INPUT       IND(J,I): INDICATOR OF CENSORING                 *
C      *                   X(J,I)  : VARIABLE                               *
C      *                    J      : J-TH DATA SETS                         *
C      *                   NVAR    : NUMBER OF THE VARIABLES                *
C      *                   NTOT    : TOTAL NUMBER OF DATA POINTS            *
C      *                                                                    *
C      *       OUTPUT      ISIGN   : IF LOWER LIMIT, ISIGN = 1              *
C      *                             IF UPPER LIMIT, ISIGN = -1             *
C      *                   ZU(K)   : UNCENSORED DATA POINTS IN X(J,I)       *
C      *                   ZC(K)   : CENSORED DATA POINTS IN X(J,I)         *
C      *                    IU     : NUMBER OF UNCENSORED DATA POINTS       *
C      *                    IC     : NUMBER OF CENSORED DATA POINTS         *
C      *                   RISK(L) : RISK SET                               *
C      *                   ATIE(L) : NUMBER OF TIED DATA POINTS             *
C      *                   LTOT    : NUMBER OF RISK SETS                    *
C      *                                                                    *
C
       IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

       DIMENSION IND(NVAR,NTOT),X(NVAR,NTOT),ZU(NTOT)
       DIMENSION ZC(NTOT),ATIE(NTOT),RISK(NTOT)
C
C      *    FIND THE CENSORSHIP OF THE DATA SET. -1 FOR THE UPPER LIMIT    *
C      *    AND 1 FOR THE LOWER LIMIT                                      *
C
       ISIGN = 1     
       DO 100 I = 1, NTOT
          IF (IND(J,I) .NE. 0) THEN
             ISIGN = IND(J,I) / IABS(IND(J,I))
             GOTO 110
          ENDIF
  100  CONTINUE
  110  CONTINUE
C
C     *     DETECTED AND CENSORED DATA POINTS ARE SEPARATED.               *
C     *     THEN RISK SETS AND TIED DATA POINTS ARE FOUND.                 *
C     *     IN CASE THE DATA HAS UPPER LIMITS IT IS MULTIPLIED BY ISIGN    *
C     *     TO MAKE THE DATA HAVE LOWER LIMITS (RIGHT CENSORSHIP).         *
C

       L = 1
       IC = 0
       IU = 0

       DO 300 I = 1, NTOT
          ATIE(I) = 0.0
          IF (IND(J,I) .EQ. 0) THEN 
              IU = IU + 1
              ZU(IU) = ISIGN * X(J,I)
              IF (IU .NE. 1) THEN
                 IF (ZU(IU) .EQ. ZU(IU-1)) THEN
                    ATIE(L) = ATIE(L) + 1.0
                    RISK(L) = REAL(NTOT - I)
                 ELSE
                    ATIE(L) = 1.0
                    RISK(L) = REAL(NTOT - I)
                    L = L + 1
                 ENDIF
              ELSE
                 ATIE(L) = 1.0
                 RISK(L) = REAL(NTOT - I)
                 L = L + 1
              ENDIF
           ELSE
              IC = IC + 1
              ZC(IC) = ISIGN * X(J,I)
           ENDIF
  300   CONTINUE
        LTOT = L - 1

        RETURN
        END
