C
C      **********************************************************************
C      *********************** SUBROUTINE AARRAY ****************************
C      **********************************************************************
C
      SUBROUTINE AARRAY(Z, IND, ISTA, JCOL, NCOL, NTOT, NG1, NG2, XY,
     +                  ID1, ID2, IU, IC, N1, N2, NCOMP, ISIGN)
C
C      *                                                                    *
C      *     INPUT       Z(I,J)  : DATA TO BE TESTED                        *
C      *                 IND(I)  : INDICATOR OF CENSORING                   *
C      *                 ISTA(I) : INDICATOR OF GROUP                       *
C      *                 JCOL    : JCOL-TH SUB-DATA SET                     *
C      *                 NCOL    : TOTAL NUMBER OF DATA SETS                *
C      *                 NTOT    : TOTAL NUMBER OF DATA POINTS              *
C      *                 NG1     : INDICATOR OF THE FIRST GROUP             *
C      *                 NG2     : INDICATOR OF THE SECOND GROUP            *
C      *                                                                    *
C      *     OUTPUT      XY(I)   : DATA FROM FIRST OR SECOND GROUPS         *
C                        ID1(I)  : CENSOR FLAG                              *
C                        ID2(I)  : GROUP NUMBER                             *
C      *                 IU      : NUMBER OF UNCENSORED DATA POINTS         *
C      *                 IC      : NUMBER OF CENSORED DATA POINTS           *
C      *                 N1      : NUMBER OF DATA POINTS IN GROUP 1         *
C      *                 N2      : NUMBER OF DATA POINTS IN GROUP 2         *
C                        NCOMP   : NUMBER OF POINTS IN TWO GROUPS           *
C      *                 ISIGN   : INDICATOR OF LOWER/UPPER LIMITS          *
C      *                                                                    *
C      *     PUT ALL OBS. IN ARRAY XY AND FORM ARRAYS ID1 AND ID2           *
C      *           ID1(I)=0  : ITH OBS. IS UNCENSORED                       *
C      *                  1  : ITH OBS. IS CENSORED                         *
C      *           ID2(I)=J  : ITH OBS. IS FROM ITH SAMPLE, J=1,2           *
C      *                                                                    *
C      *     SUBROUTINES                                                    *
C      *           REVERS
C
C*******     ALTHOUGH THIS SUBROUTINE HAS THE SAME NAME AS A PROGRAM FROM   *
C*******     "STATISTICAL METHODS FOR SURVIVAL DATA ANALYSIS" BY ELISA T.   *
C*******     LEE, 1980, LIFETIME LEARNING PUBLICATIONS (BELMONT:CA),        *
C*******     IT IS DIFFERENT EXCEPT IN THE GENERAL PURPOSE.                 *
C*******     ID1(I) IS ASSIGNED IN THE OPPOSITE WAY SO THAT THE PPROGRAM    *
C*******     CAN USE THE DATA SETS WHICH ARE MADE FOR OTHER PROGRAMS.       *
C

        IMPLICIT REAL*8 (A-H,O-Z), INTEGER (I-N)

        DIMENSION Z(NCOL,NTOT),IND(NTOT),ISTA(NTOT)
        DIMENSION XY(NTOT),ID1(NTOT),ID2(NTOT)
C
C      *    FIND THE CENSORSHIP OF THE DATA SET. -1 FOR UPPER LIMITS       *
C      *    AND 1 FOR LOWER LIMITS                                         *
C
        ISIGN = 1
        DO 100 I = 1, NTOT
           IF (IND(I) .NE. 0) THEN
              ISIGN = IND(I) / IABS(IND(I))
              GOTO 110
           ENDIF
 100    CONTINUE
 110    CONTINUE
C     
C      *    COUNT NUMBER OF DATA POINTS IN THE TWO SUBSAMPLES             *
C
        NCOMP = 0 
        N1 = 0
        N2 = 0
        IU = 0
        IC = 0
C
        DO 200 I = 1, NTOT
           IF ((ISTA(I) .EQ. NG1) .OR. (ISTA(I) .EQ. NG2)) THEN
              NCOMP = NCOMP + 1
              XY(NCOMP) = ISIGN * Z(JCOL,I)
C
              IF(ISTA(I) .EQ. NG1) THEN
                 ID2(NCOMP) = 1
                 N1 = N1 + 1
              ENDIF
C
              IF(ISTA(I) .EQ. NG2) THEN
                 ID2(NCOMP) = 2
                 N2 = N2 + 1
              ENDIF
C
              IF(IND(I) .EQ. 0) THEN
                 ID1(NCOMP) = 0
                 IU = IU + 1
              ELSE
                 ID1(NCOMP) = 1
                 IC = IC + 1
              ENDIF
           ENDIF
  200   CONTINUE
C
C      *    IF THE DATA WAS MULTIPLIED BY -1, REVERSE TO RESTORE SORT     *
C
        IF (ISIGN .EQ. -1) THEN
           CALL REVRSD (XY, NCOMP)
           CALL REVRSI (ID1, NCOMP)
           CALL REVRSI (ID2, NCOMP)
        ENDIF
C
        RETURN
        END
