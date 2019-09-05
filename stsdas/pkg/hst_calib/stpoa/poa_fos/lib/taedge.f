C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE TAEDGE(PROFIL,N1,N2,EDGES,ECENT,CTROID,ISTAT)
*
*  Module number: FOS/HRS LIBRARY ROUTINE
*
*  Module name: taedge
*
*  Keyphrase:
*  ----------
*       locate feature in one-d profile
*
*  Description:
*  ------------
*       The centroid and edge locations are found for a feature
*       in a one-dimensional profile.
*
*  FORTRAN name: taedge.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*       none
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       9/10/87   D. Lindler    Designed and Coded
*-------------------------------------------------------------------------------
C
C
C INPUT PARAMETERS
C
C       PROFIL - ONE-D VECTOR WITH FEATURE PROFILE (REAL*8)
C       N1,N2 - INTEGER INDICES IN PROFIL GIVING AREA TO SEARCH FOR THE
C               FEATURE
C
C OUTUT PARAMTERS
C
C       EDGES - REAL VECTOR OF 2 ELEMENTS GIVING LEFT AND RIGHT EDGE LOCATION
C		(REAL*8)
C       ECENT - CENTER OF FEATURE COMPUTED FROM EDGE LOCATIONS (REAL*8)
C       CTROID - CENTROID OF THE FEATURE (REAL*8)
C
C----------------------------------------------------------------------------
	IMPLICIT NONE
C
C     INCLUDE FILE FOR THE IRAF77 FORTRAN INTERFACE TO THE IRAF VOS
C
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
      INTEGER   RDWRIT
      PARAMETER (RDWRIT = 2)
      INTEGER   WRONLY
      PARAMETER (WRONLY = 3)
      INTEGER   APPEND
      PARAMETER (APPEND = 4)
C
C     CODES FOR DATA TYPES
C
      INTEGER   TYBOOL
      PARAMETER (TYBOOL = 1)
      INTEGER   TYCHAR
      PARAMETER (TYCHAR = 2)
      INTEGER   TYINT
      PARAMETER (TYINT = 4)
      INTEGER   TYREAL
      PARAMETER (TYREAL = 6)
      INTEGER   TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      INTEGER USRLOG
      PARAMETER (USRLOG = 4)
C
C     UHDAS HEADER PARM TYPES -- CB, DAO, 5-SEP-87
C
      INTEGER GENHDR
      PARAMETER (GENHDR = 0)
      INTEGER IMSPEC
      PARAMETER (IMSPEC = 1)
C
C     THIS SECTION IS FOR PARAMETERS RELEVANT TO TABLE I/O.
C
C     THESE MAY BE SET BY UTPPTI AND/OR READ BY UTPGTI:
C
C                                       LENGTH OF ROW (UNIT = SIZE OF REAL)
      INTEGER   TBRLEN
      PARAMETER (TBRLEN = 1)
C                                       INCREASE ROW LENGTH
      INTEGER   TBIRLN
      PARAMETER (TBIRLN = 2)
C                                       NUMBER OF ROWS TO ALLOCATE
      INTEGER   TBALLR
      PARAMETER (TBALLR = 3)
C                                       INCREASE ALLOC NUM OF ROWS
      INTEGER   TBIALR
      PARAMETER (TBIALR = 4)
C                                       WHICH TYPE OF TABLE? (ROW OR COLUMN)
      INTEGER   TBWTYP
      PARAMETER (TBWTYP = 5)
C                                       MAXIMUM NUMBER OF USER PARAMETERS
      INTEGER   TBMXPR
      PARAMETER (TBMXPR = 6)
C                                       MAXIMUM NUMBER OF COLUMNS
      INTEGER   TBMXCL
      PARAMETER (TBMXCL = 7)
C                                       TYPE = ROW-ORDERED TABLE
      INTEGER   TBTYPR
      PARAMETER (TBTYPR = 11)
C                                       TYPE = COLUMN-ORDERED TABLE
      INTEGER   TBTYPC
      PARAMETER (TBTYPC = 12)
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C
C     END IRAF77.INC
        DOUBLE PRECISION    PROFIL(*),EDGES(2),ECENT,CTROID
        INTEGER N1,N2,ISTAT
C
C LOCAL VARIABLES
C
        DOUBLE PRECISION    PMIN,PMAX,SUM,SUMX,HMAX,EMIN,DIF,VAL
        INTEGER I,ISTAT2
C
C ZERO OUT VALUES IN CASE WE HIT AN ERROR
C
        CTROID = 0.0
        EDGES(1) = 0.0
        EDGES(2) = 0.0
        ECENT = 0.0
        ISTAT = 0
C
C COMPUTE MIN AND MAX OF PROFILE
C
        PMAX = -1.0E-20
        PMIN = 1.0E-20
        DO 10 I = N1,N2
                IF(PROFIL(I).LT.PMIN) PMIN = PROFIL(I)
                IF(PROFIL(I).GT.PMAX) PMAX = PROFIL(I)
10      CONTINUE
C
        IF(PMIN.EQ.PMAX) THEN
           CALL UMSPUT(
     *       'MINIMUM EQUALS THE MAXIMUM VALUE OF THE OBJECT PROFILE',
     *        STDOUT+USRLOG,0,ISTAT2)
           ISTAT = 1
           GO TO 999
        ENDIF
C
C COMPUTE THE CENTROID
C
        EMIN = PROFIL(N1)
        IF(PROFIL(N2).GT.EMIN) EMIN = PROFIL(N2)
        SUM = 0.0
        SUMX = 0.0
        DO 20 I = N1,N2
                VAL = PROFIL(I)-EMIN
                IF (VAL.LT.0) VAL = 0.0
                SUM = SUM+VAL
                SUMX = SUMX+VAL*I
20      CONTINUE
        IF(SUM.EQ.0) THEN
           CALL UMSPUT('MAXIMUM OF PROFILE OCCURS AT EDGE OF IMAGE',
     *                 STDOUT+USRLOG,0,ISTAT2)
           CALL UMSPUT('UNABLE TO DETERMINE CENTROID',STDOUT+USRLOG,
     *                 0,ISTAT2)
                ISTAT = 1
           ELSE
                CTROID = SUMX/SUM
        ENDIF
C
C FIND LEFT EDGE POSITION
C
        HMAX = PMAX/2.0
        DO 30 I = N1,N2
                IF(PROFIL(I).GE.HMAX)GO TO 40
30      CONTINUE
40      IF(PROFIL(N1).GT.HMAX)THEN
                CALL UMSPUT('LEFT EDGE COULD NOT BE LOCATED',
     *                STDOUT+USRLOG,0,ISTAT2)
                ISTAT = 1
           ELSE
C
C INTERPOLATE FOR BETTER LOCATION
C
                DIF = PROFIL(I)-PROFIL(I-1)
                IF(DIF.EQ.0.0)THEN
                        EDGES(1) = I-0.5
                  ELSE
                        EDGES(1) = I - (PROFIL(I)-HMAX)/DIF
                ENDIF
        ENDIF
C
C FIND RIGHT EDGE LOCATION
C
        DO 50 I = N2,N1,-1
                IF(PROFIL(I).GE.HMAX)GO TO 60
50      CONTINUE
60      IF(PROFIL(N2).GT.HMAX)THEN
                CALL UMSPUT('RIGHT EDGE COULD NOT BE LOCATED',
     *                  STDOUT+USRLOG,0,ISTAT2)
                ISTAT = 1
            ELSE
                DIF = PROFIL(I)-PROFIL(I+1)
                IF(DIF.EQ.0.0) THEN
                        EDGES(2) = I+0.5
                   ELSE
                        EDGES(2) =  I + (PROFIL(I)-HMAX)/DIF
                ENDIF
        ENDIF
C
C COMPUTE CENTER FROM EDGE LOCATIONS
C
        IF((EDGES(1).GT.0).AND.(EDGES(2).GT.0))
     *               ECENT = (EDGES(1)+EDGES(2))/2.0
999     RETURN
        END
