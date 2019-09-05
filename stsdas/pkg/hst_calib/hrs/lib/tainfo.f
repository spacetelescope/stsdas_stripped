        SUBROUTINE TAINFO(IMAGE,NS,NL,NTS,NTL,AREA,EDGES,CTROID,ECENT,
     *         CCENT,TOT,ISTAT)
*
*  Module number: FOS/HRS LIBRARY ROUTINE
*
*  Module name: tainfo
*
*  Keyphrase:
*  ----------
*       locate feature in image
*
*  Description:
*  ------------
*       A feature is located in an FOS/HRS image using three methods
*               1) Mid-point between edge locations
*               2) Centroiding
*               3) Cross-correlation with a template
*
*  FORTRAN name: tainfo.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*       none
*
*  Subroutines Called:
*  -------------------
*  HRS/FOS library routines:
*       taedge, taccor
*
*  SDAS:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       9/10/87   D. Lindler    Designed and coded
*-------------------------------------------------------------------------------
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
C
C
C INPUT PARAMETERS
C
C       IMAGE - 2-D IMAGE WITH FEATURE TO LOCATE (REAL*8)
C       NS,NL - INTEGER DIMENSIONS OF IMAGE
C       NTS, NTL - SIZE OF CROSS-CORRELATION TEMPLATE IN LINE
C               AND SAMPLE DIRECTIONS
C       AREA - 4 WORD INTEGER VECTOR GIVING STARTING SAMPLE, ENDING SAMPLE
C               STARTING LINE, AND ENDING LINE OF IMAGE TO SEARCH FOR
C               THE FEATURE.
C OUTPUT PARAMETERS
C
C       EDGES - 4 WORD REAL VECTOR GIVING EDGE LOCATIONS;
C               LEFT, RIGHT, UPPER AND LOWER (REAL*8)
C       CTROID - 2 WORD VECTOR GIVING THE CENTROID IN THE SAMPLE
C               AND LINE DIRECTIONS (REAL*8)
C       ECENT - 2 WORD VECTOR GIVING THE CENTER COMPUTED FROM THE EDGES
C			(REAL*8)
C       CCENT - 2 WORD VECTOR GIVING THE CENTER COMPUTED USING CROSS-
C               CORRELATION (REAL*8)
C       TOT - TOTAL FLUX WITHIN SPECIFIED AREA (REAL*8)
C       ISTAT - INTEGER STATUS
C
C---------------------------------------------------------------------------
        INTEGER NS,NL,NTS,NTL,ISTAT
        DOUBLE PRECISION IMAGE(NS,NL)
        INTEGER AREA(4)
        DOUBLE PRECISION EDGES(4)
        DOUBLE PRECISION CTROID(2)
        DOUBLE PRECISION ECENT(2),CCENT(2),TOT
C
C LOCAL VARIABLES
C
        DOUBLE PRECISION PROFIL(2048)
        INTEGER IL,IS,SS,SL,ES,EL,ISTAT1,ISTAT2,ISTATX
C
C START OFF WITH NO ERROR STATUS
C
        ISTAT=0
C
C PULL OUT AREA TO PROCESS
C SS - STARTING SAMPLE,  ES - ENDING SAMPLE,  SL,EL - SAME FOR LINE DIRECTION
C
        SS=AREA(1)
        ES=AREA(2)
        SL=AREA(3)
        EL=AREA(4)
C
C COMPUTE TOTAL FLUX
C
        TOT=0.0
        DO 10 IL=SL,EL
                DO 10 IS=SS,ES
                        TOT=TOT+IMAGE(IS,IL)
10      CONTINUE
C
C GENERATE PROFILE IN SAMPLE DIRECTION
C
        DO 30 IS=SS,ES
30              PROFIL(IS)=0.0
        DO 40 IL=SL,EL
                DO 40 IS=SS,ES
                        PROFIL(IS)=PROFIL(IS)+IMAGE(IS,IL)
40      CONTINUE
C
C COMPUTE EDGE LOCATIONS AND CENTER
C
        CALL TAEDGE(PROFIL,SS,ES,EDGES(1),ECENT(1),CTROID(1),ISTAT1)
C
C COMPUTE CROSS-CORRELATED CENTER
C
        CALL TACCOR(PROFIL,SS,ES,NTS,CCENT(1),ISTAT2)
        IF ((ISTAT1+ISTAT2) .GT. 0) THEN
                CALL UMSPUT('Error occurred in sample direction',
     *                 STDOUT,0,ISTATX)
                ISTAT=1
        ENDIF
C
C GENERATE PROFILE IN LINE DIRECTION
C
        DO 50 IL=SL,EL
50              PROFIL(IL)=0.0
        DO 60 IL=SL,EL
                DO 60 IS=SS,ES
                        PROFIL(IL)=PROFIL(IL)+IMAGE(IS,IL)
60      CONTINUE
C
C COMPUTE EDGE LOCATIONS AND CENTERS
C
        CALL TAEDGE(PROFIL,SL,EL,EDGES(3),ECENT(2),CTROID(2),ISTAT1)
        CALL TACCOR(PROFIL,SL,EL,NTL,CCENT(2),ISTAT2)
        IF((ISTAT1+ISTAT2).GT.0)THEN
                CALL UMSPUT('Error occured in line direction',
     *                STDOUT,0,ISTATX)
                ISTAT=1
        ENDIF
C
C DONE
C
        RETURN
        END
