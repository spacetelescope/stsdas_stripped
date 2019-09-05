        SUBROUTINE TACCOR(PROFIL,N1,N2,NT,CENT,ISTAT)
*
*  Module number: FOS/HRS UTILITY
*
*  Module name: taccor
*
*  Keyphrase:
*  ----------
*       Cross correlation
*
*  Description:
*  ------------
*       This routine finds the position of a peak in the input
*       one-dimensional profile located between specified data
*       points.  Cross correlation with a template of specified
*       lines is used to find the center.  Quadratic refinement
*       is used to localize the center to a fraction of a pixel.
*
*  FORTRAN name: taccor.for
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
*       1       9/10/87    D. Lindler   Designed and coded
*-------------------------------------------------------------------------------
C
C INPUT PARAMETERS
C
C       PROFILE - VECTOR GIVING PROFILE OF FEATURE (REAL*8)
C       N1,N2 - INTEGERS SPECIFIING REGION TO SEARCH FOR THE FEATURE
C       NT - INTEGER GIVING WIDTH OF THE CROSS-CORRELATION TEMPLATE
C
C OUTPUT PARAMETERS
C
C       CENT - CENTER OF THE FEATURE (REAL*8)
C       ISTAT - INTEGER STATUS (NON-ZERO IF FEATURE NOT FOUND)
C
C-------------------------------------------------------------------------------
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
        DOUBLE PRECISION PROFIL(*),CENT
        INTEGER N1,N2,NT,ISTAT
        CHARACTER*130 MESS
C
C LOCAL VARIABLES
C
        DOUBLE PRECISION CMAT(2048),C1,C2,C3,PMAX,CMIN,SUMSQ
        INTEGER I1,I2,I,II,NPOINT,NT2,TSTART,TSTOP,IPOS
C
C INITIALIZATION
C
        ISTAT = 0
        CENT = 0.0
C
C MAKE SURE TEMPLATE WIDTH IS ODD
C
        IF( (NT/2*2) .EQ. NT) NT = NT+1
C
C DETERMINE IF THERE IS TEMPLATE IS SMALL ENOUGH TO USE
C
        NPOINT = (N2-N1)+1
        IF(NPOINT.LT.(NT+2)) THEN
          MESS= 'CROSS CORRELATION TEMPLATE IS TOO LARGE FOR THE REGION'
          GO TO 999
        ENDIF
C
C COMPUTE REGION TO CORRELATE
C
        NT2 = NT/2
        I1 = N1+NT2
        I2 = N2-NT2
C
C COMPUTE MAXIMUM OF REGION
C
        PMAX = -1.0E-20
        DO 10 I = N1,N2
                IF(PROFIL(I).GT.PMAX)PMAX = PROFIL(I)
10      CONTINUE
C
C LOOP ON POINTS BETWEEN I1 AND I2
C
        DO 100 II = I1,I2
C
C COMPUTE SUM OF SQUARES BETWEEN DATA AND PMAX
C
                TSTART = II-NT2
                TSTOP = TSTART+NT-1
                SUMSQ = 0.0
                DO 50 I = TSTART,TSTOP
                        SUMSQ = SUMSQ+(PMAX-PROFIL(I))**2
50              CONTINUE
C
C SAVE IN CORRELATION MATRIX
C
                CMAT(II) = SUMSQ
100     CONTINUE
C
C FIND MINIMUM OF CORRELATION MATRIX AND ITS POSITION
C
        CMIN = CMAT(I1)
        IPOS = I1
        DO 200 II = I1,I2
                IF(CMAT(II).LT.CMIN)THEN
                        IPOS = II
                        CMIN = CMAT(II)
                ENDIF
200     CONTINUE
C
C IF IT IS ON EDGE OF CORRELATED REGION WE CAN NOT DO QUADRATIC REFINEMENT
C
        IF((IPOS.EQ.I1).OR.(IPOS.EQ.I2))THEN
           MESS='MINIMUM OF CORRELATION MATRIX AT THE IMAGE EDGE'
           GO TO 999
        ENDIF
C
C USE QUADRATIC REFINEMENT TO COMPUTE EXACT LOCATION
C
           C1 = CMAT(IPOS-1)
           C2 = CMAT(IPOS)
           C3 = CMAT(IPOS+1)
           IF((C1.EQ.C2).AND.(C1.EQ.C3))THEN
                MESS='MINIMUM OF THE CORRELATION MATRIX IS TOO FLAT'
                GO TO 999
           ENDIF
           CENT = (C1-C2)/(C1+C3-2*C2)-0.5 + IPOS
        GO TO 1000
999     CALL UMSPUT(MESS,STDOUT+STDERR+USRLOG,0,ISTAT)
        CALL UMSPUT('UNABLE TO COMPUTE CROSS CORRELATION CENTER',
     *               STDOUT+STDERR+USRLOG,0,ISTAT)
        ISTAT = 1
1000    RETURN
        END
