C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE CCPRT(WAVE,DIODE,DELTAW,DELTAD,TOTAL1,TOTAL2,N,ISTAT)
*
* Module Number: FOS/HRS utility
*
* Module Name: CCPRT
*
* Keyphrase:
* ----------
*       print wavelength offset results
*
* Description:
* ------------
*       This routine prints the wavelength offset information
*       computed by crossc
*
* Fortran Name: ccprt.for
*
* Keywords of accessed files and tables:
* --------------------------------------
*       none
*
* Subroutines Called:
* -------------------
* CDBS:
*       none
* SDAS:
*       umsput
*
* History:
* --------
* version         date            Author          Description
*    1          4/1/87          D. Lindler      Designed and coded
*    2          9/25/87         D. Lindler      New SDAS standards/io
*----------------------------------------------------------------------
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
*
* INPUT PARAMETERS
*
*       wave - wavelength vector (double precision)
*       diode - diode position vector (double precision)
*       deltaw - wavelength offset vector
*       deltad - diode offset vector (double precision)
*       total1 - total count vector for first spectrum (double precision)
*       total2 - total count vector for second spectrum(double precision)
*       n - size of input vectors (integer)
*
* Output parameter
*
*       istat - error status (integer)
*
        DOUBLE PRECISION WAVE(*),DIODE(*),DELTAW(*),DELTAD(*)
        DOUBLE PRECISION TOTAL1(*),TOTAL2(*)
        INTEGER N,ISTAT
C
C LOCAL VARIABLES
C
        CHARACTER*130   MESS
        INTEGER         I
        CHARACTER*130   TITLE1,TITLE2
C
C  DATA DECLARATIONS
C
        DATA TITLE1
     *   /' Wavelength  Diode     DeltaW    DeltaD      Total counts'/
        DATA TITLE2
     *   /'                                                 1        2'/
C
C----------------------------------------------------------------------
C
C PRINT TITLES
C
        CALL UMSPUT(TITLE1,STDOUT+USRLOG,0,ISTAT)
        IF(ISTAT.NE.0)GO TO 999
        CALL UMSPUT(TITLE2,STDOUT+USRLOG,0,ISTAT)
        IF(ISTAT.NE.0)GO TO 999
C
C LOOP ON BINS
C
        DO 100 I=1,N
                WRITE(MESS,99)WAVE(I),DIODE(I),DELTAW(I),
     *               DELTAD(I),TOTAL1(I),TOTAL2(I)
99              FORMAT(F10.2,F8.2,F10.2,F10.2,E14.4,E12.4)
                CALL UMSPUT(MESS,STDOUT+USRLOG,0,ISTAT)
100     CONTINUE
C
C DONE
C
        ISTAT=0
        GO TO 1000
999     ISTAT=1
1000    RETURN
        END
