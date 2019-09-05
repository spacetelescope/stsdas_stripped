        SUBROUTINE YDCOFF(GRAT,WAVE,DELW,DIODE,DELD,N,ORDER,DC,
     *                  DCOUT,STATUS)
*
* Module Number: 14.8.4.1
*
* Module Name: ydcoff
*
* Keyphrase:
* ----------
*       Add offset to dispersion coefficients
*
* Description:
* ------------
*       Using the input offset table giving offset as a function of
*       Wavelength.  Mocifications to the Dispersion coeff. are made
*       to include the offset.  The offset can be constant, or a linear
*       or quadratic function of wavelength.  For the prism, only a constant
*       sample offset is allowed.
*
* Fortran Name: ydcoff
*
* Keywords of accessed files and tables:
* --------------------------------------
*       none
*
* Subroutines Called:
* -------------------
* CDBS:
*       polyft
* SDAS:
*       umsput
*
* History:
* --------
* version         date            Author          Description
*    1          4/1/87          D. Lindler      Designed and coded
*    2          Jan 88          D. Lindler      New sdas i/o and standards
*-------------------------------------------------------------------------
*
* INPUT PARAMETERS
*
*       grat - grating mode (char*3)
*       wave - wavelength vector (real*8)
*       delw - wavelength offsets (real*8)
*       diode - diode position vector (real*8)
*       deld - diode offsets (real*8)
*       n - number of points in wave, delw, diode, deld (integer)
*       order - order of the offset polynomial (integer)
*       dc - input dispersion coeff. (real*8)
*
* Output parameters
*
*       dcout - output dispersion coef. (real*8)
*       status - output error status (integer)
*
C-----------------------------------------------------------------------
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
        DOUBLE PRECISION WAVE(*),DELW(*),DIODE(*),DELD(*),DC(*),DCOUT(*)
        INTEGER N,ORDER
        CHARACTER*3     GRAT
        INTEGER         STATUS
C
C LOCAL VARIABLES
C
        DOUBLE PRECISION SUM,AVE
        INTEGER I,ISTAT
        CHARACTER*130   CONTXT,MESS
        DOUBLE PRECISION FIT(100),DIFF(100),DIFW(100),DWDD,XX,COEF(4)
        DOUBLE PRECISION SIGMAS(100)
        CHARACTER*130   T1,T2
C
C  DATA DECLARATIONS
C
        DATA SIGMAS/100*1.0/
        DATA T1
     *  /' Wavelength   Diode  ---- offsets ---   -residuals of fit-'/
        DATA T2
     *  /'                     Wavelength diodes   wavelength  diodes'/
C
C-----------------------------------------------------------------------
C
C COPY OLD DISPERSION COEF. TO OUTPUT
C
        DO 5 I=1,6
                DCOUT(I)=DC(I)
5       CONTINUE
C
C IF PRISM COMPUTE CONSTANT SAMPLE OFFSET ----------------------
C
        IF(GRAT.EQ.'PRI')THEN
                SUM=0.0
                DO 10 I=1,N
                        SUM=SUM+DELD(I)
10              CONTINUE
                AVE=SUM/N
                DCOUT(6)=DC(6)+AVE
C
C COMPUTE RESIDUALS
C
                DO 20 I=1,N
                        DIFF(I)=DELD(I)-AVE
20              CONTINUE
C
C CONVERT TO WAVELENGTH UNITS BY MULTIPLIING BY DIREVATIVE
C OF WAVELENGTH W.R.T. DIODE
C
                DO 25 I=1,N
                        XX=(DIODE(I)-DC(6))
                        DWDD=DC(2)/XX**2 + 2*DC(3)/XX**3 + 3*DC(4)/XX**4 +
     *                                  4*DC(5)/XX**5
                        DIFW(I)=-DIFF(I)*DWDD
25              CONTINUE
C
C ELSE GRATING MODE, FIT POLYNOMIAL ----------------------------
C
            ELSE
                IF(ORDER.EQ.0)THEN
C
C CONSTANT WAVELENGTH OFFSET
C
                        SUM=0.0
                        DO 30 I=1,N
                                SUM=SUM+DELW(I)
30                      CONTINUE
                        AVE=SUM/N
                        DCOUT(1)=DC(1)-AVE
                        DO 40 I=1,N
                                DIFW(I)=DELW(I)-AVE
40                      CONTINUE
                    ELSE
C
C FIT POLYNOMIAL
C
                        CALL POLYFT(DIODE,DELW,SIGMAS,N,ORDER,COEF,
     *                                          FIT,STATUS)
                        IF(STATUS.NE.0)THEN
                           CONTXT='Unable to fit wavelength offsets'
                           GO TO 999
                        ENDIF
                        DO 50 I=1,N
                                DIFW(I)=DELW(I)-FIT(I)
50                      CONTINUE
C
C UPDATE DISPERSION COEF.
C
                        DO 55 I=1,ORDER+1
                                DCOUT(I)=DC(I)-COEF(I)
55                      CONTINUE
               ENDIF
C
C CONVERT DIFW TO DIODES BY DIVIDING BY THE DIREVATIVE OF WAVE W.R.T. DIODE
C
               DO 60 I=1,N
                    DWDD=DC(2)+2*DC(3)*WAVE(I)+3*DC(4)*
     *                            WAVE(I)*WAVE(I)
                    DIFF(I)=DIFW(I)/DWDD
60             CONTINUE
        ENDIF
C
C WRITE RESULTS TO TERMINAL
C
        CALL UMSPUT(T1,STDOUT,0,STATUS)
        CALL UMSPUT(T2,STDOUT,0,STATUS)
        DO 100 I=1,N
                WRITE(MESS,99)WAVE(I),DIODE(I),DELW(I),DELD(I),
     *                                  DIFW(I),DIFF(I)
99              FORMAT(F10.1,F9.2,F10.2,F9.2,F10.2,F9.3)
                CALL UMSPUT(MESS,STDOUT,0,STATUS)
100     CONTINUE
C
C DONE
C
        STATUS=0
        GO TO 1000
999     STATUS=1
1000    CALL UMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        RETURN
        END
