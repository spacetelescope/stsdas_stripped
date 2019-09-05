         SUBROUTINE ZRCCR3(CCR3,DET,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCR3
*
*  Keyphrase:
*  ----------
*       Read table ccr3 (GHRS detector parameters)
*
*  Description:
*  ------------
*       This routine reads table CCR3 and extracts the parameters
*       for the specified detector.
*
*  FORTRAN name: zrccr3
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR3                    I       detector parameter table
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT, uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       ccr3 - table name (character*64)
*       det - detector number (integer)
*
* Output parameters
*
*       istat - ERROR status (integer)
**************************************************************************
        CHARACTER*64 CCR3
        INTEGER DET
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C     THIS SECTION IS FOR PARAMETERS RELEVANT TO TABLE I/O.
C
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C
C                       /CCR3PR/
C
C Common block for HRS parameters from table ccr3
C
        INTEGER S0,C1,C2,SKYMNF,SKYMDF,INTMNF,INTMDF,SKYORD,INTORD
        REAL C,MINDIO,MINPHC,MINECH,MINABS,DELTAT,RATIO,PERIOD
        COMMON /CCR3PR/ PERIOD,S0,C,C1,C2,SKYMNF,SKYMDF,SKYORD,
     *                  INTMNF,INTMDF,INTORD,MINDIO,MINPHC,MINECH,
     *                  MINABS,DELTAT,RATIO
C
C	period - orbital period in minutes
C       s0 - sample position center of the photocathode (integer)
C       c - conversion factor from sample units to millimeters (real)
C       c1 - diode offset to center of left background diodes (integer)
C       c2 - diode offset to center of right background diodes (integer)
C       skymnf - mean filter for sky spectra (integer)
C       skymdf - median filter for sky spectra (integer)
C       skyord - order of polynomial to fit to sky spectra (integer)
C       intmnf - mean filter for interorder spectra (integer)
C       intmdf - median filter for interorder spectra (integer)
C       intord - order of polynomial to fit to interorder spectra (integer)
C       mindio - minimum diode response to use (real)
C       minphc - minimum photocathode response to use (real)
C       minech - minimum echelle ripple value to use (real)
C       minabs - minimum abs. sens. value to use (real)
C       deltat - time increment for updating doppler comp. positions
C               in 125 millisec. units (real)
C       ratio - ratio of LSA size/SSA size
C
C LOCAL VARIABLES -------------------------------------------
C
        INTEGER IDIN,NROWS,COLIDS(18),ROW,IDET,I,ISTATS(18)
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(18)
        LOGICAL NULL
        DATA COLNAM/'DETECTOR','S0','C','C1','C2',
     *                  'SKY_MNFWIDTH','SKY_MDFWIDTH','SKY_ORDER',
     *			'INT_MNFWIDTH','INT_MDFWIDTH','INT_ORDER',
     *			'MIN_DIO','MIN_PHC','MIN_ECH',
     *                  'MIN_ABS','DELTA_T','AP_RATIO','PERIOD'/
C
C--------------------------------------------------------------------------
C
C Open table
C
        CALL UTTOPN(CCR3,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening ccr3 table '//CCR3
                GO TO 998
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading ccr3 table '//CCR3
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,18,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in ccr3 table '//
     *                  CCR3
                GO TO 999
        ENDIF
C
C Locate row with correct detector number
C
        DO 10 ROW=1,NROWS
                CALL UTRGTI(IDIN,COLIDS(1),1,ROW,IDET,NULL,ISTAT)
                IF(ISTAT.NE.0)THEN
                        CONTXT='ERROR reading ccr3 table '//CCR3
                        GO TO 999
                ENDIF
                IF(DET.EQ.IDET)GO TO 20
10      CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)DET
99      FORMAT('ERROR no row in ccr3 table found for detector ',I2)
        GO TO 999
C
C read values
C
20      CALL UTRGTI(IDIN,COLIDS(2),1,ROW,S0,NULL,ISTATS(1))
        CALL UTRGTR(IDIN,COLIDS(3),1,ROW,C,NULL,ISTATS(2))
        CALL UTRGTI(IDIN,COLIDS(4),1,ROW,C1,NULL,ISTATS(3))
        CALL UTRGTI(IDIN,COLIDS(5),1,ROW,C2,NULL,ISTATS(4))
        CALL UTRGTI(IDIN,COLIDS(6),1,ROW,SKYMNF,NULL,ISTATS(5))
        CALL UTRGTI(IDIN,COLIDS(7),1,ROW,SKYMDF,NULL,ISTATS(6))
        CALL UTRGTI(IDIN,COLIDS(8),1,ROW,SKYORD,NULL,ISTATS(7))
        CALL UTRGTI(IDIN,COLIDS(9),1,ROW,INTMNF,NULL,ISTATS(8))
        CALL UTRGTI(IDIN,COLIDS(10),1,ROW,INTMDF,NULL,ISTATS(9))
        CALL UTRGTI(IDIN,COLIDS(11),1,ROW,INTORD,NULL,ISTATS(10))
        CALL UTRGTR(IDIN,COLIDS(12),1,ROW,MINDIO,NULL,ISTATS(11))
        CALL UTRGTR(IDIN,COLIDS(13),1,ROW,MINPHC,NULL,ISTATS(12))
        CALL UTRGTR(IDIN,COLIDS(14),1,ROW,MINECH,NULL,ISTATS(13))
        CALL UTRGTR(IDIN,COLIDS(15),1,ROW,MINABS,NULL,ISTATS(14))
        CALL UTRGTR(IDIN,COLIDS(16),1,ROW,DELTAT,NULL,ISTATS(15))
        CALL UTRGTR(IDIN,COLIDS(17),1,ROW,RATIO,NULL,ISTATS(16))
        CALL UTRGTR(IDIN,COLIDS(18),1,ROW,PERIOD,NULL,ISTATS(17))
        DO 30 I=1,17
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading ccr3 table '//CCR3
                GO TO 999
            ENDIF
30      CONTINUE
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
