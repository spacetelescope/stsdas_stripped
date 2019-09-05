        SUBROUTINE ZRCCR7(CCR7,GRAT,TNAMES,TCS,JD,ISTAT)
*
*  Module number:
*
*  Module name: ZRCCR7
*
*  Keyphrase:
*  ----------
*       Read table CCR7 (GHRS Thermal constants)
*
*  Description:
*  ------------
*       This routine reads table CCR7 and extracts the parameters
*       for the specified grating.
*
*  FORTRAN name: ZRCCR7.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR7                    I       Thermal constant table
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT, uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       APR 89  D. Lindler      Designed and coded
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCR7 - table name (character*64)
*	GRATING - grating mode (CHARACTER*5)
*
* Output parameters
*
*     tnames - thermistors to use
*              tnames(1)=tobs, tnames(2)=tobs2, tnames(3)=dtobs
*     tcs - thermal scale factors
*          tc(1)=tc, tc(2)=tc2, tc(3)=dtc
*     jd - time cotstants
*          jd(1)=jd0, jd(2)=jd1, jd(3)=dsdt
*     istat - ERROR status (integer)
**************************************************************************
        CHARACTER*64 CCR7
        CHARACTER*5 GRAT
        INTEGER ISTAT
        CHARACTER*8 TNAMES(3)
        DOUBLE PRECISION TCS(3),JD(3)
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
C     END IRAF77.INC
C
C LOCAL VARIABLES -------------------------------------------
C
        INTEGER IDIN,NROWS,COLIDS(10),ROW,I,ISTATS(10)
        CHARACTER*5 GMODE
        CHARACTER*162 CONTXT
        CHARACTER*15 COLNAM(10)
        LOGICAL NULL
        DATA COLNAM/'GRATING','TOBS','TOBS2','DTOBS',
     &       'TC','TC2','DTC','JD0','JD1','DSDT'/
C
C--------------------------------------------------------------------------
C
C Set all values to nulls
C
        DO 12 I = 1, 3
           TNAMES(I) = ' '
           TCS(I) = 0.
           JD(I) = 0.
 12     CONTINUE
C
C Open table
C
        CALL UTTOPN(CCR7,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCR7 table '//CCR7
                GO TO 998
        ENDIF
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCR7 table '//CCR7
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,10,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
           WRITE(CONTXT,23)
 23        FORMAT('Warning: not all columns exist in CCR7 table.')
           CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           WRITE(CONTXT,24)
 24        FORMAT('Some thermal/time motion correction factors ',
     &          'may not be performed.')
           CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ENDIF
C
C Locate row with correct grating
C
        DO 10 ROW=1,NROWS
           IF(COLIDS(1).NE.0)THEN
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,GMODE,NULL,ISTAT)
             ELSE
                ISTAT=1
             ENDIF
             IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCR7 table '//CCR7
                GO TO 999
             ENDIF
             IF(GMODE.EQ.GRAT)GO TO 20
 10       CONTINUE
C
C if we made it here, we did not find a correct row
C
        WRITE(CONTXT,99)GRAT
99      FORMAT('ERROR no row in CCR7 table found for grating ',A5)
        GO TO 999
C
C Read values from the headers.  Note that all values may not
C be present.
C
 20     CONTINUE
        do 120 i = 1, 3
           IF(COLIDS(I+1).NE.0)
     &          CALL UTRGTT(IDIN,COLIDS(I+1),1,ROW,TNAMES(i),
     &          NULL,ISTATS(I))
           IF(COLIDS(I+4).NE.0)
     &          CALL UTRGTD(IDIN,COLIDS(I+4),1,ROW,TCS(I),
     &          NULL,ISTATS(I+3))
           IF(COLIDS(I+7).NE.0)
     &          CALL UTRGTD(IDIN,COLIDS(I+7),1,ROW,JD(I),
     &          NULL,ISTATS(I+6))
 120    CONTINUE
        ISTAT=0
        DO 30 I=1,9
            IF(ISTATS(I).NE.0)THEN
               WRITE(CONTXT,109)COLNAM(I+1)
 109           FORMAT('Warning: could not read column ',a15,
     &              ' from CCR7 table')
               CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTATS(I))
               ISTAT=1
            ENDIF
30      CONTINUE
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
