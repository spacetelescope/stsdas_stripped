C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE WRTWAVTAB(WAVTAB,PIXTAB,ISTAT)
*
*  Module number:
*
*  Module name: WRTWAVTAB
*
*  Keyphrase:
*  ----------
*       write the output wave table file
*
*  Description:
*  ------------
*       This routine creates the output wave table; output table
*       has the new pixel position (sub_pix correction) along with
*       the calculated wavelength for all the positions.
*
*  FORTRAN name: wrtwavtab.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  
*
*  Subroutines Called:
*  -------------------
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Jun 01  A. Alexov        Designed and coded
*       2       Oct 01  A. Alexov        Added 'new' sub-pix positions,
*                                        CONTXT declaration
*-------------------------------------------------------------------------------
* inputs:
*       PIXTAB - name of the pixel table file
*       WAVTAB - name of the wave output table file
*
* outputs:
*       ISTAT - error status
*
        CHARACTER*64 PIXTAB, WAVTAB
        INTEGER ISTAT
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
      CHARACTER*80 CONTXT

c
c the PIXTAB related global/common vars
      REAL*8 PIXPOS(3000)
      REAL*4 WAVPOS(3000)
      INTEGER PIXPOS_TOTNUM
      COMMON /PIX_POS/ PIXPOS, WAVPOS, PIXPOS_TOTNUM
C
C FILE I/O ACCESS MODES
C
        INTEGER   RDONLY
        PARAMETER (RDONLY = 1)

      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
      INTEGER   TYREAL,TYDOUB,TYPES(2)
      PARAMETER (TYDOUB = 7)
      PARAMETER (TYREAL = 6)
      INTEGER IDIN,COLIDS(2)
      CHARACTER*6 COLNAM(2)
      CHARACTER*7 FORMATS(2)
      CHARACTER*8 WAVUNIT(2)
      DATA COLNAM/'WAVPOS','NEWPIX'/
      DATA WAVUNIT/'Angstrom',''/
      DATA TYPES/TYREAL,TYDOUB/
      DATA FORMATS/'F12.5','F12.5'/

C---------------------------------------------------------------------------
C
C Create wav table
C
        CALL UTTINN(WAVTAB,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR creating wav table '//WAVTAB
                GO TO 998
        ENDIF

C Create new "wavpos" column in wav table
        CALL UTCDEF(IDIN,COLNAM,WAVUNIT,FORMATS,TYPES,2,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR creating new col in wav table '//WAVTAB
                GO TO 998
        ENDIF

C Open the wav table
        CALL UTTCRE(IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening wav table '//WAVTAB
                GO TO 998
        ENDIF

C Put the wave position data into the wavpos column
        CALL UTCPTR(IDIN,COLIDS(1),1,PIXPOS_TOTNUM,WAVPOS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR writing wavpos column to table '//WAVTAB
                GO TO 998
        ENDIF

C Put the new pixel position data into the newpix column
        CALL UTCPTD(IDIN,COLIDS(2),1,PIXPOS_TOTNUM,PIXPOS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR writing wavpos column to table '//WAVTAB
                GO TO 998
        ENDIF

C close the wav table
        CALL UTTCLO(IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR closing wav table '//WAVTAB
                GO TO 999
        ENDIF
        ISTAT=0
        GO TO 1000
C
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END

