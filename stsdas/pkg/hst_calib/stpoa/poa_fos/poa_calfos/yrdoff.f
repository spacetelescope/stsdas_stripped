C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YRDOFF(FRAME,OFFS,TOTFRM,XOFFS,YOFFS,N,ISTAT)
*
*  Module number:
*
*  Module name: YRDOFF
*
*  Keyphrase:
*  ----------
*       read gimp correction offsets table
*
*  Description:
*  ------------
*       This routine reads the table containing offsets for
*       post-pipeline reprocessing of the the gimp correction.
*
*  FORTRAN name: YROFFS.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       OFFS_TAB                I       table containing gimp offsets
*  Subroutines Called:
*  -------------------
*  CDBS
*       ymsput
*  SDAS:
*       uttopn, utpgti, utcfnd, utrgt*, uttclo
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91  S. Hulbert      Designed and coded
*     1.1       May 91  S. Hulbert      Added y-offset
*-------------------------------------------------------------------------------
*
* Input parameters
*
*	frame - readout number
*       offs - table name
*	totfrm - total number of frames
*       n - number of rows to read = number of spectra in a frame
*
* Output parameters
*
*       xoffs - array of x offsets
*       istat - error status
*
*-----------------------------------------------------------------------------
        INTEGER FRAME, N, ISTAT, TOTFRM
        CHARACTER*64 OFFS
        REAL XOFFS(N), YOFFS(N)
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
C
C Common block containing input/output file descriptions
C
C       IDS - file id numbers
C       GCOUNT - group counts
C       NAXIS - naxis
C       NAXIS1 - first dimensions
C       NAXIS2 - second dimensions
C       FILL - Fill values
C
        INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
        REAL FILL(30)
        COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL
C
C local variables
C
        INTEGER IDIN,COLID(2),ROW,NROWS,FIRST,LAST
        INTEGER ROWCNT, ISTATS(2)
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(2)
        LOGICAL NULL
        DATA COLNAM/'X_OFFSET', 'Y_OFFSET'/
C
C---------------------------------------------------------------------------
        IF (FRAME .EQ. 1) THEN
C
C Open table
C
            CALL UTTOPN(OFFS,RDONLY,IDIN,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening OFFS_TAB table '//OFFS
                GO TO 998
            ENDIF
C
C get number of rows
C
            CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading OFFS_TAB table '//OFFS
                GO TO 999
            ENDIF
            IF (NROWS .NE. TOTFRM*N) THEN
                CONTXT='ERROR insufficient rows in OFFS_TAB table '//
     $                        OFFS
                GO TO 999
            ENDIF
C
C Get column ids.
C
            CALL UTCFND(IDIN,COLNAM,2,COLID,ISTAT)
2           IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in '//
     $                        'OFFS_TAB table '//OFFS
                GO TO 999
            ENDIF
        ENDIF
C
C extract offsets
C
        FIRST = (FRAME - 1) * N + 1
        LAST = FIRST + N - 1
        ROWCNT = 0
        DO 10 ROW=FIRST,LAST
            ROWCNT = ROWCNT + 1
            CALL UTRGTR(IDIN,COLID(1),1,ROW,XOFFS(ROWCNT),
     $                NULL,ISTATS(1))
            CALL UTRGTR(IDIN,COLID(2),1,ROW,YOFFS(ROWCNT),
     $                NULL,ISTATS(2))
            IF(ISTATS(1).NE.0.OR.ISTATS(2).NE.0) THEN
                    CONTXT='ERROR reading row '//
     $                        ' from OFFS_TAB table '//OFFS
                    GO TO 999
            ENDIF
10      CONTINUE
        IF (ROWCNT .NE. N) THEN
                CONTXT='ERROR did not get needed rows from '//
     $                   'OFFS_TAB table '//OFFS
                GO TO 999
        ENDIF
        IF (FRAME .EQ. TOTFRM) THEN
            CALL UTTCLO(IDIN,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR closing OFFS_TAB table '//OFFS
                GO TO 998
            ENDIF
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
