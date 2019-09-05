        SUBROUTINE ZOXPHC(PHCFIL,GRAT,APER,MAXG,FLAG,SAMP0,DELS,
     *                          GROUPS,LINE,NG,NS,IDIN,ISTAT)
*
*  Module number:
*
*  Module name: ZOXPHC
*
*  Keyphrase:
*  ----------
*       open and index photochathode granularity file
*
*  Description:
*  ------------
*       This routine opens and indexes the photocathode granularity
*       file.  The index is a table of line position versus group
*       number.
*
*  FORTRAN name: ZOXPHC.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*   PHCFIL                      I       Granularity reference file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zsortr
*  SDAS:
*       ZMSPUT, uimopn, uigl1r, uimgid, uuopgr
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb 89  D. Lindler      Designed and coded
*       1.1     Dec 96  M. De La Pena   Added APER to reference file
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       PHCFIL - reference file name character*64
*       GRAT - grating mode
*       APER - aperture
*       MAXG - maximum number of groups allowed in the file
*
* Output parameters
*
*       flag - Calibration flag.
*       samp0 - starting sample position for each group
*       dels - delta sample position
*       groups - vector of groups, sorted by line number
*       line - line position for each group
*       ng - number of groups
*       ns - length of the groups
*	idin - file I/O id
*       istat - error status (integer)
*-------------------------------------------------------------------------------
        CHARACTER*64 PHCFIL
        CHARACTER*5  GRAT
        CHARACTER*3  APER
        CHARACTER*12 FLAG
        INTEGER ISTAT,NG,MAXG,GROUPS(1),NS,IDIN
        REAL SAMP0,DELS,LINE(1)
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
C LOCAL VARIABLES
C
        CHARACTER*3 RAPER
        CHARACTER*7 FTYPE
        CHARACTER*64 FNAME
        CHARACTER*80 CONTXT
        INTEGER DIMEN(8),NAXIS,DTYPE
C                                    --->file I/O parameters
        CHARACTER*5 GMODE
        REAL DMIN,DMAX
        INTEGER I
        DATA FTYPE/'PHCFILE'/
C------------------------------------------------------------------------------
C
C open file
C
        CALL ZREFOP(PHCFIL,'PHCHFILE','PHC_CORR',FLAG,IDIN,ISTAT)
        IF(FLAG.NE.'PERFORM')THEN
           ISTAT=1
           GO TO 1000
        ENDIF
        IF(ISTAT.NE.0)THEN
           CONTXT='ERROR opening '//FTYPE//' '//PHCFIL
           GO TO 999
        ENDIF
C
C Get file size parameters and verify
C
            CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading '//FTYPE//' '//PHCFIL
                GO TO 999
            ENDIF
            IF((NAXIS.NE.1)) THEN
                CONTXT='ERROR: '//FTYPE//' has '//
     *                  'invalid dimensions '//PHCFIL
                GO TO 999
            ENDIF
            NS = DIMEN(1)
            IF(NS.GT.4800)THEN
              CONTXT='ERROR: max. length of vectors in PHCFILE is 4800'
              GO TO 999
            ENDIF
C
C get aperture from file and verify
C
            CALL UHDGST(IDIN,'APERTURE',RAPER,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='WARNING: APERTURE keyword missing from '//
     *                                  FTYPE
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ELSE
                IF(RAPER.NE.APER)THEN
                    CONTXT='ERROR: APERTURE keyword value in the '//
     *                  FTYPE//' does not match observation'
                    GO TO 999
                ENDIF
            ENDIF
C
C get grating mode from file and verify
C
            CALL UHDGST(IDIN,'GRATING',GMODE,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='WARNING: GRATING keyword missing from '//
     *                                  FTYPE
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              ELSE
                IF(GMODE.NE.GRAT)THEN
                    CONTXT='WARNING: GRATING keyword value in the '//
     *                  FTYPE//' does not match observation'
                    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                ENDIF
            ENDIF
C
C Get number of groups
C
            CALL UHDGSI(IDIN,'GCOUNT',NG,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='Error reading GCOUNT from '//FTYPE
                GO TO 999
            ENDIF
            IF(NG.GT.MAXG)THEN
                WRITE(CONTXT,99)FTYPE,MAXG
99              FORMAT('ERROR: Too many groups in ',A7,
     *                  ' max. allowed=',I5)
                GO TO 999
            ENDIF
C
C Read staring and delta sample position
C
            CALL UHDGSR(IDIN,'SAMPBEG',SAMP0,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='Error reading SAMPBEG from '//FTYPE
                GO TO 999
            ENDIF
            CALL UHDGSR(IDIN,'SAMPOFF',DELS,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='Error reading SAMPOFF from '//FTYPE
                GO TO 999
            ENDIF
C
C Loop on groups and get line positions
C
           DO 100 I=1,NG
                CALL UUOPGR(IDIN,I,DMIN,DMAX,0,ISTAT)
                CALL ZFNAME(PHCFIL,' ',I,0,FNAME)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR moving to new group in '//PHCFIL
                    GO TO 999
                ENDIF
                CALL UHDGSR(IDIN,'LINE_POS',LINE(I),ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading LINE_POS from'//FNAME
                    GO TO 999
                ENDIF
                GROUPS(I)=I
100         CONTINUE
C
C Sort by line position
C
        CALL ZSORTR(NG,LINE,GROUPS)
        ISTAT=0
        GO TO 1000
C
C ERROR SECTION
C
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
