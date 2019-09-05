        SUBROUTINE ZRDABS(FTYPE,ABSFIL,GRAT,APER,NMAX,
     *     FLAG,VALUES,NOUT,ISTAT)
*
*  Module number:
*
*  Module name: ZRDABS
*
*  Keyphrase:
*  ----------
*       read sensitivity files
*
*  Description:
*  ------------
*       This routine reads the sensitivity file for the specfied
*       aperture.
*
*  FORTRAN name: ZRDABS.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*   ABSFIL                      I       Sensitivity reference file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       ZMSPUT, uimopn, uigl1r, uimgid, uimclo
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb 89  D. Lindler      Designed and coded
*		1Jan92	S. Hulbert	Changed declaration of FTYPE to match
*					calling sequence from zclabs
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       FTYPE - File type ABSFILE or NETFILE
*       ABSFIL - reference file name character*64
*       GRAT - grating mode
*       APER - 'Z1' == 'SSA'
*              'Z2' == 'LSA'
*       NMAX - maximum vector length
*
* Output parameters
*
*       FLAG - calibration flag.
*       NOUT - number of output points
*       VALUES - output vector (real)
*       istat - error status (integer)
*-------------------------------------------------------------------------------
        CHARACTER*6 FTYPE
        CHARACTER*64 ABSFIL
        CHARACTER*5 GRAT
        CHARACTER*3 APER
        CHARACTER*12 FLAG
        INTEGER ISTAT,NMAX,NOUT
        REAL VALUES(1)
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
        CHARACTER*80 CONTXT
        INTEGER IDIN,DIMEN(8),NAXIS,DTYPE
C                                    --->file I/O parameters
        CHARACTER*3 AP
        CHARACTER*5 GMODE
        CHARACTER*64 FNAME
        INTEGER I,GCOUNT
C------------------------------------------------------------------------------
C
C open file
C
            CALL ZFNAME(ABSFIL,' ',1,0,FNAME)
            IF(FTYPE.EQ.'absfil')
     &           CALL ZREFOP(ABSFIL,'ABSHFILE','FLX_CORR',
     &           FLAG,IDIN,ISTAT)
            IF(FTYPE.EQ.'netfil')
     &           CALL ZREFOP(ABSFIL,'NETHFILE','FLX_CORR',
     &           FLAG,IDIN,ISTAT)
            IF(FLAG.NE.'PERFORM')THEN
               ISTAT=1
               GO TO 1000
            ENDIF
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening '//FTYPE//' '//ABSFIL
                GO TO 999
            ENDIF
C
C Get file size parameters and verify
C
            CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading '//FTYPE//' '//ABSFIL
                GO TO 998
            ENDIF
            IF((NAXIS.NE.1)) THEN
                CONTXT='ERROR: '//FTYPE//' has '//
     *                  'invalid dimensions '//ABSFIL
                GO TO 998
            ENDIF
            IF((DIMEN(1).GT.NMAX))THEN
                WRITE(CONTXT,99)FTYPE,NMAX
99              FORMAT('ERROR: max. vector length for ',A7,' is',I5)
                CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
            NOUT = DIMEN(1)
C
C get grating mdoe from file and verify
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
            CALL UHDGSI(IDIN,'GCOUNT',GCOUNT,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='Error reading GCOUNT from '//FTYPE
                GO TO 999
            ENDIF
C
C Loop on groups and locate the one for the proper aperture
C
           DO 100 I=1,GCOUNT
                CALL UIMCLO(IDIN,ISTAT)
                CALL ZFNAME(ABSFIL,' ',I,0,FNAME)
                CALL UIMOPN(FNAME,RDONLY,IDIN,ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR opening '//FTYPE//' '//FNAME
                    GO TO 999
                ENDIF
                CALL UHDGST(IDIN,'APERTURE',AP,ISTAT)
                IF(ISTAT.NE.0)THEN
                    CONTXT='ERROR reading aperture from'//FNAME
                    GO TO 999
                ENDIF
                IF (AP. EQ. 'Z1') AP = 'SSA'
                IF (AP. EQ. 'Z2') AP = 'LSA'
                IF(AP.EQ.APER)GO TO 200
C                                    --->did we find it.
100         CONTINUE
C
C If we made it here, we did not find the aperture in the file
C
           WRITE(CONTXT,199)APER,FTYPE
199        FORMAT('ERROR: Aperture ',A3,' not found in ',A7)
C
C Read values
C
200         CALL UIGL1R(IDIN,VALUES,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading '//FTYPE//' '//FNAME
                GO TO 998
            ENDIF
            CALL UIMCLO(IDIN,ISTAT)
            ISTAT=0
            GO TO 1000
C
C ERROR SECTION
C
998     CALL UIMCLO(IDIN,ISTAT)
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
