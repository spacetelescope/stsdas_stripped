      SUBROUTINE ZOXVIG(VIGFIL,GRAT,APER,MAXG,FLAG,SAMP0,DELS,
     *     APERGRP,LINE,VCPOS,NG,NS,UCPOS,NU,IDIN,ISTAT)
*
*  Module number:
*
*  Module name: ZOXVIG
*
*  Keyphrase:
*  ----------
*       open and index vignetting file
*
*  Description:
*  ------------
*       This routine opens and indexes the vignetting
*       file.  The index is a table of line position and
*       carrousel position for each group.
*
*  FORTRAN name: ZOXVIG.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*   VIGFIL                      I       Vignetting reference file
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
*       2       Oct 94  J. Eisenhamer   Added aperture dependency
*       3       May 04  P. Barrett      Work-around for IMIO bug
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       VIGFIL - reference file name character*64
*       GRAT - grating mode
*       APER - aperture
*       MAXG - maximum number of groups allowed in the file
*
* Output parameters
*
*       flag - Calibration flag.
*       samp0 - starting sample position for each group
*       dels - delta sample position
*       apergrp - Groups corresponding to the particular aperture.
*       line - line position for each group
*       vcpos - carrrousel position for each group
*       ng - number of groups
*       ns - length of the groups
*       ucpos - vector of different carrousel position in file
*               sorted into ascending order
*       nu - number of different (unique) car. positions in the file
*       istat - error status (integer)
*-------------------------------------------------------------------------------
      CHARACTER*64 VIGFIL
      CHARACTER*5 GRAT
      CHARACTER*3 APER
      CHARACTER*12 FLAG
      INTEGER ISTAT,NG,MAXG,NS,NU,IDIN
      REAL SAMP0,DELS,LINE(1),VCPOS(1),UCPOS(1)
      INTEGER APERGRP(1)
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
C     LOCAL VARIABLES
C     
      CHARACTER*3 TAPER
      CHARACTER*7 FTYPE
      CHARACTER*64 FNAME
      CHARACTER*160 CONTXT
      INTEGER DIMEN(8),NAXIS,DTYPE
C     --->file I/O parameters
      CHARACTER*5 GMODE
      INTEGER I,J
      REAL C,DMIN,DMAX
      LOGICAL DOAPER
      REAL TMPDAT(4800), VIGDAT(4800,1000)
      COMMON /CALDAT/ VIGDAT
      DATA FTYPE/'VIGFILE'/
C------------------------------------------------------------------------------
C     
C     open file
C     
      CALL ZREFOP(VIGFIL,'VIGHFILE','VIG_CORR',FLAG,IDIN,ISTAT)
      IF(FLAG.NE.'PERFORM')THEN
         ISTAT=1
         GO TO 1000
      ENDIF
      IF(ISTAT.NE.0)THEN
         CONTXT='ERROR opening '//FTYPE//' '//VIGFIL
         GO TO 999
      ENDIF
C     
C     Get file size parameters and verify
C     
      CALL UIMGID(IDIN,DTYPE,NAXIS,DIMEN,ISTAT)
      IF(ISTAT.NE.0)THEN
         CONTXT='ERROR reading '//FTYPE//' '//VIGFIL
         GO TO 999
      ENDIF
      IF((NAXIS.NE.1)) THEN
         CONTXT='ERROR: '//FTYPE//' has '//
     *        'invalid dimensions '//VIGFIL
         GO TO 999
      ENDIF
      NS = DIMEN(1)
      IF(NS.GT.4800)THEN
         CONTXT='ERROR: max. length of vectors in VIGFILE is 4800'
         GO TO 999
      ENDIF
C     
C     get grating mode from file and verify
C     
      CALL UHDGST(IDIN,'GRATING',GMODE,ISTAT)
      IF(ISTAT.NE.0)THEN
         CONTXT='WARNING: GRATING keyword missing from '//
     *        FTYPE
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ELSE
         IF(GMODE.NE.GRAT)THEN
            CONTXT='WARNING: GRATING keyword value in the '//
     *           FTYPE//' does not match observation'
            CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         ENDIF
      ENDIF
C     
C     Get number of groups
C     
      CALL UHDGSI(IDIN,'GCOUNT',NG,ISTAT)
      IF(ISTAT.NE.0)THEN
         CONTXT='Error reading GCOUNT from '//FTYPE
         GO TO 999
      ENDIF
      IF(NG.GT.MAXG)THEN
         WRITE(CONTXT,99)FTYPE,MAXG
 99      FORMAT('ERROR: Too many groups in ',A7,
     *        ' max. allowed=',I5)
         GO TO 999
      ENDIF
C     
C     Read starting and delta sample position
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
C     Loop on groups and get line position and carrousel position
C     Only read in groups that refer to the current aperture.
C     If no data exists for the current aperture, then just
C     read in the data anyways but issue a warning.
C     
      DOAPER=.TRUE.
 120  CONTINUE
      NU = 0
      DO 100 I=1,NG
         CALL ZFNAME(VIGFIL,' ',I,0,FNAME)
         CALL UUOPGR(IDIN,I,DMIN,DMAX,0,ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR changing to new group in '//VIGFIL
            GO TO 999
         ENDIF

         IF(DOAPER)THEN
            CALL UHDGST(IDIN,'APERTURE',TAPER,ISTAT)
            IF(ISTAT.NE.0)THEN
               DOAPER=.FALSE.
            ELSE IF(TAPER.NE.APER)THEN
               GO TO 100
            ENDIF
         ENDIF

         NU = NU+1
         APERGRP(NU) = I
         CALL UHDGSR(IDIN,'LINE_POS',LINE(NU),ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading LINE_POS from'//FNAME
            GO TO 999
         ENDIF
         CALL UHDGSR(IDIN,'CAR_POS',VCPOS(NU),ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading CAR_POS from'//FNAME
            GO TO 999
         ENDIF
C
C The following code can be removded when the bug in the IRAF IMIO
C library is 'eventually' fixed.  The IMIO library returns corrupted
C data when it reads a previous group in a GEIS file.  Hence, as a
C work-around we progressively read all vignetting data into a buffer,
C when the file is opened. We then copy the group data from this buffer
C when needed.  This approach is OK, because the GEIS files are not
C large and it is probably faster than the original implementation.
C
         CALL UIGL1R(IDIN, TMPDAT, ISTAT)
         IF(ISTAT.NE.0)THEN
            CONTXT='ERROR reading '//FTYPE//' '//FNAME
            GO TO 999
         ENDIF
         DO 50 J = 1,NS
            VIGDAT(J,I) = TMPDAT(J)
 50      CONTINUE
 100  CONTINUE
C
C Make sure something was read.
C
      IF(NU.LE.0.AND.DOAPER)THEN
         WRITE(CONTXT,9001)APER
 9001    FORMAT('WARNING no vignetting reference available for ',
     *        'aperture ', a3)
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         WRITE(CONTXT,9002)TAPER
 9002    FORMAT('    using vignetting data for aperture ',a3)
         CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
         DOAPER=.FALSE.
         GO TO 120
      ENDIF
      IF(NU.LE.0)THEN
         WRITE(CONTXT,9000) VIGFIL
 9000    FORMAT ('WARNING no reference data found in vignetting file ',
     *        a64)
         FLAG='OMIT'
         CALL ZFLCON
         GO TO 999
      ENDIF
C     
C     Find unique carrousel positions
C     
      NG = NU
      UCPOS(1)=VCPOS(1)
      NU=1
      IF(NG.GT.1)THEN
         DO 300 I=1,NG
            C=VCPOS(I)
            DO 200 J=1,NU
               IF(C.EQ.UCPOS(J))GO TO 300
 200        CONTINUE
            NU=NU+1
            UCPOS(NU)=C
 300     CONTINUE
      ENDIF
C     
C     sort unique carrousel positions
C     
      CALL ZCLSRT(NU,UCPOS)
      ISTAT=0
      GO TO 1000
C     
C     ERROR SECTION
C     
 999  CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
      ISTAT=1
 1000 RETURN
      END
