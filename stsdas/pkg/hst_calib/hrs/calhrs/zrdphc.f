        SUBROUTINE ZRDPHC(IDIN,FTYPE,PHCFIL,LINE,NG,LINES,GROUPS,NS,
     *                  PRESP,ISTAT)
*
*  Module number:
*
*  Module name: ZRDPHC
*
*  Keyphrase:
*  ----------
*       Read photocathode granularity file
*
*  Description:
*  ------------
*       This routine reads the group of the photocathode granularity
*       file for the given observation line position.  If the line
*       position is not in the file, Linear interpolation between
*       line positions in the file is used.
*
*  FORTRAN name: zrdphc
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       PHCFIL                  I       Photocathode granularity file
*                                       or vignetting file
*
*  Subroutines Called:
*  -------------------
*  SDAS:
*       ZMSPUT, uimopn, uuopgr, uigl1r
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*
*       2       04/05/28   P.Barrett    Work-around for IRAF IMIO bug.
*-------------------------------------------------------------------------------
*
* INPUTS:
*	idin - file ID number
*       ftype - file type PHCFILE or VIGFILE
*       phcfil - name of the reference file
*       line - photocathode line position for which a response is
*               needed
*       ng - number of groups in phcfil
*       lines - line positions in phcfil
*       groups - group numbers for lines
*       ns - number of samples in each group
*
* OUTPUT:
*
*       presp - photocathode response for given line position
*       istat - error status
*
*----------------------------------------------------------------------------
      CHARACTER*64 PHCFIL
      CHARACTER*7 FTYPE
      REAL LINE,LINES(1),PRESP(1)
      INTEGER NG,GROUPS(1),NS,ISTAT,IDIN
      REAL VIGDAT(4800,1000)
      COMMON /CALDAT/ VIGDAT
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
C Local variables
C
        REAL R1(4800),R2(4800)
C                                    --->response vectors for interpolation
        INTEGER I1,I2
C                                    --->groups to interpolate between
        REAL FRAC
C       REAL DMIN,DMAX
        INTEGER I,K
C        CHARACTER*64 FNAME
        CHARACTER*80 CONTXT
C
C----------------------------------------------------------------------------
C
C Determine which groups to interpolate
C
        I1=0
        I2=0
        DO 100 I=1,NG
                K = NG - I + 1
C                                    --->Reverse order
                IF(LINES(I).LE.LINE) I1=I
                IF(LINES(K).GT.LINE) I2=K
100     CONTINUE
C
C don't extrapolate, use closest group
C
        IF(I1.EQ.0)I1=I2
        IF(I2.EQ.0)I2=I1
C
C exact match ?
C
        IF(LINES(I1).EQ.LINE)I2=I1
C
C if I1 equals I2 just return the group
C
        IF(I1.EQ.I2)THEN
C
C The following code can be uncommented when the bug in the IRAF IMIO
C library is 'eventually' fixed.  The IMIO library returns corrupted
C data when it reads a previous group in a GEIS file.  Hence, as a
C work-around we read all vignetting data into a buffer, when the file
C is open by progressively reading through the file. We then copy the
C group data from this buffer when needed.  This approach is OK,
C because the GEIS files are not large and is probably faster than the
C original implementation.
C
C               CALL UUOPGR(IDIN,GROUPS(I1),DMIN,DMAX,0,ISTAT)
C               CALL ZFNAME(PHCFIL,' ',GROUPS(I1),0,FNAME)
C               IF(ISTAT.NE.0)THEN
C                       CONTXT='ERROR moving to new group in '//PHCFIL
C                       GO TO 999
C               ENDIF
C               CALL UIGL1R(IDIN,PRESP,ISTAT)
C               IF(ISTAT.NE.0)THEN
C                       CONTXT='ERROR reading '//FTYPE//' '//FNAME
C                       GO TO 999
C               ENDIF
                 DO 200 I=1,NS
                    PRESP(I) = VIGDAT(I,GROUPS(I1))
  200            CONTINUE
          ELSE
C
C Need to interpolate between two groups, read both of them
C
C               CALL UUOPGR(IDIN,GROUPS(I1),DMIN,DMAX,0,ISTAT)
C               CALL ZFNAME(PHCFIL,' ',GROUPS(I1),0,FNAME)
C               IF(ISTAT.NE.0)THEN
C                       CONTXT='ERROR moving to new group in '//PHCFIL
C                       GO TO 999
C               ENDIF
C               CALL UIGL1R(IDIN,R1,ISTAT)
C               IF(ISTAT.NE.0)THEN
C                       CONTXT='ERROR reading '//FTYPE//' '//FNAME
C                       GO TO 999
C               ENDIF
                DO 210 I=1,NS
                   R1(I) = VIGDAT(I,GROUPS(I1))
 210            CONTINUE
C               CALL UUOPGR(IDIN,GROUPS(I2),DMIN,DMAX,0,ISTAT)
C               CALL ZFNAME(PHCFIL,' ',GROUPS(I2),0,FNAME)
C               IF(ISTAT.NE.0)THEN
C                       CONTXT='ERROR moving to new group in '//PHCFIL
C                       GO TO 999
C               ENDIF
C               CALL UIGL1R(IDIN,R2,ISTAT)
C               IF(ISTAT.NE.0)THEN
C                       CONTXT='ERROR reading '//FTYPE//' '//FNAME
C                       GO TO 999
C               ENDIF
                DO 220 I=1,NS
                   R2(I) = VIGDAT(I,GROUPS(I2))
 220            CONTINUE
C
C Interpolate
C
                FRAC = (LINE-LINES(I1)) / (LINES(I2)-LINES(I1))
                DO 300 I=1,NS
                        PRESP(I) = R1(I) + FRAC * (R2(I)-R1(I))
300             CONTINUE
        ENDIF
        ISTAT=0
        GO TO 1000
C
C ERROR section
C
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
