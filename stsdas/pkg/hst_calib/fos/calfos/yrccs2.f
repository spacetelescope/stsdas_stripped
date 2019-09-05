        SUBROUTINE YRCCS2(CCS2,DET,FGWA,FCHNL,NXSTEP,NMAX,MASK,
     *                    PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: YRCCS2
*
*  Keyphrase:
*  ----------
*       read emission line table
*
*  Description:
*  ------------
*       This routine reads the table containing emission line positions
*       and creates a mask of their locations.
*       Rows are selected by detector, fgwa_id, fchnl, and nxsteps
*
*  FORTRAN name: YRCCS2.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS2                    I       table containing CCS2 coeffecients
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
*       1       July 89  D. Lindler      Designed and coded
*	2	Mar 94	 H. Bushouse	 Mods to handle PEDIGREE keywords
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       CCS2 - table name
*       DET  - detector name ('AMBER' or 'BLUE')
*       FGWA - grating mode
*       FCHNL - first channel
*       NXSTEP - number of x-steps
*       NMAX - legnth of mask
*
* Output parameters
*       MASK - mask of emission line regions
*	PEDGRE - CCS2 PEDIGREE keyword
*	DESCRP - CCS2 DESCRIP keyword
*       ISTAT - error status
*
*-----------------------------------------------------------------------------
        INTEGER ISTAT,NXSTEP,FCHNL,NMAX
        CHARACTER*64 CCS2
	CHARACTER*68 PEDGRE, DESCRP
        CHARACTER*5 DET
        CHARACTER*3 FGWA
        INTEGER MASK(NMAX)
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
C     END IRAF77.INC
C
C local variables
C
        INTEGER IDIN,COLIDS(6),ROW,I,NROWS,NFOUND
        CHARACTER*80 CONTXT
        CHARACTER*15 COLNAM(6)
        CHARACTER*5 DET1
        CHARACTER*3 FGWA1
        INTEGER NX1,FC1,POS(2)
        LOGICAL NULL(2)
        DATA COLNAM/'DETECTOR','FGWA_ID','FCHNL','NXSTEPS','LINE_BEG',
     *              'LINE_END'/
C---------------------------------------------------------------------------
C
C Initialize mask to zeros
C
        DO 10 I=1,NMAX
                MASK(I)=0
10      CONTINUE
        NFOUND = 0
C
C Open table
C
        CALL UTTOPN(CCS2,RDONLY,IDIN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening CCS2 table '//CCS2
                GO TO 998
        ENDIF
C
C get PEDIGREE and DESCRIP header keywords
C
        CALL UTHGTT(IDIN,'PEDIGREE',PEDGRE,ISTAT)
        CALL UTHGTT(IDIN,'DESCRIP',DESCRP,ISTAT)
C
C get number of rows
C
        CALL UTPGTI(IDIN,TBNROW,NROWS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading CCS2 table '//CCS2
                GO TO 999
        ENDIF
C
C Get column ids.
C
        CALL UTCFND(IDIN,COLNAM,6,COLIDS,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR locating needed columns in CCS2 table '//
     *                  CCS2
                GO TO 999
        ENDIF
C
C Loop on rows
C
        DO 500 ROW=1,NROWS
C
C check detector
C
                CONTXT='ERROR reading CCS2 table '//CCS2
                CALL UTRGTT(IDIN,COLIDS(1),1,ROW,DET1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(DET1.NE.DET) GO TO 500
C
C Check fgwa_id
C
                CALL UTRGTT(IDIN,COLIDS(2),1,ROW,FGWA1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(FGWA1.NE.FGWA) GO TO 500
C
C Check first channel
C
                CALL UTRGTI(IDIN,COLIDS(3),1,ROW,FC1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(FCHNL.NE.FC1) GO TO 500
C
C Check nxsteps
C
                CALL UTRGTI(IDIN,COLIDS(4),1,ROW,NX1,NULL,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(NXSTEP.NE.NX1) GO TO 500
C
C read limits
C
                CALL UTRGTI(IDIN,COLIDS(5),2,ROW,POS,NULL,ISTAT)
                IF(ISTAT.NE.0) GO TO 999
C
C Check validity
C
                IF((POS(1).GT.POS(2)).OR.(POS(1).LT.1).OR.
     *                          (POS(2).GT.NMAX))THEN
                    WRITE(CONTXT,99)ROW
99                  FORMAT('ERROR: row',I5,' in CCS2 is not valid')
                ENDIF
C
C Write to userlog
C
                IF(NFOUND.EQ.0)THEN
                   CONTXT='Regions of the sky spectrum not smoothed'
                   CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                   CONTXT='    first point   last point'
                   CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                ENDIF
                WRITE(CONTXT,399)POS
399             FORMAT(2I12)
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                NFOUND = NFOUND + 1
                IF(POS(1).LT.0)POS(1)=0
                IF(POS(2).GT.(NMAX-1))POS(2)=NMAX-1
                DO 400 I=POS(1),POS(2)
                        MASK(I+1)=1
400             CONTINUE
500     CONTINUE
        CALL UTTCLO(IDIN,ISTAT)
        ISTAT=0
        GO TO 1000
999     CALL UTTCLO(IDIN,ISTAT)
998     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
