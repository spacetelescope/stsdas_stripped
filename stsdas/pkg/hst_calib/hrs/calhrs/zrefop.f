        SUBROUTINE ZREFOP(RFILE,FINAM,FLNAM,FLAG,ID,ISTAT)
*
*  Module number:
*
*  Module name: ZREFOP
*
*  Keyphrase:
*  ----------
*     Open a reference file.
*
*  Description:
*  ------------
*     Open a reference file.  Check for the PEDIGREE and DESCRIP keywords.
*     If PEDIGREE is 'DUMMY', then cancel the current calibration step.
*
*  FORTRAN name: zcldqi.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*     Any reference file.
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  SDAS:
*       ZMSPUT, uimopn
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*     1         Apr94      J. Eisenhamer    Created
*-------------------------------------------------------------------------------
*
* Input parameters
*
*     rfile - Reference file name.
*     finam - Name of the reference file keyword.
*     flnam - Name of the calibration flag keyword.
*     
* Output parameter
*
*     flag - Calibration flag
*     id - File id
*     istat - Has the following meanings:
*          0  - OK
*        other - error
*-------------------------------------------------------------------------------
      CHARACTER*64 RFILE
      CHARACTER*8 FINAM,FLNAM
      CHARACTER*12 FLAG
      INTEGER ID, ISTAT
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
C                       /HRSIO/
C Common Block containting input/output parameters
C
C   IDS(20) - input file IDs
C               1 - .shh
C               2 - .ulh
C               3 - .d0h
C               4 - .q0h
C               5 - .x0h
C               6 - .xqh
C               7 - .c0h
C               8 - .c1h
C               9 - .cqh
C               10 - .c2h
C               11 - .c3h
C               12 - .c4h
C               13 - .c5h
C   GCOUNT(20) - group counts for input files
C   MERGE - Number of bins merged in output spectra
C   OBSRPT - observation repeats
C   NGOUT - number of output groups
C   NGSDT - number of output groups for the special diode files
C   READNO - readout number
C   NSOUT - number of samples in output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C----------------------------------------------------------------------
C
C Local variables.
C
      CHARACTER*68 PED,DESC,CONTXT
C
C----------------------------------------------------------------------
C Open the file.
C
      CALL UIMOPN (RFILE,RDONLY,ID,ISTAT)
      IF(ISTAT.NE.0)GO TO 999
C
C Get PEDIGREE and DESCRIP keywords.
C
      PED=' '
      DESC=' '
      CALL UHDGST(ID,'PEDIGREE',PED,ISTAT)
      IF(ISTAT.NE.0)GO TO 1000
      CALL UHDGST(ID,'DESCRIP',DESC,ISTAT)
C
C Check the first 5 letters of PEDIGREE.  If DUMMY, 
C then skip this calibration step.
C
      IF(PED(1:5).EQ.'DUMMY')THEN
         FLAG='SKIPPED'
         CALL ZFLCON
      ENDIF
C
C Write the necessary information to the output header and to standard
C output.
C
      WRITE(CONTXT,100)FINAM,RFILE,FLNAM,FLAG
 100  FORMAT(A8,'=',A35,' ',A8,'=',A12)
      CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
      CALL ZADHIS(IDS(8),'HISTORY',CONTXT,ISTAT)
C
      CALL ZMSPUT(PED,STDOUT,0,ISTAT)
      CALL ZADHIS(IDS(8),'HISTORY',PED,ISTAT)
C
      IF(DESC.NE.' ')THEN
         CALL ZMSPUT(DESC,STDOUT,0,ISTAT)
         CALL ZADHIS(IDS(8),'HISTORY',DESC,ISTAT)
      ENDIF
C
 1000 CONTINUE
      ISTAT=0
 999  CONTINUE
      RETURN
      END
