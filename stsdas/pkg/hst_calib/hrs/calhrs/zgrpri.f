        SUBROUTINE ZGRPRI(ROOT,IFILE,GROUP,DATA,ISTAT)
*
*  Module number:
*
*  Module name: ZGRPRI
*
*  Keyphrase:
*  ----------
*       Read data from input file
*  Description:
*  ------------
*       This routine reads the specified group from an input file
*       specified by a file number.  The file number runs from 1
*       to six and identifies the following files.
*               1 - root.shh
*               2 - root.ulh
*               3 - root.d0h
*               4 - root.q0h
*               5 - root.x0h
*               6 - root.xqh
*
*  FORTRAN name:
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zfname
*  SDAS:
*       ZMSPUT, uuopgr
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Feb 89  D. Lindler      Designed and coded
*       2       Mar 94  J. Eisenhamer   Copied from zgrprd and made integer.
*-------------------------------------------------------------------------------
*
* Input parameters
*
*       root - root name of the file
*       ifile - file number (integer from 1 to 6)
*       group - group number
*
* Output parameters
*
*       data - data vector (integer)
*       istat - integer error status
*
        CHARACTER*64 ROOT
        INTEGER IFILE,GROUP,ISTAT,DATA(1)
******************************************************************************
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     CODES FOR DATA TYPES
C
      INTEGER   TYREAL
      PARAMETER (TYREAL = 6)
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
C
C LOCAL VARIABLES
C
        CHARACTER*80 CONTXT
        REAL DMIN,DMAX
        CHARACTER*3 QUAL(6)
        DATA QUAL/'shh','ulh','d0h','q0h','x0h','xqh'/
C
C--------------------------------------------------------------------------
C
C move to specfied group
C
        CALL UUOPGR(IDS(IFILE),GROUP,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,99)GROUP,QUAL(IFILE)
99            FORMAT('ERROR changing to group ',I5,' of the ',A3,
     *                                        ' file')
              GO TO 999
        ENDIF
C
C read data
C
        CALL UIGL1I(IDS(IFILE),DATA,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR: error reading '//QUAL(IFILE)//' file '
                GO TO 999
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
