        SUBROUTINE SPGREF(REFFIL,ISTAT)
*
*  Module number:
*
*  Module name: SPGREF.FOR
*
*  Keyphrase:
*  ----------
*       get reference file names
*
*  Description:
*  ------------
*       This routine reads the reference file and table names from
*       header parameters in the .c1h file.
*
*  FORTRAN name: spgref.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.c1h          I       FOS science data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uhdgst
*
*  History:
*  --------
*  Version      Date        Author          Description
*	1	Sep 93	H. Bushouse	Modified version of YGTREF
*	1.1	Nov 94  H. Bushouse	Modified for new REFFIL list
*       1.2     Feb 98  M. De La Pena   Added the PCPHFILE for post-COSTAR.
*-------------------------------------------------------------------------------
*
* outputs
*	reffil - reference file and table names
*	istat - error status
        CHARACTER*64 REFFIL(*)
        INTEGER ISTAT
C                                    --->error status
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C	HEADER I/O status message
C
      INTEGER USHPNF
      PARAMETER (USHPNF = 40)
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
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        LOGICAL     KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
C
C local variables
C
        INTEGER I, ISTATS(27)
        CHARACTER*80 CONTXT
        CHARACTER*8 NAMES(27)
        DATA NAMES/'BACHFILE','FL1HFILE','FL2HFILE','IV1HFILE',
     *             'IV2HFILE','RETHFILE','DDTHFILE','DQ1HFILE',
     *		   'DQ2HFILE','CCG2','CCS0','CCS1','CCS2','CCS3',
     *		   'CCS4','CCS5','CCS6','CCS7','OFFS_TAB','CCS8',
     *		   'CCS9','CCSA','CCSB','CCSC','CCSD','AISHFILE',
     *             'PCPHFILE'/
C---------------------------------------------------------------------
	DO 10 I=1,27
	   ISTATS(I) = 0
10	CONTINUE
C
c       DO 100 I=1,27
c           CALL UHDGST(IDS(1), NAMES(I), REFFIL(I), ISTATS(I))
            CALL UHDGST(IDS(12), NAMES(6), REFFIL(6), ISTATS(6))
            CALL UHDGST(IDS(12), NAMES(12), REFFIL(12), ISTATS(12))
            CALL UHDGST(IDS(12), NAMES(15), REFFIL(15), ISTATS(15))
C
C       PCPHFILE - post-COSTAR polarimetry corrections
C
            IF (KYDPLY) THEN
                CALL UHDGST(IDS(12), NAMES(27), REFFIL(27), ISTATS(27))
            END IF
C
C
        DO 101 I=1,27
            IF (ISTATS(I) .NE. 0) THEN
                CONTXT='ERROR: reading .c1h header keyword '//
     $                  NAMES(I)
                GO TO 999
            ENDIF
101     CONTINUE
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
