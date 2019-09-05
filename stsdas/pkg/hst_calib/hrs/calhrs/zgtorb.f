	SUBROUTINE ZGTORB(ISTAT)
*
*  Module number:
*
*  Module name: ZGTORB
*
*  Keyphrase:
*  ----------
*       Read shp header keywords for orbital and positional information
*
*  Description:
*  ------------
*       This routine reads keywords from the shp that give
*	positional and orbital information about HST and
*       places the values in a common block.
*
*  FORTRAN name: zgtorb.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.SHH          I       GHRS data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zmsput
*  SDAS:
*       uhdgs*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Oct 96  M. De La Pena   Copied and modified from YGTORB.FOR
*                                       written by S. Hulbert for CALFOS
*       1.1     Jan 97  M. De La Pena   Added SAAAVOID keyword
*-------------------------------------------------------------------------------
C
C PASSED PARAMETERS
C
        INTEGER ISTAT
C                                    --->ERROR status
C-------------------------------------------------------------------------------
C
C                       /HRSIO/
C Common Block containing input/output parameters
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
C
C                       /ORBPAR/
C Common block containing orbital/positional parameters
C
        DOUBLE PRECISION EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI
        DOUBLE PRECISION ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3
        DOUBLE PRECISION FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER
        DOUBLE PRECISION RASCASCN,SINEINCL,SEMILREC
        DOUBLE PRECISION PSANGLV3,RTASCNV1,DECLNV1
        CHARACTER*2      SAAAVOID
        COMMON /ORBPAR/ EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI,
     *  ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3,
     *  FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER,
     *  RASCASCN,SINEINCL,SEMILREC,
     *  PSANGLV3,RTASCNV1,DECLNV1,SAAAVOID
C-------------------------------------------------------------------------------
C
C ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C-------------------------------------------------------------------------------
C
C Local variables
C
        INTEGER      I,ID,ISTATS(21)
        CHARACTER*8  NAMES(21)
        CHARACTER*80 CONTXT
C
        DATA NAMES/'EPCHTIME','SDMEANAN','CIRVELOC','COSINCLI',
     $  'ECCENTRY','ECCENTX2','ECBDX4D3','ESQDX5D2','ECBDX3',
     $  'FDMEANAN','RCASCNRV','ARGPERIG','MEANANOM','RCARGPER',
     $  'RASCASCN','SINEINCL','SEMILREC',
     $  'PA_V3','RA_V1','DEC_V1','SAAAVOID'/
C-------------------------------------------------------------------------------
C
        ID = IDS(1)
        CALL UHDGSD(ID,NAMES(1),EPCHTIME,ISTATS(1))
        CALL UHDGSD(ID,NAMES(2),SDMEANAN,ISTATS(2))
        CALL UHDGSD(ID,NAMES(3),CIRVELOC,ISTATS(3))
        CALL UHDGSD(ID,NAMES(4),COSINCLI,ISTATS(4))
        CALL UHDGSD(ID,NAMES(5),ECCENTRY,ISTATS(5))
        CALL UHDGSD(ID,NAMES(6),ECCENTX2,ISTATS(6))
        CALL UHDGSD(ID,NAMES(7),ECBDX4D3,ISTATS(7))
        CALL UHDGSD(ID,NAMES(8),ESQDX5D2,ISTATS(8))
        CALL UHDGSD(ID,NAMES(9),ECBDX3,ISTATS(9))
        CALL UHDGSD(ID,NAMES(10),FDMEANAN,ISTATS(10))
        CALL UHDGSD(ID,NAMES(11),RCASCNRV,ISTATS(11))
        CALL UHDGSD(ID,NAMES(12),ARGPERIG,ISTATS(12))
        CALL UHDGSD(ID,NAMES(13),MEANANOM,ISTATS(13))
        CALL UHDGSD(ID,NAMES(14),RCARGPER,ISTATS(14))
        CALL UHDGSD(ID,NAMES(15),RASCASCN,ISTATS(15))
        CALL UHDGSD(ID,NAMES(16),SINEINCL,ISTATS(16))
        CALL UHDGSD(ID,NAMES(17),SEMILREC,ISTATS(17))
        CALL UHDGSD(ID,NAMES(18),PSANGLV3,ISTATS(18))
        CALL UHDGSD(ID,NAMES(19),RTASCNV1,ISTATS(19))
        CALL UHDGSD(ID,NAMES(20),DECLNV1,ISTATS(20))
        CALL UHDGST(ID,NAMES(21),SAAAVOID,ISTATS(21))
C
        DO 100 I=1,21
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading keyword '//NAMES(I)//
     *                  ' from .shh file'
                GO TO 999
            ENDIF
100     CONTINUE
C
        ISTAT=0
        GO TO 1000
999     CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
