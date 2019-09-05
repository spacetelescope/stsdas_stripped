	SUBROUTINE YGTORB(ISTAT)
*
*  Module number:
*
*  Module name: YGTORB
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
*  FORTRAN name: ygtorb.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.SHH          I       FOS data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uhdgs*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91  S. Hulbert      Designed and coded
*     1.1       Jun 91  S. Hulbert      Fixed bug in reading attitiude keywords
*-------------------------------------------------------------------------------
        INTEGER ISTAT
C                                    --->ERROR status
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
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
C Common block containing orbital/positional parameters
C
        DOUBLE PRECISION EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI
        DOUBLE PRECISION ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3
        DOUBLE PRECISION FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER
        DOUBLE PRECISION RASCASCN,SINEINCL,SEMILREC
        DOUBLE PRECISION PSANGLV3,RTASCNV1,DECLNV1
        COMMON /ORBPAR/ EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI,
     $  ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3,
     $  FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER,
     $  RASCASCN,SINEINCL,SEMILREC,
     $  PSANGLV3,RTASCNV1,DECLNV1
C
C Local variables
C
        INTEGER I, ID, ISTATS(20)
        CHARACTER*8 NAMES(20)
        CHARACTER*80 CONTXT
        DATA NAMES/'EPCHTIME','SDMEANAN','CIRVELOC','COSINCLI',
     $  'ECCENTRY','ECCENTX2','ECBDX4D3','ESQDX5D2','ECBDX3',
     $  'FDMEANAN','RCASCNRV','ARGPERIG','MEANANOM','RCARGPER',
     $  'RASCASCN','SINEINCL','SEMILREC',
     $  'PA_V3','RA_V1','DEC_V1'/
C----------------------------------------------------------------
        ID = IDS(6)
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
C
        DO 100 I=1,20
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR reading keyword '//NAMES(I)//
     *                  ' from .shh file'
                GO TO 999
            ENDIF
100     CONTINUE
C
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
