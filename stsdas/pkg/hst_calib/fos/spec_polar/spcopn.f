        SUBROUTINE SPCOPN(ROOT,ISTAT)
*
*  Module number:
*
*  Module name: SPCOPN
*
*  Keyphrase:
*  ----------
*       open input data files for polarimetry reduction
*
*  Description:
*  ------------
*       This routine opens the input data files (except the .c1h file
*	which is already open).
*
*  FORTRAN name: spcopn.for
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput,yfname
*  SDAS:
*       uimopn, uhdgsr, uimgid
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*     1         Oct 92      D. Bazell       Modified version of YCLOPN
*     2		Sep 93	    H. Bushouse	    Modified to read all input files
*     2.1       Feb 98      M. De La Pena   Added KYDPLY to CONFG1.
*-------------------------------------------------------------------------------
* inputs:
*       ROOT - root name of the file
*
* outputs:
*       ISTAT - error status
*
        CHARACTER*64 ROOT
        INTEGER ISTAT
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      INTEGER RDONLY
      PARAMETER (RDONLY = 1)
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME,HEADER,TRAILR,
     *          DEFDDT
C
C Common block containing input/output file descriptions
C
	INTEGER IDS(30),NAXIS(30),NAXIS1(30),NAXIS2(30),GCOUNT(30)
	REAL FILL(30)
	COMMON /IOCOM/IDS,GCOUNT,NAXIS,NAXIS1,NAXIS2,FILL
C
C Common block containing input file name
C
        CHARACTER*64 INFILE, INEXT
        COMMON /CINFILE/INFILE, INEXT
C
C Local variables
C
        CHARACTER*3 QUAL(4)
        INTEGER DIMEN(8),DTYPE,I
        CHARACTER*64 NAME
        CHARACTER*80 CONTXT
	DATA QUAL/'c0h','c1h','c2h','cqh'/
C---------------------------------------------------------------------------
C
C loop on input files (skip c1h, it's already open)
C
        DO 100 I=1,4
	   IF (I .EQ. 2) GO TO 100
C
C Open the file
C
           CALL YFNAME(ROOT,QUAL(I),1,0,NAME)
c          CALL UIMOPN(NAME,RDONLY,INID(I),ISTAT)
           CALL UIMOPN(NAME,RDONLY,IDS(I+10),ISTAT)
           IF(ISTAT.NE.0)THEN
              CONTXT='ERROR opening data file '//NAME
              GO TO 999
           ENDIF
C
C get dimensions of the file
C     
c          CALL UIMGID(INID(I),DTYPE,INNAX(I),DIMEN,ISTAT)
           CALL UIMGID(IDS(I+10),DTYPE,NAXIS(I+10),DIMEN,ISTAT)
           IF(ISTAT.NE.0)THEN
              CONTXT='ERROR reading file '//NAME
              GO TO 999
           ENDIF
c          IF(INNAX(I).GT.2)THEN
           IF(NAXIS(I+10).GT.2)THEN
              CONTXT='INVALID naxis for file '//NAME
              GOTO 999
           ENDIF
c          INNAX1(I)=DIMEN(1)
c          INNAX2(I)=DIMEN(2)
           NAXIS1(I+10)=DIMEN(1)
           NAXIS2(I+10)=DIMEN(2)
C
C get number of groups in the file
C
c          CALL UHDGSI(INID(I),'GCOUNT',INGCNT(I),ISTAT)
           CALL UHDGSI(IDS(I+10),'GCOUNT',GCOUNT(I+10),ISTAT)
           IF(ISTAT.NE.0)THEN
              CONTXT='ERROR getting GCOUNT from file '//NAME
              GO TO 999
           ENDIF
 100    CONTINUE
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
