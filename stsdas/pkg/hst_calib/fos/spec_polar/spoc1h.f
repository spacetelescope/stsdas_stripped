        SUBROUTINE SPOC1H(ROOT,GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: SPOC1H
*
*  Keyphrase:
*  ----------
*       Open FOS data file
*
*  Description:
*  ------------
*       This routine opens an FOS data file (.c1h) and extracts
*       the ground mode and the rootname of the observation.
*       It places the file id into common block IOCOM
*       It also reads values of HEADER, DEFDDTBL and TRAILER and places them
*       into the configuration common block
*
*  FORTRAN name: spoc1h.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.c1h          I       FOS data file
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput,yfname
*  SDAS:
*       uimopn, uhdgst, uimgid, uhdgsi
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*	1	Sep 93	H. Bushouse	Modified version of YOPD0H
*      1.1      Feb 98  M. De La Pena   Added KYDPLY to CONFG1.
*-------------------------------------------------------------------------------
* inputs:
*       ROOT - root name of the file
*
* outputs:
*       GRNDMD - ground mode
*       ISTAT - error status
*
        CHARACTER*64 ROOT
        CHARACTER*18 GRNDMD
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
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME
        LOGICAL HEADER,TRAILR,DEFDDT,KYDPLY
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LVTIME,HEADER,TRAILR,
     *          DEFDDT
C
        CHARACTER*10 ROOTNM
        COMMON /YMSGCM/ ROOTNM
C
C Local variables
C
        INTEGER DIMEN(8),DTYPE
        CHARACTER*64 NAME
        CHARACTER*80 CONTXT
C---------------------------------------------------------------------------
C
C create file name and open it
C
c       CALL YFNAME(ROOT,'d0h',1,0,NAME)
        CALL YFNAME(ROOT,'c1h',1,0,NAME)
c       CALL UIMOPN(NAME,RDONLY,IDS(1),ISTAT)
        CALL UIMOPN(NAME,RDONLY,IDS(12),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening .c1h data file '//NAME
                GO TO 999
        ENDIF
C
C get rootname for the observation
C
c       CALL UHDGST(IDS(1),'ROOTNAME',ROOTNM,ISTAT)
        CALL UHDGST(IDS(12),'ROOTNAME',ROOTNM,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting ROOTNAME keyword value from '//
     *                  NAME
                GO TO 999
        ENDIF
C
C get dimensions of the file
C
c       CALL UIMGID(IDS(1),DTYPE,NAXIS(1),DIMEN,ISTAT)
        CALL UIMGID(IDS(12),DTYPE,NAXIS(12),DIMEN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading .c1h file '//NAME
                GO TO 999
        ENDIF
        IF(NAXIS(12).GT.2)THEN
                CONTXT='INVALID naxis in .c1h file '//NAME
                GOTO 999
        ENDIF
c       NAXIS1(1)=DIMEN(1)
        NAXIS1(12)=DIMEN(1)
c       IF(NAXIS(1).EQ.2)THEN
        IF(NAXIS(12).EQ.2)THEN
c           NAXIS2(1)=DIMEN(2)
            NAXIS2(12)=DIMEN(2)
          ELSE
c           NAXIS2(1)=1
            NAXIS2(12)=1
        ENDIF
c       IF((NAXIS1(1)*NAXIS2(1)).GT.12384)THEN
        IF((NAXIS1(12)*NAXIS2(12)).GT.12384)THEN
              CONTXT='ERROR: number .c1h data points/frame too large'
              GO TO 999
        ENDIF
C
C get number of groups in the file
C
c       CALL UHDGSI(IDS(1),'GCOUNT',GCOUNT(1),ISTAT)
        CALL UHDGSI(IDS(12),'GCOUNT',GCOUNT(12),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting GCOUNT from .c1h file '//NAME
                GO TO 999
        ENDIF
C
C Assign the value for fill data
C This used to be read from the header keyword, DQMSKFV
C
c       FILL(1) = 800.
        FILL(12) = 800.
C
C get ground mode
C
c       CALL UHDGST(IDS(1),'GRNDMODE',GRNDMD,ISTAT)
        CALL UHDGST(IDS(12),'GRNDMODE',GRNDMD,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting GRNDMODE from header of '//
     *                  NAME
                GO TO 999
        ENDIF
C
C GET defddtbl, header and trailer values
C
c       CALL UHDGSB(IDS(1),'DEFDDTBL',DEFDDT,ISTAT)
c       CALL UHDGSB(IDS(12),'DEFDDTBL',DEFDDT,ISTAT)
c       IF(ISTAT.NE.0)THEN
c               CONTXT='ERROR getting DEFDDTBL from header of '//
c    *                  NAME
c               GO TO 999
c       ENDIF
c       CALL UHDGSB(IDS(1),'HEADER',HEADER,ISTAT)
c       CALL UHDGSB(IDS(12),'HEADER',HEADER,ISTAT)
c       IF(ISTAT.NE.0)THEN
c               CONTXT='ERROR getting HEADER from header of '//NAME
c               GO TO 999
c       ENDIF
c       CALL UHDGSB(IDS(1),'TRAILER',TRAILR,ISTAT)
c       CALL UHDGSB(IDS(12),'TRAILER',TRAILR,ISTAT)
c       IF(ISTAT.NE.0)THEN
c               CONTXT='ERROR getting TRAILER from header of '//NAME
c               GO TO 999
c       ENDIF
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
