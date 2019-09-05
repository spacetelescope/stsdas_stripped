        SUBROUTINE YOPD0H(ROOT,GRNDMD,ISTAT)
*
*  Module number:
*
*  Module name: YOPD0H
*
*  Keyphrase:
*  ----------
*       Open FOS data file
*
*  Description:
*  ------------
*       This routine opens an FOS data file (.d0h) and extracts
*       the ground mode and the rootname of the observation.
*       It places the file id into common block IOCOM
*       It also reads values of HEADER, DEFDDTBL and TRAILER and places them
*       into the configuration common block
*
*  FORTRAN name: yopd0h.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.d0h          I       FOS data file
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
*       1       Jul 89  D. Lindler      Designed and coded
*       1.1     Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
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
        CALL YFNAME(ROOT,'d0h',1,0,NAME)
        CALL UIMOPN(NAME,RDONLY,IDS(1),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR opening .d0h data file '//NAME
                GO TO 999
        ENDIF
C
C get rootname for the observation
C
        CALL UHDGST(IDS(1),'ROOTNAME',ROOTNM,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting ROOTNAME keyword value from '//
     *                  NAME
                GO TO 999
        ENDIF
C
C get dimensions of the file
C
        CALL UIMGID(IDS(1),DTYPE,NAXIS(1),DIMEN,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading .d0h file '//NAME
                GO TO 999
        ENDIF
        IF(NAXIS(1).GT.2)THEN
                CONTXT='INVALID naxis in .d0h file '//NAME
                GOTO 999
        ENDIF
        NAXIS1(1)=DIMEN(1)
        IF(NAXIS(1).EQ.2)THEN
            NAXIS2(1)=DIMEN(2)
          ELSE
            NAXIS2(1)=1
        ENDIF
        IF((NAXIS1(1)*NAXIS2(1)).GT.12384)THEN
              CONTXT='ERROR: number .d0h data points/frame too large'
              GO TO 999
        ENDIF
C
C get number of groups in the file
C
        CALL UHDGSI(IDS(1),'GCOUNT',GCOUNT(1),ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting GCOUNT from .d0h file '//NAME
                GO TO 999
        ENDIF
C
C Assign the value for fill data
C This used to be read from the header keyword, DQMSKFV
C
        FILL(1) = 800.
C
C get ground mode
C
        CALL UHDGST(IDS(1),'GRNDMODE',GRNDMD,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting GRNDMODE from header of '//
     *                  NAME
                GO TO 999
        ENDIF
C
C GET defddtbl, header and trailer values
C
        CALL UHDGSB(IDS(1),'DEFDDTBL',DEFDDT,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting DEFDDTBL from header of '//
     *                  NAME
                GO TO 999
        ENDIF
        CALL UHDGSB(IDS(1),'HEADER',HEADER,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting HEADER from header of '//NAME
                GO TO 999
        ENDIF
        CALL UHDGSB(IDS(1),'TRAILER',TRAILR,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR getting TRAILER from header of '//NAME
                GO TO 999
        ENDIF
        ISTAT=0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
