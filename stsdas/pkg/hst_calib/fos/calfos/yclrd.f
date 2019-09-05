        SUBROUTINE YCLRD(FRAME,DATA,EPS,TRAIL,EPST,ISTAT)
*
*  Module number:
*
*  Module name: YCLRD
*
*  Keyphrase:
*  ----------
*       Read input data
*
*  Description:
*  ------------
*       This routine reads the next frame of input data, including
*       trailer and data quality files.
*
*  FORTRAN name: yclrd.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       <rootname>.d0h          I       data file
*       <rootname>.q0h          I       data quality
*       <rootname>.d1h          I       trailer
*       <rootname>.q1h          I       trailer quality
*
*  Subroutines called:
*  -------------------
*  CDBS:
*       ymsput
*  SDAS:
*       uuogpr, uimgsr, uigl1r
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       JUL 89  D. Lindler      Designed and coded
*     1.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*     1.2       Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* Inputs:
*       frame - frame number
*
* Outputs:
*       data - data vector
*       eps - data quality vector
*       trail - trailer vector
*       epst  - epsilon for the trailer
*       istat - error status
*-----------------------------------------------------------------------------
        INTEGER ISTAT,FRAME
        REAL EPS(*),DATA(*),TRAIL(*),EPST(*)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
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
        INTEGER NX,NOBJ,NSKY,NBCK
        COMMON /CONFG3/NX,NOBJ,NSKY,NBCK
        REAL EXPO
        COMMON /CONFG4/EXPO
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
C local variables
C
        CHARACTER*80 CONTXT
        INTEGER FPIX(2),LPIX(2),IREAD,CLEARS,NREPS,I,NPTS
        REAL DMIN,DMAX
C--------------------------------------------------------------------------
C
C read data --------------------------------------------------  d0h file
C
C point to correct frame
C
        CALL UUOPGR(IDS(1),FRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR moving to next group of the .d0h file'
                GO TO 999
        ENDIF
C
C Read data
C
        IF(NAXIS(1).EQ.1)THEN
                CALL UIGL1R(IDS(1),DATA,ISTAT)
           ELSE
                FPIX(1)=1
                FPIX(2)=1
                LPIX(1)=NAXIS1(1)
                LPIX(2)=NAXIS2(1)
                CALL UIMGSR(IDS(1),FPIX,LPIX,DATA,ISTAT)
        ENDIF
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading .d0h file'
                GO TO 999
        ENDIF
C
C read data quality  -----------------------------------------  q0h file
C
C point to correct frame
C
        CALL UUOPGR(IDS(2),FRAME,DMIN,DMAX,0,ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR moving to next group of the .q0h file'
                GO TO 999
        ENDIF
C
C Read data
C
        IF(NAXIS(2).EQ.1)THEN
                CALL UIGL1R(IDS(2),EPS,ISTAT)
                DO 111 I=1,NAXIS1(2)
                    IF(EPS(I).EQ.16)EPS(I)=800
                    IF(EPS(I).EQ.1)EPS(I)=100
111             CONTINUE
           ELSE
                FPIX(1)=1
                FPIX(2)=1
                LPIX(1)=NAXIS1(2)
                LPIX(2)=NAXIS2(2)
                CALL UIMGSR(IDS(2),FPIX,LPIX,EPS,ISTAT)
                NPTS = NAXIS1(2)*NAXIS2(2)
                DO 115 I=1,NPTS
                        IF(EPS(I).EQ.16)EPS(I)=800
                        IF(EPS(I).EQ.1)EPS(I)=100
115             CONTINUE
        ENDIF
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading .q0h file'
                GO TO 999
        ENDIF
C
C read trailer --------------------------------------------------  d1h file
C
C point to correct frame
C
        IF(IDS(3).GT.0)THEN
            CALL UUOPGR(IDS(3),FRAME,DMIN,DMAX,0,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR moving to next group of the .d1h file'
                GO TO 999
            ENDIF
C
C Read data
C
            CALL UIGL1R(IDS(3),TRAIL,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading .d1h file'
                GO TO 999
            ENDIF
        ENDIF
C
C read trailer quality ------------------------------------------ q1h file
C
C point to correct frame
C
        IF(IDS(4).GT.0)THEN
            CALL UUOPGR(IDS(4),FRAME,DMIN,DMAX,0,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR moving to next group of the .q1h file'
                GO TO 999
            ENDIF
C
C Read data
C
            CALL UIGL1R(IDS(4),EPST,ISTAT)
            IF(ISTAT.NE.0)THEN
                CONTXT='ERROR reading .q1h file'
                GO TO 999
            ENDIF
            DO 211 I=1,NAXIS1(1)
                    IF(EPS(I).EQ.16)EPS(I)=800
                    IF(EPS(I).EQ.1)EPS(I)=100
211         CONTINUE
        ENDIF
C
C Compute exposure time for the frame
C
        CLEARS = (FRAME-1)/NREAD
        IREAD = FRAME - CLEARS*NREAD
        NREPS = IREAD*OVERSN*NPAT*INTS
        EXPO = (LVTIME*7.8125E-6)*NREPS
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
