        SUBROUTINE YCLRR(FRAME,ROOT,NFRAME,PFLAGS,NX,FILL,
     *                                  DATA,ERR,EPS,ISTAT)
*
*  Module number:
*
*  Module name: yclrr
*
*  Keyphrase:
*  ----------
*       Rapid readout processing
*
*  Description:
*  ------------
*       This routine computes the total and its statistical error for
*       each frame of rapid readout data.
*
*  FORTRAN name: yclrr.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, yclwrt
*  SDAS:
*
*  Others:
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Aug 89  D. Lindler      Designed and coded
*     1.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1).
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords;
*					(change PFLAGS to CHAR datatype).
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       root - output root name
*       nframe - number of frames in the obervations
*       pflags - processing flags
*       nx - number of data points
*       fill - fill data value
*       data - data array
*       err - error array
*       eps - data quality array
*
* OUTPUTS:
*       istat - error status
*----------------------------------------------------------------------
        INTEGER FRAME,NX,ISTAT,NFRAME
        CHARACTER*64 ROOT
        CHARACTER*8 PFLAGS(*)
        REAL FILL,DATA(*),ERR(*),EPS(*)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C COMMON SCRATCH AREA SHARED BY ALL special procesing routines
C
        REAL AVE(80000),SIGAVE(80000),JUNK(1460)
        COMMON /YSCRTC/AVE,SIGAVE,JUNK
C
C Local variables
C
        CHARACTER*80 CONTXT
        DOUBLE PRECISION SUM,SUMSQ
        INTEGER N,I
        INTEGER ERRCNT
C
C-----------------------------------------------------------------------------
        IF(FRAME.GT.80000)GO TO 1000

        IF(FRAME.EQ.1)THEN
                ERRCNT=0
                CONTXT = 'Special mode processing for RAPID-READOUT'
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF

        N = 0
        SUM = 0.0
        SUMSQ = 0.0
        DO 10 I=1,NX
            IF(EPS(I).LT.FILL)THEN
                SUM = SUM + DATA(I)
                SUMSQ = SUMSQ + ERR(I)*ERR(I)
                N = N+1
            ENDIF
10      CONTINUE
C
C Check if fill data was present
C
        IF(N.NE.NX)THEN
            ERRCNT=ERRCNT+1
            IF(ERRCNT.LT.20)THEN
                WRITE(CONTXT,99)FRAME
99              FORMAT('WARNING: fill data in object, frame',I6)
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
             ELSE
                IF(ERRCNT.EQ.20)THEN
                  CONTXT='WARNING: No longer notifying you of '//
     *                          'fill data'
                  CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                ENDIF
            ENDIF
C
C Crude adjustment for fill data
C
            IF(N.GT.0)THEN
                SUM = (SUM * NX)/N
                SUMSQ = (SUMSQ * NX)/N
            ENDIF
        ENDIF
C
C Write results
C
        AVE(FRAME)=SUM
        SIGAVE(FRAME)=SQRT(SUMSQ)
        ISTAT=0
        IF((FRAME.EQ.80000).OR.(FRAME.EQ.NFRAME))THEN
            CALL YCLWRT(ROOT,1,PFLAGS,AVE,15,'RAP',ISTAT)
            IF(ISTAT.EQ.0)
     *            CALL YCLWRT(ROOT,2,PFLAGS,SIGAVE,15,'RAP',ISTAT)
        ENDIF
C
1000    RETURN
        END
