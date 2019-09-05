C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLTR(ROOT,PFLAGS,FILL,DATA,ERR,EPS,ISTAT)
*
*  Module number:
*
*  Module name: ycltr
*
*  Keyphrase:
*  ----------
*       Time-resolved mode processing
*
*  Description:
*  ------------
*       This routine averages the slices and computes differences from
*       the averages for the last frame of time resolved data.
*
*  FORTRAN name: ycltr.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yclwrt, ymsput
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
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*     2.1       Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       root - root name of output data file
*       pflags - processing flags
*       fill - fill data epsilon threshold
*       data - data array
*       err - error array
*       eps - epsilon array
*
* OUTPUTS:
*       istat - error status
*----------------------------------------------------------------------------
        CHARACTER*64 ROOT
        INTEGER ISTAT
        REAL DATA(*),ERR(*),EPS(*),FILL
        CHARACTER*8 PFLAGS(*)
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
C
C scratch area
C
        REAL AVE(10000),EAV(10000),DIF(10000),EDIF(10000)
        INTEGER NADDS(10000),JUNK(111460)
        COMMON /YSCRTC/AVE,EAV,DIF,EDIF,NADDS,JUNK
C
C Local variables
C
        CHARACTER*80 CONTXT
        INTEGER IS,OBJOFF,I,K,IY,IREC
        LOGICAL FOUND
C-----------------------------------------------------------------------------
C
C Find which ystep is the object
C
        CALL YMSPUT('Special Processing for TIME-RESOLVED mode',
     *               STDOUT,0,ISTAT)
        IF(YSTEPS.GT.3)THEN
                IY=1
           ELSE
                DO 10 IY=1,3
                        IF(YTYPE(IY).EQ.'OBJ')GO TO 20
10              CONTINUE
20              CONTINUE
        ENDIF
C
C initialization
C
        DO 100 I=1,NX
                AVE(I)=0.0
                EAV(I)=0.0
                NADDS(I)=0
100     CONTINUE
C
C loop on slices and compute the average
C
        DO 200 IS=1,SLICES
            FOUND=.FALSE.
C                                    --->fill data found in the slice
            OBJOFF = (IS-1)*NX*YSTEPS + (IY-1)*NX
            DO 150 I=1,NX
                K=OBJOFF+I
                IF(EPS(K).LT.FILL)THEN
                    AVE(I) = AVE(I)+DATA(K)
                    EAV(I) = EAV(I)+ERR(K)**2
                    NADDS(I) = NADDS(I) + 1
                  ELSE
                    FOUND = .TRUE.
                ENDIF
150         CONTINUE
            IF(FOUND)THEN
                WRITE(CONTXT,199)IS
199             FORMAT('WARNING: fill data in slice',I4,
     *                          ' of the last frame')
                CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            ENDIF
200     CONTINUE
C
C Compute average
C
        DO 300 I=1,NX
            IF(NADDS(I).GT.0)THEN
                AVE(I)=AVE(I)/NADDS(I)
                EAV(I)=SQRT(EAV(I))/NADDS(I)
            ENDIF
300     CONTINUE
        CALL YCLWRT(ROOT,1,PFLAGS,AVE,15,'TIM',ISTAT)
        IF(ISTAT.NE.0) GO TO 1000
        CALL YCLWRT(ROOT,2,PFLAGS,EAV,15,'TIM',ISTAT)
        IF(ISTAT.NE.0) GO TO 1000
C
C compute differences and errors in the differences
C
        IREC = 2
        DO 400 IS=1,SLICES
            OBJOFF = (IS-1)*NX*YSTEPS + (IY-1)*NX
            DO 350 I=1,NX
                K = OBJOFF+I
                IF(EPS(K).LT.FILL)THEN
                    DIF(I) = DATA(K) - AVE(I)
                    EDIF(I) = SQRT(EAV(I)**2 + ERR(K)**2)
                ENDIF
350         CONTINUE
            IREC = IREC + 1
            CALL YCLWRT(ROOT,IREC,PFLAGS,DIF,15,'TIM',ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
            IREC = IREC + 1
            CALL YCLWRT(ROOT,IREC,PFLAGS,EDIF,15,'TIM',ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
400     CONTINUE
C
        ISTAT=0
1000    RETURN
        END
