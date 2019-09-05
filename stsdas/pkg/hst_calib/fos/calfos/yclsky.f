        SUBROUTINE YCLSKY(FRAME,CCS0,CCS2,CCS5,MEDIAN,MEAN,FILL,
     *                    PFLAGS,EPS,DATA,ERR,PEDGR0,PEDGR2,PEDGR5,
     *                    DESCR0,DESCR2,DESCR5,ISTAT)
*
*  Module number:
*
*  Module name: yclsky
*
*  Keyphrase:
*  ----------
*       subtract sky from object spectra
*
*  Description:
*  ------------
*       This routine smooths the sky, scales it by the aperture size
*       ratio, shifts it and subtracts it from the object spectra.
*
*  FORTRAN name:
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCS0                    I       Aperture size ref. table
*       CCS2                    I       Emission line position table
*       CCS5                    I       Sky shift table
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yrccs0, yrccs2, yrccs5, ymsput, yclrep, ymedn, ymean
*
*  SDAS:
*
*  Others:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Aug 89  D. Lindler      Designed and coded
*     1.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*	2	Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*     2.1       Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       ccs0, ccs2, ccs5 - reference table names
*       mean - mean filter width
*       median - median filter width
*       fill - epsilon threshold for bad data
*	pflags - processing flags
*       eps - data quality values (epsilons)
*
* INPUT/OUTPUT
*
*       data - data array
*       err - error array
*
* OUTPUTS:
*	pedgr0 - CCS0 PEDIGREE keyword
*	pedgr2 - CCS2 PEDIGREE keyword
*	pedgr5 - CCS5 PEDIGREE keyword
*	descr0 - CCS0 DESCRIP keyword
*	descr2 - CCS2 DESCRIP keyword
*	descr5 - CCS5 DESCRIP keyword
*       istat - error status
*
*-----------------------------------------------------------------------------
        INTEGER FRAME,ISTAT,MEAN,MEDIAN
	CHARACTER*8  PFLAGS(*)
        CHARACTER*64 CCS0,CCS2,CCS5
	CHARACTER*68 PEDGR0,PEDGR2,PEDGR5,DESCR0,DESCR2,DESCR5
        REAL FILL,EPS(*),DATA(*),ERR(*)
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
        LOGICAL PAIRED
        REAL YUPPER,YLOWER
        COMMON /CCS1CM/PAIRED,YUPPER,YLOWER
C
C Local variables
C
        REAL SBACK(5000),MBACK(5000),AREA(2)
        INTEGER MASK(5000)
C                                    --->mask containing emission line pos.
        INTEGER IY,IS,SOFF,YOFF,IYSKY,SKYOFF,K,NSHIFT,I,II
        REAL YPOS,R
        CHARACTER*6 APSKY,APOBJ
        CHARACTER*80 CONTXT
C
C Flag for fill data in sky
C
        REAL EPSFIL
        DATA EPSFIL/120.0/
C----------------------------------------------------------------------------
C
C Do we need to even do it?
C
        IF((NOBJ.EQ.0).OR.(NSKY.EQ.0).OR.(.NOT.PAIRED))THEN
           CONTXT='No sky to subtract, SKY_CORR will be skipped'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           PFLAGS(7) = 'SKIPPED'
           GO TO 1000
	ENDIF
C
C First frame processing
C
        IF(FRAME.EQ.1)THEN
C
C read areas
C
            CALL YRCCS0(CCS0,DET,APERID,AREA,PEDGR0,DESCR0,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
C
C if the reference table contains dummy data, then skip correction
C
	    IF (PEDGR0(1:5).EQ.'DUMMY') THEN
	       CONTXT='WARNING: PEDIGREE = DUMMY for CCS0 '//CCS0
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       CONTXT='         Sky subtraction will be skipped'
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       GO TO 1000
	    END IF
C
C create mask of emission lines
C
            CALL YRCCS2(CCS2,DET,FGWAID,FCHNL,NXSTEP,5000,MASK,
     *			PEDGR2,DESCR2,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
C
C if the reference table contains dummy data, then skip correction
C
	    IF (PEDGR2(1:5).EQ.'DUMMY') THEN
	       CONTXT='WARNING: PEDIGREE = DUMMY for CCS2 '//CCS2
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       CONTXT='         Sky subtraction will be skipped'
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       GO TO 1000
	    END IF
C
C Determine ystep of the sky
C
            DO 10 IY=1,YSTEPS
                IF(YTYPE(IY).EQ.'SKY')IYSKY=IY
10          CONTINUE

            YPOS = YBASE + (32*YRANGE)/YSTEPS*(IYSKY-1)
C
C Determine aperture and ratio of object/sky aperture
C
            IF(ABS(YPOS-YUPPER).LT.ABS(YPOS-YLOWER))THEN
                APSKY = 'UPPER'
                R = AREA(2)/AREA(1)
             ELSE
                APSKY = 'LOWER'
                R = AREA(1)/AREA(2)
            ENDIF
            WRITE(CONTXT,99)R
99          FORMAT('Ratio of object/sky aperture areas =',G16.8)
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Check to make sure we do not think object is in the same aperture
C
            DO 20 IY=1,YSTEPS

              IF(YTYPE(IY).EQ.'OBJ')THEN
                YPOS = YBASE + (32*YRANGE)/YSTEPS*(IY-1)
                IF(ABS(YPOS-YUPPER).LT.ABS(YPOS-YLOWER))THEN
                        APOBJ = 'UPPER'
                  ELSE
                        APOBJ = 'LOWER'
                ENDIF

                IF(APOBJ.EQ.APSKY)THEN
                    CONTXT='ERROR: sky and object computed to '//
     *                          'be in same aperture'
                    GO TO 999
                ENDIF
              ENDIF

20          CONTINUE
C
C read sky shift
C
            CALL YRCCS5(CCS5,DET,FGWAID,APERID,APSKY,NXSTEP,
     *                  NSHIFT,PEDGR5,DESCR5,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
C
C if the reference table contains dummy data, then skip correction
C
	    IF (PEDGR5(1:5).EQ.'DUMMY') THEN
	       CONTXT='WARNING: PEDIGREE = DUMMY for CCS5 '//CCS5
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       CONTXT='         Sky subtraction will be skipped'
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       GO TO 1000
	    END IF
        ENDIF
C
C Next section is done for all frames ----------------------------------------
C
C
C loop on slices and ysteps
C
        DO 500 IS=1,SLICES
            SOFF = (IS-1)*NX*YSTEPS
C                                    --->offset in data array
            SKYOFF = SOFF+(IYSKY-1)*NX
C                                    --->offset in data array

C
C repair bad points in the sky
C
            CALL YCLREP('sky       ',FILL,NX,FRAME,IS,DATA(SKYOFF+1),          
     *                  EPS(SKYOFF+1),ISTAT)

C
C If can not repair, do not subtract the sky
C
            IF(ISTAT.NE.0)GO TO 500
C
C smooth sky with median filter
C
            IF(MEDIAN.GT.1)THEN
                CALL YMEDN(DATA(SKYOFF+1),NX,MASK,MEDIAN,MBACK,ISTAT)
              ELSE
                DO 50 I=1,NX
                        MBACK(I)=DATA(SKYOFF+I)
50              CONTINUE
            ENDIF
C
C smooth sky with mean filter done twice
C
            IF(MEAN.GT.1)THEN
                CALL YMEAN(MBACK,NX,MASK,MEAN,SBACK,ISTAT)
                CALL YMEAN(SBACK,NX,MASK,MEAN,MBACK,ISTAT)
60              CONTINUE
            ENDIF
C
C loop on ysteps
C
            DO 400 IY=1,YSTEPS
                IF(YTYPE(IY).EQ.'OBJ')THEN
                    YOFF = SOFF + (IY-1)*NX
                    DO 300 I=1,NX
                        II = YOFF+I
                        K = I+NSHIFT
                        IF(K.LT.1)K=1
                        IF(K.GT.NX)K=NX
                        IF(EPS(II).LT.FILL)DATA(II)=DATA(II)-MBACK(K)*R
C
C Flag repaired data points
C
                        IF(EPS(SKYOFF+K).GE.FILL)THEN
                            IF(EPS(II).LT.EPSFIL)EPS(II)=EPSFIL
                        ENDIF
300                 CONTINUE
                ENDIF
400         CONTINUE
500     CONTINUE
C
C Write out more junk
C
        IF(FRAME.EQ.1)THEN
            IF(MEDIAN.GT.1)THEN
                WRITE(CONTXT,599)MEDIAN
599             FORMAT('Sky smoothed by a',I4,' point median filter')
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            ENDIF
            IF(MEAN.GT.1)THEN
                WRITE(CONTXT,699)MEAN
699             FORMAT('Sky smoothed by a',I4,
     *                     ' point mean filter done twice')
                CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            ENDIF
            WRITE(CONTXT,199)NSHIFT
199         FORMAT('Sky shifted ',I3,' Data points before subtraction')
            CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
C
        ISTAT = 0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT = 1
1000    RETURN
        END
