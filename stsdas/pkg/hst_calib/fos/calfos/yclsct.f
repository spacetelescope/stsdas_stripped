        SUBROUTINE YCLSCT(FRAME,CCS9,BADEPS,PFLAGS,DATA,EPS,ERR,
     +			  SCTVAL,SCTERR,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: yclsct
*
*  Keyphrase:
*  ----------
*	Subtract scattered light from OBJ and SKY
*
*  Description:
*  ------------
*       This routine calculates the amplitude of scattered light from
*       regions of gratings that have no sensitivity and subtracts this
*       amount from OBJ and SKY spectra.
*
*  FORTRAN name: yclsct.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name				I/O	Description / Comments
*	CCS9			I	No sensitivity diode range table
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, yrccs9
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1          Mar 94	H. Bushouse	Designed and coded
*    2		Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*    3          Dec 95  J. Eisenhamer   Added median filtering.
*    3.1        Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* INPUTS:
*	frame - frame number
*	ccs9 - CCS9 reference table name
*       badeps - bad epsilon limit
*	eps - epsilon array
*       err - data statistical error array.
*
* INPUT/OUTPUT:
*	pflags - processing flags
*       data - sky and object spectral data array
*
* OUTPUTS:
*	sctval - scattered light value array
*	scterr - scattered light error array
*	pedgre - CCS9 PEDIGREE keyword
*	descrp - CCS9 DESCRIP  keyword
*       istat - error status
*
*------------------------------------------------------------------------------
	IMPLICIT NONE
C
	CHARACTER*8  PFLAGS(*)
	CHARACTER*64 CCS9
	CHARACTER*68 PEDGRE, DESCRP
        INTEGER FRAME, ISTAT
        REAL BADEPS, DATA(*), EPS(*), ERR(*), SCTVAL(*), SCTERR(*)
C
C DATA QUALITY VALUE FOR SCATTERED LIGHT THAT IS NOT INCLUDED IN
C THE CORRECTION
C
        REAL REJEPS
        PARAMETER (REJEPS = 170.0)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(1)
      INTEGER*2        MEMS(1)
      INTEGER*4        MEMI(1)
      INTEGER*4        MEML(1)
      REAL             MEMR(1)
      DOUBLE PRECISION MEMD(1)
      COMPLEX          MEMX(1)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C
      INTEGER TYINT
      PARAMETER (TYINT = 4)
      INTEGER TYREAL
      PARAMETER (TYREAL=6)
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
C Local variables
C
        CHARACTER*3 DTYPE
        INTEGER YOFF, SOFF, I, II, IS, IY, NSPEC
	INTEGER NGOOD,NGOOD2,NREJ,MAXREJ
        INTEGER DARR,IARR
	INTEGER POSI(2),ISTATS(2)
	REAL POS(2), LCHNL
	REAL MEAN, SIGMA
        REAL MEDIAN, ERROR, REJECT
        CHARACTER*80 CONTXT
C---------------------------------------------------------------------------
	ISTAT = 0
C
C Processing done on first frame only
C
	IF (FRAME .EQ. 1) THEN
C
C Read the diode range from the CCS9 reference table
C
	    CALL YRCCS9(CCS9,DET,FGWAID,POS,PEDGRE,DESCRP,ISTAT)
	    IF (ISTAT.NE.0) GO TO 1000
C
C If the reference table contains dummy data, then skip the correction
C
	    IF (PEDGRE(1:5).EQ.'DUMMY') THEN
		CONTXT='WARNING: PEDIGREE = DUMMY for CCS9 '//CCS9
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		CONTXT='         Scattered light correction will '//
     *                 'be skipped'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		GO TO 1000
	    END IF
C
C Check diode range; if zero, then correction is not
C possible for this detector/grating combination and will be skipped.
C
	    IF (POS(1).EQ.0.0 .AND. POS(2).EQ.0.0) THEN
		CONTXT='WARNING: Scattered light correction not '//
     &                 'available for'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                CONTXT='         detector/grating combination '//DET//
     &                 '/'//FGWAID//';'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		CONTXT='         Scattered light correction will '//
     &                 'be skipped'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		PFLAGS(5) = 'SKIPPED'
		GO TO 1000
	    ENDIF
C
C Check validity of diode range limits;
C
C	First check for outright error in CCS9 values
C
	    IF ((POS(1).GT.POS(2)) .OR. (POS(1).LT.0.0)) THEN
		WRITE(CONTXT,97) POS
97		FORMAT('ERROR: CCS9 diode range ',F8.2,' to ',
     &			F8.2,' is not valid')
		GO TO 999
	    ENDIF
C
C	Now check to make sure limits are within range of data
C
	    LCHNL = FCHNL+NCHNLS+OVERSN-2+(NXSTEP-1)/NXSTEP
	    IF ((POS(1).GT.LCHNL) .OR. (POS(2).LT.FCHNL)) THEN
		WRITE(CONTXT,98) POS
98		FORMAT('WARNING: CCS9 diode range ',F6.2,' to ',
     &			F6.2,' not within data limits')
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		CONTXT='WARNING: SCT_CORR will be skipped'
		CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
		PFLAGS(5) = 'SKIPPED'
		GO TO 1000
	    ENDIF
C
C Convert diode range to pixel range; diode values are 0 indexed,
C while pixel values are 1 indexed.
C
	    POSI(1) = NINT (1 + NXSTEP*(POS(1)-FCHNL))
	    POSI(2) = NINT (1 + NXSTEP*(POS(2)-FCHNL))
C
C Ensure that beginning and ending values are within limits of data
C
	    POSI(1) = MAX (POSI(1), 1)
	    POSI(2) = MIN (POSI(2), NX)
C
C Report pixel range being used
C
	    CONTXT='Scattered light correction using '//CCS9
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
	    WRITE(CONTXT,199) POSI
199         FORMAT('Scattered light measured from pixels ',I4,' to ',I4)
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Allocate sorting arrays.
C
            CALL UDMGET (NX,TYREAL,DARR,ISTATS(1))
            CALL UDMGET (NX,TYINT,IARR,ISTATS(2))
            DO 101 I = 1, 2
               IF (ISTATS(I).NE.0) THEN
                  CONTXT = 'ERROR allocating memory for scattered light'
                  GO TO 999
               ENDIF
 101        CONTINUE
            MAXREJ = 0
	END IF
C
C Next section is done for all frames -----------------------------------------
C
C Loop on slices
C
        DO 700 IS = 1, SLICES
           SOFF = NX*YSTEPS*(IS-1)
C                                    --->offset for the slice
C loop on ysteps
C     
           DO 600 IY = 1, YSTEPS
              NSPEC = (IS-1)*YSTEPS + IY
              SCTVAL(NSPEC) = 0.0
              SCTERR(NSPEC) = 0.0
C
C Is this ystep OBJ or SKY?
C
              IF (IY .GT. 3) THEN
                 DTYPE = 'OBJ'
              ELSE
                 DTYPE = YTYPE(IY)
              END IF
C
C Apply correction to ysteps that are OBJ or SKY
C
              IF ((DTYPE.EQ.'OBJ') .OR. (DTYPE.EQ.'SKY')) THEN
C
C Get good data into the sort arrays.
C
                 YOFF = SOFF + (IY-1)*NX
                 NGOOD2 = 0
                 DO 200 I = POSI(1), POSI(2)
                    II = YOFF + I
                    IF (EPS(II) .LT. BADEPS) THEN
                       NGOOD2 = NGOOD2 + 1
                       MEMR(DARR+NGOOD2-1)=DATA(II)
                       MEMI(IARR+NGOOD2-1)=II
                    END IF
 200             CONTINUE
C
C All data in no sensitivity region bad?
C
                 IF (NGOOD2 .EQ. 0) THEN
                    WRITE (CONTXT, 300)
 300                FORMAT ('WARNING: No good data for scattered ',
     &                   'light measurement in')
                    CALL YMSPUT (CONTXT, STDOUT+STDERR, 0, ISTAT)
                    WRITE (CONTXT, 301) FRAME, IS, IY
 301                FORMAT ('         frame ',I5,' slice ',I3,
     &                   ' ystep ',I3)
                    CALL YMSPUT (CONTXT, STDOUT+STDERR, 0, ISTAT)
                    WRITE (CONTXT, 302)
 302                FORMAT ('         It will not be subtracted')
                    CALL YMSPUT (CONTXT, STDOUT+STDERR, 0, ISTAT)
C
C Else, Correct for scattered light
C
                 ELSE
C
C Find median value of the scattered light and get error of the median.
C
                    CALL YSRRI(NGOOD2,MEMR(DARR),MEMI(IARR))
                    I = MAX (NGOOD2/2,1)
                    MEDIAN = MEMR(DARR+I-1)
                    ERROR = ERR(MEMI(IARR+I-1))
                    IF(ERROR.EQ.0.)ERROR=1./EXPO
                    REJECT=4.0*ERROR
C
C Determine mean with rejection.
C
                    NGOOD=0
                    MEAN=0.
                    NREJ=0
                    DO 350 I = 1, NGOOD2
                       IF (ABS(MEMR(DARR+I-1)-MEDIAN).LE.REJECT) THEN
                          MEAN=MEAN+MEMR(DARR+I-1)
                          NGOOD=NGOOD+1
                       ELSE
                          II=MEMI(IARR+I-1)
                          EPS(II) = MAX(EPS(II),REJEPS)
                          NREJ=NREJ+1
                       ENDIF
 350                CONTINUE
C
C Calculate mean and sigma.
C
                    MAXREJ = MAX(MAXREJ, NREJ)
                    MEAN = MEAN / NGOOD
                    SIGMA = 0.0
                    IF (NGOOD .GT. 1) THEN
                       DO 400 I = 1, NGOOD
                          II = MEMI(IARR+I-1)
                          IF (EPS(II).LT.REJEPS) THEN
                             SIGMA = SIGMA + 
     *                            (MEMR(DARR+I-1) - MEAN)**2
                          END IF
 400                   CONTINUE
                       SIGMA = SQRT ( SIGMA / (NGOOD * (NGOOD-1)) )
                    END IF
C
C Subtract mean scattered light from data
C
                    DO 500 I = 1, NX
                       II = YOFF + I
                       IF (EPS(II).LT.BADEPS) DATA(II) = DATA(II)-MEAN
 500                CONTINUE
                    SCTVAL(NSPEC) = MEAN
                    SCTERR(NSPEC) = SIGMA
                 END IF
              END IF
C                     -------> next ystep
 600       CONTINUE
C                -------> next slice
 700    CONTINUE
C
C If last frame, free memory and report on scattered light pixels rejected.
C
        IF (FRAME.EQ.GCOUNT(1)) THEN
           CALL UDMFRE (DARR,TYREAL,ISTAT)
           CALL UDMFRE (IARR,TYINT,ISTAT)
           IF (MAXREJ.GT.0) THEN
              WRITE(CONTXT,990)MAXREJ,ABS(POSI(2)-POSI(1))
 990          FORMAT('Scattered light maximum rejection of ',
     *             i3, ' pixels out of ', i4)
              CALL YMSPUT (CONTXT, STDOUT, 0, ISTAT)
           ENDIF
        ENDIF
        ISTAT = 0
        GO TO 1000
999     CALL YMSPUT (CONTXT, STDOUT+STDERR, 0, ISTAT)
        ISTAT = 1
1000    RETURN
        END
