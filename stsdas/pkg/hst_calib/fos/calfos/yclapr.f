        SUBROUTINE YCLAPR(FRAME,CCSB,BADEPS,WAVE,DATA,ERR,EPS,
     *                    REFAPR,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: yclapr
*
*  Keyphrase:
*  ----------
*       Aperture throughput correction
*
*  Description:
*  ------------
*       This routine divides object spectra by the appropriate
*       aperture throughput ratio relative to the reference aperture
*	that was used to derive the IVS.
*
*  FORTRAN name: yclapr.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCSB                    I       Aper. throughput table (CCSB)
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, yrccsb
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1		Oct 94	H. Bushouse	Designed and coded
*    1.1        Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* INPUTS:
*       frame - frame number
*       ccsb  - names of the aperture throughput reference table
*	fill  - bad epsilon limit
*	wave  - wavelength array
*       eps   - epsilon array
*
* INPUT/OUTPUT
*       data - data array
*       err  - error array
*
* OUTPUT:
*	pedgre - ref table PEDIGREE keyword
*	descrp - ref table DESCRIP keyword
*       istat - error status
*
      IMPLICIT NONE
C
      INTEGER FRAME,ISTAT
      CHARACTER*3  REFAPR
      CHARACTER*64 CCSB
      CHARACTER*68 PEDGRE,DESCRP
      REAL BADEPS,WAVE(*),DATA(*),ERR(*),EPS(*)
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
C------------------------------------------------------------------------------
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
      INTEGER TYREAL
      PARAMETER (TYREAL=6)
C------------------------------------------------------------------------------
C
C Local variables
C
        INTEGER APCOR1,APCOR2,LASTFR,FOUND1,FOUND2
        INTEGER YOFF,SOFF,I,II,IS,IY,J,CNUM
	REAL COEF(3,5,2),WMIN(5,2),WMAX(5,2),VAL,YPOS
        CHARACTER*3 DTYPE
        CHARACTER*80 CONTXT
C---------------------------------------------------------------------------
C
C read reference table if first frame
C
        IF (FRAME.EQ.1) THEN
	    LASTFR = GCOUNT(1)
            FOUND1=0
            FOUND2=0
C
C report the name of the table
C
	    CONTXT='Aperture throughput coefficients from '//CCSB
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C for paired apertures search the table for 'upper' and 'lower' entries
C
	    IF(PAIRED)THEN
               CALL YRCCSB(CCSB,DET,FGWAID,APERID,'LOWER ',COEF,WMIN,
     *	   		   WMAX,REFAPR,FOUND1,PEDGRE,DESCRP,ISTAT)
               IF(ISTAT.NE.0)GO TO 1000
               CALL YRCCSB(CCSB,DET,FGWAID,APERID,'UPPER ',COEF,WMIN,
     *			   WMAX,REFAPR,FOUND2,PEDGRE,DESCRP,ISTAT)
               IF(ISTAT.NE.0)GO TO 1000
C
C for single apertures search the table for 'single'
C
	    ELSE
               CALL YRCCSB(CCSB,DET,FGWAID,APERID,'SINGLE',COEF,WMIN,
     *			   WMAX,REFAPR,FOUND1,PEDGRE,DESCRP,ISTAT)
               IF(ISTAT.NE.0)GO TO 1000
	    ENDIF
C
C If the reference file contains dummy data, then skip correction
C
	    IF(PEDGRE(1:5).EQ.'DUMMY')THEN
	       CONTXT='WARNING: PEDIGREE = DUMMY for CCSB '//CCSB
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       CONTXT='         Aperture throughput correction will '//
     *		      'be skipped'
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       GO TO 1000
	    END IF
C
C report the values of the coefficients used
C
	    IF(PAIRED)CALL YMSPUT('  LOWER aperture:',STDOUT,0,ISTAT)
	    DO 20 I=1,FOUND1
	       WRITE(CONTXT,10)WMIN(I,1),WMAX(I,1)
10	       FORMAT('  wmin = ',F7.1,'  wmax = ',F7.1)
	       CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
	       WRITE(CONTXT,15)COEF(1,I,1),COEF(2,I,1),COEF(3,I,1)
15	       FORMAT('  c0 = ',G13.6,'  c1 = ',G13.6,'  c2 = ',
     *                 G13.6)
	       CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
20	    CONTINUE
	    IF(PAIRED)THEN
	       CALL YMSPUT('  UPPER aperture:',STDOUT,0,ISTAT)
	       DO 25 I=1,FOUND2
		  WRITE(CONTXT,10)WMIN(I,2),WMAX(I,2)
	          CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
	          WRITE(CONTXT,15)COEF(1,I,2),COEF(2,I,2),COEF(3,I,2)
	          CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
25	       CONTINUE
	    ENDIF
C
C Allocate dynamic memory for correction vector(s)
C
	    CALL UDMGET(NX, TYREAL, APCOR1, ISTAT)
	    IF(ISTAT.NE.0)THEN
	       CONTXT='Error allocating dynamic memory'
	       GO TO 999
	    ENDIF
	    IF(PAIRED)THEN
	       CALL UDMGET(NX, TYREAL, APCOR2, ISTAT)
	       IF(ISTAT.NE.0)THEN
	          CONTXT='Error allocating dynamic memory'
	          GO TO 999
	       ENDIF
	    ENDIF
C
C Initialize correction vector(s)
C
	    DO 30 I=1,NX
  	       MEMR(APCOR1+I-1) = 0.0
	       IF(PAIRED) MEMR(APCOR2+I-1) = 0.0
30	    CONTINUE
C
C Fill correction vector(s) with appropriate values
C
	    IF (PAIRED) THEN
C
C If paired aperture, loop over y-steps
C
		DO 150 IY=1,YSTEPS
C
C Don't use this y-step if it isn't OBJ data (assume all ysteps>3 are OBJ)
C
                   IF (IY.LE.3) THEN
		       IF (YTYPE(IY).NE.'OBJ') GO TO 150
		   ENDIF
		   YOFF = (IY-1)*NX
		   YPOS=YBASE+(YRANGE*32)/YSTEPS*(IY-1)
C
C Lower aperture
		   IF(ABS(YPOS-YLOWER).LT.ABS(YPOS-YUPPER))THEN
		      DO 110 I=1,NX
		         II = YOFF + I
		         DO 100 J = 1, FOUND1
		            IF(WAVE(II).GE.WMIN(J,1) .AND.
     *			       WAVE(II).LE.WMAX(J,1)) THEN
			       MEMR(APCOR1+I-1) = COEF(1,J,1) +
     *			            COEF(2,J,1)*WAVE(II) +
     *			            COEF(3,J,1)*WAVE(II)*WAVE(II)
		            ENDIF
100		         CONTINUE
110		      CONTINUE
C
C Upper aperture
		   ELSE
		      DO 130 I=1,NX
		         II = YOFF + I
		         DO 120 J = 1, FOUND2
		            IF(WAVE(II).GE.WMIN(J,2) .AND.
     *			       WAVE(II).LE.WMAX(J,2)) THEN
			       MEMR(APCOR2+I-1) = COEF(1,J,2) +
     *			            COEF(2,J,2)*WAVE(II) +
     *			            COEF(3,J,2)*WAVE(II)*WAVE(II)
		            ENDIF
120		         CONTINUE
130		      CONTINUE
		   ENDIF
150		CONTINUE
C
C Single aperture
C
	    ELSE
		DO 170 I=1,NX
		   DO 160 J = 1, FOUND1
		      IF(WAVE(I).GE.WMIN(J,1) .AND.
     *		         WAVE(I).LE.WMAX(J,1)) THEN
			 MEMR(APCOR1+I-1) = COEF(1,J,1) +
     *			      COEF(2,J,1)*WAVE(I) +
     *			      COEF(3,J,1)*WAVE(I)*WAVE(I)
		      ENDIF
160		   CONTINUE
170		CONTINUE
	    ENDIF
C
C Take inverse of correction vector(s)
C
	    DO 200 I=1,NX
	       IF(MEMR(APCOR1+I-1).NE.0) 
     *		  MEMR(APCOR1+I-1) = 1.0 / MEMR(APCOR1+I-1)
	       IF(PAIRED)THEN
		  IF(MEMR(APCOR2+I-1).NE.0)
     *		  MEMR(APCOR2+I-1) = 1.0 / MEMR(APCOR2+I-1)
	       ENDIF
200	    CONTINUE
C
	ENDIF
C------------------ end of frame 1 only processing ----------------------------
C
C Loop on slices
C
        DO 500 IS=1,SLICES
            SOFF = NX*YSTEPS*(IS-1)
C                                    --->offset for the slice
C
C loop on ysteps
C
            DO 400 IY=1,YSTEPS
C
C Is it a OBJ?
C
                IF (IY.GT.3) THEN
                   DTYPE='OBJ'
                ELSE
                   DTYPE=YTYPE(IY)
                ENDIF
                IF (DTYPE.EQ.'OBJ') THEN
C
C Determine which of the 2 possible correction vectors to use
C
		    CNUM=1
		    IF(PAIRED)THEN
		       YPOS=YBASE+(YRANGE*32)/YSTEPS*(IY-1)
		       IF(ABS(YPOS-YUPPER).LT.ABS(YPOS-YLOWER))
     *				CNUM=2
		    ENDIF
C
C multiply by the (inverse) aperture correction
C
                    YOFF = SOFF + (IY-1)*NX
                    DO 300 I=1,NX
                        II = YOFF + I
                        IF (EPS(II).LT.BADEPS) THEN
			    VAL = MEMR(APCOR1+I-1)
			    IF(CNUM.EQ.2)VAL = MEMR(APCOR2+I-1)
                            DATA(II) = DATA(II)*VAL
                            ERR(II)  = ERR(II) *VAL
                        ENDIF
300                 CONTINUE
                ENDIF
400         CONTINUE
500     CONTINUE
C
C Deallocate dynamic memory
C
	IF(FRAME.EQ.LASTFR)THEN
	   CALL UDMFRE(APCOR1, TYREAL, ISTAT)
	   IF(PAIRED)CALL UDMFRE(APCOR2, TYREAL, ISTAT)
	   IF(ISTAT.NE.0)THEN
	      CONTXT='Error deallocating dynamic memory'
	      GO TO 999 
	   ENDIF
	ENDIF
C
        ISTAT = 0
        GO TO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT = 1
1000    CONTINUE
        RETURN
        END
