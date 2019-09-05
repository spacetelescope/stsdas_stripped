        SUBROUTINE YCLTIM(FRAME,CCSD,BADEPS,WAVE,DATA,ERR,EPS,
     *                    PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: ycltim
*
*  Keyphrase:
*  ----------
*       Time correction
*
*  Description:
*  ------------
*       This routine divides object spectra by the appropriate
*       factor that accounts for changes in detector sensitivity
*	as a function of time.
*
*  FORTRAN name: ycltim.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCSD                    I       Sensitivity correction factors table
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, yrccsd
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
*	ccsd  - name of the sensitivity correction table
*	fill  - bad epsilon limit
*	wave  - wavelength array
*       eps   - epsilon array
*
* INPUT/OUTPUT
*       data - data array
*       err  - error array
*
* OUTPUT:
*	pedgre - CCSD ref table PEDIGREE keyword
*	descrp - CCSD ref table DESCRIP keyword
*       istat - error status
*
	IMPLICIT NONE
C
        INTEGER FRAME,ISTAT
        CHARACTER*64 CCSD
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
      INTEGER TYDOUB
      PARAMETER (TYDOUB=7)
C------------------------------------------------------------------------------
C
C Local variables
C
	DOUBLE PRECISION MJD
        INTEGER TNWV, LASTFR
        INTEGER TWAV, TCOR, WAVED, TMCORR
        CHARACTER*3 DTYPE
        INTEGER YOFF,SOFF,I,II,IS,IY
        CHARACTER*80 CONTXT
C---------------------------------------------------------------------------
C
        IF (FRAME.EQ.1) THEN
	    LASTFR = GCOUNT(1)
C
C Allocate dynamic memory for the correction factors
C
	    CALL UDMGET(100, TYDOUB, TWAV, ISTAT)
	    CALL UDMGET(100, TYDOUB, TCOR, ISTAT)
	    CALL UDMGET( NX, TYDOUB, WAVED, ISTAT)
	    CALL UDMGET( NX, TYDOUB, TMCORR, ISTAT)
	    IF(ISTAT.NE.0)THEN
		CONTXT='Error allocating dynamic memory'
		GO TO 999
	    ENDIF
C
C Initialize correction factors
C
	    TNWV = 0
	    DO 10 I=1,100
	       MEMD(TWAV+I-1)=0.0d0
	       MEMD(TCOR+I-1)=1.0d0
10	    CONTINUE
	    DO 20 I=1,NX
	       MEMD(WAVED+I-1) =WAVE(I)
	       MEMD(TMCORR+I-1)=1.0d0
20	    CONTINUE
C
C Get first packet time from the d0h file
C
            CALL UHDGSD(IDS(1),'FPKTTIME',MJD,ISTAT)
            IF(ISTAT.NE.0)THEN
               CONTXT='Error reading FPKTTIME from d0h'
               GO TO 999
            ENDIF
C
C Read time correction factors from CCSD table
C
	    CALL YRCCSD(CCSD,DET,FGWAID,MJD,MEMD(TWAV),MEMD(TCOR),
     *			TNWV,PEDGRE,DESCRP,ISTAT)
	    IF(ISTAT.NE.0)GO TO 1000
	    CONTXT='Sensitivity time corrections from '//CCSD
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C If the reference file contains dummy data, then skip correction
C
            IF(PEDGRE(1:5).EQ.'DUMMY')THEN
               CONTXT='WARNING: PEDIGREE = DUMMY for CCSD '//CCSD
               CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               CONTXT='         Sensitivity correction will be skipped'
               CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
               GO TO 1000
            END IF
C
C Sort the wavelength and correction vectors in order of increasing
C wavelength.
C
	    CALL ZPKSR2(TNWV,MEMD(TWAV),MEMD(TCOR))
C
C Fill correction vector by linear interpolation in wavelength
C
	    CALL ZLINTP(MEMD(TWAV),MEMD(TCOR),TNWV,MEMD(WAVED),
     *			MEMD(TMCORR),NX,ISTAT)
C
C Take inverse of correction vector
C
	    DO 30 I=1,NX
  	       IF(MEMD(TMCORR+I-1).NE.0)
     *            MEMD(TMCORR+I-1) = 1.0d0 / MEMD(TMCORR+I-1)
30	    CONTINUE
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
C multiply by the (inverse) sensitivity correction
C
                    YOFF = SOFF + (IY-1)*NX
                    DO 300 I=1,NX
                        II = YOFF + I
                        IF (EPS(II).LT.BADEPS) THEN
                            DATA(II) = DATA(II)*MEMD(TMCORR+I-1)
                            ERR(II)  = ERR(II) *MEMD(TMCORR+I-1)
                        ENDIF
300                 CONTINUE
                ENDIF
400         CONTINUE
500     CONTINUE
C
C Deallocate dynamic memory for the correction factors
C
	IF(FRAME.EQ.LASTFR)THEN
	    CALL UDMFRE(TWAV,   TYDOUB, ISTAT)
	    CALL UDMFRE(TCOR,   TYDOUB, ISTAT)
	    CALL UDMFRE(WAVED,  TYDOUB, ISTAT)
	    CALL UDMFRE(TMCORR, TYDOUB, ISTAT)
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
1000    RETURN
        END
