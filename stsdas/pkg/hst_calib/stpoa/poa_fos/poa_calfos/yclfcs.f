C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YCLFCS(FRAME,CCSA,CCSC,BADEPS,WAVE,DATA,ERR,EPS,
     *                    PEDGR1,PEDGR2,DESCR1,DESCR2,ISTAT)
*
*  Module number:
*
*  Module name: yclfcs
*
*  Keyphrase:
*  ----------
*       Focus correction
*
*  Description:
*  ------------
*       This routine divides object spectra by the appropriate
*       factor that accounts for changes in aperture throughput as
*	a function of OTA focus.
*
*  FORTRAN name: yclfcs.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCSA                    I       Focus history table
*	CCSC			I	Aper. throughput vs. focus table
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       ymsput, yrccsa, yrccsc
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
*       ccsa  - name of the OTA focus history table
*	ccsc  - name of the aper. throughput vs. focus table
*	fill  - bad epsilon limit
*	wave  - wavelength array
*       eps   - epsilon array
*
* INPUT/OUTPUT
*       data - data array
*       err  - error array
*
* OUTPUT:
*	pedgr1 - CCSA ref table PEDIGREE keyword
*	pedgr2 - CCSC ref table PEDIGREE keyword
*	descr1 - CCSA ref table DESCRIP keyword
*	descr2 - CCSC ref table DESCRIP keyword
*       istat - error status
*
	IMPLICIT NONE
C
        INTEGER FRAME,ISTAT
        CHARACTER*64 CCSA, CCSC
	CHARACTER*68 PEDGR1,PEDGR2,DESCR1,DESCR2
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
	DOUBLE PRECISION MJD, FOCUS
	REAL AVE
        INTEGER FNWV, LASTFR
	INTEGER FWAV, FTHR, WAVED, FOCORR
        CHARACTER*3 DTYPE
        INTEGER YOFF,SOFF,I,II,IS,IY
        CHARACTER*80 CONTXT
C---------------------------------------------------------------------------
C
        IF (FRAME.EQ.1) THEN
	    LASTFR = GCOUNT(1)
C
C Allocate dynamic memory for correction factors
C
	    CALL UDMGET(25, TYDOUB, FWAV,   ISTAT)
	    CALL UDMGET(25, TYDOUB, FTHR,   ISTAT)
	    CALL UDMGET(NX, TYDOUB, WAVED,  ISTAT)
	    CALL UDMGET(NX, TYDOUB, FOCORR, ISTAT)
	    IF(ISTAT.NE.0)THEN
	       CONTXT='Error allocating dynamic memory'
	       GO TO 999
	    ENDIF
C
C Initialize correction factors
C
	    FOCUS = 0.0d0
	    FNWV = 0
	    DO 10 I=1,25
	       MEMD(FWAV+I-1)=0.0d0
	       MEMD(FTHR+I-1)=1.0d0
10	    CONTINUE
	    DO 20 I=1,NX
	       MEMD(WAVED+I-1)=WAVE(I)
	       MEMD(FOCORR+I-1)=1.0d0
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
C Read the focus history table and interpolate a focus value
C
            CALL YRCCSA(CCSA,MJD,FOCUS,PEDGR1,DESCR1,ISTAT)
            IF(ISTAT.NE.0)GO TO 1000
	    CONTXT='Focus history from '//CCSA
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
	    WRITE(CONTXT,100) MJD, FOCUS
100	    FORMAT('  Focus value for MJD ',f9.3,' = ',g12.5,' microns')
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Read focus correction factors from CCSC table
C
	    CALL YRCCSC(CCSC,DET,APERID,FOCUS,MEMD(FWAV),MEMD(FTHR),
     *			FNWV,PEDGR2,DESCR2,ISTAT)
	    IF(ISTAT.NE.0)GO TO 1000
	    CONTXT='Focus corrections from '//CCSC
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C If the reference file contains dummy data, then skip correction
C
C
C If the reference file contains dummy data, then skip correction
C
	    IF(PEDGR1(1:5).EQ.'DUMMY')THEN
	       CONTXT='WARNING: PEDIGREE = DUMMY for CCSA '//CCSA
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	    END IF
            IF(PEDGR2(1:5).EQ.'DUMMY')THEN
               CONTXT='WARNING: PEDIGREE = DUMMY for CCSC '//CCSC
               CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
            END IF
            IF(PEDGR1(1:5).EQ.'DUMMY'.AND.PEDGR2(1:5).EQ.'DUMMY')THEN
	       CONTXT='         Focus correction will be skipped'
	       CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	       FOCUS = 0.0d0
	       GO TO 1000
            ENDIF
C
C Sort the wavelength and throughput correction arrays in order of
C increasing wavelength.
C
	    CALL ZPKSR2(FNWV,MEMD(FWAV),MEMD(FTHR))
C
C Fill correction vector by linear interpolation in wavelength
C
	    CALL ZLINTP(MEMD(FWAV),MEMD(FTHR),FNWV,MEMD(WAVED),
     *			MEMD(FOCORR),NX,ISTAT)
C
C Take inverse of correction vector
C
	    AVE = 0.0
	    DO 30 I=1,NX
	       AVE = AVE + MEMD(FOCORR+I-1)
  	       MEMD(FOCORR+I-1)= 1.0d0 / MEMD(FOCORR+I-1)
30	    CONTINUE
	    AVE = AVE / NX
	    WRITE(CONTXT,35) AVE
35	    FORMAT('  Mean aperture throughput focus correction = ',
     *		      g12.5)
	    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
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
C multiply by the (inverse) focus correction
C
                    YOFF = SOFF + (IY-1)*NX
                    DO 300 I=1,NX
                        II = YOFF + I
                        IF (EPS(II).LT.BADEPS) THEN
                            DATA(II) = DATA(II)*MEMD(FOCORR+I-1)
                            ERR(II)  = ERR(II) *MEMD(FOCORR+I-1)
                        ENDIF
300                 CONTINUE
                ENDIF
400         CONTINUE
500     CONTINUE
C
C Deallocate dynamic memory
C
	IF(FRAME.EQ.LASTFR)THEN
	   CALL UDMFRE(FWAV,   TYDOUB, ISTAT)
	   CALL UDMFRE(FTHR,   TYDOUB, ISTAT)
	   CALL UDMFRE(WAVED,  TYDOUB, ISTAT)
	   CALL UDMFRE(FOCORR, TYDOUB, ISTAT)
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
