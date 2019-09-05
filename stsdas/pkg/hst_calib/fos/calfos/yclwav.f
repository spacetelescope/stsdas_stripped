        SUBROUTINE YCLWAV(NAME,NDATA,WAVE,PEDGRE,DESCRP,ISTAT)
*
*  Module number:
*
*  Module name: yclwav
*
*  Keyphrase:
*  ----------
*       Compute wavelength scale
*
*  Description:
*  ------------
*       This routine computes a wavelength scale for each object
*       spectra. (or if no object spectra then for each sky spectra).
*       Wavelengths are computed using coefficients stored in table
*       CCS6.
*
*  FORTRAN name: yclwav.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ccs6                    I       wavelength coefficient table
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       yrccs6, ymsput
*
*  History:
*  --------
*  Version      Date        Author          Description
*    1          Aug 89  D. Lindler      Designed and coded
*		Sep 90	S. Hulbert	update BUNITS
*    1.1	Apr 93	H. Bushouse	Declare passed arrays as (*), not (1)
*    2		Mar 94	H. Bushouse	Mods to handle PEDIGREE keywords
*    2.1        Mar 97  M. De La Pena   Added KYDEPLOY to CONFG1
*-------------------------------------------------------------------------------
*
* Inputs:
*       name - reference table name
*
* Outputs:
*       wave - wavelength array
*	pedgre - reference table PEDIGREE keyword
*	descrp - reference table DESCRIP keyword
*       istat - error status
*
        INTEGER ISTAT
        CHARACTER*64 NAME
	CHARACTER*68 PEDGRE, DESCRP
        REAL WAVE(*)
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
C common block containing minimum and maximum wavelength
C
        REAL MINWAV, MAXWAV
        COMMON /MNMXWV/ MINWAV, MAXWAV        
C
C brigtness units for output files
C
        CHARACTER * 20 BUNITS(10)
        COMMON /BUNITS/ BUNITS
C
C Local variables
C
C  coefficient arrays for both apertures (upper/lower)
C  or both pass directions
C
        DOUBLE PRECISION COEF(5,2),XZERO(2),XX,X2,X3,X4,X,XS
        REAL YPOS,W
        LOGICAL FOUND(2),PROCES
        INTEGER ILAST,IOFF,I,IY,IS,IC,ICOEF,NP
        CHARACTER*80 CONTXT
        CHARACTER*20 MESS
        CHARACTER*3 DTYPE
        CHARACTER*5 APPOS(2)
        CHARACTER*2 NUMER(2)
        DATA APPOS/'UPPER','LOWER'/
        DATA NUMER/' 1',' 2'/
C
C-----------------------------------------------------------------------------
C
C read dispersion coefficients
C
        CALL YRCCS6(NAME,DET,FGWAID,APERID,POLID,PAIRED,COEF,XZERO,
     *       FOUND,PEDGRE,DESCRP,ISTAT)
        IF(ISTAT.NE.0)GO TO 1000
C
C if the reference table contains dummy data, then leave in pixel space
C
	IF (PEDGRE(1:5).EQ.'DUMMY') THEN
           CONTXT='WARNING: PEDIGREE = DUMMY for CCS6 '//NAME
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           CONTXT='         Wavelength calibration will be skipped'
           CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
	END IF
C
C determine what we are processing (obj,sky, or bck)
C
        DTYPE = 'OBJ'
        IF(NOBJ.EQ.0) DTYPE='SKY'
        IF((NOBJ.EQ.0).AND.(NSKY.EQ.0)) DTYPE='BCK'
C
C initialization of loop
C
        ICOEF = 0
C                              --->last set of coef. used (1 or 2)
        ILAST = 0
C                              --->offset in wave for last portion calibrated
        XS = NXSTEP
C                              --->Floated number of xsteps
C
C Loop on ysteps for first slice
C
        DO 100 IY=1,YSTEPS
C
C Should we process this ystep
C
           PROCES = .FALSE.
           IF(IY.GT.3)THEN
              PROCES = .TRUE.
           ELSE
              IF(YTYPE(IY).EQ.DTYPE)PROCES=.TRUE.
           ENDIF
           IF(PROCES)THEN
C
C If paired determine which set of coef. to use
C
              IC = 1
              IF(PAIRED)THEN
                 YPOS = YBASE + (32.0*YRANGE)/YSTEPS*(IY-1)
                 IF( ABS(YLOWER-YPOS).LT.ABS(YUPPER-YPOS) ) IC=2
                 IF(.NOT.FOUND(IC))THEN
                    WRITE(CONTXT,99)APPOS(IC)
 99                 FORMAT('ERROR: No wavelength coef. found in ',
     *                   'CCS6 for ',A5,' aperture')
                    GO TO 999
                 ENDIF
                 MESS = APPOS(IC)//' aperture'
              ELSE
                 MESS = ' '
              ENDIF
C
C Determine which pass direction if polarizer mode
C     
              IF(POLID.NE.'C')THEN
                 IC = IY
                 IF(IY.GT.2)IC=2
                 IF(.NOT.FOUND(IC))THEN
                    WRITE(CONTXT,199)IC
 199                FORMAT('ERROR: No wavelength coef. found in ',
     *                   'CCS6 for pass direction ',I2)
                    GO TO 999
                 ENDIF
                 MESS = 'pass direction '//NUMER(IC)
              ENDIF
C
C Do we need to compute wavelengths or can we reuse old ones.
C
              IOFF = NX * (IY-1)
C                                    --->offset in wave array
              IF(IC.NE.ICOEF)THEN
C
C Write coefficients used
C
                 CONTXT='Wavelength coefficients: '//MESS
                 CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                 DO 29 I=1,4
                    WRITE(CONTXT,19)I-1,COEF(I,IC)
 19                 FORMAT('   COEF_',I1,' = ',G24.16)
                    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
 29              CONTINUE
                 IF(FGWAID.EQ.'PRI')THEN
                    WRITE(CONTXT,19)4,COEF(5,IC)
                    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                    WRITE(CONTXT,28)XZERO(IC)
 28                 FORMAT('    XZERO = ',F10.3)
                    CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
                 ENDIF
C     
C compute wavelengths for the y-step
C
                 IF(PEDGRE(1:5).EQ.'DUMMY')THEN
                    DO 31 I = 1, NX
                       WAVE(IOFF+I) = I
 31                 CONTINUE
		 ELSE
C
                    DO 30 I=1,NX
                       X = (I-1)/XS+FCHNL
C                              ---> x position
                       IF(FGWAID.EQ.'PRI')THEN
C     
C Prism mode
C
                          XX = X - XZERO(IC)
                          IF(ABS(XX).GE.1.0D0)THEN
                             X2 = XX*XX
                             X3 = X2*XX
                             X4 = X3*XX
                             W = COEF(1,IC) + COEF(2,IC)/XX +
     *                            COEF(3,IC)/X2 + COEF(4,IC)/X3 +
     *                            COEF(5,IC)/X4
                          ELSE
                             W = 0.0D0
                          ENDIF
                       ELSE
C
C Grating mode
C
                          X2 = X*X
                          X3 = X2*X
                          W = COEF(1,IC) + COEF(2,IC)*X +
     *                         COEF(3,IC)*X2 + COEF(4,IC)*X3
                       ENDIF
                       IF((W.LT.900.0).OR.(W.GT.10000.0))W=0.0
                       WAVE(IOFF+I)=W
 30                 CONTINUE
                 END IF
                 ICOEF = IC
                 ILAST = IOFF
              ELSE
C
C Reuse previously computed wavelengths
C
                 DO 40 I=1,NX
                    WAVE(IOFF+I)=WAVE(ILAST+I)
 40              CONTINUE
              ENDIF
           ENDIF
 100    CONTINUE
C
C Use same wavelength scales for all slices
C
        IF(SLICES.GT.1)THEN
           NP = NX*YSTEPS
C                                    --->points per slice
           DO 200 IS=2,SLICES
              IOFF = (IS-1)*NP
              DO 150 I=1,NP
                 WAVE(IOFF+I)=WAVE(I)
 150          CONTINUE
 200       CONTINUE
        ENDIF
C
C calculate min and max wavelength
C
        CALL YWMNMX(WAVE,NDATA,MINWAV,MAXWAV)
C
C update brightness units
C
	IF (PEDGRE(1:5).EQ.'DUMMY') THEN
           BUNITS(1) = 'PIXELS'
	ELSE
           BUNITS(1) = 'ANGSTROMS'
	END IF
C
        ISTAT=0
        GO TO 1000
 999    CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
 1000   RETURN
        END
