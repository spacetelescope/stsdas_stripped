        SUBROUTINE ZCLADC(IREAD,PASS,CCR5,CCR6,CCR7,
     *     CCRC,FGWC,NS,NSPEC,WAVE,ORDER,ISTAT)
*
*  Module number:
*
*  Module name: zcladc
*
*  Keyphrase:
*  ----------
*       Apply dispersion coefficients.
*
*  Description:
*  ------------
*       This routine reads the thermal motion coefficients and
*       dispersion coefficients.  It then calls routines
*       ZCLORD to compute spectral orders and ZCLWAV to compute
*       wavelenghts.
*
*  FORTRAN name: zcladc.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       CCR5                    I       Spectral order constant table
*       CCR6                    I       Dispersion coef. table
*       CCR7                    I       Thermal motion coef. table
*       CCRC                    I       Global wavelength coef. table
*       <rootname>.shh          I       Standard header packet table
*                                       (temperature read from header)
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zclord, zclwav, zccr6, zccr7, zccrc
*  SDAS:
*       ZMSPUT, uhdgsr
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*	1.1	Feb 91	S. Hulbert	Added carpos to zclwav call in order
*					to add cubic term. Changed dimension
*					of coef. 
*	1.2  	Jul 91	S. Hulbert	Changed keyword ORDER to SPORDER
*	1.3  	Sep 91	S. Hulbert	Implemented PASS flag           
*	1.3.1  	Feb 92	S. Hulbert	Bug fix--don't exit with error if
*					unable to update SPORDER keyword
*       1.4     Oct 93  J. Eisenhamer   Implement GHRS teams solution.
*       1.5     Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* INPUTS:
*       IREAD - readout number
*       PASS - Logical varaible set to true on first call
*       CCR5 - name of spectral order constant table
*       CCR6 - name of dispersion coef. table
*       CCR7 - name of thermal constant table
*       CCRC - global wavelength coefficient table
*       FGWC - Use the global wavelength coefficients
*       NS - number of output samples
*       NSPEC - number of output spectra
*
* OUTPUTS:
*       WAVE - wavelength array NS x NSPEC double precision
*       ORDER - spectral order
*       ISTAT - error status
*
*----------------------------------------------------------------------------
        INTEGER PASS
        CHARACTER*64 CCR5,CCR6,CCR7,CCRC
        CHARACTER*12 FGWC
        INTEGER NS,NSPEC,ISTAT,ORDER,IREAD
        DOUBLE PRECISION WAVE(NS,NSPEC)
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C                       /HRSIO/
C Common Block containting input/output parameters
C
C   IDS(20) - input file IDs
C               1 - .shh
C               2 - .ulh
C               3 - .d0h
C               4 - .q0h
C               5 - .x0h
C               6 - .xqh
C               7 - .c0h
C               8 - .c1h
C               9 - .cqh
C               10 - .c2h
C               11 - .c3h
C               12 - .c4h
C               13 - .c5h
C   GCOUNT(20) - group counts for input files
C   MERGE - Number of bins merged in output spectra
C   OBSRPT - observation repeats
C   NGOUT - number of output groups
C   NGSDT - number of output groups for special diode files
C   READNO - readout number
C   NSOUT - number of samples in the output data sets
C
        INTEGER IDS(20),MERGE,OBSRPT,NGOUT,GCOUNT(20),READNO,NSOUT,NGSDT
        COMMON /HRSIO/ IDS,GCOUNT,MERGE,OBSRPT,NGOUT,READNO,NSOUT,NGSDT
C
C                       /HRSDEF/
C Deflection pattern common block
C       NBINS - Number of supstep bins
C       NINIT - number of initial deflection pairs
C       IXDEF(5) - initial x-deflections
C       XDEF(7) - x-deflections for each bin
C       YDEF(7) - y-deflections for each bin
C       RCODES(7) - repeat codes for each bin
C       BINIDS(7) - substep bins ids for each bin
C       SAMPLE(7) - starting sample position for each spectra
C       LINE(7) - starting line position for each spectra
C       DELTAS(7) - sample position increment for each spectra
C       HOFF(7),VOFF(7) - horizontal and vertical offsets
C       DOPMAG, DOPZER - dopler magnitude and zero time
C       XDCAL, XDCALP - x-deflection calibration parameters
C       STPTIM - integration time at each step pattern position
C
        INTEGER NBINS,NINIT,IXDEF(5),XDEF(7),YDEF(7),RCODES(7)
        INTEGER BINIDS(7),HOFF(7),VOFF(7)
        REAL SAMPLE(7),LINE(7),DELTAS(7),DOPMAG,XDCAL,XDCALP
        DOUBLE PRECISION DOPZER,STPTIM
        COMMON /HRSDEF/ DOPZER,STPTIM,NBINS,NINIT,IXDEF,XDEF,YDEF,
     *                  RCODES,BINIDS,SAMPLE,LINE,DELTAS,HOFF,VOFF,
     *                  DOPMAG,XDCAL,XDCALP
C
C                       /HRSMOD/
C Common block containing observing mode parameters
C
C   GRAT - grating mode
C   DET - detector
C   SCLAMP - spectral calibration lamp
C   CARPOS - carrousel position
C   OBSMOD - observation mode (DIR, ACC, TAR)
C   APER - aperture (LSA, SSA, SC1, SC2)
C
        CHARACTER*3 OBSMOD,APER
        CHARACTER*5 GRAT
        INTEGER DET,SCLAMP,CARPOS
        COMMON /HRSMOD/ DET,SCLAMP,CARPOS
        COMMON /HRSMD1/ GRAT,OBSMOD,APER
C
C			/ZREFID/
C
C COMMON BLOCK containing id's for reference files which remain
C open throughout the whole calibration.
C
        INTEGER IDCCR2,IDCCR6,IDCCR8,IDCCR9,IDCCRC,IDPHC,IDVIG
        COMMON /ZREFID/ IDCCR2,IDCCR6,IDCCR8,IDCCR9,IDCCRC,IDPHC,
     *       IDVIG
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
C LOCAL VARIABLES
C
        CHARACTER*160 CONTXT
C                                    --->A message
        DOUBLE PRECISION COEF(8)
C                                    --->Dispersion coefficients
        DOUBLE PRECISION TEMPS(3)
C                                    --->Observations temperature
        CHARACTER*8 TNAMES(3)
C                                    --->Which temperatures to use
        DOUBLE PRECISION TCS(3)
C                                    --->Thermal motion constants
        DOUBLE PRECISION JD(3)
C                                    ---> Time motion constants
        INTEGER CLAST
C                                    --->previous carrousel position processed
        INTEGER I
C                                    --->loop index
        INTEGER GRATRW
C                                    --->Row of CCR6 to find information.
        DOUBLE PRECISION CAP(2)
C                                    --->CAP(1)=CAP_A, CAP(2)=CAP_C
C
C----------------------------------------------------------------------------
C
C First call?
C
        IF(PASS.EQ.FIRST)THEN
           CLAST = -999
C
C Determine spectral order
C
           CALL ZCLORD(PASS,CCR5,GRAT,SCLAMP,BINIDS,CARPOS,YDEF,
     &          FGWC,CAP,ORDER,ISTAT)
           IF(ISTAT.NE.0) GO TO 999
           WRITE(CONTXT,199)ORDER
 199       FORMAT('Spectral order =',I3)
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C write ORDER to output header
C
           CALL UHDPSI(IDS(8),'SPORDER',ORDER,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,208)
 208          FORMAT('ERROR adding spectral order ',
     &             'to the output header')
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
           ENDIF
C
C Open the coefficient tables.
C
           IF(FGWC.EQ.'PERFORM')THEN
              WRITE(CONTXT,217)CCRC
 217          FORMAT('Reading global wavelength coefficients from',
     &             ' CCRC table ',a64)
              CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
              CALL UTTOPN(CCRC,RDONLY,IDCCRC,ISTAT)
              IF(ISTAT.NE.0)THEN
                 WRITE(CONTXT,209)CCRC
 209             FORMAT('Error reading CCRC table ', a64)
                 CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,I)
                 GO  TO 999
              ENDIF
C
C Determine row of CCRC table to use.
C
              CALL ZRCROW(IDCCRC,GRAT,GRATRW,ISTAT)
              IF(ISTAT.NE.0)GO TO 999
C
           ELSE
              WRITE(CONTXT,219)CCR6
 219          FORMAT('Reading dispersion coefficients from',
     &             ' CCR6 table ',a64)
              CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
              CALL UTTOPN(CCR6,RDONLY,IDCCR6,ISTAT)
              IF(ISTAT.NE.0)THEN
                 WRITE(CONTXT,211)CCR6
 211             FORMAT('Error reading CCR6 table ', a64)
                 CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,I)
                 GO  TO 999
              ENDIF
           ENDIF
C
C read thermal motion constants
C
           CALL ZRCCR7(CCR7,GRAT,TNAMES,TCS,JD,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,109)
 109          FORMAT('Some thermal/time motion ',
     &             'correction factors may not be used.')
              CALL ZMSPUT(CONTXT,STDERR+STDOUT,0,ISTAT)
           ENDIF
C
C read the observation's temperature
C
           CALL ZGTEMP(IDS(1),TNAMES,TEMPS)
C
C End of first pass initializations.
C
        ENDIF
C
C If new carrousel position, recompute dispersion.
C
        IF(CARPOS.NE.CLAST) THEN
C
C Get dispersion coefficients
C
           IF(FGWC.EQ.'PERFORM')THEN
              CALL ZGLOBC(PASS,IDCCRC,GRATRW,CARPOS,TNAMES,TCS,
     &             JD,TEMPS,CAP,COEF,ISTAT)
           ELSE
              WRITE(CONTXT,389)TCS(1),TNAMES(1),TEMPS(1)
 389          FORMAT('Thermal motion factor=',F8.5,
     *             ' for obs. temperature ',A8,'=',F6.1)
              CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
              CALL ZRCCR6(PASS,IDCCR6,GRAT,CARPOS,TNAMES(1),TCS(1),
     &             TEMPS(1),COEF,ISTAT)
           ENDIF
           IF(ISTAT.NE.0)GO TO 999
           WRITE(CONTXT,399)CARPOS
 399       FORMAT('Dispersion coefficients for carrousel position',
     *          I7)
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
           DO 10 I=1,8
              WRITE(CONTXT,499)I-1,COEF(I)
 499          FORMAT('   A',I1,'=',G25.16)
              CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
 10        CONTINUE
           CLAST=CARPOS
        ENDIF
C
C Determine wavelengths
C
        DO 100 I=1,NSPEC
                CALL ZCLWAV(GRAT,ORDER,CARPOS,CAP,COEF,SAMPLE(I),
     *                          DELTAS(I),NS,WAVE(1,I),ISTAT)
                IF(ISTAT.NE.0) GO TO 999
100     CONTINUE
C
C If last pass, close the wavelength relation tables.
C
        IF(PASS.EQ.LAST)THEN
           IF(FGWC.EQ.'PERFORM')THEN
              CALL UTTCLO(IDCCRC,ISTAT)
           ELSE
              CALL UTTCLO(IDCCR6,ISTAT)
           ENDIF
        ENDIF
C
        ISTAT=0
999     RETURN
        END
