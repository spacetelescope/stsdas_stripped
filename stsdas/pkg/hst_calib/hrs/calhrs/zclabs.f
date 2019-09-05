        SUBROUTINE ZCLABS(PASS,ABSFIL,NETFIL,GRAT,MINABS,NSPEC,
     *          NS,WAVE,FLAG,FLUX,ERR,EPS,ISTAT)
*
*  Module number:
*
*  Module name: zclabs
*
*  Keyphrase:
*  ----------
*       Convert to absolute flux units
*
*  Description:
*  ------------
*       This routine converts the input flux to absolute flux units
*       by dividing by a sensitivity stored in ABSFIL (sensitivities)
*       and NETFIL (wavelengths for the sensitivities).  Quadtratic
*       interpolation is used within the sensitivity file to compute
*       sensitivities for the input wavelenghts
*
*  FORTRAN name: zclabs.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*       ABSFIL                  I       absolute sensitivity file
*       NETFIL                  I       wavelengths for ABSFIL
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*       zrdabs, zclqtp
*  SDAS:
*       usmput
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 89  D. Lindler      Designed and coded
*      1.1      Sep 91  S. Hulbert	Implememnted PASS flag
*      1.2      Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*-------------------------------------------------------------------------------
*
* INPUTS:
*       pass - integer variable set to 1 for first call, -1 for last
*       absfil - name of the sensitivity file
*       netfil - wavelength file for absfil
*       grat - grating mode
*       minabs - minimum sensitivity to use
*       nspec - number of spectra to process
*       ns - number of data points in the spectra
*       wave - wavelength array
*
* INPUT/OUTPUTS:
*       flag - calibration flag.
*       flux - flux array
*       err - error array
*       eps - data quality array
*
* OUTPUT:
*       istat - error status
*
*-------------------------------------------------------------------------
        INTEGER PASS
        CHARACTER*64 ABSFIL,NETFIL
        CHARACTER*5 GRAT
        INTEGER NSPEC,NS,ISTAT
        DOUBLE PRECISION WAVE(NS,NSPEC)
        CHARACTER*12 FLAG
        REAL FLUX(NS,NSPEC),
     *       ERR(NS,NSPEC),EPS(NS,NSPEC),MINABS
C
C     ZMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C HRS epsilons
C
	INTEGER EPSFIL
	PARAMETER (EPSFIL = 800)
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
C local variables
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        CHARACTER*80 CONTXT
        REAL WSENS(2000),SENS(2000)
C                                    --->sensitivity table
        REAL SINT(2000)
C                                    --->interpolated sensitivities
        CHARACTER*3 APER
C                                    --->target aperture 'SSA' or 'LSA'
        INTEGER NSENS,NWAVE
        REAL W1
C                                    --->last beginning wavelength
        REAL W2
C                                    --->last ending wavelength
        INTEGER I,ISPEC
C
C-----------------------------------------------------------------------
C
C on first call, read sensitivities
C
        IF(PASS.EQ.FIRST)THEN
C
C Determine which sensitivity array to use
C
                APER = 'SSA'
                IF(BINIDS(1).EQ.2) APER='LSA'
                CALL ZRDABS('absfil',ABSFIL,GRAT,APER,
     *               2000,FLAG,SENS,NSENS,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                CALL ZRDABS('netfil',NETFIL,GRAT,APER,
     *               2000,FLAG,WSENS,NWAVE,ISTAT)
                IF(ISTAT.NE.0)GO TO 999
                IF(NSENS.NE.NWAVE)THEN
                    CONTXT='ERROR: ABSFILE size does not agree with '
     *                          //'NETFILE'
                    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                    ISTAT=1
                    GO TO 999
                ENDIF
                W1 = -1.0
C                                    --->last first wavelength point
                W2 = -1.0
        ENDIF

C
C Loop on spectra
C
        DO 100 ISPEC=1,NSPEC
C
C Do we need to interpolate new sensitivities
C
            IF(WAVE(1,ISPEC).EQ.0.0)GO TO 100
C                                    --->wavelenghts computed?
            IF((WAVE(1,ISPEC).NE.W1).OR.
     *                          (WAVE(NS,ISPEC).NE.W2) )THEN
                CALL ZCLQTP(NSENS,WSENS,SENS,NS,WAVE(1,ISPEC),SINT)
                W1=WAVE(1,ISPEC)
                W2=WAVE(1,ISPEC)
            ENDIF
C
C divide by the sensitivity
C
            DO 50 I=1,NS
                IF(EPS(I,ISPEC).NE.EPSFIL)THEN
                    IF(SINT(I).GE.MINABS)THEN
                            FLUX(I,ISPEC)=FLUX(I,ISPEC)/SINT(I)
                            ERR(I,ISPEC)=ERR(I,ISPEC)/SINT(I)
                        ELSE
                            FLUX(I,ISPEC)=0.0
                            ERR(I,ISPEC)=0.0
                    ENDIF
                ENDIF
50          CONTINUE
100     CONTINUE
        IF(PASS.EQ.FIRST)THEN
            CONTXT='Conversion to abs. flux for aperture '//APER//
     *         ' using file '//ABSFIL
            CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
        ENDIF
        ISTAT=0
999     RETURN
        END
