      SUBROUTINE ZCLBLM(PASS,CCRD,NSPEC,NS,EPSM)
*
* Module number:
*
* Module name: ZCLBLM
*
* Keyphrase:
* ---------
*     Flag photocathode blemishes
*
* Description:
* -----------
*     This routines modifies the data quality array to flag those areas
*     of the spectrum that may be affected by photocathode blemishes.
*     The regions where blemishes exist is located in the table CCRD.
*     Using the line/mapping parameters, the areas where the spectrum
*     may be affected are flagged.
*
* FORTRAN name: ZCLBLM.FOR
*
* Subroutines Called:
* ------------------
*
* History:
* -------
*  Version      Date        Author          Description
*       1       Feb94     J.Eisenhamer    Created
*      1.1      Nov 96  M. De La Pena   Added STPTIM to HRSDEF
*      1.2      Jun 98  M. De La Pena   Fixed syntax on FORMAT statements
*----------------------------------------------------------------------
*
* Input paramters
*
*     pass - integer flag set to 1 on first call, -1 on last
*     ccrd - Name of the CCRD reference table
*     nspec - Number of spectrum to operate on.
*     ns - Size of the spectrum
*
* Input/Output paramters
*
*     epsm - The epsilon vector/data quality flags.
*
***********************************************************************
      INTEGER PASS,NSPEC,NS
      CHARACTER*64 CCRD
      REAL EPSM(NS,NSPEC)
C
      INTEGER TYINT
      PARAMETER (TYINT=4)
      INTEGER TYREAL
      PARAMETER (TYREAL=6)
C
C     FILE I/O ACCESS MODES
C
      INTEGER   RDONLY
      PARAMETER (RDONLY = 1)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
C     THIS SECTION IS FOR PARAMETERS RELEVANT TO TABLE I/O.
C
C
C     THESE MAY BE READ BY UTPGTI BUT MAY NOT BE SET:
C
C                                       NUMBER OF ROWS WRITTEN TO
      INTEGER   TBNROW
      PARAMETER (TBNROW = 21)
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
        INTEGER NCOLS
        PARAMETER (NCOLS=6)
C
C                      /BLMDEF/
C
        INTEGER ID,COLID(NCOLS),SLINE,BSAMP,BLEM
        LOGICAL DOBLEM
        COMMON /BLMDEF/ ID,COLID,SLINE,BSAMP,BLEM,DOBLEM
C
C----------------------------------------------------------------------------
C LOCAL VARIABLES
C
        INTEGER FIRST
        PARAMETER (FIRST = 1)
        INTEGER LAST
        PARAMETER (LAST = -1)
C
        CHARACTER*15 COLNAM(NCOLS)
        CHARACTER*162 CONTXT
        INTEGER ISTAT,STAT(10),I,SPEC
C
        DATA COLNAM /'DETECTOR','LINE1','LINE2','SAMPLE1','SAMPLE2',
     &       'EPSILON'/
C
C First pass initializations
C
        IF(PASS.EQ.FIRST)THEN
           DOBLEM=.TRUE.
           ID=0
           SLINE=0
           BSAMP=0
           BLEM=0
C
C Open the table
C
           IF(CCRD.EQ.' ')THEN
              WRITE(CONTXT,11)
 11           FORMAT('Warning: No CCRD table specified, ',
     &             'blemish marking will not occur.')
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              DOBLEM=.FALSE.
              GO TO 1000
           ENDIF
C
           CALL UTTOPN(CCRD,RDONLY,ID,ISTAT)
           IF(ISTAT.NE.0)THEN
              WRITE(CONTXT,1)CCRD
 1            FORMAT('Error: could not open CCRD table ',a64)
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              WRITE(CONTXT,2)
 2            FORMAT('    Photocathode blemishes will not be flagged')
              CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
              DOBLEM=.FALSE.
              GO TO 1000
           ENDIF
C
C Find the columns
C    
           CALL UTCFND(ID,COLNAM,NCOLS,COLID,ISTAT)
           IF(ISTAT.NE.0)THEN
              DO 100 I = 1, NCOLS
                 IF(COLID(I).EQ.0)THEN
                    WRITE(CONTXT,3)COLNAM(I),CCRD
 3                  FORMAT('Error: could not find column ',a15,
     &                   'in CCRD table ',a64)
                    CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                 ENDIF
 100          CONTINUE
              DOBLEM=.FALSE.
              GO TO 1000
           ENDIF
C
           WRITE(CONTXT,6)CCRD
 6         FORMAT('Flagging photocathode blemishes from CCRD table ',
     &          a64)
           CALL ZMSPUT(CONTXT,STDOUT,0,ISTAT)
C
C Initialize arrays.
C
           SLINE=0
           BSAMP=0
           BLEM=0
           CALL UDMGET(NSPEC,TYREAL,SLINE,STAT(1))
           CALL UDMGET(NSPEC,TYREAL,BSAMP,STAT(2))
           CALL UDMGET(NSPEC*NS,TYREAL,BLEM,STAT(3))
           DO 920 I = 1, NSPEC
              MEMR(SLINE+I-1)=0.
              MEMR(BSAMP+I-1)=0.
 920       CONTINUE
           DO 930 I = 1, NSPEC*NS
              MEMR(BLEM+I-1)=0.
 930       CONTINUE
           DO 900 I = 1, 3
              IF(STAT(I).NE.0)THEN
                 WRITE(CONTXT,4)
 4               FORMAT('Error: could not allocate memory for ',
     &                'photocathode blemish removal.')
                 CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                 WRITE(CONTXT,5)
 5               FORMAT('Photocathode blemish removal will not ',
     &                'be done.')
                 CALL ZMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
                 DOBLEM=.FALSE.
                 GO TO 1000
              ENDIF
 900       CONTINUE
           DO 910 I = 1, NSPEC
              MEMR(SLINE+I-1)=-1
 910       CONTINUE
C
        ENDIF
C
        IF(DOBLEM)THEN
C
C Loop through all the spectra.
C
           DO 2000 SPEC = 1, NSPEC
C
C See if a new epsilon array needs to be calculated.
C
              IF(MEMR(SLINE+SPEC-1).NE.LINE(SPEC).OR.
     &             MEMR(BSAMP+SPEC-1).NE.SAMPLE(SPEC))
     &             CALL ZBLMEP(ID,COLID,DET,LINE(SPEC),SAMPLE(SPEC),
     &             DELTAS(SPEC),MEMR(SLINE+SPEC-1),
     &             MEMR(BSAMP+SPEC-1),
     &             MEMR(BLEM+((SPEC-1)*NS)),NS,ISTAT)
              IF(ISTAT.NE.0)THEN
                 DOBLEM=.FALSE.
                 GO TO 1000
              ENDIF
C
C Merge the epsilons into the observation's data quality.
C
              DO 3000 I = 1, NS
                 EPSM(I,SPEC)=MAX(EPSM(I,SPEC),
     &                MEMR(BLEM+((SPEC-1)*NS)+I-1))
 3000         CONTINUE
C
 2000      CONTINUE
C
        ENDIF
C
C That's all folks
C
 1000   CONTINUE
        IF(PASS.EQ.LAST)THEN
           IF(ID.NE.0) CALL UTTCLO(ID,ISTAT)
           IF(SLINE.NE.0) CALL UDMFRE(SLINE,TYREAL,ISTAT)
           IF(BSAMP.NE.0) CALL UDMFRE(SLINE,TYREAL,ISTAT)
           IF(BLEM.NE.0) CALL UDMFRE(SLINE,TYREAL,ISTAT)
        ENDIF
        RETURN
        END
