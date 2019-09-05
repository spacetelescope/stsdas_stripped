        SUBROUTINE GMPGMP(XSCALE,YSCALE,XOFF,YOFF,NSPEC,ISTAT)
*
*  Module number:
*
*  Module name: GMPGMP
*
*  Keyphrase:
*  ----------
*       Perform FOS GIMP correction
*
*  Description:
*  ------------
*       This routine calculates and applies the GIMP correction.
*       Uses spherical harmonics model of earth's magnetic field.
*	The output is an offset in the fos x-direction.
*
*  FORTRAN name: GMPGMP.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments

*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91	S. Hulbert      Designed and coded
*     1.1       Jun 91	S. Hulbert      added y-offset     
*     1.2       Aug 91	S. Hulbert      change sign of y-offset     
*     1.3       Jul 92  D. Bazell       Fix bug in dyn. mem. deallocation
*     1.4       Mar 93  J. Eisenhamer   Ripped from CALFOS just to do gimp.
*     1.5      Dec 2000 M. De La Pena   Added KYDPLY to CONFG1 common.
*-------------------------------------------------------------------------------
*
* INPUTS:
*	xscale - scale factor for x-offsets as a function of detector
*	yscale - scale factor for y-offsets as a function of detector
*	nspec - number of spectra in a frame
*
* INPUT/OUTPUT:
*
* OUTPUT:
*	xoff - array of offsets in fos x-direction
*	yoff - array of offsets in fos y-direction
*       istat - error status
*
*----------------------------------------------------------------------------
        INTEGER NSPEC, ISTAT, ISTATS(10)
        REAL XSCALE, YSCALE, XOFF(NSPEC), YOFF(NSPEC)
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
C------------------------------------------------------------------------------
      INTEGER TYDOUB
      PARAMETER (TYDOUB = 7)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.1415926535898)
C
C Common block containing confiquration parameters
C
        CHARACTER*5 DET
        CHARACTER*3 FGWAID,APERID,YTYPE(3)
        CHARACTER*1 POLID
        INTEGER FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM
        LOGICAL HEADER,TRAILR,DEFDDT
        COMMON /CONFG1/KYDPLY,DET,FGWAID,APERID,POLID,YTYPE
        COMMON /CONFG2/FCHNL,NCHNLS,OVERSN,NXSTEP,YBASE,YRANGE,YSTEPS,
     *          INTS,SLICES,NPAT,NREAD,NCLEAR,LIVETM,HEADER,TRAILR,
     *          DEFDDT
        INTEGER DEADTM
        COMMON /CONFG5/DEADTM
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
C LOCAL VARIABLES ------------------------------------------------------
        CHARACTER*80 CONTXT
        DOUBLE PRECISION FPKTIM
        DOUBLE PRECISION EXPYST, EXPRDT
        DOUBLE PRECISION BV1,BV2,BV3
        INTEGER MIDTIM, XPOS, YPOS, ZPOS, LAT, LNG, ST, BCOMPS
C
C first element for amber detector
C second element for blue detector
C angle b: rotation about y-axis
C angle c: rotation about x-axis
C angle d: rotation about z-axis
C angle e: EXB electronoptic drift angle
C xfactor: motion (diodes/gauss)   (from ccs7)
C yfactor: motion (ybase units/gauss)  (from ccs7)
C
        DOUBLE PRECISION ANGLEB(2), ANGLEC(2), ANGLED(2), ANGLEE(2) 
C        DOUBLE PRECISION XFACTOR(2), YFACTOR(2)
        DATA ANGLEB / -8.0,  8.0/
        DATA ANGLEC /-23.0,-23.0/
        DATA ANGLED /135.0,135.0/
        DATA ANGLEE / 17.6, 17.6/
C        DATA XFACTOR / 2.95,  0.7/
C        DATA YFACTOR /189.0, 45.0/
C
C-----------------------------------------------------------------------
C
C get an index based on the detector for use later
C
        IF (DET .EQ. 'AMBER') THEN
            ID = 1
        ELSE IF (DET .EQ. 'BLUE ') THEN
            ID = 2
        ELSE
            CONTXT = 'Invalid detector encountered during deGIMPing'
            GO TO 999
        ENDIF
C
C get first packet time from the d0h file
C
        CALL UHDGSD(IDS(1),'FPKTTIME',FPKTIM,ISTAT)
C
C determine exposure time for each ystep and readout in days
C
        EXPYST = (LIVETM+DEADTM)*7.8125e-6*OVERSN*NXSTEP*
     $                NPAT*INTS/24./3600.
        EXPRDT = EXPYST * NSPEC
C
C compute observation time midpoint of each ystep/slice
C
        CALL UDMGET (NSPEC, TYDOUB, MIDTIM, ISTAT)
        IF(ISTAT.NE.0)THEN
                CONTXT='ERROR allocating dynamic memory'
                GO TO 999
        ENDIF
C
C Don says: "if npat greater then 1 then all ysteps and slices
C for a readout will be at the midpoint of the readout. If npat=1 then
C the readout will be divided into YSTEPS*SLICES intervals"
C
        DO 100 I = 1, NSPEC
            IF (NPAT .GT. 1) THEN
                MEMD(MIDTIM+I-1) = FPKTIM - EXPRDT/2.
            ELSE
                MEMD(MIDTIM+I-1) = FPKTIM - EXPRDT + 
     $                             (2.*I-1.)/2. * EXPYST
            ENDIF
100     CONTINUE
C
C compute spacecraft position at midpoint 
C
        CALL UDMGET (NSPEC, TYDOUB, XPOS, ISTATS(1))
        CALL UDMGET (NSPEC, TYDOUB, YPOS, ISTATS(2))
        CALL UDMGET (NSPEC, TYDOUB, ZPOS, ISTATS(3))
        CALL UDMGET (NSPEC, TYDOUB, LNG, ISTATS(4))
        CALL UDMGET (NSPEC, TYDOUB, LAT, ISTATS(5))
        CALL UDMGET (NSPEC, TYDOUB, ST, ISTATS(6))
        DO 150 I = 1, 6
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR allocating dynamic memory'
                GO TO 999
            ENDIF
150        CONTINUE
        CALL HSTPOS(MEMD(MIDTIM),NSPEC,MEMD(XPOS),MEMD(YPOS),
     $                MEMD(ZPOS),MEMD(LNG),MEMD(LAT),MEMD(ST),ISTAT)
C
C compute earth's magnetic field at spacecraft positions
C BCOMPS is a (3,nspec) array
C BCOMPS(1,I)=V2, BCOMPS(2,I)=V3, BCOMPS(3,I)=V1 for the Ith spectrum
C
        CALL UDMGET (3*NSPEC, TYDOUB, BCOMPS, ISTAT)
        IF(ISTAT.NE.0)THEN
            CONTXT='ERROR allocating dynamic memory'
            GO TO 999
        ENDIF
        CALL YMAGFD(MEMD(MIDTIM),MEMD(XPOS),MEMD(YPOS),MEMD(ZPOS),
     $          MEMD(LNG),MEMD(LAT),MEMD(ST),NSPEC,MEMD(BCOMPS),ISTAT)
C
C free memory
C
        CALL UDMFRE (MIDTIM, TYDOUB, ISTATS(1))
        CALL UDMFRE (XPOS, TYDOUB, ISTATS(2))
        CALL UDMFRE (YPOS, TYDOUB, ISTATS(3))
        CALL UDMFRE (ZPOS, TYDOUB, ISTATS(4))
        CALL UDMFRE (LNG, TYDOUB, ISTATS(5))
        CALL UDMFRE (LAT, TYDOUB, ISTATS(6))
        CALL UDMFRE (ST, TYDOUB, ISTATS(7))
        DO 160 I = 1, 7
            IF(ISTATS(I).NE.0)THEN
                CONTXT='ERROR deallocating dynamic memory'
                GO TO 999
            ENDIF
160        CONTINUE
C
C convert from v1/v2/v3 to fos coordinates
C
C convert anglee to radians for use later
C
        THETA = ANGLEE(ID) * PI/180.
C
        DO 200 I = 1, NSPEC
C convert to FOS coordinates
C
           BV1=MEMD(BCOMPS+(I-1)*3+2)
           BV2=MEMD(BCOMPS+(I-1)*3)
           BV3=MEMD(BCOMPS+(I-1)*3+1)
            CALL ROTATE (MEMD(BCOMPS+(I-1)*3),-ANGLED(ID),3)
            CALL ROTATE (MEMD(BCOMPS+(I-1)*3),ANGLEC(ID),1)
            CALL ROTATE (MEMD(BCOMPS+(I-1)*3),ANGLEB(ID),2)
C
C rotate for EXB electron optical drift
C
            BX = MEMD(BCOMPS+(I-1)*3)
            BY = MEMD(BCOMPS+(I-1)*3+1)
            BEFFX = COS(THETA) * BX - SIN(THETA) * BY
            BEFFY = COS(THETA) * BY + SIN(THETA) * BX
            XOFF(I) = XSCALE * BEFFX
C
C calculate y offset (flip sign because somewhere we goofed)
C
            YOFF(I) = -YSCALE * BEFFY
C
C write out the results.
C
            IF (.NOT.(I.GT.1.AND.POLID.NE.' ')) THEN
               WRITE(CONTXT,2001)XOFF(I),YOFF(I),BV1,BV2,BV3
 2001          FORMAT(F10.4,1x,F10.4,3x,F10.6,1x,F10.6,1x,
     $                F10.6,1x,F10.6)
               CALL YMSPUT(CONTXT,STDOUT,0,ISTAT)
            ENDIF
200     CONTINUE
C
C free buffer
C
        CALL UDMFRE (BCOMPS, TYDOUB, ISTAT)
C
        ISTAT=0
        GOTO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
1000    RETURN
        END
