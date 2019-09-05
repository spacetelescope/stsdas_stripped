C 	Copyright restrictions apply - see stsdas$copyright.stsdas 
C 
        SUBROUTINE YMAGFD(TIME,X,Y,Z,LNG,LAT,ST,NSPEC,B,ISTAT)
*
*  Module number:
*
*  Module name: YMAGFD
*
*  Keyphrase:
*  ----------
*       Calculate earth's magnetic field in v1-v2-v3 coordinates
*
*  Description:
*  ------------
*       This routine calculates the calculate earth's magnetic 
*	field in v1-v2-v3 coordinates
*
*  FORTRAN name: YMAGFD.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments
*
*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91	S. Hulbert      Designed and coded
*     1.1       Jul 92  D. Bazell       Fixed bug in dyn. mem. deallocation
*-------------------------------------------------------------------------------
*
* INPUTS:
*	time -  array of observation MJD's
*	x - array of x positions of HST (meters)
*	y - array of y positions of HST (meters)
*	z - array of y positions of HST (meters)
*	long - array of longitudes (degrees) 
*	lat - array of latitudes (degrees)
*	st - array of sidereal times (radians)
*	nspec - number of array elements = number of spectra in a frame
*
* INPUT/OUTPUT:
*
* OUTPUT:
*	b - 2d array of magnetic field components in v1-v2-v3 coord.dd
*       istat - error status
*
*----------------------------------------------------------------------------
        INTEGER NSPEC, ISTAT
        DOUBLE PRECISION TIME(NSPEC), X(NSPEC), Y(NSPEC), Z(NSPEC)
        DOUBLE PRECISION LNG(NSPEC), LAT(NSPEC), ST(NSPEC)
        DOUBLE PRECISION B(3,NSPEC)
C
C     UMSPUT DESTINATIONS -- CB, DAO, 4-SEP-87
C
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
C
      INTEGER TYDOUB
      PARAMETER (TYDOUB = 7)
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
C
C Common block containing orbital/positional parameters
C
        DOUBLE PRECISION EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI
        DOUBLE PRECISION ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3
        DOUBLE PRECISION FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER
        DOUBLE PRECISION RASCASCN,SINEINCL,SEMILREC
        DOUBLE PRECISION PSANGLV3,RTASCNV1,DECLNV1
        COMMON /ORBPAR/ EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI,
     $  ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3,
     $  FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER,
     $  RASCASCN,SINEINCL,SEMILREC,
     $  PSANGLV3,RTASCNV1,DECLNV1
C
        REAL REARTH
        PARAMETER (REARTH = 6371.2)
        DOUBLE PRECISION PI
        PARAMETER (PI = 3.1415926535898)
C
C LOCAL VARIABLES ------------------------------------------------------
C
        CHARACTER*80 CONTXT
        INTEGER ALT, I
        DOUBLE PRECISION LST
C
C-----------------------------------------------------------------------
C
C allocate memory for altitude
C
        CALL UDMGET (NSPEC, TYDOUB, ALT, ISTAT)
        IF (ISTAT .NE. 0) THEN
            CONTXT = 'Error allocating dynamic memory'
            GO TO 999
        ENDIF
C
C calculate altitude in kilometers
C
        DO 100 I = 1, NSPEC
            MEMD(ALT+I-1) = SQRT(X(I)**2+Y(I)**2+Z(I)**2) /
     $                        1000.0 - REARTH
100        CONTINUE
C
C calculate earth's magnetic field 
C
        CALL BFIELD(TIME,MEMD(ALT),LNG,LAT,NSPEC,B,ISTAT)
C
C convert from north/east/down coordinate system to v1v2v3
C
        DO 200 I = 1, NSPEC
C
            B(3,I) = -B(3,I)
            CALL ROTATE(B(1,I),180.0D0,3)
            CALL ROTATE(B(1,I),LAT(I)-90.0D0,2)
            B(2,I) = -B(2,I)
            LST = ST(I)*180./PI + LNG(I)
            CALL ROTATE(B(1,I),-LST,3)
C
            CALL ROTATE(B(1,I),RTASCNV1,3)
            CALL ROTATE(B(1,I),90.0D0-DECLNV1,2)
            CALL ROTATE(B(1,I),90.0D0-PSANGLV3,3)
            B(3,I) = -B(3,I)
C
200        CONTINUE
C
           CALL UDMFRE (ALT, TYDOUB, ISTAT)
           IF (ISTAT.NE.0) THEN
              CONTXT = 'Error deallocating dynamic memory'
              GOTO 999
           ENDIF

        ISTAT=0
        GOTO 1000
999     CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)
        ISTAT=1
1000    RETURN
        END
