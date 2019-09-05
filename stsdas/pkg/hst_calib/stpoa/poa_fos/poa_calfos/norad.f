        SUBROUTINE NORAD(EMIDTIM,X,Y,Z,R,VX,VY,VZ,
     1                   LNG,LAT,RHST,DHST,GMST,ISTAT)
c                                                               
c  Ephemeris of HST using NORAD TLES and "spacetrack" code (SGP4/8)
c
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NORAD TLES" based ephemeris program (NASA)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c      1.2       Jun 01  A. Alexov       Added IDS's common block for file
c                                        header access
c      1.2.1     Oct 01  A. Alexov       Moved "IMPLICIT NONE" statement
c                                        to the beginning to fix Dec Alpha
c                                        compilation error
c  ------------------------------------------------------------------------
c
      IMPLICIT NONE

      INTEGER ISTAT
      INTEGER STDOUT
      PARAMETER (STDOUT = 1)
      INTEGER STDERR
      PARAMETER (STDERR = 2)
c      CHARACTER*80 CONTXT

        REAL*8 EMIDTIM,X,Y,Z,R,VX,VY,VZ,LNG,LAT,RHST,DHST,GMST
        REAL*8 PI,TPI
        PARAMETER (PI = 3.1415926535898D0)
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
        REAL*8 RA, DEC
c        REAL*8 RGREN,RAGREN
c        REAL*8 DELTAPOS
        REAL*8 STRAD,MIDNIT,FRACEN,STMIDN
        INTEGER*4 jepo,ISTAT 
C-----------------------------------------------------------------------
      TPI = 2.0D0*PI
C... spacetrack
      CALL sgp4_driver (EMIDTIM,4,X,Y,Z,VX,VY,VZ,jepo,ISTAT)
      IF(ISTAT.NE.0)THEN
              GO TO 999
      ENDIF

      R = DSQRT(X*X+Y*Y+Z*Z) 
C need sidereal time to compute longitude and latitude, mjd at midnite (days)
      MIDNIT = INT(EMIDTIM)
C fractional centuries since 2000
      FRACEN = (MIDNIT - 51544.5D0)/36525.0D0
C sidereal time at midnite in seconds 
      STMIDN = MOD(24110.54841D0+8640184.812866D0*FRACEN
     1            +0.093104*FRACEN*FRACEN,86400.0D0)+86400.0D0
      GMST = STMIDN/3600.0D0
C sidereal time in radians
      STRAD = TPI/86400.0D0*MOD(STMIDN+((EMIDTIM-MIDNIT)
     1           *86400.0D0)*1.0027379093D0,86400.0D0)
c      GMST = STRAD*180.0D0/PI*24.0D0/360.0D0
      GMST = STMIDN/3600.0D0
C latitude, orgiginal version  OK
      DEC = DASIN(Z/R)
      LAT = 180.0D0/PI*DEC
C longitude (make sure the quadrant is correct!), else original version OK
      RA = DATAN2(Y,X)
      IF (RA .LT. 0.0D0) RA = RA + TPI
      LNG = 180.0D0/PI*(RA - STRAD)      ! this is correct
      IF (LNG .GT. 360.0D0) THEN
        LNG = LNG - 360.0D0
      ELSE IF (LNG .LT. 0.0D0) THEN
        LNG = LNG + 360.0D0
      ENDIF
      RHST = RA*180.0D0/PI
      DHST = LAT 
C
 9999  CONTINUE
      ISTAT=0
      GOTO 1000

 999    ISTAT=1
 1000   RETURN
        END
c-- end of NORAD
