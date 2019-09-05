      DOUBLE PRECISION FUNCTION ACTAN(SINX,COSX)
c
c  Calculate angle from sin and cos in correct quadrant
c
c  ------------------------------------------------------------------------
c
c  Called from SGP4, SGP8
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NORAD TLE" based ephemeris program  (NASA)
c 
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
      REAL*8 SINX,COSX,TEMP 

      REAL*8 PI,PIO2,TWOPI,X3PIO2 
      REAL*8 DE2RA
      COMMON /C2/ DE2RA,PI,PIO2,TWOPI,X3PIO2 

      ACTAN=0. 
      IF (COSX.EQ.0. ) GOTO 5 
      IF (COSX.GT.0. ) GOTO 1 
      ACTAN=PI 
      GOTO 7 
1     IF (SINX.EQ.0. ) GOTO 8 
      IF (SINX.GT.0. ) GOTO 7 
      ACTAN=TWOPI 
      GOTO 7 
5     IF (SINX.EQ.0. ) GOTO 8 
      IF (SINX.GT.0. ) GOTO 6 
      ACTAN=X3PIO2 
      GOTO 8 
6     ACTAN=PIO2 
      GOTO 8 
7     TEMP=SINX/COSX 
      ACTAN=ACTAN+ATAN(TEMP) 
8     RETURN 
      END 
