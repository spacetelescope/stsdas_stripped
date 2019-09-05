      DOUBLE PRECISION FUNCTION FMOD2P(X) 
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NORAD TLES" base ephemeris program (NASA)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
c
c return angle (in rad) in range 0 < angle < 2pi
c 
      INTEGER*4 I
      REAL*8 X

      REAL*8 DE2RA,PI,PIO2,TWOPI,X3PIO2 
      COMMON /C2/ DE2RA,PI,PIO2,TWOPI,X3PIO2 

      FMOD2P=X 
      I=FMOD2P/TWOPI 
      FMOD2P=FMOD2P-I*TWOPI 
      IF(FMOD2P.LT.0) FMOD2P=FMOD2P+TWOPI 
      RETURN 
      END 

