      DOUBLE PRECISION FUNCTION THETAG(EP,D) 
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NORAD TLES" based ephemeris program (NASA)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
c
c 1- convert Epoch time (YYYYddd.fracofday) to days since 1950 Jan 0 0H UTC 
c 2- calc RA at Greenwich at epoch (in rad)
c  
      INTEGER*4 JY,N,I 

      REAL*8 D,THETA,TWOPI,YR,TEMP,EP

      REAL*8 XMO,XNODEO,OMEGAO,EO,XINCL,XNO,XNDT2O,
     &           XNDD6O,BSTAR,X,Y,Z,XDOT,YDOT,ZDOT
      REAL*8 EPOCH,DS50
      COMMON /E1/EPOCH,DS50,XMO,XNODEO,OMEGAO,EO,XINCL,XNO,XNDT2O,
     &           XNDD6O,BSTAR,X,Y,Z,XDOT,YDOT,ZDOT

c      TWOPI=6.28318530717959D0 
       TWOPI = 8.D0*DATAN(1.D0) 

      YR=(EP+2.D-7)*1.D-3 
      JY=YR 
      YR=JY 
      D=EP-YR*1.D3 
      IF(JY.LT.10) JY=JY+80 
      N=(JY-69)/4 
      IF(JY.LT.70) N=(JY-72)/4 
      DS50=7305.D0 + 365.D0*(JY-70) +N + D 
c      JDS50 = 2449718-2433282+D
c      type *,'DS50,JDS50',DS50,JDS50

      THETA=1.72944494D0 + 6.3003880987D0*DS50 
      TEMP=THETA/TWOPI 
      I=TEMP 
      TEMP=I 
      THETAG=THETA-TEMP*TWOPI 
      IF(THETAG.LT.0.D0) THETAG=THETAG+TWOPI 

      RETURN 
      END 

