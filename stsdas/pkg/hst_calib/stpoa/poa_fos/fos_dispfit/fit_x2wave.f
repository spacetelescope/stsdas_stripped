      SUBROUTINE FIT_X2WAVE(IND,X,NP,PARAM,Y1,DERIV)
*
*  Module number:
*
*  Module name: FIT_X2WAVE
*
*  Keyphrase:
*  ----------
*       user defined function fit fitting
*
*  Description:
*  ------------
*       
*
*  FORTRAN name: fit_x2wave.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  
*
*  Subroutines Called:
*  -------------------
*
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       Nov 01  A. Alexov       Copy of user03.for from midas
*------------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C.VERSION: 1.0  ESO-FORTRAN Conversion, AA  16:03 - 20 DEC 1987
C.LANGUAGE: F77+ESOext
C
C.AUTHOR: M.R. Rosa (STECF)
C--------------------------------------------------------------
C The final FOS dispersion tan(SIN) including S distortion
C saved as tfb_user03.for
C
C
C Dependent: observed: y1 (diode position [0:512])
C Independent:        x  (lambda)
C
C Dispersion relation for FOS:
C 
C Inverse:
C Full            y-y0 = f(lambda) ; y0 is optical axis on diode array
C Grating only    z-z0 = g(lambda) ; z0 is optical axis on cathode
C Digicon         y-y0 = d(z-z0)   
C
C Forward:
C Full            lambda = f'(y-y0) ; y0 is optical axis on diode array
C Grating only    lambda = g'(z-z0) ; z0 is optical axis on cathode
C Digicon         z-z0   = d'(y-y0)   
C
C 1) 
C SIN for dispersion relations lambda = f(y); a=gra.const.=m/s
C c = half config angle; b = grating angle; d = y0; e=focal l.
C
C Inverse:  z = d + e * tan( b+c + arcsin (lambda/a - sin(b-c))]  
C  + S-Distortion: 
C   In extenso:   y-y0 = p(z-z0-r0) + q*abs(tan(s*(z-z0-r0))
C   Simplified:   y = o + p*z + q*abs(tan(s*(z-r0))
C     o = y0; p = linear scale factor; q = amplitude of distortion;
C     s = scale length of distortion; r0 = distance Digicon-optical axis;
C     r = r0+z0.
C
C Forward: 
C  y --> z :  y = o + p*z + q*abs(tan(s*(z-r0))
C             z + q*abs(tan(s*(z-r0) = (y-o)/p 
C             |q/p*tan(s*(z-r0)| << |z|  --> determine z by iteration    
C
C  z --> lambda: z = d + e*tan(b+c + arcsin(lambda/a - sin(b-c)))
C                (z-d)/e = tan(b+c + arcsin(lambda/a - sin(b-c)))
C                arctan((z-d)/e) = b+c + arcsin(lambda/a - sin(b-c))
C                arcsin(lambda/a-sin(b-c))) = arctan((z-d)/e) -(b+c)  
C                lambda = a*(sin(b-c) + sin(arctan((z-d)/e) -(b+c)))
C     
      IMPLICIT NONE
      INTEGER NP,IND,ITER
      REAL X
      DOUBLE PRECISION Y1,PARAM(NP),DERIV(NP),S,T,U,V,W,XX
      DOUBLE PRECISION PI,Z,Z1,Z2,ZL,F,FL,RT,DZ,XP,SWAP

      DATA PI/3.1415926535897932384626433D0/ 
      DATA Z1,Z2/-100.0D0,600.0D0/
      XX     = DBLE(X)
C
C Obtain z at x for f(z) =  z + q*tan(s(z-r)) -(x-o)/p = 0
C We know that -52 < z < 578 (10% margin on 0-516) 
C We know that |q*tan(s(z-r) - (x-o)/p| << |z|
C Function is smooth, so we use secant method and
C precalculate a bracket using zg = (x-o)/p
C We want final epsilon < 1E-5 * z
C
      XP = XX-PARAM(6)
      Z1 = XP/PARAM(7)+50.
      Z2 = XP/PARAM(7)-50. 
 10   FL = Z1+( PARAM(8)*DTAN(PARAM(10)*(Z1-PARAM(9)))
     1         -XP)/PARAM(7)
      F  = Z2+( PARAM(8)*DTAN(PARAM(10)*(Z2-PARAM(9)))
     1         -XP)/PARAM(7)
C Pick bound with smaller function value as most recent guess
      IF(DABS(FL).LT.DABS(F)) THEN
        RT = Z1
        ZL = Z2
        SWAP = FL
        FL = F
        F = SWAP
      ELSE
        ZL = Z1
        RT = Z2 
      ENDIF
C Secant loop
      ITER=0
 20   DZ = (ZL-RT)*F/(F-FL)
      ITER=1
      ZL = RT
      FL = F
      RT = RT+DZ
      F = RT+( PARAM(8)*DTAN(PARAM(10)*(RT-PARAM(9)))
     1         -XP)/PARAM(7)
C      IF(DABS(DZ).GE.1.0D-10.OR.F.NE.0.0D0) GOTO 20
      IF(DABS(DZ).GE.1.0D-10) GOTO 20

C We have a good guess of z
      Z = RT
C
      S      = PARAM(2)-PARAM(3)             ! b-c
      U      = (Z-PARAM(4))/PARAM(5)         ! 
      T      = DATAND(U)-PARAM(2)-PARAM(3)   ! arctand((z-d)/e) -(b+c))
      V      = PARAM(1)*DCOSD(S)         ! 
      W      = PARAM(1)*DCOSD(T)         ! 

      DERIV(1) = DSIND(S)+DSIND(T) !d/da = sin(b-c)+sin(arctan((z-d)/e)-(b+c))

C     lambda = a*(sin(b-c) + sin(arctan((z-d)/e) -(b+c)))
      Y1       = PARAM(1)*DERIV(1)

      DERIV(2) = V-W                             
      DERIV(3) = -V-W
      DERIV(4) = -W/(1.0D0-U*U)/PARAM(5)
      DERIV(5) = DERIV(4)*U
ccc      write(6,100) X,Z,Y1
ccc 100  FORMAT(1X,3F15.7)

      RETURN
      END
