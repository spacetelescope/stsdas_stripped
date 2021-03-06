C
      SUBROUTINE YMODPRSM(XX,W,PARAM)
*
*  Module number:
*
*  Module name: YMODPRSM
*
*  ------------------------------------------------------------------------
*  Part of "poa_calfos" (STECF/IPMG)
*  ------------------------------------------------------------------------
*
*  Keyphrase:
*  ----------
*       Modify the dispersion solution.
*
*  Description:
*  ------------
*  This is the final FOS dispersion model including S distortion
*  for the PRISM
*
*  Optical path is:   Prism --> Photocathode --> Diodes
*    
*  Between Prism and Photocathode: projection lamdba --> z
*  For prisms:  lambda = f(z); z = g(lambda);
*               a: Cauchy A; b = Cauchy B  (material constants)
*               c: effective incident angle
*               d: offset z0; e: focal length
*
*  Inverse relation:
*      z = d + e * [ b+c + arcsin (s/m * lambda - sin(b-c))]  
* 
*  Between Photocathode and Diodes: projection z --> x
*  For electro-magneto-optic in good approximation
*                      x = o + pz + q*tan(r+sz)
*
*  Solving the double projection by first finding z = h(x)
*  through iteration, then  lambda = f(z) ; need dble precision !!
*
*  From forward relation  x --> z ; pz + q*tan(r+sz) = x-o
*                         z + q/p*tan(r+sz) = (x-o)/p
*     since |q/p*tan(r+sz)| << |z|  --> determine z by iteration    
*
*  then:    lambda = m/s [sin(b-c) + sin((z-d)/e - (b+c))] 
* 
*  
*
*  FORTRAN name: ymodprsm.for
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*      
*                 I       
*
*  Subroutines Called:
*  -------------------
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1.1   Apr 10,2001   M. Rosa      Designed and coded
*-----------------------------------------------------------------------------

      IMPLICIT NONE
      REAL   W
      DOUBLE PRECISION PARAM(10),S,P2,Q,WW,XX
      DOUBLE PRECISION PI,Z,Z1,Z2,ZL,F,FL,RT,DZ,XP,SWAP
      INTEGER ITER

C      INTEGER ISTAT
C      INTEGER STDOUT
C      PARAMETER (STDOUT = 1)
C      INTEGER STDERR
C      PARAMETER (STDERR = 2)
C      CHARACTER*80 CONTXT

C      COMMON DISPCOEF /NP,PARAM/

      DATA PI/3.1415926535897932384626433D0/ 
      DATA Z1,Z2/-100.0D0,600.0D0/

C The final FOS dispersion tan(SIN) including S distortion
C saved as tfb_user03.for
C
C
C Dependent: observed: y1 (diode position [0:512])
C Independent:        x  (lambda)
C
C Dispersion relation for FOS:          PRISM
C 
C Inverse:
C Full            y-y0 = f(lambda) ; y0 is optical axis on diode array
C Prism           z-z0 = g(lambda) ; z0 is optical axis on cathode
C Digicon         y-y0 = d(z-z0)   
C
C Forward:
C Full            lambda = f'(y-y0) ; y0 is optical axis on diode array
C Prism           lambda = g'(z-z0) ; z0 is optical axis on cathode
C Digicon         z-z0   = d'(y-y0)   
C
C 1) 
C SIN for dispersion relations lambda = f(y); a=gra.const.=m/s
C c = half config angle; b = grating angle; d = y0; e=focal l.
C
C Inverse:  z = d + e * tan( c + (a/lambda^2 + b/lambda^4))
C  + S-Distortion: 
C   In extenso:   y-y0 = p(z-z0-r0) + q*abs(tan(s*(z-z0-r0))
C   Simplified:   y = o + p*z + q*abs(tan(s*(z-r0))
C     o = y0; p = linear scale factor; q = amplitude of distortion;
C     s = scale length of distortion; r0 = distance Digicon-optical axis;
C     r = r0+z0.
C
C Forward: 
C  go from diode array back to photocathode
C  y --> z :  y = o + p*z + q*abs(tan(s*(z-r0))
C             z + q*abs(tan(s*(z-r0) = (y-o)/p 
C             |q/p*tan(s*(z-r0)| << |z|  --> determine z by iteration    
C
C  z --> lambda: z = d + e*tan(c + a/lambda^2 +b/lambda^4)
C                (z-d)/e = tan(c + a/lambda^2 +b/lambda^4)
C                arctan((z-d)/e) = a/lamda^2 + b/lambda^4
C                lambda^4*arctan((z-d)/e) - a*lamda^2 - b = 0
C                p = -a/arctan((z-d)/e)         
C                lambda = a*(sin(b-c) + sin(arctan((z-d)/e) -(b+c)))
C     

C
C Obtain Z for XX using    z + (q*tan(r+sz) -(x-o))/p = 0
C We know that -52 < z < 578 (10% margin on 0-516 diode array) 
C We know that |(q*tan(r+sz) -(x-o))/p| << |z|  (distortion small)
C Function is smooth, so we use secant method and
C precalculate a bracket using zg = (x-o)/p
C We want final epsilon to be < 1E-5 * z
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
      IF(XX.LT.25.0)THEN
         WW=100000.0
      ELSE
         S      = PARAM(3)*PI/180.0D0        !  rad(c)   
C roots of the biquadratic equation
         P2     = -PARAM(1)/2.0D0/(DATAN((Z-PARAM(4))/PARAM(5))-S)
         Q      = -PARAM(2)/(DATAN((Z-PARAM(4))/PARAM(5))-S)

         WW     = SQRT(-P2+DSQRT(P2*P2-Q))
      ENDIF
      W      = SNGL(WW)
      
C Finished, got lambda from diode

C          WRITE(CONTXT, 800), XX,Z,WW
C 800      FORMAT('XX = ', F17.7, ', Z = ', F17.7, ', WW = ', F17.7)
C          CALL YMSPUT(CONTXT,STDOUT+STDERR,0,ISTAT)

      RETURN
      END
