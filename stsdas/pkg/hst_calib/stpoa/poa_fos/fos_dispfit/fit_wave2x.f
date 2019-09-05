      SUBROUTINE FIT_WAVE2X(IND,X,NP,PARAM,Y1,DERIV)
*
*  Module number:
*
*  Module name: FIT_WAVE2X
*
*  Keyphrase:
*  ----------
*       user defined function fit fitting
*
*  Description:
*  ------------
*       
*
*  FORTRAN name: fit_wave2x.for
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
*       1       Nov 01  A. Alexov       Copy of user01.for from midas
*------------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C.VERSION: 1.0  ESO-FORTRAN Conversion, AA  16:03 - 20 DEC 1987
C.LANGUAGE: F77+ESOext
C
C.AUTHOR: M.R. Rosa (STECF)
C--------------------------------------------------------------
C The final FOS dispersion tan(ARCSIN) + S distortion
C saved as tafb_user01.for
C TEST version 1.01 11-3-97 
C
C Dependent observed: y1 (diode position [0:512])
C Independent:        x  (lambda)
C
C Dispersion relation for FOS:
C
C Full            y-y0 = f(lambda) ; y0 is optical axis on diode array
C Grating only    z-z0 = g(lambda) ; z0 is optical axis on cathode
C Digicon         y-y0 = d(z-z0)   
C
C 1)      z = d + e * tan( b + c + arcsin(lambda/a - sin(b-c)))
C    ARCSIN for dispersion relations y = f(lambda); a=gra.const.=m/s
C    c = half config angle; b = grating angle; d = z0; e=focal l.
C
C zz = PARAM(4)+PARAM(5)*DERIV(5) 
C    = PARAM(4)+PARAM(5)*DTAN(PARAM(2)+PARAM(3))*PI/180.0D0+V)
C    = PARAM(4)+PARAM(5)*DTAN(PARAM(2)+PARAM(3))*PI/180.0D0+
C      +DASIN(S))
C    = PARAM(4)+PARAM(5)*DTAN(PARAM(2)+PARAM(3))*PI/180.0D0+
C      +DASIN(XX/PARAM(1)-DSIND(R))
C    = PARAM(4)+PARAM(5)*DTAN(PARAM(2)+PARAM(3))*PI/180.0D0+
C      +DASIN(XX/PARAM(1)-DSIND(PARAM(2)-PARAM(3)))
C 
C 2) Projection of tangent-like thing onto inclined array
C
C   In extenso:   y-y0 = p(z-z0-r0) + q*abs(tan(s*(z-z0-r0))
C   Simplified:   y = o + p*z + q*abs(tan(s*(z-r0))
C
C     o = y0; p = linear scale factor; q = amplitude of distortion;
C     s = scale length of distortion; r0 = distance Digicon-optical axis;
C     r = r0+z0.
C
      IMPLICIT NONE
      INTEGER NP, IND
      REAL X
      DOUBLE PRECISION Y1,PARAM(NP),DERIV(NP),R,S,T,U,V,W
ccc      DOUBLE PRECISION O,P,Q,PI,XX,ZZ,ZW,ZD,ZR
      DOUBLE PRECISION O,P,Q,PI,XX,ZZ,ZW

      DATA PI/3.1415926535897932384626433D0/ 

      XX     = DBLE(X)
      R      = PARAM(2)-PARAM(3)                 ! b-c
      S      = XX/PARAM(1)-DSIND(R)              ! x/a
      T      = 1.0D0/DSQRT(1.0D0-S*S)            ! d(arcsin(s))/ds
      U      = DCOSD(R)                          ! d(sin(r))/dr
      V      = DASIN(S)                          ! arcsin(s) 
      W      = -XX/PARAM(1)/PARAM(1)             ! ds/da
      O      = (PARAM(2)+PARAM(3))*PI/180.0D0+V  ! rad(b+c)+arcsin(s)
      P      = 1.0D0/DCOS(O)/DCOS(O)             !
 
      DERIV(4) = 1.0D0           ! dz/dd = 1 
      DERIV(5) = DTAN(O)         ! dz/de = m = tan(rad(b+c)+arcsin(s))    

      DERIV(1) = PARAM(5)*P*T*W       ! dz/da = dz/dm dm/do do/ds ds/da  
      DERIV(2) = PARAM(5)*P*(1.0D0-T*U)  ! dz/db = dz/dm dm/do (1-dv/ds ds/dr)
      DERIV(3) = PARAM(5)*P*(1.0D0+T*U)  ! dz/db = dz/dm dm/do (1-dv/ds ds/dr)

      ZZ       = PARAM(4)+PARAM(5)*DERIV(5)
C
C next is distortion in tube  zz --> zd = q*tan(s*(zz-r))
C
C       ZR       = PARAM(10)*(ZZ-PARAM(9))
C       Q        = PARAM(8)/DCOS(ZR)/DCOS(ZR)
C       DERIV(8) = DTAN(ZR)    ! dzd/dq of q*(tan...)
C       DERIV(9) = -Q*PARAM(10)      ! d/dr of q*abs(tan(s(zz-r))
C       DERIV(10)= Q*(ZZ-PARAM(9))   ! d/ds of q*abs(tan(s(zz-r))

C       ZD       = PARAM(8)*DERIV(8)
C
C finally projection onto diodes   zd --> y1 = y0 + p*zd
C
C       DERIV(6) = 1.0D0
C       DERIV(7) = ZD

C       Y1 = PARAM(6)+PARAM(7)*ZD


C       

       ZW       = PARAM(10)*(ZZ-PARAM(9))
       Q        = PARAM(8)/DCOS(ZW)/DCOS(ZW)

       DERIV(8) = DTAN(ZW)    ! d/dq of q*(tan...)
       DERIV(9) = -Q*PARAM(10)   ! d/dr of q*abs(tan(s(zz-r))
       DERIV(10)= Q*(ZZ-PARAM(9))   ! d/ds of q*abs(tan(s(zz-r))

       DERIV(6) = 1.0D0
       DERIV(7) = ZZ
 
C       type *,XX,SNGL(ZZ),SNGL(Y1)
       Y1       = PARAM(6)+PARAM(7)*DERIV(7)+PARAM(8)*DERIV(8)
ccc        write(11,*) xx,y1
ccc        write(11,*) (deriv(i),i=1,10)

      RETURN
      END






