      SUBROUTINE DIGICON (geobx,geoby,geobz,vdetx,vdety,vdetz,
     1                    x,y,z,etime)
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  ------------------------------------------------------------------------
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c AUTHOR: M.R.Rosa   STECF   
c
c NOTE:   current shielding and HV, permanent B-field are for FOS/RD
c         for FOS/BL either new values or scale factors outside  
c         since all effects way smaller in FOS/BL, this latter 
c         approximation is almost perfect
c 
c PURPOSE: Determine electron path in crossed E and B fields.
c          Physics happens in digicons restframe.
c          Coordinates:  right hand  x parallel diode/dispersion array
c                        right hand  y perpendicular diode array
c                        right hand  z in digicon axix pointing 
c                                      from cathode to diode array
c  
C          For the FOS digicons, the resulting "drift" (ExB drift) is
c          approximately 17.6 degrees  
c 
c INPUT:   geobx,geoby,geobz     -   R*4   - B-Field in x,y,z, coordinates
c          vdetx,vdety,vdetz     -   R*4   - velocities (km/s) in external
c                                            frame
c OUTPUT:  x,y,z                 -   R*4   - impact location on diode array
c          etime                 -   R*4   - flight time [sec]
c    
c History:
c --------
c Version   Date        Author          Description
c     1.0       May 98  M. Rosa         POA calibration code
c
c DETAILS:
c non-relativistic:   v = sqrt(2e/m U);  dv = sqrt(e/(2m))*sqrt(1/U) dU 
cc relativistic correction: m = m0 / sqrt(1-(v/c)**2)
c                    dU is propto dx 
c                    hence: loop in time, find dx(dt) which is dU; that gives
c                    a new dv; add to v; that gives a new v....
c Units are v [m/sec], 
c! electron in digicon,  loop is in E-11 sec
c! e/m*U/z = e/m*20kV/0.15m = 2.34667E+16 m/sec**2  (at kV=0; e/m = 1.76)
c! e/m*U/z = e/m*20kV/0.15m = 2.34667E+16 m/sec**2  (at kV=10000; e/m = 1.72)
c! rel corr for 20kV: = 1.0406   --> e/m = 1.69
c! 2.29E+16*1.E-11 = 2.29E+5
c dp/dt = e (E + (V X B)/c)
c dv/dt = 1/m dp/dt
c dv = e/m E dt + e/m (v/c*B) dt 
c calc in rest frame of digicon
c
      IMPLICIT NONE
     
      REAL*8 px,py,pz,vx,vy,vz,ovx,ovy,ovz
      REAL*8 divx,divy,divz,dt,vdx,vdy,vdz
      REAL*8 dovx,dovy,dovz
      REAL*8 edig,emx,emy,emz,vc,ivx,ivy,ivz
      REAL*8 bdig,bshx,bshy,bshz,zdig,hv,em0
      REAL*8 bxi,bxo,byi,byo,bzi,bzo
      REAL*8 geobx,geoby,geobz,vdetx,vdety,vdetz
      REAL*8 x,y,z
      REAL*8 etime
      INTEGER*4 i
 
      DATA ovx,ovy,ovz/0.D0,0.D0,0.D0/
      DATA ivx,ivy,ivz/0.D0,0.D0,0.D0/
      DATA vx,vy,vz/0.D0,0.D0,0.D0/
      DATA px,py,pz/0.D0,0.D0,0.D0/
      DATA bdig/105/
      DATA dt/1.D-12/
c      DATA edig/2.346667D+16/           ! edig gets computed = hv/zdig
      DATA hv/2.176471D+04/              ! V
      DATA zdig/0.15/              ! m
      DATA em0/1.7588803D+11/           ! C/g
      DATA bshx,bshy,bshz /10.2,10.2,10.2/   ! red detecor shielding
      DATA vc/3.0D+8/ 

        edig = hv/zdig     ![V/m*g/kg]
        bxi = 0.0
        byi = 0.
        bzi = bdig*1.D-04                 !internal Bz field 
        bxo = (geobx/bshx)*1.D-04*1.
        byo = -(geoby/bshy)*1.D-04*1.     !why - 
        bzo = (geobz/bshz)*1.D-04*1.
        vdx = (vdetx) !*0.
        vdy = (vdety) !*0.
        vdz = (vdetz) !*0.
        px = 0.
        py = 0.
        pz = 0.
        ivx = 0.
        ivy = 0.
        ivz = 0.
        ovx = 0.
        ovy = 0.
        ovz = 0.
        i = 0 

c now integrate the path
100     i = i+1
c    the lorentz-transformed e/m at the current v
        emx = em0*dsqrt(1.D0-(ivz/vc)**2)       ! C/kg = A*sec/kg
        emy = em0*dsqrt(1.D0-(ivz/vc)**2) 
        emz = em0*dsqrt(1.D0-(ivz/vc)**2) 
c    dv = e/m U/z dt + v X e/m B dt
c 1.  delta v from inner field and edig in dig restframe 
        divx = (ivy*emz*bzi - ivz*emy*byi)*dt        ! m/sec + m/sec + m/sec
        divy = (ivz*emx*bxi - ivx*emz*bzi)*dt
        divz = (ivx*emy*byi - ivy*emx*bxi)*dt + edig*emz*dt
c.2.  now delta v from outer field in the outer rest frame
        ovx = ovx + vdx
        ovy = ovy + vdy
        ovz = ovz + vdz
        dovx = (-ovy*emz*bzo - ovz*emy*byo)*dt     ! m/sec
        dovy = (ovz*emx*bxo - ovx*emz*bzo)*dt
        dovz = (ovx*emy*byo + ovy*emx*bxo)*dt
c    v = oldv + dv
        ivx = ivx + divx + dovx 
        ivy = ivy + divy + dovy 
        ivz = ivz + divz + dovz 
        ovx = ivx - vdx
        ovy = ivy - vdy
        ovz = ivz - vdz
c    x = x + v*dt 
        px = px + (ivx)*dt
        py = py + (ivy)*dt
        pz = pz + (ivz)*dt
c        px = px + (ovx+vdx)*dt
c        py = py + (ovy+vdy)*dt
c        pz = pz + (ovz+vdz)*dt

c    test if electron hit the anode at pz = zdig
        if (pz.lt.zdig) goto 100
 900  continue

      etime = dt*i
c    coords in pix (z = 15cm, pix=50/4 mu)   
      x = px*1.E+6/50.*4.
      y = py*1.E+6/50.*4.
      z = pz

      return
      end
