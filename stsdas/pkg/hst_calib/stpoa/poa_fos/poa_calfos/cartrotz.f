      subroutine cartrotz(a, vin, vout)
c                                                               
c  Cart. Coord System  Rotation about Z Axis 
c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------
c
      real*8  a, ac, as, vin(3), vout(3)
      DOUBLE PRECISION  DGTORD, PI
      DATA PI /3.14159265358979323846D+00/
      DGTORD = PI / 180.0D0  

      ac = dcos(DGTORD*a)
      as = dsin(DGTORD*a) 
      vout(1) = vin(1)*ac - vin(2)*as  
      vout(2) = vin(1)*as + vin(2)*ac  
      vout(3) = vin(3)
      return
      end
