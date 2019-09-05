      SUBROUTINE MJDTODATE (MJD,YR,MN,DY,UT)

c  ------------------------------------------------------------------------
c  Part of "poa_calfos" (STECF/IPMG)
c  Part of "NORAD TLES" based ephemeris program (NASA)
c
c  History:
c  --------
c  Version   Date        Author          Description
c      1.0       Jul 00  M. Rosa         POA calibration code
c  ------------------------------------------------------------------------

c convert Modified Julian Date  to Year, Month, Day and UT(hrs)
   
      double precision  mjd,ut
      double precision  juld
      integer*4 yr,mn,dy
      integer*4         ja, jb, jc, jd, je

      juld = mjd+2400000.5e0
      ja = nint(juld)
      ut = 24.d0 * (juld - dfloat(ja) + 0.5d0)

      if (ja .ge. 2299161) then
        jb = int (((ja - 1867216) - 0.25) / 36524.25)
        ja = ja + 1 + jb - int (jb / 4)
      endif
      
      jb = ja + 1524
      jc = int (6680. + ((jb - 2439870) - 122.1) / 365.25d0 )
      jd = 365 * jc + int (jc / 4)
      je = int ((jb - jd) / 30.6001)
      dy = jb - jd - int (30.6001 * je)
      mn = je - 1
      if (mn .gt. 12) mn = mn - 12
      yr = jc - 4715
      if (mn .gt. 2) yr = yr - 1
      if (yr .lt. 0) yr = yr - 1

      return
      end

