include <math.h>
include	"orbdat.h"

# GETPOS -- Get position at a given time 
#
#
# S. Hulbert, Oct 91    Original

procedure getpos (orb,mjd,lng,lat)

pointer	orb	#I: pointer to orbital data structure
double	mjd	#I: mjd
real	lng	#O: longitude in degrees
real	lat	#O: latitude in degrees

double	sec85,deltim,m,v,r,wsmall,wbig,f,x,y,z
double	midnit,fracen,st,stmidn,dec,ra
double	x1,x2,x3

begin


	# convert MJD to seconds since 1jan85
        sec85 = (mjd - 46066.0d0) * 86400.0d0

	# calculate time difference between observation and epoch time
        deltim = sec85 - EPCHTIME(orb)

	# mean anomaly
        #m = MEANANOM(orb)+TWOPI*(FDMEANAN(orb)*deltim+
	#	0.5*SDMEANAN(orb)*deltim**2)
        x1 = MEANANOM(orb)
	x2 = FDMEANAN(orb)*deltim
	x3 = 0.5*SDMEANAN(orb)*deltim**2
	m = x1 + TWOPI*(x2 + x3)

	# true anomaly (equation of the center)
        v = m+sin(m)*(ECCENTX2(orb)+ECBDX3(orb)*cos(m)**2-
		ECBDX4D3(orb)*sin(m)**2+ESQDX5D2(orb)*cos(m))

	# distance
        r = SEMILREC(orb)/(1.0d0+ECCENTRY(orb)*cos(v))

	# argument of perigee
        wsmall = TWOPI*(ARGPERIG(orb)+RCARGPER(orb)*deltim)

	# longitude of the ascending node
        wbig = TWOPI*(RASCASCN(orb)+RCASCNRV(orb)*deltim)

	# calculate the rectangular coordinates 
	# (see Smart, Spherical Astronomy, section 75, page 122-124
        f = wsmall+v
        x = r*(cos(wbig)*cos(f)-COSINCLI(orb)*sin(wbig)*sin(f))
        y = r*(sin(wbig)*cos(f)+COSINCLI(orb)*cos(wbig)*sin(f))
        z = r*SINEINCL(orb)*sin(f)

	# calculate orbital velocities (we don't need this right now)
        #a0 = CIRVELOC(orb)*ECCENTRY(orb)*sin(v)/r
        #a1 = CIRVELOC(orb)*(1.0d0+ECCENTRY(orb)*cos(v))+TWOPI*RCARGPER(orb)*r
        #vx = a0*x-a1*(cos(wbig)*sin(f)+COSINCLI(orb)*sin(wbig)*cos(f)-
	#	TWOPI*RCASCNRV(orb)*y
        #vy = a0*y-a1*(sin(wbig)*sin(f)+COSINCLI(orb)*cos(wbig)*cos(f)+
	#	TWOPI*RCASCNRV(orb)*x
        #vz = a0*z+a1*SINEINCL(orb)*cos(f)

	# need sidereal time to compute longitude and latitude
	# mjd at midnite
        midnit = int(mjd)

	# fractional centuries since 2000
        fracen = (midnit - 51544.5d0)/36525.0d0

	# sidereal time at midnite in seconds
        stmidn = 24110.548d0 + mod(8640184.812866d0*fracen,86400.0d0)

	# sidereal time in radians
        st = TWOPI/86400.0d0*(stmidn+(mjd-midnit)*86400.0d0*1.0027379093d0)

	# latitude
        dec = asin(z/r)
        lat = RADTODEG(dec)

	# longitude (make sure the quadrant is correct!)
        ra = atan2(y,x)
        if (ra < 0.) 
	    ra = ra + TWOPI
        lng = RADTODEG(ra - st)
        if (lng > 180.) 
            lng = lng - 360.
         else if (lng < -180.)
            lng = lng + 360.

end
