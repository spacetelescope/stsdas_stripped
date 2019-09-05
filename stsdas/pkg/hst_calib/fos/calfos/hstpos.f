        SUBROUTINE HSTPOS(MIDTIM,NSPEC,X,Y,Z,LNG,LAT,ST,ISTAT)
*
*  Module number:
*
*  Module name: HSTPOS
*
*  Keyphrase:
*  ----------
*       Calculate HST position
*
*  Description:
*  ------------
*       This routine calculates the position of HST for a list of 
*	times
*
*  FORTRAN name: HSTPOS.FOR
*
*  Keywords of accessed files and tables:
*  --------------------------------------
*  Name                         I/O     Description / Comments

*  Subroutines Called:
*  -------------------
*  CDBS:
*
*  History:
*  --------
*  Version      Date        Author          Description
*       1       May 91      S. Hulbert      Designed and coded
*	1.1	Aug 93	    H. Bushouse	    Added ISTAT to argument list
*-------------------------------------------------------------------------------
*
* INPUTS:
*	midtim - array of mjd's at mid-exposure
*	nspec - number of midtim's
*
* INPUT/OUTPUT:
*
* OUTPUT:
*	x - array of x positions
*	y - array of y positions
*	z - array of z positions
*	lng - array of longitudes
*	lat - array of latitudes
*	st - array of sidereal times corresponding to the midtime's
*	istat - error status
*
*----------------------------------------------------------------------------
        INTEGER NSPEC, ISTAT
        DOUBLE PRECISION MIDTIM(NSPEC),X(NSPEC),Y(NSPEC),Z(NSPEC)
        DOUBLE PRECISION LNG(NSPEC),LAT(NSPEC),ST(NSPEC)
C
        DOUBLE PRECISION PI
        PARAMETER (PI = 3.1415926535898D0)
C
C Common block containing orbital/positional parameters
C
        DOUBLE PRECISION EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI
        DOUBLE PRECISION ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3
        DOUBLE PRECISION FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER
        DOUBLE PRECISION RASCASCN,SINEINCL,SEMILREC
        DOUBLE PRECISION PSANGLV3,RTASCNV1,DECLNV1
        COMMON /ORBPAR/ EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI,
     $  ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3,
     $  FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER,
     $  RASCASCN,SINEINCL,SEMILREC,
     $  PSANGLV3,RTASCNV1,DECLNV1
C
C LOCAL VARIABLES ------------------------------------------------------
C
        DOUBLE PRECISION M, MIDNIT, MIDSEC
        DOUBLE PRECISION FRACEN, STMIDN
        DOUBLE PRECISION DELTIM, R, V, F, WSMALL, WBIG
        DOUBLE PRECISION RA, DEC
	INTEGER I
C
C-----------------------------------------------------------------------
C
C calculate orbital positions (see flight software ephemeris algorithm)
C
        DO 100 I = 1, NSPEC
C
C convert MJD to seconds since 1jan85
C
            MIDSEC = (MIDTIM(I) - 46066.0D0) * 86400.0D0
C
C calculate time difference between observation and epoch time
C
            DELTIM = MIDSEC - EPCHTIME
C
C mean anomaly
C
            M = MEANANOM+2.0D0*PI*(FDMEANAN*DELTIM+0.5*SDMEANAN*
     $                        DELTIM**2)
C
C true anomaly (equation of the center)
C
            V = M+SIN(M)*(ECCENTX2+ECBDX3*COS(M)**2-ECBDX4D3*SIN(M)**2+
     $                        ESQDX5D2*COS(M))
C
C distance
C
            R = SEMILREC/(1.0D0+ECCENTRY*COS(V))
C
C argument of perigee
C
            WSMALL = 2.0D0*PI*(ARGPERIG+RCARGPER*DELTIM)
C
C longitude of the ascending node
C
            WBIG = 2.0D0*PI*(RASCASCN+RCASCNRV*DELTIM)
C
C calculate the rectangular coordinates (see Smart, Spherical Astronomy,
C section 75, page 122-124
C
            F = WSMALL+V
            X(I) = R*(COS(WBIG)*COS(F)-COSINCLI*SIN(WBIG)*SIN(F))
            Y(I) = R*(SIN(WBIG)*COS(F)+COSINCLI*COS(WBIG)*SIN(F))
            Z(I) = R*SINEINCL*SIN(F)
C
C calculate orbital velocities (we don't need this right now)
C
C            A0 = CIRVELOC*ECCENTRY*SIN(V)/R
C            A1 = CIRVELOC*(1+ECCENTRY*COS(V))+2*PI*RCARGPER*R
C            VX = A0*X-A1*(COS(WBIG)*SIN(F)+COSINCLI*SIN(WBIG)*
C     $                        COS(F)-2*PI*RCASCNRV*Y
C            VY = A0*Y-A1*(SIN(WBIG)*SIN(F)+COSINCLI*COS(WBIG)*
C     $                        COS(F)+2*PI*RCASCNRV*X
C            VZ = A0*Z+Z1*SINEINCL*COS(F)
C
C need sidereal time to compute longitude and latitude
C
C mjd at midnite 
C
            MIDNIT = INT(MIDTIM(I))
C
C fractional centuries since 2000
C
            FRACEN = (MIDNIT - 51544.5D0)/36525.0D0
C
C sidereal time at midnite in seconds 
C
            STMIDN = 24110.548D0 + MOD(8640184.812866D0 * FRACEN, 
     $                        86400.0D0)
C
C sidereal time in radians
C
            ST(I) = 2.0D0*PI/86400.0D0*(STMIDN+(MIDTIM(I)-MIDNIT)*
     $                        86400.0D0*1.0027379093D0)
C
C latitude
C
            DEC = ASIN(Z(I)/R)
            LAT(I) = 180.0D0/PI*DEC
C
C longitude (make sure the quadrant is correct!)
C
            RA = ATAN2(Y(I),X(I))
            IF (RA .LT. 0.0D0) RA = RA + 2.0D0*PI
            LNG(I) = 180.0D0/PI*(RA - ST(I))
            IF (LNG(I) .GT. 180.0D0) THEN
                LNG(I) = LNG(I) - 360.0D0
            ELSE IF (LNG(I) .LT. -180.0D0) THEN
                LNG(I) = LNG(I) + 360.0D0
            ENDIF

100     CONTINUE
C
	ISTAT = 0
        RETURN
        END
