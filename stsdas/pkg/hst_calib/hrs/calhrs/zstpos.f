        SUBROUTINE ZSTPOS(EXPTIM,LNG,LAT,ISTAT)
*
*  Module number:
*
*  Module name: ZSTPOS
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
*  FORTRAN name: ZSTPOS.FOR
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
*       1.2     Oct 96      M. De La Pena   Removed extra variables for CALHRS
*                                           Based upon HSTPOS for CALFOS
*       1.3     Jan 97      M. De La Pena   Add SAAAVOID to ORBPAR
*-------------------------------------------------------------------------------
*
* INPUTS:
*	exptim - exposure time
*
* INPUT/OUTPUT:
*
* OUTPUT:
*	lng   - longitude
*	lat   - latitude
*	istat - error status
*
*----------------------------------------------------------------------------
        INTEGER ISTAT
        DOUBLE PRECISION EXPTIM
        DOUBLE PRECISION LNG,LAT
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
        CHARACTER*2      SAAAVOID
        COMMON /ORBPAR/ EPCHTIME,SDMEANAN,CIRVELOC,COSINCLI,
     $  ECCENTRY,ECCENTX2,ECBDX4D3,ESQDX5D2,ECBDX3,
     $  FDMEANAN,RCASCNRV,ARGPERIG,MEANANOM,RCARGPER,
     $  RASCASCN,SINEINCL,SEMILREC,
     $  PSANGLV3,RTASCNV1,DECLNV1,SAAAVOID
C
C LOCAL VARIABLES ------------------------------------------------------
C
        DOUBLE PRECISION X, Y, Z, ST
        DOUBLE PRECISION M, MJDNIT, MJDSEC
        DOUBLE PRECISION FRACEN, STMIDN
        DOUBLE PRECISION DELTIM, R, V, F, WSMALL, WBIG
        DOUBLE PRECISION RA, DEC
C
C-----------------------------------------------------------------------
C
C calculate orbital positions (see flight software ephemeris algorithm)
C
C convert MJD to seconds since 1jan85
C
            MJDSEC = (EXPTIM - 46066.0D0) * 86400.0D0
C
C calculate time difference between observation and epoch time
C
            DELTIM = MJDSEC - EPCHTIME
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
            X = R*(COS(WBIG)*COS(F)-COSINCLI*SIN(WBIG)*SIN(F))
            Y = R*(SIN(WBIG)*COS(F)+COSINCLI*COS(WBIG)*SIN(F))
            Z = R*SINEINCL*SIN(F)
C
C need sidereal time to compute longitude and latitude
C
C mjd at midnite 
C
            MJDNIT = INT(EXPTIM)
C
C fractional centuries since 2000
C
            FRACEN = (MJDNIT - 51544.5D0)/36525.0D0
C
C sidereal time at midnite in seconds 
C
            STMIDN = 24110.548D0 + MOD(8640184.812866D0 * FRACEN, 
     $                        86400.0D0)
C
C sidereal time in radians
C
            ST = 2.0D0*PI/86400.0D0*(STMIDN+(EXPTIM-MJDNIT)*
     $                        86400.0D0*1.0027379093D0)
C
C latitude
C
            DEC = ASIN(Z/R)
            LAT = 180.0D0/PI*DEC
C
C longitude (make sure the quadrant is correct!)
C
            RA = ATAN2(Y,X)
            IF (RA .LT. 0.0D0) RA = RA + 2.0D0*PI
            LNG = 180.0D0/PI*(RA - ST)
            IF (LNG .GT. 180.0D0) THEN
                LNG = LNG - 360.0D0
            ELSE IF (LNG .LT. -180.0D0) THEN
                LNG = LNG + 360.0D0
            ENDIF
C
	    ISTAT = 0
            RETURN
            END
