# include <stdio.h>
# include <math.h>
# include <ximio.h>
# include "estreak.h"

/*  G_STREAKANGLE:  Computes streak angle from the SHP header information
 *
 *
 *  Use The following SHP header information to calculate the streak angle
 *  (1) position angle of V3 axis, (2) RA and Dec of V1 axis, (3) space craft 
 *  velocity.
 *
 *  Header keywords needed:
 *  ----------------------
 *
 *  "PA_V3"		Position angle of the V3 axis (PSANGLV3 in old files)
 *  "RA_V1"		Right ascension of the V1 axis (RTASCNV1 in old files)
 *  "DEC_V1"		Declination of the V1 axis (DECLNV1 in old files)
 *  "VELOCSTX"		X-component of the space craft velocity
 *  "VELOCSTY"		Y-component of the space craft velocity
 *  "VELOCSTZ"		Z-component of the space craft velocity
 *
 *
 *  This is a plain translation of the SPP code written by J.C. Hsu.
 *
 *
 *   Revision history:
 *   ---------------
 *   06 May 96  -  Implementation (IB)
 *   07 Oct 96  -  Revised after code review (IB)
 *
 */


int g_streakAngle (char *fshp, float *strangle) {

	IRAFPointer                              ip;
	float                  pa_v3, ra_v1, dec_v1;
	float	                         vx, vy, vz;
	float      cos_ra, sin_ra, cos_dec, sin_dec;
	float                            v_v2, v_v3;
	Bool                     keywExist, keywNew;

	/* Open the SHP file. The "[0]" is needed to make sure the
         * primary header will be accessed in case of INHERIT=F 
         */ 
	strcat (fshp, "[0]");
	ip = c_immap (fshp, IRAF_READ_ONLY, 0);
	if (ip == (IRAFPointer)NULL) {
	    sprintf (ErrText, "Cannot open file %s", fshp);
	    g_error (ErrText);
	    return (1);
	}

	/* Read proper keywords. */
	keywExist = False;
	keywNew   = False;
	if (c_imaccf (ip, "PA_V3") && c_imaccf (ip, "RA_V1") &&
	    c_imaccf (ip, "DEC_V1")) {
	    keywExist = True;
	    keywNew   = True;
	} else if (c_imaccf (ip, "PSANGLV3") && c_imaccf (ip, "RTASCNV1") &&
	           c_imaccf (ip, "DECLNV1")) {
	    keywExist = True;
	    keywNew   = False;
	} 
	if (keywExist && keywNew) {
	    pa_v3  = c_imgetr (ip, "PA_V3");
	    ra_v1  = c_imgetr (ip, "RA_V1");
	    dec_v1 = c_imgetr (ip, "DEC_V1");
	} else if (keywExist && !keywNew) {
	     pa_v3  = c_imgetr (ip, "PSANGLV3");
	     ra_v1  = c_imgetr (ip, "RTASCNV1");
	     dec_v1 = c_imgetr (ip, "DECLNV1");
	} else {
	    c_imunmap (ip);
	    return (1);
	}
	if (c_imaccf (ip, "VELOCSTX") && c_imaccf (ip, "VELOCSTY") &&
	    c_imaccf (ip, "VELOCSTZ")) {
	    vx = c_imgetr (ip, "VELOCSTX");
	    vy = c_imgetr (ip, "VELOCSTY");
	    vz = c_imgetr (ip, "VELOCSTZ");
	} else {
	    c_imunmap (ip);
	    return (1);
	}

	cos_ra  = cos ((double)ra_v1  * DEG2RAD);
	sin_ra  = sin ((double)ra_v1  * DEG2RAD);
	cos_dec = cos ((double)dec_v1 * DEG2RAD);
	sin_dec = sin ((double)dec_v1 * DEG2RAD);

/*	Let i, j, k represent the unit vectors of the coordinate system XYZ,
 *      v1, v2, v3 the unit vectors of the ST vehicle cooordinate system
 *      (corresponding to V1, V2, V3 axes with zero V3 position angle).
 *      The vector [v1, v2, v3] can be obtained by rotating [i, j, k]
 *      by ra around Z axis, and then dec around V2 axis, i. e. 
 *          /  \    /			     \ /		    \ / \
 *          |v1|    | cos(dec)  0   sin(dec)| |cos(ra)  sin(ra)  0 | |i|
 *          |v2| =  |   0      1      0     | |-sin(ra) cos(ra)  0 | |j|
 *          |v3|    | -sin(dec) 0   cos(dec)| |  0        0      1 | |k|
 *          \  /    \			     / \		    / \ /
 */

	v_v3 = -cos_ra * sin_dec * vx - sin_ra * sin_dec * vy + cos_dec * vz;
	v_v2 = -sin_ra * vx + cos_ra *vy; 
	*strangle = atan2 (v_v2, v_v3) / DEG2RAD - pa_v3 + 90.0F;

	c_imunmap (ip);
	return (0);
}




