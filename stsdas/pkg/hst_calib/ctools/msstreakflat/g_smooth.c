# include <stdio.h>
# include <math.h>
# include <stdlib.h>
# include "estreak.h"

# define   THRESHOLD   0.3

/*  G_SMOOTH  --  Do the 1-D smoothing of the streak flats.
 *
 *  Translated from the WFPC version written by J.C. Hsu.  
 *
 *
 *
 *  Revision history:
 *  ---------------
 *  29 Apr 96  -  Implementation  (IB)
 *  07 Oct 96  -  Revised after code review (IB)
 *
 */

int g_smooth (floatArray *pic, float theta, int hwidth) {

	floatArray    work1, work2;
	float               dx, dy; /* Intervals in X and Y between 
                                       successive points within one streak */ 
	float             bdx, bdy; /* Intervals in X and Y borders between
                                       successive streaks. */
	float     x0, y0, bx0, by0;
	float         rtheta, x, y;
	float              *strbuf;
	int   bysteps, angle, i, j;
	long                     l;

	char *malloc_err = "Boxcar filter: cannot allocate memory.";

	float g_interpolate (floatArray *, float, float, float);
	void g_boxcar (float *, int, int, float, float, float, float, 
                       floatArray *, floatArray *);

	rtheta = theta;
	while (rtheta >   90.0F) rtheta -= 180.0F;
	while (rtheta <= -90.0F) rtheta += 180.0F;
	rtheta = rtheta * DEG2RAD;

	/* Alloc work areas. */
	if (allocArray (&work1, pic->nx, pic->ny))  {
	    g_error (malloc_err); 
	    return (1);
	}
	if (allocArray (&work2, pic->nx, pic->ny))  {
	    g_error (malloc_err); 
	    return (1);
	}
	strbuf = (float *) calloc ((2*pic->nx), (size_t)sizeof(float));
	if (strbuf == NULL) {
	    g_error (malloc_err); 
	    return (1);
	}

	/* Verify if streaks are parallel to image's axis. */
	if (fabs(rtheta) < 0.5*atan2(1.0,(double)pic->nx)) 
	    angle = 0;
	else if (fabs(fabs(rtheta)-PI2) < 0.5*atan2(1.0,(double)pic->ny))
	    angle = 90;
	else if (rtheta > 0.)
	    angle = 1;
	else
	    angle = -1;

	/* Set border starting points. */
	bx0 = 0.0F;
	by0 = 0.0F;
	if (angle == 1)
	    by0 = (float)pic->ny-1;

	/* Calculate the step sizes.  */ 
	/* Case of zero streak angle. */
	if (angle == 0) {
	    dx  = 1.0F;
	    dy  = 0.0F;
	    bdy = 1.0F;
	
	/* Case of 90 degree streak angle. */
	} else if (angle == 90) {
	    dx  = 0.0F;
	    dy  = 1.0F;
	    bdx = 1.0F;

	} else {
	    dx = cos(rtheta);
	    dy = sin(rtheta);
	    bdx = fabs(1.0 / dy);
	    if (angle == 1)
		bdy = -1.0 / dx;
	    else
	    	bdy = 1.0 / dx;
	}

	/* Do each streak intersecting with the left side. The separation 
	 * between successive streaks is (artificially) set to ONE pixel. 
         */
	if (angle != 90) {
	    bysteps = (int)((float)pic->ny / fabs((double)bdy));

	    x0 = bx0;
	    for (i = 0; i < bysteps; i++) {
	        y0 = by0 + (float)i * bdy;

	        /* Populate streak values. */
	        for (j = 0; j < 2*pic->nx; j++) {
		    x = x0 + (float)j * dx;
		    y = y0 + (float)j * dy;
		    if (x < 0.0F || x >= (float)pic->nx || 
                        y < 0.0F || y >= (float)pic->ny)
		        break;
	   	    strbuf[j] = g_interpolate (pic, x, y, y0); 
	        }

	        /* Boxcar-smooth this streak. */
	        g_boxcar (strbuf, j-1, hwidth, x0, y0, dx, dy, 
                          &work1, &work2);
	    }
	} else
	    y0 = by0;


	/* Now do each streak intersecting with the top or 
         * bottom side. Use the last y0 from the last loop. 
         */
	if (angle != 0) {
	    bysteps = (int)((float)pic->nx / bdx);

	    /* y0 should start at top or bottom. */
	    if (angle ==  1) y0 = 0.0F;
	    if (angle == -1) y0 = (float)pic->ny-1;
	    for (i = 0; i < bysteps; i++) {
	        x0 = bx0 + (float)i * bdx;
	    
	        /* Populate streak values. */
	        for (j = 0; j < 2*pic->ny; j++) {
		    x = x0 + (float)j * dx;
		    y = y0 + (float)j * dy;
		    if (x < 0.0F || x >= (float)pic->nx || 
                        y < 0.0F || y >= (float)pic->ny)
		        break;
	   	    strbuf[j] = g_interpolate (pic, x, y, y0); 
	        }

	        /* Boxcar-smooth this streak. */
	        g_boxcar (strbuf, j-1, hwidth, x0, y0, dx, dy, 
                          &work1, &work2);
	    }
	}

	/* Calculate the final smoothed streak patterns. */
	for (l = 0; l < work1.bufsize; l++) {
	    if (work2.data[l] < THRESHOLD)
	        pic->data[l] = BADVAL;
	    else
	        pic->data[l] = work1.data[l] / work2.data[l];

	}

	free (strbuf);
	freeArray (&work2);
	freeArray (&work1);

	return (0);
}



/*  G_INTERPOLATE  --  Interpolate pixel value by using values of the 
 *                     surrounding 4 pixels.
 */

float g_interpolate (floatArray *pic, float x, float y, float y0) { 

	float    val, b, d;
	int           i, j;

	i = (int)x;
	j = (int)y;

	if (i < 0 || i > (pic->nx)-2 || j < 0 || j > (pic->ny)-2) 
	    return (BADVAL);

	if (GPPix(pic,i,j)   <= BADVAL || GPPix(pic,i+1,j)   <= BADVAL ||
            GPPix(pic,i,j+1) <= BADVAL || GPPix(pic,i+1,j+1) <= BADVAL)
	    return (BADVAL);
	else {
	    b = x - (float)i;
	    d = y - (float)j;
	    val = (1.0F - b) * (1.0F -d)  * GPPix(pic,i,j)   +
		  (1.0F - b) *  d         * GPPix(pic,i,j+1) +
		   b         * (1.0F - d) * GPPix(pic,i+1,j) +
	 	   b         *  d         * GPPix(pic,i+1,j+1);
	    return (val);
	} 
}	



/*  G_BOXCAR  --  Do the "boxcar" averaging.  */

void g_boxcar (float *strbuf, int len, int hwidth, 
               float x0, float y0, float dx, float dy, 
               floatArray *work1, floatArray *work2) {

	float            x, y;
	double	          sum;
	int       j, hw, npts;
	int     ifront, iback;
	void g_putBack (floatArray *, floatArray *, float, float, float);

	hw = hwidth;
	if (hwidth >= len)
	    hw = len - 1;

	/* Do the summation for the first point, less the rightmost point. */
	sum  = 0.0;
	npts = 0;
	for (j = 0; j < hw; j++) {
	    if (strbuf[j] != BADVAL) {
		sum += strbuf[j];
		npts++;
	    }
	}

	/* Do the averaging. */
	for (j = 0; j < len; j++) {
	    iback = j - hw;
	    ifront = j + hw;
	    if (iback >= 0) {
		if (strbuf[iback] != BADVAL) {
		    sum -= strbuf[iback];
		    npts--;
		}
	    }
	    if (ifront < len) {
		if (strbuf[ifront] != BADVAL) {
		    sum += strbuf[ifront];
		    npts++;
		}
	    }
	    if (npts > 0) {

		/* Put the smoothed streak values back. */
		x = x0 + (float)j * dx;
		y = y0 + (float)j * dy;
		g_putBack (work1, work2, x, y, (float)(sum/(double)npts));
	    }
	} 
}



/*  G_PUTBACK  --  Put back smoothed pixel values by distributing to the 
 *                 surrounding 4 pixels.
 */

void g_putBack (floatArray *work1, floatArray *work2, float x, float y, 
                float pixval) {


	float   wt, b, d;
	int     i, j;

	i = (int)x;
	j = (int)y;

	if (i < 0 || i > (work1->nx)-2 || j < 0 || j > (work1->ny)-2) 
	    return;

	b = x - (float)i;
	d = y - (float)j;

	wt = (1.0F - b) * (1.0F - d);
	GPPix(work1,i,j) += pixval * wt;
	GPPix(work2,i,j) += wt;

	wt = (1.0F - b) * d;
	GPPix(work1,i,j+1) += pixval * wt;
	GPPix(work2,i,j+1) += wt;

	wt = b * (1.0F - d);
	GPPix(work1,i+1,j) += pixval * wt;
	GPPix(work2,i+1,j) += wt;

	wt = b * d;
	GPPix(work1,i+1,j+1) += pixval * wt;
	GPPix(work2,i+1,j+1) += wt;
}	









