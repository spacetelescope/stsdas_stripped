# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "pedsub.h"

/*  N_GETSTATREGIONS  --   Reads pset with image statistics regions.
**
**
**   Revision history:
**   ---------------
**   H. Bushouse	05-May-1999	Implementation
**
*/

int n_getStatRegions (PedInfo *info) {

	/* Local variables */
	IRAFPointer ps;
	char *sec;

	/* Function declarations */
	int parse_sec (char *, int *, int *, int *, int *);

# if defined(NATIVE_IRAF)

	/* Allocate memory for the section string */
	sec = NULL;
	sec = (char *)calloc(41, sizeof(char));
	if (sec == NULL) {
	    sprintf (MsgText, "Can't allocate memory in n_getStatRegions");
	    n_error (MsgText);
	    return (1);
	}

	/* Open the pset */
	ps  = c_clopset (STATREGIONS);

	/* Get and parse section specification for quad1 */
	c_clgpseta (ps, "quad1", sec, 40);
	if (parse_sec (sec, &info->qx1[0], &info->qx2[0], &info->qy1[0],
		       &info->qy2[0]))
	    return (1);

	/* Get and parse section specification for quad2 */
	c_clgpseta (ps, "quad2", sec, 40);
	if (parse_sec (sec, &info->qx1[1], &info->qx2[1], &info->qy1[1],
		       &info->qy2[1]))
	    return (1);

	/* Get and parse section specification for quad3 */
	c_clgpseta (ps, "quad3", sec, 40);
	if (parse_sec (sec, &info->qx1[2], &info->qx2[2], &info->qy1[2],
		       &info->qy2[2]))
	    return (1);

	/* Get and parse section specification for quad4 */
	c_clgpseta (ps, "quad4", sec, 40);
	if (parse_sec (sec, &info->qx1[3], &info->qx2[3], &info->qy1[3],
		       &info->qy2[3]))
	    return (1);

	/* Get and parse section specification for statsec */
	c_clgpseta (ps, "statsec", sec, 40);
	if (parse_sec (sec, &info->statlim[0], &info->statlim[1],
		       &info->statlim[2], &info->statlim[3]))
	    return (1);

	/* Close the pset */
	c_clcpset (ps);

	/* Free memory */
	free (sec);

	return(0);

# else

	int q;

	for (q=0; q<4; q++) {
	     info->qx1[q] = QXI[q];
	     info->qx2[q] = QXF[q];
	     info->qy1[q] = QYI[q];
	     info->qy2[q] = QYF[q];
	}

	info->statlim[0] = 0;
	info->statlim[1] = NIC_XSIZE-1;
	info->statlim[2] = 0;
	info->statlim[3] = NIC_YSIZE-1;

	return(0);
# endif

}

int parse_sec (char *sec, int *x1, int *x2, int *y1, int *y2) {

	/* Local variables */
	int  len, n;
	char *p;
	char *first;

	len = strlen (sec);

	/* Check for non-null string */
	if (len == 0) goto error;
	n = 0;
	p = &sec[n];

	/* Check for opening bracket */
	if (*p != '[')
	    goto error;

	/* Decode x-limits */
	first = NULL;
	*x1 = 0; *x2 = 0;
	n++; p++;
	while (n < len) {
	     if (*p == '*') {
		 *x1 = 1; *x2 = NIC_XSIZE;
		 break;
	     } else if (*p == ':') {
		 if (first != NULL) {
		     *x1 = atoi(first);
		     first = NULL;
		 } else goto error;
	     } else if (*p == ',') {
		 if (first != NULL) {
		     *x2 = atoi(first);
		     break;
		 } else goto error;
	     } else {
		 if (first == NULL)
		     first = p;
	     }
	     n++; p++;
	}

	/* Check for valid limits */
	if (*x1 < 1 || *x1 > NIC_XSIZE || *x2 < 1 || *x2 > NIC_XSIZE ||
	    *x1 > *x2) goto error;
	    
	/* Decode y-limits */
	first = NULL;
	*y1 = 0; *y2 = 0;
	n++; p++;
	while (n < len) {
	     if (*p == '*') {
		 *y1 = 1; *y2 = NIC_YSIZE;
		  break;
	     } else if (*p == ':') {
		  if (first != NULL) {
		      *y1 = atoi(first);
		      first = NULL;
		  } else goto error;
	     } else if (*p == ']') {
		  if (first != NULL) {
		      *y2 = atoi(first);
		      break;
		  } else goto error;
	     } else {
		  if (first == NULL)
		      first = p;
	     }
	     n++; p++;
	}

	/* Check for valid limits */
	if (*y1 < 1 || *y1 > NIC_YSIZE || *y2 < 1 || *y2 > NIC_YSIZE ||
	    *y1 > *y2) goto error;
	    
	/* Reset all limits to be zero-indexed */
	*x1 -= 1; *x2 -= 1; *y1 -= 1; *y2 -= 1;

	return (0);

error:
	sprintf (MsgText, "Invalid section specification in statregions");
	n_error (MsgText);
	return (1);

}

