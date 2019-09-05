# include <stdlib.h>
# include <string.h>
# include <math.h>

# define IBUFLEN    80			/* length of input text buffers */

static char *fromHMS (char *cp, double *value);
static double onePart (char *cp, int *ip, int *done);

/*
Conversion from hms format.
	2003 July 30  Phil Hodge
*/

double getRA (char **cp) {

	double	ra;		/* value to be read from cp */
	int	found_colon;	/* true if ':' is present in string */
	char	*cp_l;		/* local pointer to cp */

	while (**cp == ' ' && **cp != '\0')	/* skip over leading blanks */
	    (*cp)++;

	found_colon = 0;
	cp_l = *cp;
	while (*cp_l != ' ' && *cp_l != ',' && *cp_l != '\0') {
	    if (*cp_l == ':') {
		found_colon = 1;
		break;
	    }
	    cp_l++;
	}

	*cp = fromHMS (*cp, &ra);

	if (found_colon)
	    ra *= 15.;

	return (ra);
}

double getDec (char **cp) {

	double	dec;		/* value to be read from cp */

	while ((**cp == ' ' || **cp == ',') && **cp != '\0')
	    (*cp)++;

	*cp = fromHMS (*cp, &dec);

	return (dec);
}

static char *fromHMS (char *cp, double *value) {

/* Read hours, minutes, seconds (or degrees, arcmin, arcsec) from a string
   pointed to by cp.  The time (or angle) is in the format HH:MM:SS.d
   (or HH:MM.d) or decimal degrees.  The field containing the value to be
   read is terminated by a blank, comma or null.
*/

	double	hrs, min, sec;
	int	ip;		/* offset from beginning of cp */
	int	chgsign, done;

	if (*cp == '-') {
	    chgsign = 1;
	    cp++;
	} else {
	    chgsign = 0;
	}

	done = 0;		/* done and ip are updated by onePart */
	ip = 0;
	*value = 0.;

	hrs = onePart (cp, &ip, &done);
	*value = hrs;

	if ( ! done ) {
	    min = onePart (cp, &ip, &done);
	    *value += min / 60.;

	    if ( ! done ) {
		sec = onePart (cp, &ip, &done);
		*value += sec / 3600.;
	    }
	}

	if (chgsign)
	    *value = -*value;

	return (cp + ip);
}

static double onePart (char *cp, int *ip, int *done) {

/* arguments:
char	*cp;		i: input string
int	*ip;		io: offset within string
int	*done;		io: set to true if EOS reached
*/

	char	buf[IBUFLEN+1];
	int	k;

	cp += *ip;		/* offset according to ip */

	/* Copy to a local buffer, stopping at ':'. */
	for (k = 0;  k < IBUFLEN;  k++) {
	    if (*cp == '\0') {
		*done = 1;
		break;
	    } else if (*cp == ' ' || *cp == ',') {
		(*ip)++;
		*done = 1;
		break;
	    } else if (*cp == ':') {
		(*ip)++;
		break;
	    } else {
		buf[k] = *cp;
		cp++;
		(*ip)++;
	    }
	}
	buf[k] = '\0';

	return (atof (buf));
}
