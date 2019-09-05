# include <hstio.h>
# include <stdio.h>
# include <math.h>
# include "msarith.h"


/* N_KEYWORD: These routines read and write keyword values in an image header.
** When writing a keyword value and the keyword doesn't already exist,
** the keyword and comment are added to the header. The routines are:
**
** getKeyB: read a Boolean keyword
** getKeyD: read a double precision keyword
** getKeyF: read a (single precision) floating-point keyword
** getKeyI: read an integer keyword
** getKeyS: read a string keyword
**
** putKeyB: write a Boolean keyword
** putKeyD: write a double precision keyword
** putKeyF: write a (single precision) floating-point keyword
** putKeyI: write an integer keyword
** putKeyS: write a string keyword


   Borrowed from CALNICA code in 01 Feb 96 (I. Busko)


*/

int getKeyB (Hdr *hdr, char *keyword, Bool *value) {

/* Arguments:
**	hdr	i: pointer to header to be read
**	keyword	i: name of keyword
**	value	o: value of keyword
*/

	/* Local variables */
	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound) {
	    sprintf (ErrText, "Keyword `%s' not found", keyword);
	    n_error (ErrText);
	    return (1);
	} else
	    *value = getBoolKw (kw);

	if (hstio_err())
	    return (1);

	return (0);
}

int getKeyD (Hdr *hdr, char *keyword, double *value) {

/* Arguments:
**	hdr	i: pointer to header to be read
**	keyword	i: name of keyword
**	value	o: value of keyword
*/

	/* Local variables */
	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound) {
	    sprintf (ErrText, "Keyword `%s' not found", keyword);
	    n_error (ErrText);
	    return (1);
	} else
	    *value = getDoubleKw (kw);

	if (hstio_err())
	    return (1);

	return (0);
}

int getKeyF (Hdr *hdr, char *keyword, float *value) {

/* Arguments:
**	hdr	i: pointer to header to be read
**	keyword	i: name of keyword
**	value	o: value of keyword
*/

	/* Local variables */
	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound) {
	    sprintf (ErrText, "Keyword `%s' not found", keyword);
	    n_error (ErrText);
	    return (1);
	} else
	    *value = getFloatKw (kw);

	if (hstio_err())
	    return (1);

	return (0);
}

int getKeyI (Hdr *hdr, char *keyword, int *value) {

/* Arguments:
**	hdr	i: pointer to header to be read
**	keyword	i: name of keyword
**	value	o: value of keyword
*/

	/* Local variables */
	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound) {
	    sprintf (ErrText, "Keyword `%s' not found", keyword);
	    n_error (ErrText);
	    return (1);
	} else
	    *value = getIntKw (kw);

	if (hstio_err())
	    return (1);

	return (0);
}

int getKeyS (Hdr *hdr, char *keyword, char *value) {

/* Arguments:
**	hdr	i: pointer to header to be read
**	keyword	i: name of keyword
**	value	o: value of keyword
*/

	/* Local variables */
	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound) {
	    sprintf (ErrText, "Keyword `%s' not found", keyword);
	    n_error (ErrText);
	    return (1);
	} else
	    getStringKw (kw, value, 70);

	if (hstio_err())
	    return (1);

	return (0);
}

int putKeyB (Hdr *hdr, char *keyword, Bool value, char *comment) {

/* Arguments:
**	hdr	i: pointer to header to be updated
**	keyword	i: name of keyword
**	value	i: value of keyword
**	comment	i: comment to add with keyword if keyword doesn't exist
*/

	FitsKw kw;	/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound)
	    addBoolKw (hdr, keyword, value, comment);
	else
	    putBoolKw (kw, value);

	if (hstio_err())
	    return (1);

	return (0);
}

int putKeyD (Hdr *hdr, char *keyword, double value, char *comment) {

/* Arguments:
**	hdr	i: pointer to header to be updated
**	keyword	i: name of keyword
**	value	i: value of keyword
**	comment	i: comment to add with keyword if keyword doesn't exist
*/

	FitsKw kw;	/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound)
	    addDoubleKw (hdr, keyword, value, comment);
	else
	    putDoubleKw (kw, value);

	if (hstio_err())
	    return (1);

	return (0);
}

int putKeyF (Hdr *hdr, char *keyword, float value, char *comment) {

/* Arguments:
**	hdr	i: pointer to header to be updated
**	keyword	i: name of keyword
**	value	i: value of keyword
**	comment	i: comment to add with keyword if keyword doesn't exist
*/

	FitsKw kw;	/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound) {
	    addFloatKw (hdr, keyword, value, comment);
	} else {
	    putFloatKw (kw, value);
	}

	if (hstio_err())
	    return (1);

	return (0);
}

int putKeyI (Hdr *hdr, char *keyword, int value, char *comment) {

/* Arguments:
**	hdr	i: pointer to header to be updated
**	keyword	i: name of keyword
**	value	i: value of keyword
**	comment	i: comment to add with keyword if keyword doesn't exist
*/

	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound)
	    addIntKw (hdr, keyword, value, comment);
	else
	    putIntKw (kw, value);

	if (hstio_err())
	    return (1);

	return (0);
}

int putKeyS (Hdr *hdr, char *keyword, char *value, char *comment) {

/* Arguments:
**	hdr	i: pointer to header to be updated
**	keyword	i: name of keyword
**	value	i: value of keyword
**	comment	i: comment to add with keyword if keyword doesn't exist
*/

	FitsKw kw;		/* keyword pointer */

	kw = findKw (hdr, keyword);
	if (kw == NotFound)
	    addStringKw (hdr, keyword, value, comment);
	else
	    putStringKw (kw, value);

	if (hstio_err())
	    return (1);

	return (0);
}

