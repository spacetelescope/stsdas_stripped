#include "libsynphot.h"

/* BREAKMODE -- Break instrument mode string into separate keywords */

 breakmode (mode, mxlist, mxparam, nmode, nparam, paramlist, modelist)

char	mode[ARB]			/* i: instrument mode string */
int	mxlist				/* i: Maximum number of keywords in list */
int	mxparam				/* i: Max number of params per keyword */
int	nmode				/* o: number of keywords in mode string */
int	nparam[ARB]			/* o: number of parameters per keyword */
real	paramlist[ARB][mxparam]		/* o: parameters of each keyword */
char	modelist[ARB][SZ_KEYWRD]		/* o: list of instrument mode keywords */
/*-- */
int	ic

string	overlist  "Too many keywords in list. Last keyword read"

int	word_fetch()


{
	/* Break instrument mode string into keywords */

	ic = 1
	for (nmode = 1; nmode <= mxlist; nmode = nmode + 1) {
	    if (word_fetch (mode, ic, modelist[nmode][1], SZ_KEYWRD) == 0) {
		nmode = nmode - 1
		break
	    }

	    /* Extract any parameters from keyword string */

	    strlwr (modelist[nmode][1])

	    breakparam (modelist[nmode][1], mxparam, nparam[nmode], 
			     paramlist[nmode][1])
	}

	if (nmode > mxlist)
	    synphoterr (overlist, modelist[mxlist][1])

}
