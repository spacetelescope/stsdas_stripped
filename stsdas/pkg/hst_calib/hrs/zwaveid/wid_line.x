include	"line.h"

#---------------------------------------------------------------------------
.help line.x 11Apr95 source
.ih
NAME
.nf
.fi
.endhelp
#---------------------------------------------------------------------------
pointer procedure wid_ll_alloc (n)

int	n			# I:  Initial number of lines in list.

# Declarations
pointer	ll			# Line List object.

errchk	calloc, malloc

begin
	call calloc (ll, LL_SZ, TY_STRUCT)

	if (IS_INDEFI(n))
	    LL_MAXN(ll) = LL_GROW
	else
	    LL_MAXN(ll) = max (n, LL_GROW)
	
	call malloc (LL_WAVE_PTR(ll), LL_MAXN(ll), TY_DOUBLE)

	call malloc (LL_WAVEO_PTR(ll), LL_MAXN(ll), TY_DOUBLE)
	call amovkd (INDEFD, LL_WAVEO(ll,1), LL_MAXN(ll))

	call malloc (LL_INTP_PTR(ll), LL_MAXN(ll), TY_REAL)
	call amovkr (INDEFR, LL_INTP(ll,1), LL_MAXN(ll))

	call malloc (LL_POSP_PTR(ll), LL_MAXN(ll), TY_REAL)
	call amovkr (INDEFR, LL_POSP(ll,1), LL_MAXN(ll))

	call malloc (LL_POSO_PTR(ll), LL_MAXN(ll), TY_REAL)
	call amovkr (INDEFR, LL_POSO(ll,1), LL_MAXN(ll))

	return (ll)
end
#---------------------------------------------------------------------------
# End of wid_ll_alloc
#---------------------------------------------------------------------------
procedure wid_ll_free (ll)

pointer	ll			# IO: Line List, NULL on return.

# Declarations.
errchk	mfree

begin
	call mfree (LL_WAVE_PTR(ll), TY_DOUBLE)
	call mfree (LL_WAVEO_PTR(ll), TY_DOUBLE)
	call mfree (LL_INTP_PTR(ll), TY_REAL)
	call mfree (LL_POSP_PTR(ll), TY_REAL)
	call mfree (LL_POSO_PTR(ll), TY_REAL)
	
	call mfree (ll, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of wid_ll_free
#---------------------------------------------------------------------------
procedure wid_ll_add (ll, wave, intp, posp, poso, waveo)

pointer	ll			# I:  Line List object.
double	wave			# I:  Wavelength.
real	intp			# I:  Intensity (predicted)
real	posp			# I:  Position (pixel, predicted)
real	poso			# I:  Postition (pixel, observed)
double	waveo			# I:  Wavelength (observed)

begin
	LL_N(ll) = LL_N(ll) + 1
	if (LL_N(ll) > LL_MAXN(ll)) {
	    LL_MAXN(ll) = LL_MAXN(ll) + LL_GROW

	    call realloc (LL_WAVE_PTR(ll), LL_MAXN(ll), TY_DOUBLE)

	    call realloc (LL_WAVEO_PTR(ll), LL_MAXN(ll), TY_DOUBLE)
	    call amovkd (INDEFD, LL_WAVEO(ll,LL_N(Ll)), LL_MAXN(ll))

	    call realloc (LL_INTP_PTR(ll), LL_MAXN(ll), TY_REAL)
	    call amovkr (INDEFR, LL_INTP(ll,LL_N(Ll)), LL_MAXN(ll))

	    call realloc (LL_POSP_PTR(ll), LL_MAXN(ll), TY_REAL)
	    call amovkr (INDEFR, LL_POSP(ll,LL_N(Ll)), LL_MAXN(ll))

	    call realloc (LL_POSO_PTR(ll), LL_MAXN(ll), TY_REAL)
	    call amovkr (INDEFR, LL_POSO(ll,LL_N(Ll)), LL_MAXN(ll))
	}

	LL_WAVE(ll,LL_N(ll)) = wave
	LL_INTP(ll,LL_N(ll)) = intp
	LL_POSP(ll,LL_N(ll)) = posp
	LL_POSO(ll,LL_N(ll)) = poso
	LL_WAVEO(ll,LL_N(ll)) = waveo
end
#---------------------------------------------------------------------------
# End of wid_ll_add
#---------------------------------------------------------------------------
procedure wid_ll_no_dups (ll)

pointer	ll			# I:  Line List to remove duplicates from.

# Declarations
int	g			# Element to copy into next element.
int	l			# Next element in input list.
int	n			# Next element in the non-duplicated list.
int	p, np			# Observed pixel positions of lines.

begin
	n = 1
	g = 1
	p = LL_POSO(ll,1) + 0.5
	do l = 2, LL_N(ll) {
	    np = LL_POSO(ll,l) + 0.5
	    if (np == p) {
		if (LL_INTP(ll,l) > LL_INTP(ll,g)) {
		    g = l
		}
	    } else {
		LL_WAVE(ll,n) = LL_WAVE(ll,g)
		LL_INTP(ll,n) = LL_INTP(ll,g)
		LL_POSP(ll,n) = LL_POSP(ll,g)
		LL_POSO(ll,n) = LL_POSO(ll,g)
		LL_WAVEO(ll,n) = LL_WAVEO(ll,g)
		LL_INTP(ll,n) = LL_INTP(ll,g)
		n = n + 1
		p = np
		g = l
	    }
	}

	if (n > 1)
	    n = n - 1
	LL_N(ll) = n
end
#---------------------------------------------------------------------------
# End of wid_ll_no_dups
#---------------------------------------------------------------------------
