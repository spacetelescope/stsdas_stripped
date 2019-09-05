include "iraf$pkg/xtools/icfit/icfit.h"
include "pls.h"

# DOSETUP -- Setup the fit.  This is called at the start of each call
# to ic_fit to update the fitting parameters if necessary.

procedure dosetup (ic, pn, plate_model, npts, nterms, mdx, mdy)

pointer	ic				# ICFIT pointer
pointer pn 
int	plate_model[NTERMS_MODEL]
int	npts				# Number of points in data
int	nterms				# Number of terms in working model
double	mdx[npts,nterms]		# o: Current plate model array in X
double	mdy[npts,nterms]		# o: Current plate model array in Y
int	tn
int	star, i, k, deleted, jj, jof

begin
	# Set sample points.
	if (npts == 0)
	   call error (0, "No data points for fit")

	tn = 0
	do k = 1, NTERMS_MODEL {
	   if (plate_model[k] == 1) {
	      tn = tn + 1
	      jof = (k-1)*npts
	      do star = 1, npts {
		 jj = star-1 + jof		# XA(star,k)
		 mdx[star,tn] = Memd[XPA(pn)+jj]
		 mdy[star,tn] = Memd[YPA(pn)+jj]
	      }
	   }
	}
	IC_NFIT(ic) = npts - IC_NREJECT(ic)

	if (IC_NEWWTS(ic) == YES) {   
	   deleted = 0
	   do i = 0, npts-1 {
	      if (Memr[PW(pn)+i] == 0) {
		 Memi[POFLAG(pn)+i) = 0
		 deleted = deleted + 1	      
	      }	
	   }
	   IC_NREJECT(ic) = deleted
	   IC_NFIT(ic) = npts - deleted
	   IC_NEWWTS(ic) = NO
	}
end
