include	<error.h>
include	"wf.h"

# Memory management.
define	X1			Memd[x1+$1-1]
define	X2			Memd[x2+$1-1]
define	Y			Memd[y+$1-1]
define  Sig			Memd[sig+$1-1]
define	Yc			Memd[yc+$1-1]
define	Diff			Memd[diff+$1-1]
define	Covar			Memd[covar]
define	V			Memd[v+$1-1]
define	Lista			Memi[lista+$1-1]

#---------------------------------------------------------------------------
.help wf_wavefit 24Mar95 source
.ih
NAME
wf_wavefit -- Fit Wavelength/Order to observations.
.endhelp
#---------------------------------------------------------------------------
procedure wf_wavefit (wf, model, func)

pointer	wf			# I:  Wavefit object.
char	model[ARB]		# I:  Name of table to write model out.
extern	func			# I:  Function to do the fitting with.

# Declarations.
double	chisq			# Chisq of fit.
pointer	covar			# Covariance matrix.
double	dx			# Generic.
int	i, j			# Generic.
int	iter			# Current iteration in fit regression.
pointer	lista			# List of coefficients to fit.
double	mean, sigma		# Status on the difference.
int	mfit			# Number of coefficients fit.
int	ndata, nndata		# Number of data points left to fit.
pointer	v			# Values for the basis functions.
pointer	x1, x2, y, sig		# Temporary arrays to hold the data to fit.
pointer	yc, diff		# Fit computation and comparison.

errchk	malloc, mfree

begin
        call malloc (x1, WF_NDATA(wf), TY_DOUBLE)
        call malloc (x2, WF_NDATA(wf), TY_DOUBLE)
        call malloc (y, WF_NDATA(wf), TY_DOUBLE)
        call malloc (sig, WF_NDATA(wf), TY_DOUBLE)
        call malloc (yc, WF_NDATA(wf), TY_DOUBLE)
        call malloc (diff, WF_NDATA(wf), TY_DOUBLE)
        call malloc (covar, WF_NCOEF(wf)*WF_NCOEF(wf), TY_DOUBLE)
        call malloc (v, WF_NCOEF(wf), TY_DOUBLE)
        call malloc (lista, WF_NCOEF(wf), TY_INT)

        call amovd (WF_X1(wf,1), X1(1), WF_NDATA(wf))
        call amovd (WF_X2(wf,1), X2(1), WF_NDATA(wf))
        call amovd (WF_Y(wf,1), Y(1), WF_NDATA(wf))
        call amovd (WF_SIG(wf,1), Sig(1), WF_NDATA(wf))
        ndata = WF_NDATA(wf)

        # Fill list of coefficients being fitted for.
        mfit = 0
        do i = 1, WF_NCOEF(wf)
            if (WF_AFIT(wf,i) == YES) {
                mfit = mfit + 1
                Lista(mfit) = i
            }

        # Fit and throw out data until max tries have been reached or no
        # data is removed.
        do iter = 1, WF_NTRY(wf) {

            # Fit the data.
            call lfit2 (X1(1), X2(1), Y(1), Sig(1), ndata, WF_A(wf,1),
                        WF_NCOEF(wf), Lista(1), mfit, Covar, WF_NCOEF(wf),
                        chisq, func, i)
            if (i != 0)
                call error (1, "could not fit dispersion coefficients")

            call printf ("wavefit: Iteration %d: %d lines fit, chisq = %g\n")
            call pargi (iter)
            call pargi (ndata)
            call pargd (chisq)

            # Compute model based on fit.
            do i = 1, ndata {
                Yc(i) = 0.d0
                call func (X1(i), X2(i), V(1), WF_NCOEF(wf))
                do j = 1, WF_NCOEF(wf)
                    Yc(i) = Yc(i) + WF_A(wf,j) * V(j)
            }

            # Compute sigma of fit.
            call asubd (Y(1), Yc(1), Diff(1), ndata)
            call aavgd (Diff(1), ndata, mean, sigma)

            # If the last iteration, don't bother doing the rejection.
            if (iter == WF_NTRY(wf)) {
                call eprintf ("wavefit: maximum iterations reached.\n")
                break
            }

            # Remove lines outside of the threshold.
            nndata = 0
            do i = 1, ndata
                if (abs(Diff(i)) <= WF_NSIG(wf) * sigma) {
                    nndata = nndata + 1
                    X1(nndata) = X1(i)
                    X2(nndata) = X2(i)
                    Y(nndata) = Y(i)
                    Sig(nndata) = Sig(i)
                }

            # If no points are removed, then we're done.
            if (nndata == ndata)
                break

            # Setup for next iteration.
            call printf ("\tRemoving %d lines and fitting again...\n")
            call pargi (ndata - nndata)
            ndata = nndata

            # Check to see if there are enough points to continue fitting.
            if (ndata <= WF_NCOEF(wf)) {
                call eprintf ("wavefit: not enough points left for fitting\n")
                break
            }
        }

        # Write out the fit and residules.
	iferr (call wf_m_wtab (model, WF_GRATING(wf), WF_APER(wf),
			       WF_CARPOS(wf), X1(1), X2(1), Y(1), Yc(1),
                               Diff(1), ndata)) {
            call erract (EA_WARN)
            call eprintf ("\tModel table will not be written.\n")
        }

        # Convert the coefficients from the lamp to SSA
	j = WF_CARPOS(wf)
        if (!IS_INDEFD(WF_CS(wf,1))) {
            dx = 1.0d0 - (WF_CS(wf,3) + WF_CS(wf,4) * j)
            do i = 1, WF_NCOEF(wf)
                WF_A(wf,i) = WF_A(wf,i) * dx
            WF_A(wf,1) = WF_A(wf,1) - (WF_CS(wf,1) + WF_CS(wf,2)*j +
                                 WF_CS(wf,5)*j*j)
            WF_A(wf,4) = WF_A(wf,4) - WF_CS(wf,6)
        }

        # That's all folks.
        call mfree (x1, TY_DOUBLE)
        call mfree (x2, TY_DOUBLE)
        call mfree (y, TY_DOUBLE)
        call mfree (sig, TY_DOUBLE)
        call mfree (yc, TY_DOUBLE)
        call mfree (diff, TY_DOUBLE)
        call mfree (covar, TY_DOUBLE)
        call mfree (v, TY_DOUBLE)
        call mfree (lista, TY_INT)
end
#---------------------------------------------------------------------------
# End of wf_wavefit
#---------------------------------------------------------------------------
