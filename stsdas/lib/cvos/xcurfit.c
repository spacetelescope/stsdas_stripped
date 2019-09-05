# include <xcurfit.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define dcvb1g_ dcvb1g
# endif
	extern void dcvb1g_(double *, int *, double *, double *, double *);
void c_dcv_b1leg(double *x, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dcvb1g_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvb1b_ dcvb1b
# endif
	extern void dcvb1b_(double *, int *, double *, double *, double *);
void c_dcv_b1cheb(double *x, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dcvb1b_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvb11_ dcvb11
# endif
	extern void dcvb11_(double *, int *, double *, double *, double *, int *);
void c_dcv_b1spline1(double *x, int *npieces, double *k1, double *k2, double *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	dcvb11_(x, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvb13_ dcvb13
# endif
	extern void dcvb13_(double *, int *, double *, double *, double *, int *);
void c_dcv_b1spline3(double *x, int *npieces, double *k1, double *k2, double *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	dcvb13_(x, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvb1g_ rcvb1g
# endif
	extern void rcvb1g_(float *, int *, float *, float *, float *);
void c_rcv_b1leg(float *x, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rcvb1g_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvb1b_ rcvb1b
# endif
	extern void rcvb1b_(float *, int *, float *, float *, float *);
void c_rcv_b1cheb(float *x, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rcvb1b_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvb11_ rcvb11
# endif
	extern void rcvb11_(float *, int *, float *, float *, float *, int *);
void c_rcv_b1spline1(float *x, int *npieces, float *k1, float *k2, float *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	rcvb11_(x, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvb13_ rcvb13
# endif
	extern void rcvb13_(float *, int *, float *, float *, float *, int *);
void c_rcv_b1spline3(float *x, int *npieces, float *k1, float *k2, float *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	rcvb13_(x, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvbcb_ dcvbcb
# endif
	extern void dcvbcb_(double *, int *, int *, double *, double *, double *);
void c_dcv_bcheb(double *x, int *npts, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dcvbcb_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvblg_ dcvblg
# endif
	extern void dcvblg_(double *, int *, int *, double *, double *, double *);
void c_dcv_bleg(double *x, int *npts, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dcvblg_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvbs1_ dcvbs1
# endif
	extern void dcvbs1_(double *, int *, int *, double *, double *, double *, int *);
void c_dcv_bspline1(double *x, int *npts, int *npieces, double *k1, double *k2, double *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	dcvbs1_(x, npts, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvbs3_ dcvbs3
# endif
	extern void dcvbs3_(double *, int *, int *, double *, double *, double *, int *);
void c_dcv_bspline3(double *x, int *npts, int *npieces, double *k1, double *k2, double *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	dcvbs3_(x, npts, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvbcb_ rcvbcb
# endif
	extern void rcvbcb_(float *, int *, int *, float *, float *, float *);
void c_rcv_bcheb(float *x, int *npts, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rcvbcb_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvblg_ rcvblg
# endif
	extern void rcvblg_(float *, int *, int *, float *, float *, float *);
void c_rcv_bleg(float *x, int *npts, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rcvblg_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvbs1_ rcvbs1
# endif
	extern void rcvbs1_(float *, int *, int *, float *, float *, float *, int *);
void c_rcv_bspline1(float *x, int *npts, int *npieces, float *k1, float *k2, float *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	rcvbs1_(x, npts, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvbs3_ rcvbs3
# endif
	extern void rcvbs3_(float *, int *, int *, float *, float *, float *, int *);
void c_rcv_bspline3(float *x, int *npts, int *npieces, float *k1, float *k2, float *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	rcvbs3_(x, npts, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvevb_ dcvevb
# endif
	extern void dcvevb_(double *, double *, double *, int *, int *, double *, double *);
void c_dcv_evcheb(double *coeff, double *x, double *yfit, int *npts, int *order, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	dcvevb_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvevg_ dcvevg
# endif
	extern void dcvevg_(double *, double *, double *, int *, int *, double *, double *);
void c_dcv_evleg(double *coeff, double *x, double *yfit, int *npts, int *order, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	dcvevg_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvev1_ dcvev1
# endif
	extern void dcvev1_(double *, double *, double *, int *, int *, double *, double *);
void c_dcv_evspline1(double *coeff, double *x, double *yfit, int *npts, int *npieces, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	dcvev1_(coeff, x, yfit, npts, npieces, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvev3_ dcvev3
# endif
	extern void dcvev3_(double *, double *, double *, int *, int *, double *, double *);
void c_dcv_evspline3(double *coeff, double *x, double *yfit, int *npts, int *npieces, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	dcvev3_(coeff, x, yfit, npts, npieces, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvevb_ rcvevb
# endif
	extern void rcvevb_(float *, float *, float *, int *, int *, float *, float *);
void c_rcv_evcheb(float *coeff, float *x, float *yfit, int *npts, int *order, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	rcvevb_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvevg_ rcvevg
# endif
	extern void rcvevg_(float *, float *, float *, int *, int *, float *, float *);
void c_rcv_evleg(float *coeff, float *x, float *yfit, int *npts, int *order, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	rcvevg_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvev1_ rcvev1
# endif
	extern void rcvev1_(float *, float *, float *, int *, int *, float *, float *);
void c_rcv_evspline1(float *coeff, float *x, float *yfit, int *npts, int *npieces, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	rcvev1_(coeff, x, yfit, npts, npieces, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvev3_ rcvev3
# endif
	extern void rcvev3_(float *, float *, float *, int *, int *, float *, float *);
void c_rcv_evspline3(float *coeff, float *x, float *yfit, int *npts, int *npieces, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	rcvev3_(coeff, x, yfit, npts, npieces, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvacm_ dcvacm
# endif
	extern void dcvacm_(IRAFPointer *, double *, double *, double *, int *);
void c_dcvaccum(IRAFPointer *cv, double *x, double *y, double *w, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	dcvacm_(cv, x, y, w, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvaccm_ cvaccm
# endif
	extern void cvaccm_(IRAFPointer *, float *, float *, float *, int *);
void c_cvaccum(IRAFPointer *cv, float *x, float *y, float *w, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	cvaccm_(cv, x, y, w, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvacs_ dcvacs
# endif
	extern void dcvacs_(IRAFPointer *, double *, double *, double *, int *, int *);
void c_dcvacpts(IRAFPointer *cv, double *x, double *y, double *w, int *npts, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	dcvacs_(cv, x, y, w, npts, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvacps_ cvacps
# endif
	extern void cvacps_(IRAFPointer *, float *, float *, float *, int *, int *);
void c_cvacpts(IRAFPointer *cv, float *x, float *y, float *w, int *npts, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	cvacps_(cv, x, y, w, npts, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvchc_ dcvchc
# endif
	extern void dcvchc_(double *, int *, int *, double *, int *);
void c_dcvchofac(double *matrix, int *nbands, int *nrows, double *matfac, int *ier) {
	clear_cvoserr();
	xerpsh_();
	dcvchc_(matrix, nbands, nrows, matfac, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvchv_ dcvchv
# endif
	extern void dcvchv_(double *, int *, int *, double *, double *);
void c_dcvchoslv(double *matfac, int *nbands, int *nrows, double *vector, double *coeff) {
	clear_cvoserr();
	xerpsh_();
	dcvchv_(matfac, nbands, nrows, vector, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvchc_ rcvchc
# endif
	extern void rcvchc_(float *, int *, int *, float *, int *);
void c_rcvchofac(float *matrix, int *nbands, int *nrows, float *matfac, int *ier) {
	clear_cvoserr();
	xerpsh_();
	rcvchc_(matrix, nbands, nrows, matfac, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvchv_ rcvchv
# endif
	extern void rcvchv_(float *, int *, int *, float *, float *);
void c_rcvchoslv(float *matfac, int *nbands, int *nrows, float *vector, float *coeff) {
	clear_cvoserr();
	xerpsh_();
	rcvchv_(matfac, nbands, nrows, vector, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvcof_ dcvcof
# endif
	extern void dcvcof_(IRAFPointer *, double *, int *);
void c_dcvcoeff(IRAFPointer *cv, double *coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvcof_(cv, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvcoef_ cvcoef
# endif
	extern void cvcoef_(IRAFPointer *, float *, int *);
void c_cvcoeff(IRAFPointer *cv, float *coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	cvcoef_(cv, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvers_ dcvers
# endif
	extern void dcvers_(IRAFPointer *, double *, double *, double *, int *, double *, double *);
void c_dcverrors(IRAFPointer *cv, double *y, double *w, double *yfit, int *npts, double *chisqr, double *errors) {
	clear_cvoserr();
	xerpsh_();
	dcvers_(cv, y, w, yfit, npts, chisqr, errors);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cverrs_ cverrs
# endif
	extern void cverrs_(IRAFPointer *, float *, float *, float *, int *, float *, float *);
void c_cverrors(IRAFPointer *cv, float *y, float *w, float *yfit, int *npts, float *chisqr, float *errors) {
	clear_cvoserr();
	xerpsh_();
	cverrs_(cv, y, w, yfit, npts, chisqr, errors);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvevl_ dcvevl
# endif
	extern double dcvevl_(IRAFPointer *, double *);
double c_dcveval(IRAFPointer *cv, double *x) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dcvevl_(cv, x);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define cveval_ cveval
# endif
	extern float cveval_(IRAFPointer *, float *);
float c_cveval(IRAFPointer *cv, float *x) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = cveval_(cv, x);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dcvfit_ dcvfit
# endif
	extern void dcvfit_(IRAFPointer *, double *, double *, double *, int *, int *, int *);
void c_dcvfit(IRAFPointer *cv, double *x, double *y, double *w, int *npts, int *wtflag, int *ier) {
	clear_cvoserr();
	xerpsh_();
	dcvfit_(cv, x, y, w, npts, wtflag, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvfit_ cvfit
# endif
	extern void cvfit_(IRAFPointer *, float *, float *, float *, int *, int *, int *);
void c_cvfit(IRAFPointer *cv, float *x, float *y, float *w, int *npts, int *wtflag, int *ier) {
	clear_cvoserr();
	xerpsh_();
	cvfit_(cv, x, y, w, npts, wtflag, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvfre_ dcvfre
# endif
	extern void dcvfre_(IRAFPointer *);
void c_dcvfree(IRAFPointer *cv) {
	clear_cvoserr();
	xerpsh_();
	dcvfre_(cv);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvfree_ cvfree
# endif
	extern void cvfree_(IRAFPointer *);
void c_cvfree(IRAFPointer *cv) {
	clear_cvoserr();
	xerpsh_();
	cvfree_(cv);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvint_ dcvint
# endif
	extern void dcvint_(IRAFPointer *, int *, int *, double *, double *);
void c_dcvinit(IRAFPointer *cv, int *curve_type, int *order, double *xmin, double *xmax) {
	clear_cvoserr();
	xerpsh_();
	dcvint_(cv, curve_type, order, xmin, xmax);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvinit_ cvinit
# endif
	extern void cvinit_(IRAFPointer *, int *, int *, float *, float *);
void c_cvinit(IRAFPointer *cv, int *curve_type, int *order, float *xmin, float *xmax) {
	clear_cvoserr();
	xerpsh_();
	cvinit_(cv, curve_type, order, xmin, xmax);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvpor_ dcvpor
# endif
	extern void dcvpor_(IRAFPointer *, double *, int *);
void c_dcvpower(IRAFPointer *cv, double *ps_coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvpor_(cv, ps_coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvepr_ dcvepr
# endif
	extern void dcvepr_(IRAFPointer *, double *, double *, double *, int *, double *, double *);
void c_dcvepower(IRAFPointer *cv, double *y, double *w, double *yfit, int *npts, double *chisqr, double *perrors) {
	clear_cvoserr();
	xerpsh_();
	dcvepr_(cv, y, w, yfit, npts, chisqr, perrors);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvmln_ dcvmln
# endif
	extern void dcvmln_(double *, int *);
void c_dcv_mlegen(double *matrix, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvmln_(matrix, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvetm_ dcvetm
# endif
	extern void dcvetm_(IRAFPointer *, double *, double *, double *, int *);
void c_dcv_etransform(IRAFPointer *cv, double *covar, double *elm, double *perrors, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvetm_(cv, covar, elm, perrors, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvlen_ dcvlen
# endif
	extern void dcvlen_(double *, double *, double *, int *);
void c_dcv_legen(double *matrix, double *cf_coeff, double *ps_coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvlen_(matrix, cf_coeff, ps_coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvlef_ dcvlef
# endif
	extern double dcvlef_(int *, int *);
double c_dcv_legcoeff(int *k, int *n) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dcvlef_(k, n);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dcvmcy_ dcvmcy
# endif
	extern void dcvmcy_(double *, int *);
void c_dcv_mcheby(double *matrix, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvmcy_(matrix, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvchy_ dcvchy
# endif
	extern void dcvchy_(double *, double *, double *, int *);
void c_dcv_cheby(double *matrix, double *cf_coeff, double *ps_coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvchy_(matrix, cf_coeff, ps_coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvchf_ dcvchf
# endif
	extern double dcvchf_(int *, int *);
double c_dcv_chebcoeff(int *m, int *n) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dcvchf_(m, n);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dcvnoe_ dcvnoe
# endif
	extern void dcvnoe_(IRAFPointer *, double *, int *);
void c_dcv_normalize(IRAFPointer *cv, double *ps_coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvnoe_(cv, ps_coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvene_ dcvene
# endif
	extern void dcvene_(IRAFPointer *, double *, int *);
void c_dcv_enormalize(IRAFPointer *cv, double *elm, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvene_(cv, elm, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvbcf_ dcvbcf
# endif
	extern double dcvbcf_(int *, int *);
double c_dcv_bcoeff(int *n, int *i) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dcvbcf_(n, i);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dcvfal_ dcvfal
# endif
	extern double dcvfal_(int *);
double c_dcv_factorial(int *n) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dcvfal_(n);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define cvmmud_ cvmmud
# endif
	extern void cvmmud_(double *, double *, double *, int *);
void c_cv_mmuld(double *a, double *b, double *c, int *ndim) {
	clear_cvoserr();
	xerpsh_();
	cvmmud_(a, b, c, ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvpowr_ cvpowr
# endif
	extern void cvpowr_(IRAFPointer *, float *, int *);
void c_cvpower(IRAFPointer *cv, float *ps_coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	cvpowr_(cv, ps_coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvepor_ cvepor
# endif
	extern void cvepor_(IRAFPointer *, float *, float *, float *, int *, float *, float *);
void c_cvepower(IRAFPointer *cv, float *y, float *w, float *yfit, int *npts, float *chisqr, float *perrors) {
	clear_cvoserr();
	xerpsh_();
	cvepor_(cv, y, w, yfit, npts, chisqr, perrors);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvmln_ rcvmln
# endif
	extern void rcvmln_(double *, int *);
void c_rcv_mlegen(double *matrix, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	rcvmln_(matrix, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvetm_ rcvetm
# endif
	extern void rcvetm_(IRAFPointer *, double *, double *, float *, int *);
void c_rcv_etransform(IRAFPointer *cv, double *covar, double *elm, float *perrors, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	rcvetm_(cv, covar, elm, perrors, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvlen_ rcvlen
# endif
	extern void rcvlen_(double *, float *, float *, int *);
void c_rcv_legen(double *matrix, float *cf_coeff, float *ps_coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	rcvlen_(matrix, cf_coeff, ps_coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvlef_ rcvlef
# endif
	extern double rcvlef_(int *, int *);
double c_rcv_legcoeff(int *k, int *n) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rcvlef_(k, n);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rcvmcy_ rcvmcy
# endif
	extern void rcvmcy_(double *, int *);
void c_rcv_mcheby(double *matrix, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	rcvmcy_(matrix, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvchy_ rcvchy
# endif
	extern void rcvchy_(double *, float *, float *, int *);
void c_rcv_cheby(double *matrix, float *cf_coeff, float *ps_coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	rcvchy_(matrix, cf_coeff, ps_coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvchf_ rcvchf
# endif
	extern double rcvchf_(int *, int *);
double c_rcv_chebcoeff(int *m, int *n) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rcvchf_(m, n);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rcvnoe_ rcvnoe
# endif
	extern void rcvnoe_(IRAFPointer *, float *, int *);
void c_rcv_normalize(IRAFPointer *cv, float *ps_coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	rcvnoe_(cv, ps_coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvene_ rcvene
# endif
	extern void rcvene_(IRAFPointer *, double *, int *);
void c_rcv_enormalize(IRAFPointer *cv, double *elm, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	rcvene_(cv, elm, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rcvbcf_ rcvbcf
# endif
	extern double rcvbcf_(int *, int *);
double c_rcv_bcoeff(int *n, int *i) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rcvbcf_(n, i);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rcvfal_ rcvfal
# endif
	extern double rcvfal_(int *);
double c_rcv_factorial(int *n) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rcvfal_(n);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define cvmmur_ cvmmur
# endif
	extern void cvmmur_(float *, float *, float *, int *);
void c_cv_mmulr(float *a, float *b, float *c, int *ndim) {
	clear_cvoserr();
	xerpsh_();
	cvmmur_(a, b, c, ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvret_ dcvret
# endif
	extern void dcvret_(IRAFPointer *, double *, double *, double *, int *);
void c_dcvrefit(IRAFPointer *cv, double *x, double *y, double *w, int *ier) {
	clear_cvoserr();
	xerpsh_();
	dcvret_(cv, x, y, w, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvreft_ cvreft
# endif
	extern void cvreft_(IRAFPointer *, float *, float *, float *, int *);
void c_cvrefit(IRAFPointer *cv, float *x, float *y, float *w, int *ier) {
	clear_cvoserr();
	xerpsh_();
	cvreft_(cv, x, y, w, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvrjt_ dcvrjt
# endif
	extern void dcvrjt_(IRAFPointer *, double *, double *, double *);
void c_dcvrject(IRAFPointer *cv, double *x, double *y, double *w) {
	clear_cvoserr();
	xerpsh_();
	dcvrjt_(cv, x, y, w);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvrjet_ cvrjet
# endif
	extern void cvrjet_(IRAFPointer *, float *, float *, float *);
void c_cvrject(IRAFPointer *cv, float *x, float *y, float *w) {
	clear_cvoserr();
	xerpsh_();
	cvrjet_(cv, x, y, w);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvree_ dcvree
# endif
	extern void dcvree_(IRAFPointer *, double *);
void c_dcvrestore(IRAFPointer *cv, double *fit) {
	clear_cvoserr();
	xerpsh_();
	dcvree_(cv, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvrese_ cvrese
# endif
	extern void cvrese_(IRAFPointer *, float *);
void c_cvrestore(IRAFPointer *cv, float *fit) {
	clear_cvoserr();
	xerpsh_();
	cvrese_(cv, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvsae_ dcvsae
# endif
	extern void dcvsae_(IRAFPointer *, double *);
void c_dcvsave(IRAFPointer *cv, double *fit) {
	clear_cvoserr();
	xerpsh_();
	dcvsae_(cv, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvsave_ cvsave
# endif
	extern void cvsave_(IRAFPointer *, float *);
void c_cvsave(IRAFPointer *cv, float *fit) {
	clear_cvoserr();
	xerpsh_();
	cvsave_(cv, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvset_ dcvset
# endif
	extern void dcvset_(IRAFPointer *, int *, double *, double *, double *, int *);
void c_dcvset(IRAFPointer *cv, int *curve_type, double *xmin, double *xmax, double *coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dcvset_(cv, curve_type, xmin, xmax, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvset_ cvset
# endif
	extern void cvset_(IRAFPointer *, int *, float *, float *, float *, int *);
void c_cvset(IRAFPointer *cv, int *curve_type, float *xmin, float *xmax, float *coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	cvset_(cv, curve_type, xmin, xmax, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvsoe_ dcvsoe
# endif
	extern void dcvsoe_(IRAFPointer *, int *);
void c_dcvsolve(IRAFPointer *cv, int *ier) {
	clear_cvoserr();
	xerpsh_();
	dcvsoe_(cv, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvsole_ cvsole
# endif
	extern void cvsole_(IRAFPointer *, int *);
void c_cvsolve(IRAFPointer *cv, int *ier) {
	clear_cvoserr();
	xerpsh_();
	cvsole_(cv, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvsti_ dcvsti
# endif
	extern int dcvsti_(IRAFPointer *, int *);
int c_dcvstati(IRAFPointer *cv, int *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dcvsti_(cv, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dcvstd_ dcvstd
# endif
	extern double dcvstd_(IRAFPointer *, int *);
double c_dcvstatd(IRAFPointer *cv, int *param) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dcvstd_(cv, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define cvstai_ cvstai
# endif
	extern int cvstai_(IRAFPointer *, int *);
int c_cvstati(IRAFPointer *cv, int *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = cvstai_(cv, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define cvstar_ cvstar
# endif
	extern float cvstar_(IRAFPointer *, int *);
float c_cvstatr(IRAFPointer *cv, int *param) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = cvstar_(cv, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dcvver_ dcvver
# endif
	extern void dcvver_(IRAFPointer *, double *, double *, int *);
void c_dcvvector(IRAFPointer *cv, double *x, double *yfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	dcvver_(cv, x, yfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvvecr_ cvvecr
# endif
	extern void cvvecr_(IRAFPointer *, float *, float *, int *);
void c_cvvector(IRAFPointer *cv, float *x, float *yfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	cvvecr_(cv, x, yfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dcvzeo_ dcvzeo
# endif
	extern void dcvzeo_(IRAFPointer *);
void c_dcvzero(IRAFPointer *cv) {
	clear_cvoserr();
	xerpsh_();
	dcvzeo_(cv);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvzero_ cvzero
# endif
	extern void cvzero_(IRAFPointer *);
void c_cvzero(IRAFPointer *cv) {
	clear_cvoserr();
	xerpsh_();
	cvzero_(cv);
	if (xerpoi_())
	    set_cvoserr();
}

