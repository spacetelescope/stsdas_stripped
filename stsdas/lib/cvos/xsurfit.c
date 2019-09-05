# include <xsurfit.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define iscoef_ iscoef
# endif
	extern void iscoef_(IRAFPointer *, float *, int *);
void c_iscoeff(IRAFPointer *sf, float *coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	iscoef_(sf, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iseval_ iseval
# endif
	extern float iseval_(IRAFPointer *, float *, float *);
float c_iseval(IRAFPointer *sf, float *x, float *y) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = iseval_(sf, x, y);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define isfree_ isfree
# endif
	extern void isfree_(IRAFPointer *);
void c_isfree(IRAFPointer *sf) {
	clear_cvoserr();
	xerpsh_();
	isfree_(sf);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define isinit_ isinit
# endif
	extern void isinit_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
void c_isinit(IRAFPointer *sf, int *surf_type, int *xorder, int *yorder, int *xterms, int *ncols, int *nlines) {
	clear_cvoserr();
	xerpsh_();
	isinit_(sf, surf_type, xorder, yorder, xterms, ncols, nlines);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define islacm_ islacm
# endif
	extern void islacm_(IRAFPointer *, int *, int *, float *, float *, int *, int *);
void c_islaccum(IRAFPointer *sf, int *cols, int *lineno, float *z, float *w, int *ncols, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	islacm_(sf, cols, lineno, z, w, ncols, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define islfit_ islfit
# endif
	extern void islfit_(IRAFPointer *, int *, int *, float *, float *, int *, int *, int *);
void c_islfit(IRAFPointer *sf, int *cols, int *lineno, float *z, float *w, int *ncols, int *wtflag, int *ier) {
	clear_cvoserr();
	xerpsh_();
	islfit_(sf, cols, lineno, z, w, ncols, wtflag, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define islret_ islret
# endif
	extern void islret_(IRAFPointer *, int *, int *, float *, float *);
void c_islrefit(IRAFPointer *sf, int *cols, int *lineno, float *z, float *w) {
	clear_cvoserr();
	xerpsh_();
	islret_(sf, cols, lineno, z, w);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define islsoe_ islsoe
# endif
	extern void islsoe_(IRAFPointer *, int *, int *);
void c_islsolve(IRAFPointer *sf, int *lineno, int *ier) {
	clear_cvoserr();
	xerpsh_();
	islsoe_(sf, lineno, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define islzeo_ islzeo
# endif
	extern void islzeo_(IRAFPointer *, int *);
void c_islzero(IRAFPointer *sf, int *lineno) {
	clear_cvoserr();
	xerpsh_();
	islzeo_(sf, lineno);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define isrepe_ isrepe
# endif
	extern void isrepe_(IRAFPointer *, float *);
void c_isreplace(IRAFPointer *sf, float *fit) {
	clear_cvoserr();
	xerpsh_();
	isrepe_(sf, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define isrese_ isrese
# endif
	extern void isrese_(IRAFPointer *, int *, int *);
void c_isresolve(IRAFPointer *sf, int *lines, int *ier) {
	clear_cvoserr();
	xerpsh_();
	isrese_(sf, lines, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define issave_ issave
# endif
	extern void issave_(IRAFPointer *, float *);
void c_issave(IRAFPointer *sf, float *fit) {
	clear_cvoserr();
	xerpsh_();
	issave_(sf, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define issole_ issole
# endif
	extern void issole_(IRAFPointer *, int *, int *, int *);
void c_issolve(IRAFPointer *sf, int *lines, int *nlines, int *ier) {
	clear_cvoserr();
	xerpsh_();
	issole_(sf, lines, nlines, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define isvecr_ isvecr
# endif
	extern void isvecr_(IRAFPointer *, float *, float *, float *, int *);
void c_isvector(IRAFPointer *sf, float *x, float *y, float *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	isvecr_(sf, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iszero_ iszero
# endif
	extern void iszero_(IRAFPointer *);
void c_iszero(IRAFPointer *sf) {
	clear_cvoserr();
	xerpsh_();
	iszero_(sf);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfb1lg_ sfb1lg
# endif
	extern void sfb1lg_(float *, int *, float *, float *, float *);
void c_sf_b1leg(float *x, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	sfb1lg_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfb1cb_ sfb1cb
# endif
	extern void sfb1cb_(float *, int *, float *, float *, float *);
void c_sf_b1cheb(float *x, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	sfb1cb_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfb1s1_ sfb1s1
# endif
	extern void sfb1s1_(float *, int *, float *, float *, float *, int *);
void c_sf_b1spline1(float *x, int *npieces, float *k1, float *k2, float *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	sfb1s1_(x, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfb1s3_ sfb1s3
# endif
	extern void sfb1s3_(float *, int *, float *, float *, float *, int *);
void c_sf_b1spline3(float *x, int *npieces, float *k1, float *k2, float *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	sfb1s3_(x, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfbchb_ sfbchb
# endif
	extern void sfbchb_(float *, int *, int *, float *, float *, float *);
void c_sf_bcheb(float *x, int *npts, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	sfbchb_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfbleg_ sfbleg
# endif
	extern void sfbleg_(float *, int *, int *, float *, float *, float *);
void c_sf_bleg(float *x, int *npts, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	sfbleg_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfbsp1_ sfbsp1
# endif
	extern void sfbsp1_(float *, int *, int *, float *, float *, float *, int *);
void c_sf_bspline1(float *x, int *npts, int *npieces, float *k1, float *k2, float *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	sfbsp1_(x, npts, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfbsp3_ sfbsp3
# endif
	extern void sfbsp3_(float *, int *, int *, float *, float *, float *, int *);
void c_sf_bspline3(float *x, int *npts, int *npieces, float *k1, float *k2, float *basis, int *left) {
	clear_cvoserr();
	xerpsh_();
	sfbsp3_(x, npts, npieces, k1, k2, basis, left);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvevcb_ cvevcb
# endif
	extern void cvevcb_(float *, float *, float *, int *, int *, float *, float *);
void c_cv_evcheb(float *coeff, float *x, float *yfit, int *npts, int *order, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	cvevcb_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvevlg_ cvevlg
# endif
	extern void cvevlg_(float *, float *, float *, int *, int *, float *, float *);
void c_cv_evleg(float *coeff, float *x, float *yfit, int *npts, int *order, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	cvevlg_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvevs1_ cvevs1
# endif
	extern void cvevs1_(float *, float *, float *, int *, int *, float *, float *);
void c_cv_evspline1(float *coeff, float *x, float *yfit, int *npts, int *npieces, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	cvevs1_(coeff, x, yfit, npts, npieces, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cvevs3_ cvevs3
# endif
	extern void cvevs3_(float *, float *, float *, int *, int *, float *, float *);
void c_cv_evspline3(float *coeff, float *x, float *yfit, int *npts, int *npieces, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	cvevs3_(coeff, x, yfit, npts, npieces, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfevcb_ sfevcb
# endif
	extern void sfevcb_(float *, float *, float *, float *, int *, int *, int *, int *, float *, float *, float *, float *);
void c_sf_evcheb(float *coeff, float *x, float *y, float *zfit, int *npts, int *xterms, int *xorder, int *yorder, float *k1x, float *k2x, float *k1y, float *k2y) {
	clear_cvoserr();
	xerpsh_();
	sfevcb_(coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfevlg_ sfevlg
# endif
	extern void sfevlg_(float *, float *, float *, float *, int *, int *, int *, int *, float *, float *, float *, float *);
void c_sf_evleg(float *coeff, float *x, float *y, float *zfit, int *npts, int *xterms, int *xorder, int *yorder, float *k1x, float *k2x, float *k1y, float *k2y) {
	clear_cvoserr();
	xerpsh_();
	sfevlg_(coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfevs3_ sfevs3
# endif
	extern void sfevs3_(float *, float *, float *, float *, int *, int *, int *, float *, float *, float *, float *);
void c_sf_evspline3(float *coeff, float *x, float *y, float *zfit, int *npts, int *nxpieces, int *nypieces, float *k1x, float *k2x, float *k1y, float *k2y) {
	clear_cvoserr();
	xerpsh_();
	sfevs3_(coeff, x, y, zfit, npts, nxpieces, nypieces, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfevs1_ sfevs1
# endif
	extern void sfevs1_(float *, float *, float *, float *, int *, int *, int *, float *, float *, float *, float *);
void c_sf_evspline1(float *coeff, float *x, float *y, float *zfit, int *npts, int *nxpieces, int *nypieces, float *k1x, float *k2x, float *k1y, float *k2y) {
	clear_cvoserr();
	xerpsh_();
	sfevs1_(coeff, x, y, zfit, npts, nxpieces, nypieces, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfchoc_ sfchoc
# endif
	extern void sfchoc_(float *, int *, int *, float *, int *);
void c_sfchofac(float *matrix, int *nbands, int *nrows, float *matfac, int *ier) {
	clear_cvoserr();
	xerpsh_();
	sfchoc_(matrix, nbands, nrows, matfac, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define sfchov_ sfchov
# endif
	extern void sfchov_(float *, int *, int *, float *, float *);
void c_sfchoslv(float *matfac, int *nbands, int *nrows, float *vector, float *coeff) {
	clear_cvoserr();
	xerpsh_();
	sfchov_(matfac, nbands, nrows, vector, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

