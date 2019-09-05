# include <xgsurfit.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define dgsb1l_ dgsb1l
# endif
	extern void dgsb1l_(double *, int *, double *, double *, double *);
void c_dgs_b1pol(double *x, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dgsb1l_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsb1g_ dgsb1g
# endif
	extern void dgsb1g_(double *, int *, double *, double *, double *);
void c_dgs_b1leg(double *x, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dgsb1g_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsb1b_ dgsb1b
# endif
	extern void dgsb1b_(double *, int *, double *, double *, double *);
void c_dgs_b1cheb(double *x, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dgsb1b_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsb1l_ rgsb1l
# endif
	extern void rgsb1l_(float *, int *, float *, float *, float *);
void c_rgs_b1pol(float *x, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rgsb1l_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsb1g_ rgsb1g
# endif
	extern void rgsb1g_(float *, int *, float *, float *, float *);
void c_rgs_b1leg(float *x, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rgsb1g_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsb1b_ rgsb1b
# endif
	extern void rgsb1b_(float *, int *, float *, float *, float *);
void c_rgs_b1cheb(float *x, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rgsb1b_(x, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsbpl_ dgsbpl
# endif
	extern void dgsbpl_(double *, int *, int *, double *, double *, double *);
void c_dgs_bpol(double *x, int *npts, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dgsbpl_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsbcb_ dgsbcb
# endif
	extern void dgsbcb_(double *, int *, int *, double *, double *, double *);
void c_dgs_bcheb(double *x, int *npts, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dgsbcb_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsblg_ dgsblg
# endif
	extern void dgsblg_(double *, int *, int *, double *, double *, double *);
void c_dgs_bleg(double *x, int *npts, int *order, double *k1, double *k2, double *basis) {
	clear_cvoserr();
	xerpsh_();
	dgsblg_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsbpl_ rgsbpl
# endif
	extern void rgsbpl_(float *, int *, int *, float *, float *, float *);
void c_rgs_bpol(float *x, int *npts, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rgsbpl_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsbcb_ rgsbcb
# endif
	extern void rgsbcb_(float *, int *, int *, float *, float *, float *);
void c_rgs_bcheb(float *x, int *npts, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rgsbcb_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsblg_ rgsblg
# endif
	extern void rgsblg_(float *, int *, int *, float *, float *, float *);
void c_rgs_bleg(float *x, int *npts, int *order, float *k1, float *k2, float *basis) {
	clear_cvoserr();
	xerpsh_();
	rgsblg_(x, npts, order, k1, k2, basis);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgschc_ dgschc
# endif
	extern void dgschc_(double *, int *, int *, double *, int *);
void c_dgschofac(double *matrix, int *nbands, int *nrows, double *matfac, int *ier) {
	clear_cvoserr();
	xerpsh_();
	dgschc_(matrix, nbands, nrows, matfac, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgschv_ dgschv
# endif
	extern void dgschv_(double *, int *, int *, double *, double *);
void c_dgschoslv(double *matfac, int *nbands, int *nrows, double *vector, double *coeff) {
	clear_cvoserr();
	xerpsh_();
	dgschv_(matfac, nbands, nrows, vector, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgschc_ rgschc
# endif
	extern void rgschc_(float *, int *, int *, float *, int *);
void c_rgschofac(float *matrix, int *nbands, int *nrows, float *matfac, int *ier) {
	clear_cvoserr();
	xerpsh_();
	rgschc_(matrix, nbands, nrows, matfac, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgschv_ rgschv
# endif
	extern void rgschv_(float *, int *, int *, float *, float *);
void c_rgschoslv(float *matfac, int *nbands, int *nrows, float *vector, float *coeff) {
	clear_cvoserr();
	xerpsh_();
	rgschv_(matfac, nbands, nrows, vector, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgs1dy_ dgs1dy
# endif
	extern void dgs1dy_(double *, double *, double *, int *, int *, double *, double *);
void c_dgs_1devpoly(double *coeff, double *x, double *yfit, int *npts, int *order, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	dgs1dy_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgs1db_ dgs1db
# endif
	extern void dgs1db_(double *, double *, double *, int *, int *, double *, double *);
void c_dgs_1devcheb(double *coeff, double *x, double *yfit, int *npts, int *order, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	dgs1db_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgs1dg_ dgs1dg
# endif
	extern void dgs1dg_(double *, double *, double *, int *, int *, double *, double *);
void c_dgs_1devleg(double *coeff, double *x, double *yfit, int *npts, int *order, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	dgs1dg_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgs1dy_ rgs1dy
# endif
	extern void rgs1dy_(float *, float *, float *, int *, int *, float *, float *);
void c_rgs_1devpoly(float *coeff, float *x, float *yfit, int *npts, int *order, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	rgs1dy_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgs1db_ rgs1db
# endif
	extern void rgs1db_(float *, float *, float *, int *, int *, float *, float *);
void c_rgs_1devcheb(float *coeff, float *x, float *yfit, int *npts, int *order, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	rgs1db_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgs1dg_ rgs1dg
# endif
	extern void rgs1dg_(float *, float *, float *, int *, int *, float *, float *);
void c_rgs_1devleg(float *coeff, float *x, float *yfit, int *npts, int *order, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	rgs1dg_(coeff, x, yfit, npts, order, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsevy_ dgsevy
# endif
	extern void dgsevy_(double *, double *, double *, double *, int *, int *, int *, int *, double *, double *, double *, double *);
void c_dgs_evpoly(double *coeff, double *x, double *y, double *zfit, int *npts, int *xterms, int *xorder, int *yorder, double *k1x, double *k2x, double *k1y, double *k2y) {
	clear_cvoserr();
	xerpsh_();
	dgsevy_(coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsevb_ dgsevb
# endif
	extern void dgsevb_(double *, double *, double *, double *, int *, int *, int *, int *, double *, double *, double *, double *);
void c_dgs_evcheb(double *coeff, double *x, double *y, double *zfit, int *npts, int *xterms, int *xorder, int *yorder, double *k1x, double *k2x, double *k1y, double *k2y) {
	clear_cvoserr();
	xerpsh_();
	dgsevb_(coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsevg_ dgsevg
# endif
	extern void dgsevg_(double *, double *, double *, double *, int *, int *, int *, int *, double *, double *, double *, double *);
void c_dgs_evleg(double *coeff, double *x, double *y, double *zfit, int *npts, int *xterms, int *xorder, int *yorder, double *k1x, double *k2x, double *k1y, double *k2y) {
	clear_cvoserr();
	xerpsh_();
	dgsevg_(coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsasud_ gsasud
# endif
	extern void gsasud_(double *, double *, double *, double *, int *);
void c_gs_asumvpd(double *a, double *b, double *c, double *d, int *npts) {
	clear_cvoserr();
	xerpsh_();
	gsasud_(a, b, c, d, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsevy_ rgsevy
# endif
	extern void rgsevy_(float *, float *, float *, float *, int *, int *, int *, int *, float *, float *, float *, float *);
void c_rgs_evpoly(float *coeff, float *x, float *y, float *zfit, int *npts, int *xterms, int *xorder, int *yorder, float *k1x, float *k2x, float *k1y, float *k2y) {
	clear_cvoserr();
	xerpsh_();
	rgsevy_(coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsevb_ rgsevb
# endif
	extern void rgsevb_(float *, float *, float *, float *, int *, int *, int *, int *, float *, float *, float *, float *);
void c_rgs_evcheb(float *coeff, float *x, float *y, float *zfit, int *npts, int *xterms, int *xorder, int *yorder, float *k1x, float *k2x, float *k1y, float *k2y) {
	clear_cvoserr();
	xerpsh_();
	rgsevb_(coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rgsevg_ rgsevg
# endif
	extern void rgsevg_(float *, float *, float *, float *, int *, int *, int *, int *, float *, float *, float *, float *);
void c_rgs_evleg(float *coeff, float *x, float *y, float *zfit, int *npts, int *xterms, int *xorder, int *yorder, float *k1x, float *k2x, float *k1y, float *k2y) {
	clear_cvoserr();
	xerpsh_();
	rgsevg_(coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x, k1y, k2y);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsasur_ gsasur
# endif
	extern void gsasur_(float *, float *, float *, float *, int *);
void c_gs_asumvpr(float *a, float *b, float *c, float *d, int *npts) {
	clear_cvoserr();
	xerpsh_();
	gsasur_(a, b, c, d, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsacm_ dgsacm
# endif
	extern void dgsacm_(IRAFPointer *, double *, double *, double *, double *, int *);
void c_dgsaccum(IRAFPointer *sf, double *x, double *y, double *z, double *w, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	dgsacm_(sf, x, y, z, w, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsaccm_ gsaccm
# endif
	extern void gsaccm_(IRAFPointer *, float *, float *, float *, float *, int *);
void c_gsaccum(IRAFPointer *sf, float *x, float *y, float *z, float *w, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	gsaccm_(sf, x, y, z, w, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsacs_ dgsacs
# endif
	extern void dgsacs_(IRAFPointer *, double *, double *, double *, double *, int *, int *);
void c_dgsacpts(IRAFPointer *sf, double *x, double *y, double *z, double *w, int *npts, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	dgsacs_(sf, x, y, z, w, npts, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsacps_ gsacps
# endif
	extern void gsacps_(IRAFPointer *, float *, float *, float *, float *, int *, int *);
void c_gsacpts(IRAFPointer *sf, float *x, float *y, float *z, float *w, int *npts, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	gsacps_(sf, x, y, z, w, npts, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsadd_ dgsadd
# endif
	extern void dgsadd_(IRAFPointer *, IRAFPointer *, IRAFPointer *);
void c_dgsadd(IRAFPointer *sf1, IRAFPointer *sf2, IRAFPointer *sf3) {
	clear_cvoserr();
	xerpsh_();
	dgsadd_(sf1, sf2, sf3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsadd_ gsadd
# endif
	extern void gsadd_(IRAFPointer *, IRAFPointer *, IRAFPointer *);
void c_gsadd(IRAFPointer *sf1, IRAFPointer *sf2, IRAFPointer *sf3) {
	clear_cvoserr();
	xerpsh_();
	gsadd_(sf1, sf2, sf3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgscof_ dgscof
# endif
	extern void dgscof_(IRAFPointer *, double *, int *);
void c_dgscoeff(IRAFPointer *sf, double *coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	dgscof_(sf, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gscoef_ gscoef
# endif
	extern void gscoef_(IRAFPointer *, float *, int *);
void c_gscoeff(IRAFPointer *sf, float *coeff, int *ncoeff) {
	clear_cvoserr();
	xerpsh_();
	gscoef_(sf, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgscoy_ dgscoy
# endif
	extern void dgscoy_(IRAFPointer *, IRAFPointer *);
void c_dgscopy(IRAFPointer *sf1, IRAFPointer *sf2) {
	clear_cvoserr();
	xerpsh_();
	dgscoy_(sf1, sf2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gscopy_ gscopy
# endif
	extern void gscopy_(IRAFPointer *, IRAFPointer *);
void c_gscopy(IRAFPointer *sf1, IRAFPointer *sf2) {
	clear_cvoserr();
	xerpsh_();
	gscopy_(sf1, sf2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsder_ dgsder
# endif
	extern void dgsder_(IRAFPointer *, double *, double *, double *, int *, int *, int *);
void c_dgsder(IRAFPointer *sf1, double *x, double *y, double *zfit, int *npts, int *nxd, int *nyd) {
	clear_cvoserr();
	xerpsh_();
	dgsder_(sf1, x, y, zfit, npts, nxd, nyd);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsder_ gsder
# endif
	extern void gsder_(IRAFPointer *, float *, float *, float *, int *, int *, int *);
void c_gsder(IRAFPointer *sf1, float *x, float *y, float *zfit, int *npts, int *nxd, int *nyd) {
	clear_cvoserr();
	xerpsh_();
	gsder_(sf1, x, y, zfit, npts, nxd, nyd);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsers_ dgsers
# endif
	extern void dgsers_(IRAFPointer *, double *, double *, double *, double *, double *);
void c_dgserrors(IRAFPointer *sf, double *z, double *w, double *zfit, double *chisqr, double *errors) {
	clear_cvoserr();
	xerpsh_();
	dgsers_(sf, z, w, zfit, chisqr, errors);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gserrs_ gserrs
# endif
	extern void gserrs_(IRAFPointer *, float *, float *, float *, float *, float *);
void c_gserrors(IRAFPointer *sf, float *z, float *w, float *zfit, float *chisqr, float *errors) {
	clear_cvoserr();
	xerpsh_();
	gserrs_(sf, z, w, zfit, chisqr, errors);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsevl_ dgsevl
# endif
	extern double dgsevl_(IRAFPointer *, double *, double *);
double c_dgseval(IRAFPointer *sf, double *x, double *y) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dgsevl_(sf, x, y);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gseval_ gseval
# endif
	extern float gseval_(IRAFPointer *, float *, float *);
float c_gseval(IRAFPointer *sf, float *x, float *y) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gseval_(sf, x, y);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dgsfit_ dgsfit
# endif
	extern void dgsfit_(IRAFPointer *, double *, double *, double *, double *, int *, int *, int *);
void c_dgsfit(IRAFPointer *sf, double *x, double *y, double *z, double *w, int *npts, int *wtflag, int *ier) {
	clear_cvoserr();
	xerpsh_();
	dgsfit_(sf, x, y, z, w, npts, wtflag, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsfit_ gsfit
# endif
	extern void gsfit_(IRAFPointer *, float *, float *, float *, float *, int *, int *, int *);
void c_gsfit(IRAFPointer *sf, float *x, float *y, float *z, float *w, int *npts, int *wtflag, int *ier) {
	clear_cvoserr();
	xerpsh_();
	gsfit_(sf, x, y, z, w, npts, wtflag, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsfre_ dgsfre
# endif
	extern void dgsfre_(IRAFPointer *);
void c_dgsfree(IRAFPointer *sf) {
	clear_cvoserr();
	xerpsh_();
	dgsfre_(sf);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsfree_ gsfree
# endif
	extern void gsfree_(IRAFPointer *);
void c_gsfree(IRAFPointer *sf) {
	clear_cvoserr();
	xerpsh_();
	gsfree_(sf);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsgcf_ dgsgcf
# endif
	extern double dgsgcf_(IRAFPointer *, int *, int *);
double c_dgsgcoeff(IRAFPointer *sf, int *xorder, int *yorder) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dgsgcf_(sf, xorder, yorder);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gsgcof_ gsgcof
# endif
	extern float gsgcof_(IRAFPointer *, int *, int *);
float c_gsgcoeff(IRAFPointer *sf, int *xorder, int *yorder) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gsgcof_(sf, xorder, yorder);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dgsint_ dgsint
# endif
	extern void dgsint_(IRAFPointer *, int *, int *, int *, int *, double *, double *, double *, double *);
void c_dgsinit(IRAFPointer *sf, int *surface_type, int *xorder, int *yorder, int *xterms, double *xmin, double *xmax, double *ymin, double *ymax) {
	clear_cvoserr();
	xerpsh_();
	dgsint_(sf, surface_type, xorder, yorder, xterms, xmin, xmax, ymin, ymax);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsinit_ gsinit
# endif
	extern void gsinit_(IRAFPointer *, int *, int *, int *, int *, float *, float *, float *, float *);
void c_gsinit(IRAFPointer *sf, int *surface_type, int *xorder, int *yorder, int *xterms, float *xmin, float *xmax, float *ymin, float *ymax) {
	clear_cvoserr();
	xerpsh_();
	gsinit_(sf, surface_type, xorder, yorder, xterms, xmin, xmax, ymin, ymax);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsret_ dgsret
# endif
	extern void dgsret_(IRAFPointer *, double *, double *, double *, double *, int *);
void c_dgsrefit(IRAFPointer *sf, double *x, double *y, double *z, double *w, int *ier) {
	clear_cvoserr();
	xerpsh_();
	dgsret_(sf, x, y, z, w, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsreft_ gsreft
# endif
	extern void gsreft_(IRAFPointer *, float *, float *, float *, float *, int *);
void c_gsrefit(IRAFPointer *sf, float *x, float *y, float *z, float *w, int *ier) {
	clear_cvoserr();
	xerpsh_();
	gsreft_(sf, x, y, z, w, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsrej_ dgsrej
# endif
	extern void dgsrej_(IRAFPointer *, double *, double *, double *, double *, int *);
void c_dgsrej(IRAFPointer *sf, double *x, double *y, double *z, double *w, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	dgsrej_(sf, x, y, z, w, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsrej_ gsrej
# endif
	extern void gsrej_(IRAFPointer *, float *, float *, float *, float *, int *);
void c_gsrej(IRAFPointer *sf, float *x, float *y, float *z, float *w, int *wtflag) {
	clear_cvoserr();
	xerpsh_();
	gsrej_(sf, x, y, z, w, wtflag);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsree_ dgsree
# endif
	extern void dgsree_(IRAFPointer *, double *);
void c_dgsrestore(IRAFPointer *sf, double *fit) {
	clear_cvoserr();
	xerpsh_();
	dgsree_(sf, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsrese_ gsrese
# endif
	extern void gsrese_(IRAFPointer *, float *);
void c_gsrestore(IRAFPointer *sf, float *fit) {
	clear_cvoserr();
	xerpsh_();
	gsrese_(sf, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgssae_ dgssae
# endif
	extern void dgssae_(IRAFPointer *, double *);
void c_dgssave(IRAFPointer *sf, double *fit) {
	clear_cvoserr();
	xerpsh_();
	dgssae_(sf, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gssave_ gssave
# endif
	extern void gssave_(IRAFPointer *, float *);
void c_gssave(IRAFPointer *sf, float *fit) {
	clear_cvoserr();
	xerpsh_();
	gssave_(sf, fit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsscf_ dgsscf
# endif
	extern void dgsscf_(IRAFPointer *, int *, int *, double *);
void c_dgsscoeff(IRAFPointer *sf, int *xorder, int *yorder, double *coeff) {
	clear_cvoserr();
	xerpsh_();
	dgsscf_(sf, xorder, yorder, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsscof_ gsscof
# endif
	extern void gsscof_(IRAFPointer *, int *, int *, float *);
void c_gsscoeff(IRAFPointer *sf, int *xorder, int *yorder, float *coeff) {
	clear_cvoserr();
	xerpsh_();
	gsscof_(sf, xorder, yorder, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgssoe_ dgssoe
# endif
	extern void dgssoe_(IRAFPointer *, int *);
void c_dgssolve(IRAFPointer *sf, int *ier) {
	clear_cvoserr();
	xerpsh_();
	dgssoe_(sf, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gssole_ gssole
# endif
	extern void gssole_(IRAFPointer *, int *);
void c_gssolve(IRAFPointer *sf, int *ier) {
	clear_cvoserr();
	xerpsh_();
	gssole_(sf, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsged_ dgsged
# endif
	extern double dgsged_(IRAFPointer *, int *);
double c_dgsgetd(IRAFPointer *sf, int *parameter) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dgsged_(sf, parameter);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dgsgei_ dgsgei
# endif
	extern int dgsgei_(IRAFPointer *, int *);
int c_dgsgeti(IRAFPointer *sf, int *parameter) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = dgsgei_(sf, parameter);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gsgetr_ gsgetr
# endif
	extern float gsgetr_(IRAFPointer *, int *);
float c_gsgetr(IRAFPointer *sf, int *parameter) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gsgetr_(sf, parameter);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gsgeti_ gsgeti
# endif
	extern int gsgeti_(IRAFPointer *, int *);
int c_gsgeti(IRAFPointer *sf, int *parameter) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gsgeti_(sf, parameter);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define dgssub_ dgssub
# endif
	extern void dgssub_(IRAFPointer *, IRAFPointer *, IRAFPointer *);
void c_dgssub(IRAFPointer *sf1, IRAFPointer *sf2, IRAFPointer *sf3) {
	clear_cvoserr();
	xerpsh_();
	dgssub_(sf1, sf2, sf3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gssub_ gssub
# endif
	extern void gssub_(IRAFPointer *, IRAFPointer *, IRAFPointer *);
void c_gssub(IRAFPointer *sf1, IRAFPointer *sf2, IRAFPointer *sf3) {
	clear_cvoserr();
	xerpsh_();
	gssub_(sf1, sf2, sf3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgsver_ dgsver
# endif
	extern void dgsver_(IRAFPointer *, double *, double *, double *, int *);
void c_dgsvector(IRAFPointer *sf, double *x, double *y, double *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	dgsver_(sf, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gsvecr_ gsvecr
# endif
	extern void gsvecr_(IRAFPointer *, float *, float *, float *, int *);
void c_gsvector(IRAFPointer *sf, float *x, float *y, float *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	gsvecr_(sf, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define dgszeo_ dgszeo
# endif
	extern void dgszeo_(IRAFPointer *);
void c_dgszero(IRAFPointer *sf) {
	clear_cvoserr();
	xerpsh_();
	dgszeo_(sf);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gszero_ gszero
# endif
	extern void gszero_(IRAFPointer *);
void c_gszero(IRAFPointer *sf) {
	clear_cvoserr();
	xerpsh_();
	gszero_(sf);
	if (xerpoi_())
	    set_cvoserr();
}

