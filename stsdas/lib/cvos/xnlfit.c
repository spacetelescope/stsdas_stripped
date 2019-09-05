# include <xnlfit.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define nlacpd_ nlacpd
# endif
	extern double nlacpd_(IRAFPointer *, double *, double *, double *, int *, int *);
double c_nlacptsd(IRAFPointer *nl, double *x, double *z, double *w, int *npts, int *nvars) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlacpd_(nl, x, z, w, npts, nvars);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlresd_ nlresd
# endif
	extern double nlresd_(IRAFPointer *, double *, double *, double *, int *, int *);
double c_nlresidd(IRAFPointer *nl, double *x, double *z, double *w, int *npts, int *nvars) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlresd_(nl, x, z, w, npts, nvars);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlaccd_ nlaccd
# endif
	extern void nlaccd_(double *, int *, double *, double *, int *, double *, double *);
void c_nl_accumd(double *deriv, int *list, double *w, double *dz, int *nfit, double *alpha, double *beta) {
	clear_cvoserr();
	xerpsh_();
	nlaccd_(deriv, list, w, dz, nfit, alpha, beta);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlacpr_ nlacpr
# endif
	extern float nlacpr_(IRAFPointer *, float *, float *, float *, int *, int *);
float c_nlacptsr(IRAFPointer *nl, float *x, float *z, float *w, int *npts, int *nvars) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlacpr_(nl, x, z, w, npts, nvars);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlresr_ nlresr
# endif
	extern float nlresr_(IRAFPointer *, float *, float *, float *, int *, int *);
float c_nlresidr(IRAFPointer *nl, float *x, float *z, float *w, int *npts, int *nvars) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlresr_(nl, x, z, w, npts, nvars);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlaccr_ nlaccr
# endif
	extern void nlaccr_(float *, int *, float *, float *, int *, float *, float *);
void c_nl_accumr(float *deriv, int *list, float *w, float *dz, int *nfit, float *alpha, float *beta) {
	clear_cvoserr();
	xerpsh_();
	nlaccr_(deriv, list, w, dz, nfit, alpha, beta);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlchfd_ nlchfd
# endif
	extern void nlchfd_(double *, int *, int *, double *, int *);
void c_nl_chfacd(double *matrix, int *nbands, int *nrows, double *matfac, int *ier) {
	clear_cvoserr();
	xerpsh_();
	nlchfd_(matrix, nbands, nrows, matfac, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlchsd_ nlchsd
# endif
	extern void nlchsd_(double *, int *, int *, double *, double *);
void c_nl_chslvd(double *matfac, int *nbands, int *nrows, double *vector, double *coeff) {
	clear_cvoserr();
	xerpsh_();
	nlchsd_(matfac, nbands, nrows, vector, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nldamd_ nldamd
# endif
	extern void nldamd_(double *, double *, double *, int *, int *);
void c_nl_dampd(double *inmatrix, double *outmatrix, double *constant, int *nbands, int *nrows) {
	clear_cvoserr();
	xerpsh_();
	nldamd_(inmatrix, outmatrix, constant, nbands, nrows);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlchfr_ nlchfr
# endif
	extern void nlchfr_(float *, int *, int *, float *, int *);
void c_nl_chfacr(float *matrix, int *nbands, int *nrows, float *matfac, int *ier) {
	clear_cvoserr();
	xerpsh_();
	nlchfr_(matrix, nbands, nrows, matfac, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlchsr_ nlchsr
# endif
	extern void nlchsr_(float *, int *, int *, float *, float *);
void c_nl_chslvr(float *matfac, int *nbands, int *nrows, float *vector, float *coeff) {
	clear_cvoserr();
	xerpsh_();
	nlchsr_(matfac, nbands, nrows, vector, coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nldamr_ nldamr
# endif
	extern void nldamr_(float *, float *, float *, int *, int *);
void c_nl_dampr(float *inmatrix, float *outmatrix, float *constant, int *nbands, int *nrows) {
	clear_cvoserr();
	xerpsh_();
	nldamr_(inmatrix, outmatrix, constant, nbands, nrows);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nldumd_ nldumd
# endif
	extern void nldumd_(int *, IRAFPointer *);
void c_nl_dumpd(int *fd, IRAFPointer *nl) {
	clear_cvoserr();
	xerpsh_();
	nldumd_(fd, nl);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nladud_ nladud
# endif
	extern void nladud_(int *, double *, int *, int *);
void c_nl_adumpd(int *fd, double *a, int *nrows, int *ncols) {
	clear_cvoserr();
	xerpsh_();
	nladud_(fd, a, nrows, ncols);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nldumr_ nldumr
# endif
	extern void nldumr_(int *, IRAFPointer *);
void c_nl_dumpr(int *fd, IRAFPointer *nl) {
	clear_cvoserr();
	xerpsh_();
	nldumr_(fd, nl);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nladur_ nladur
# endif
	extern void nladur_(int *, float *, int *, int *);
void c_nl_adumpr(int *fd, float *a, int *nrows, int *ncols) {
	clear_cvoserr();
	xerpsh_();
	nladur_(fd, a, nrows, ncols);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlerrg_ nlerrg
# endif
	extern void nlerrg_(int *, short *, int *);
void c_nlerrmsg(int ier, char *errmsg, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	nlerrg_(&ier, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(errmsg,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define nlerrd_ nlerrd
# endif
	extern void nlerrd_(IRAFPointer *, double *, double *, double *, int *, double *, double *, double *);
void c_nlerrorsd(IRAFPointer *nl, double *z, double *zfit, double *w, int *npts, double *variance, double *chisqr, double *errors) {
	clear_cvoserr();
	xerpsh_();
	nlerrd_(nl, z, zfit, w, npts, variance, chisqr, errors);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlinvd_ nlinvd
# endif
	extern void nlinvd_(double *, double *, double *, double *, int *, int *, double *);
void c_nlinvertd(double *alpha, double *chofac, double *cov, double *errors, int *list, int *nfit, double *variance) {
	clear_cvoserr();
	xerpsh_();
	nlinvd_(alpha, chofac, cov, errors, list, nfit, variance);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlerrr_ nlerrr
# endif
	extern void nlerrr_(IRAFPointer *, float *, float *, float *, int *, float *, float *, float *);
void c_nlerrorsr(IRAFPointer *nl, float *z, float *zfit, float *w, int *npts, float *variance, float *chisqr, float *errors) {
	clear_cvoserr();
	xerpsh_();
	nlerrr_(nl, z, zfit, w, npts, variance, chisqr, errors);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlinvr_ nlinvr
# endif
	extern void nlinvr_(float *, float *, float *, float *, int *, int *, float *);
void c_nlinvertr(float *alpha, float *chofac, float *cov, float *errors, int *list, int *nfit, float *variance) {
	clear_cvoserr();
	xerpsh_();
	nlinvr_(alpha, chofac, cov, errors, list, nfit, variance);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlevad_ nlevad
# endif
	extern double nlevad_(IRAFPointer *, double *, int *);
double c_nlevald(IRAFPointer *nl, double *x, int *nvars) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlevad_(nl, x, nvars);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlevar_ nlevar
# endif
	extern float nlevar_(IRAFPointer *, float *, int *);
float c_nlevalr(IRAFPointer *nl, float *x, int *nvars) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlevar_(nl, x, nvars);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlfitd_ nlfitd
# endif
	extern void nlfitd_(IRAFPointer *, double *, double *, double *, int *, int *, int *, int *);
void c_nlfitd(IRAFPointer *nl, double *x, double *z, double *w, int *npts, int *nvars, int *wtflag, int *stat) {
	clear_cvoserr();
	xerpsh_();
	nlfitd_(nl, x, z, w, npts, nvars, wtflag, stat);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlscad_ nlscad
# endif
	extern double nlscad_(IRAFPointer *, double *, double *, double *, int *, int *);
double c_nlscatterd(IRAFPointer *nl, double *x, double *z, double *w, int *npts, int *nvars) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlscad_(nl, x, z, w, npts, nvars);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlfitr_ nlfitr
# endif
	extern void nlfitr_(IRAFPointer *, float *, float *, float *, int *, int *, int *, int *);
void c_nlfitr(IRAFPointer *nl, float *x, float *z, float *w, int *npts, int *nvars, int *wtflag, int *stat) {
	clear_cvoserr();
	xerpsh_();
	nlfitr_(nl, x, z, w, npts, nvars, wtflag, stat);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlscar_ nlscar
# endif
	extern float nlscar_(IRAFPointer *, float *, float *, float *, int *, int *);
float c_nlscatterr(IRAFPointer *nl, float *x, float *z, float *w, int *npts, int *nvars) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlscar_(nl, x, z, w, npts, nvars);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlfred_ nlfred
# endif
	extern void nlfred_(IRAFPointer *);
void c_nlfreed(IRAFPointer *nl) {
	clear_cvoserr();
	xerpsh_();
	nlfred_(nl);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlfrer_ nlfrer
# endif
	extern void nlfrer_(IRAFPointer *);
void c_nlfreer(IRAFPointer *nl) {
	clear_cvoserr();
	xerpsh_();
	nlfrer_(nl);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlinid_ nlinid
# endif
	extern void nlinid_(IRAFPointer *, int *, int *, double *, double *, int *, int *, int *, double *, int *);
void c_nlinitd(IRAFPointer *nl, int *fnc, int *dfnc, double *params, double *dparams, int *nparams, int *plist, int *nfparams, double *tol, int *itmax) {
	clear_cvoserr();
	xerpsh_();
	nlinid_(nl, fnc, dfnc, params, dparams, nparams, plist, nfparams, tol, itmax);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlinir_ nlinir
# endif
	extern void nlinir_(IRAFPointer *, int *, int *, float *, float *, int *, int *, int *, float *, int *);
void c_nlinitr(IRAFPointer *nl, int *fnc, int *dfnc, float *params, float *dparams, int *nparams, int *plist, int *nfparams, float *tol, int *itmax) {
	clear_cvoserr();
	xerpsh_();
	nlinir_(nl, fnc, dfnc, params, dparams, nparams, plist, nfparams, tol, itmax);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlited_ nlited
# endif
	extern void nlited_(IRAFPointer *, double *, double *, double *, int *, int *, int *);
void c_nliterd(IRAFPointer *nl, double *x, double *z, double *w, int *npts, int *nvars, int *ier) {
	clear_cvoserr();
	xerpsh_();
	nlited_(nl, x, z, w, npts, nvars, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nliter_ nliter
# endif
	extern void nliter_(IRAFPointer *, float *, float *, float *, int *, int *, int *);
void c_nliterr(IRAFPointer *nl, float *x, float *z, float *w, int *npts, int *nvars, int *ier) {
	clear_cvoserr();
	xerpsh_();
	nliter_(nl, x, z, w, npts, nvars, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nllist_ nllist
# endif
	extern void nllist_(int *, int *, int *);
void c_nl_list(int *list, int *nlist, int *nfit) {
	clear_cvoserr();
	xerpsh_();
	nllist_(list, nlist, nfit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlpged_ nlpged
# endif
	extern void nlpged_(IRAFPointer *, double *, int *);
void c_nlpgetd(IRAFPointer *nl, double *params, int *nparams) {
	clear_cvoserr();
	xerpsh_();
	nlpged_(nl, params, nparams);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlpger_ nlpger
# endif
	extern void nlpger_(IRAFPointer *, float *, int *);
void c_nlpgetr(IRAFPointer *nl, float *params, int *nparams) {
	clear_cvoserr();
	xerpsh_();
	nlpger_(nl, params, nparams);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlsold_ nlsold
# endif
	extern void nlsold_(IRAFPointer *, int *);
void c_nlsolved(IRAFPointer *nl, int *ier) {
	clear_cvoserr();
	xerpsh_();
	nlsold_(nl, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlsolr_ nlsolr
# endif
	extern void nlsolr_(IRAFPointer *, int *);
void c_nlsolver(IRAFPointer *nl, int *ier) {
	clear_cvoserr();
	xerpsh_();
	nlsolr_(nl, ier);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlstad_ nlstad
# endif
	extern double nlstad_(IRAFPointer *, int *);
double c_nlstatd(IRAFPointer *nl, int *param) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlstad_(nl, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlstai_ nlstai
# endif
	extern int nlstai_(IRAFPointer *, int *);
int c_nlstati(IRAFPointer *nl, int *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlstai_(nl, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlstar_ nlstar
# endif
	extern float nlstar_(IRAFPointer *, int *);
float c_nlstatr(IRAFPointer *nl, int *param) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = nlstar_(nl, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define nlvecd_ nlvecd
# endif
	extern void nlvecd_(IRAFPointer *, double *, double *, int *, int *);
void c_nlvectord(IRAFPointer *nl, double *x, double *zfit, int *npts, int *nvars) {
	clear_cvoserr();
	xerpsh_();
	nlvecd_(nl, x, zfit, npts, nvars);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlvecr_ nlvecr
# endif
	extern void nlvecr_(IRAFPointer *, float *, float *, int *, int *);
void c_nlvectorr(IRAFPointer *nl, float *x, float *zfit, int *npts, int *nvars) {
	clear_cvoserr();
	xerpsh_();
	nlvecr_(nl, x, zfit, npts, nvars);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlzerd_ nlzerd
# endif
	extern void nlzerd_(IRAFPointer *);
void c_nlzerod(IRAFPointer *nl) {
	clear_cvoserr();
	xerpsh_();
	nlzerd_(nl);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define nlzerr_ nlzerr
# endif
	extern void nlzerr_(IRAFPointer *);
void c_nlzeror(IRAFPointer *nl) {
	clear_cvoserr();
	xerpsh_();
	nlzerr_(nl);
	if (xerpoi_())
	    set_cvoserr();
}

