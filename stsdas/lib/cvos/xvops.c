# include <xvops.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define aabsd_ aabsd
# endif
	extern void aabsd_(double *, double *, int *);
void c_aabsd(double *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aabsd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aabsi_ aabsi
# endif
	extern void aabsi_(int *, int *, int *);
void c_aabsi(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aabsi_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aabsl_ aabsl
# endif
	extern void aabsl_(int *, int *, int *);
void c_aabsl(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aabsl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aabsr_ aabsr
# endif
	extern void aabsr_(float *, float *, int *);
void c_aabsr(float *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aabsr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aabss_ aabss
# endif
	extern void aabss_(short *, short *, int *);
void c_aabss(short *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aabss_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aabsx_ aabsx
# endif
	extern void aabsx_(Complex *, Complex *, int *);
void c_aabsx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aabsx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddd_ aaddd
# endif
	extern void aaddd_(double *, double *, double *, int *);
void c_aaddd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddi_ aaddi
# endif
	extern void aaddi_(int *, int *, int *, int *);
void c_aaddi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddkd_ aaddkd
# endif
	extern void aaddkd_(double *, double *, double *, int *);
void c_aaddkd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddki_ aaddki
# endif
	extern void aaddki_(int *, int *, int *, int *);
void c_aaddki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddkl_ aaddkl
# endif
	extern void aaddkl_(int *, int *, int *, int *);
void c_aaddkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddkr_ aaddkr
# endif
	extern void aaddkr_(float *, float *, float *, int *);
void c_aaddkr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddks_ aaddks
# endif
	extern void aaddks_(short *, short *, short *, int *);
void c_aaddks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddkx_ aaddkx
# endif
	extern void aaddkx_(Complex *, Complex *, Complex *, int *);
void c_aaddkx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddl_ aaddl
# endif
	extern void aaddl_(int *, int *, int *, int *);
void c_aaddl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddr_ aaddr
# endif
	extern void aaddr_(float *, float *, float *, int *);
void c_aaddr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aadds_ aadds
# endif
	extern void aadds_(short *, short *, short *, int *);
void c_aadds(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aadds_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aaddx_ aaddx
# endif
	extern void aaddx_(Complex *, Complex *, Complex *, int *);
void c_aaddx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aaddx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aandi_ aandi
# endif
	extern void aandi_(int *, int *, int *, int *);
void c_aandi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aandi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aandki_ aandki
# endif
	extern void aandki_(int *, int *, int *, int *);
void c_aandki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aandki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aandkl_ aandkl
# endif
	extern void aandkl_(int *, int *, int *, int *);
void c_aandkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aandkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aandks_ aandks
# endif
	extern void aandks_(short *, short *, short *, int *);
void c_aandks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aandks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aandl_ aandl
# endif
	extern void aandl_(int *, int *, int *, int *);
void c_aandl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aandl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aands_ aands
# endif
	extern void aands_(short *, short *, short *, int *);
void c_aands(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aands_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aavgd_ aavgd
# endif
	extern void aavgd_(double *, int *, double *, double *);
void c_aavgd(double *a, int *npix, double *mean, double *sigma) {
	clear_cvoserr();
	xerpsh_();
	aavgd_(a, npix, mean, sigma);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aavgi_ aavgi
# endif
	extern void aavgi_(int *, int *, float *, float *);
void c_aavgi(int *a, int *npix, float *mean, float *sigma) {
	clear_cvoserr();
	xerpsh_();
	aavgi_(a, npix, mean, sigma);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aavgl_ aavgl
# endif
	extern void aavgl_(int *, int *, double *, double *);
void c_aavgl(int *a, int *npix, double *mean, double *sigma) {
	clear_cvoserr();
	xerpsh_();
	aavgl_(a, npix, mean, sigma);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aavgr_ aavgr
# endif
	extern void aavgr_(float *, int *, float *, float *);
void c_aavgr(float *a, int *npix, float *mean, float *sigma) {
	clear_cvoserr();
	xerpsh_();
	aavgr_(a, npix, mean, sigma);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aavgs_ aavgs
# endif
	extern void aavgs_(short *, int *, float *, float *);
void c_aavgs(short *a, int *npix, float *mean, float *sigma) {
	clear_cvoserr();
	xerpsh_();
	aavgs_(a, npix, mean, sigma);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aavgx_ aavgx
# endif
	extern void aavgx_(Complex *, int *, float *, float *);
void c_aavgx(Complex *a, int *npix, float *mean, float *sigma) {
	clear_cvoserr();
	xerpsh_();
	aavgx_(a, npix, mean, sigma);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abavd_ abavd
# endif
	extern void abavd_(double *, double *, int *, int *);
void c_abavd(double *a, double *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	abavd_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abavi_ abavi
# endif
	extern void abavi_(int *, int *, int *, int *);
void c_abavi(int *a, int *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	abavi_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abavl_ abavl
# endif
	extern void abavl_(int *, int *, int *, int *);
void c_abavl(int *a, int *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	abavl_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abavr_ abavr
# endif
	extern void abavr_(float *, float *, int *, int *);
void c_abavr(float *a, float *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	abavr_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abavs_ abavs
# endif
	extern void abavs_(short *, short *, int *, int *);
void c_abavs(short *a, short *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	abavs_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abavx_ abavx
# endif
	extern void abavx_(Complex *, Complex *, int *, int *);
void c_abavx(Complex *a, Complex *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	abavx_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqd_ abeqd
# endif
	extern void abeqd_(double *, double *, int *, int *);
void c_abeqd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqi_ abeqi
# endif
	extern void abeqi_(int *, int *, int *, int *);
void c_abeqi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqkd_ abeqkd
# endif
	extern void abeqkd_(double *, double *, int *, int *);
void c_abeqkd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqki_ abeqki
# endif
	extern void abeqki_(int *, int *, int *, int *);
void c_abeqki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqkl_ abeqkl
# endif
	extern void abeqkl_(int *, int *, int *, int *);
void c_abeqkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqkr_ abeqkr
# endif
	extern void abeqkr_(float *, float *, int *, int *);
void c_abeqkr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqks_ abeqks
# endif
	extern void abeqks_(short *, short *, int *, int *);
void c_abeqks(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqkx_ abeqkx
# endif
	extern void abeqkx_(Complex *, Complex *, int *, int *);
void c_abeqkx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeql_ abeql
# endif
	extern void abeql_(int *, int *, int *, int *);
void c_abeql(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeql_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqr_ abeqr
# endif
	extern void abeqr_(float *, float *, int *, int *);
void c_abeqr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqs_ abeqs
# endif
	extern void abeqs_(short *, short *, int *, int *);
void c_abeqs(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqs_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abeqx_ abeqx
# endif
	extern void abeqx_(Complex *, Complex *, int *, int *);
void c_abeqx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abeqx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abged_ abged
# endif
	extern void abged_(double *, double *, int *, int *);
void c_abged(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abged_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgei_ abgei
# endif
	extern void abgei_(int *, int *, int *, int *);
void c_abgei(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgei_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgekd_ abgekd
# endif
	extern void abgekd_(double *, double *, int *, int *);
void c_abgekd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgekd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgeki_ abgeki
# endif
	extern void abgeki_(int *, int *, int *, int *);
void c_abgeki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgeki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgekl_ abgekl
# endif
	extern void abgekl_(int *, int *, int *, int *);
void c_abgekl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgekl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgekr_ abgekr
# endif
	extern void abgekr_(float *, float *, int *, int *);
void c_abgekr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgekr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgeks_ abgeks
# endif
	extern void abgeks_(short *, short *, int *, int *);
void c_abgeks(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgeks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgekx_ abgekx
# endif
	extern void abgekx_(Complex *, Complex *, int *, int *);
void c_abgekx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgekx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgel_ abgel
# endif
	extern void abgel_(int *, int *, int *, int *);
void c_abgel(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgel_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abger_ abger
# endif
	extern void abger_(float *, float *, int *, int *);
void c_abger(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abger_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abges_ abges
# endif
	extern void abges_(short *, short *, int *, int *);
void c_abges(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abges_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgex_ abgex
# endif
	extern void abgex_(Complex *, Complex *, int *, int *);
void c_abgex(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgex_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtd_ abgtd
# endif
	extern void abgtd_(double *, double *, int *, int *);
void c_abgtd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgti_ abgti
# endif
	extern void abgti_(int *, int *, int *, int *);
void c_abgti(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgti_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtkd_ abgtkd
# endif
	extern void abgtkd_(double *, double *, int *, int *);
void c_abgtkd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtki_ abgtki
# endif
	extern void abgtki_(int *, int *, int *, int *);
void c_abgtki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtkl_ abgtkl
# endif
	extern void abgtkl_(int *, int *, int *, int *);
void c_abgtkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtkr_ abgtkr
# endif
	extern void abgtkr_(float *, float *, int *, int *);
void c_abgtkr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtks_ abgtks
# endif
	extern void abgtks_(short *, short *, int *, int *);
void c_abgtks(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtkx_ abgtkx
# endif
	extern void abgtkx_(Complex *, Complex *, int *, int *);
void c_abgtkx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtl_ abgtl
# endif
	extern void abgtl_(int *, int *, int *, int *);
void c_abgtl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtr_ abgtr
# endif
	extern void abgtr_(float *, float *, int *, int *);
void c_abgtr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgts_ abgts
# endif
	extern void abgts_(short *, short *, int *, int *);
void c_abgts(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgts_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abgtx_ abgtx
# endif
	extern void abgtx_(Complex *, Complex *, int *, int *);
void c_abgtx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abgtx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abled_ abled
# endif
	extern void abled_(double *, double *, int *, int *);
void c_abled(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abled_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablei_ ablei
# endif
	extern void ablei_(int *, int *, int *, int *);
void c_ablei(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablei_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablekd_ ablekd
# endif
	extern void ablekd_(double *, double *, int *, int *);
void c_ablekd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablekd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ableki_ ableki
# endif
	extern void ableki_(int *, int *, int *, int *);
void c_ableki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ableki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablekl_ ablekl
# endif
	extern void ablekl_(int *, int *, int *, int *);
void c_ablekl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablekl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablekr_ ablekr
# endif
	extern void ablekr_(float *, float *, int *, int *);
void c_ablekr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablekr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ableks_ ableks
# endif
	extern void ableks_(short *, short *, int *, int *);
void c_ableks(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ableks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablekx_ ablekx
# endif
	extern void ablekx_(Complex *, Complex *, int *, int *);
void c_ablekx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablekx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablel_ ablel
# endif
	extern void ablel_(int *, int *, int *, int *);
void c_ablel(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablel_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abler_ abler
# endif
	extern void abler_(float *, float *, int *, int *);
void c_abler(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abler_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ables_ ables
# endif
	extern void ables_(short *, short *, int *, int *);
void c_ables(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ables_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablex_ ablex
# endif
	extern void ablex_(Complex *, Complex *, int *, int *);
void c_ablex(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablex_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltd_ abltd
# endif
	extern void abltd_(double *, double *, int *, int *);
void c_abltd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablti_ ablti
# endif
	extern void ablti_(int *, int *, int *, int *);
void c_ablti(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablti_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltkd_ abltkd
# endif
	extern void abltkd_(double *, double *, int *, int *);
void c_abltkd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltki_ abltki
# endif
	extern void abltki_(int *, int *, int *, int *);
void c_abltki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltkl_ abltkl
# endif
	extern void abltkl_(int *, int *, int *, int *);
void c_abltkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltkr_ abltkr
# endif
	extern void abltkr_(float *, float *, int *, int *);
void c_abltkr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltks_ abltks
# endif
	extern void abltks_(short *, short *, int *, int *);
void c_abltks(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltkx_ abltkx
# endif
	extern void abltkx_(Complex *, Complex *, int *, int *);
void c_abltkx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltl_ abltl
# endif
	extern void abltl_(int *, int *, int *, int *);
void c_abltl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltr_ abltr
# endif
	extern void abltr_(float *, float *, int *, int *);
void c_abltr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ablts_ ablts
# endif
	extern void ablts_(short *, short *, int *, int *);
void c_ablts(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	ablts_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abltx_ abltx
# endif
	extern void abltx_(Complex *, Complex *, int *, int *);
void c_abltx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abltx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abned_ abned
# endif
	extern void abned_(double *, double *, int *, int *);
void c_abned(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abned_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abnei_ abnei
# endif
	extern void abnei_(int *, int *, int *, int *);
void c_abnei(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abnei_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abnekd_ abnekd
# endif
	extern void abnekd_(double *, double *, int *, int *);
void c_abnekd(double *a, double *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abnekd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abneki_ abneki
# endif
	extern void abneki_(int *, int *, int *, int *);
void c_abneki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abneki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abnekl_ abnekl
# endif
	extern void abnekl_(int *, int *, int *, int *);
void c_abnekl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abnekl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abnekr_ abnekr
# endif
	extern void abnekr_(float *, float *, int *, int *);
void c_abnekr(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abnekr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abneks_ abneks
# endif
	extern void abneks_(short *, short *, int *, int *);
void c_abneks(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abneks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abnekx_ abnekx
# endif
	extern void abnekx_(Complex *, Complex *, int *, int *);
void c_abnekx(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abnekx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abnel_ abnel
# endif
	extern void abnel_(int *, int *, int *, int *);
void c_abnel(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abnel_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abner_ abner
# endif
	extern void abner_(float *, float *, int *, int *);
void c_abner(float *a, float *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abner_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abnes_ abnes
# endif
	extern void abnes_(short *, short *, int *, int *);
void c_abnes(short *a, short *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abnes_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abnex_ abnex
# endif
	extern void abnex_(Complex *, Complex *, int *, int *);
void c_abnex(Complex *a, Complex *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abnex_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abori_ abori
# endif
	extern void abori_(int *, int *, int *, int *);
void c_abori(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abori_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aborki_ aborki
# endif
	extern void aborki_(int *, int *, int *, int *);
void c_aborki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aborki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aborkl_ aborkl
# endif
	extern void aborkl_(int *, int *, int *, int *);
void c_aborkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aborkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aborks_ aborks
# endif
	extern void aborks_(short *, short *, short *, int *);
void c_aborks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aborks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aborl_ aborl
# endif
	extern void aborl_(int *, int *, int *, int *);
void c_aborl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aborl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define abors_ abors
# endif
	extern void abors_(short *, short *, short *, int *);
void c_abors(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	abors_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define absud_ absud
# endif
	extern void absud_(double *, double *, int *, int *);
void c_absud(double *a, double *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	absud_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define absui_ absui
# endif
	extern void absui_(int *, int *, int *, int *);
void c_absui(int *a, int *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	absui_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define absul_ absul
# endif
	extern void absul_(int *, int *, int *, int *);
void c_absul(int *a, int *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	absul_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define absur_ absur
# endif
	extern void absur_(float *, float *, int *, int *);
void c_absur(float *a, float *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	absur_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define absus_ absus
# endif
	extern void absus_(short *, short *, int *, int *);
void c_absus(short *a, short *b, int *nblocks, int *npix_per_block) {
	clear_cvoserr();
	xerpsh_();
	absus_(a, b, nblocks, npix_per_block);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtdd_ achtdd
# endif
	extern void achtdd_(double *, double *, int *);
void c_achtdd(double *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtdd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtdi_ achtdi
# endif
	extern void achtdi_(double *, int *, int *);
void c_achtdi(double *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtdi_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtdl_ achtdl
# endif
	extern void achtdl_(double *, int *, int *);
void c_achtdl(double *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtdl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtdr_ achtdr
# endif
	extern void achtdr_(double *, float *, int *);
void c_achtdr(double *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtdr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtds_ achtds
# endif
	extern void achtds_(double *, short *, int *);
void c_achtds(double *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtds_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtdx_ achtdx
# endif
	extern void achtdx_(double *, Complex *, int *);
void c_achtdx(double *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtdx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtid_ achtid
# endif
	extern void achtid_(int *, double *, int *);
void c_achtid(int *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtid_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtii_ achtii
# endif
	extern void achtii_(int *, int *, int *);
void c_achtii(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtii_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtil_ achtil
# endif
	extern void achtil_(int *, int *, int *);
void c_achtil(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtil_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtir_ achtir
# endif
	extern void achtir_(int *, float *, int *);
void c_achtir(int *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtir_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtis_ achtis
# endif
	extern void achtis_(int *, short *, int *);
void c_achtis(int *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtis_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtix_ achtix
# endif
	extern void achtix_(int *, Complex *, int *);
void c_achtix(int *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtix_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtld_ achtld
# endif
	extern void achtld_(int *, double *, int *);
void c_achtld(int *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtld_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtli_ achtli
# endif
	extern void achtli_(int *, int *, int *);
void c_achtli(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtli_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtll_ achtll
# endif
	extern void achtll_(int *, int *, int *);
void c_achtll(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtll_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtlr_ achtlr
# endif
	extern void achtlr_(int *, float *, int *);
void c_achtlr(int *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtlr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtls_ achtls
# endif
	extern void achtls_(int *, short *, int *);
void c_achtls(int *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtls_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtlx_ achtlx
# endif
	extern void achtlx_(int *, Complex *, int *);
void c_achtlx(int *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtlx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtrd_ achtrd
# endif
	extern void achtrd_(float *, double *, int *);
void c_achtrd(float *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtrd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtri_ achtri
# endif
	extern void achtri_(float *, int *, int *);
void c_achtri(float *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtri_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtrl_ achtrl
# endif
	extern void achtrl_(float *, int *, int *);
void c_achtrl(float *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtrl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtrr_ achtrr
# endif
	extern void achtrr_(float *, float *, int *);
void c_achtrr(float *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtrr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtrs_ achtrs
# endif
	extern void achtrs_(float *, short *, int *);
void c_achtrs(float *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtrs_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtrx_ achtrx
# endif
	extern void achtrx_(float *, Complex *, int *);
void c_achtrx(float *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtrx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtsd_ achtsd
# endif
	extern void achtsd_(short *, double *, int *);
void c_achtsd(short *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtsd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtsi_ achtsi
# endif
	extern void achtsi_(short *, int *, int *);
void c_achtsi(short *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtsi_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtsl_ achtsl
# endif
	extern void achtsl_(short *, int *, int *);
void c_achtsl(short *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtsl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtsr_ achtsr
# endif
	extern void achtsr_(short *, float *, int *);
void c_achtsr(short *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtsr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtss_ achtss
# endif
	extern void achtss_(short *, short *, int *);
void c_achtss(short *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtss_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtsx_ achtsx
# endif
	extern void achtsx_(short *, Complex *, int *);
void c_achtsx(short *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtsx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtxd_ achtxd
# endif
	extern void achtxd_(Complex *, double *, int *);
void c_achtxd(Complex *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtxd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtxi_ achtxi
# endif
	extern void achtxi_(Complex *, int *, int *);
void c_achtxi(Complex *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtxi_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtxl_ achtxl
# endif
	extern void achtxl_(Complex *, int *, int *);
void c_achtxl(Complex *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtxl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtxr_ achtxr
# endif
	extern void achtxr_(Complex *, float *, int *);
void c_achtxr(Complex *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtxr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtxs_ achtxs
# endif
	extern void achtxs_(Complex *, short *, int *);
void c_achtxs(Complex *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtxs_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define achtxx_ achtxx
# endif
	extern void achtxx_(Complex *, Complex *, int *);
void c_achtxx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	achtxx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acjgx_ acjgx
# endif
	extern void acjgx_(Complex *, Complex *, int *);
void c_acjgx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	acjgx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aclrd_ aclrd
# endif
	extern void aclrd_(double *, int *);
void c_aclrd(double *a, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aclrd_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aclri_ aclri
# endif
	extern void aclri_(int *, int *);
void c_aclri(int *a, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aclri_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aclrl_ aclrl
# endif
	extern void aclrl_(int *, int *);
void c_aclrl(int *a, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aclrl_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aclrr_ aclrr
# endif
	extern void aclrr_(float *, int *);
void c_aclrr(float *a, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aclrr_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aclrs_ aclrs
# endif
	extern void aclrs_(short *, int *);
void c_aclrs(short *a, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aclrs_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aclrx_ aclrx
# endif
	extern void aclrx_(Complex *, int *);
void c_aclrx(Complex *a, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aclrx_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvd_ acnvd
# endif
	extern void acnvd_(double *, double *, int *, double *, int *);
void c_acnvd(double *in, double *out, int *npix, double *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvd_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvi_ acnvi
# endif
	extern void acnvi_(int *, int *, int *, int *, int *);
void c_acnvi(int *in, int *out, int *npix, int *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvi_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvl_ acnvl
# endif
	extern void acnvl_(int *, int *, int *, int *, int *);
void c_acnvl(int *in, int *out, int *npix, int *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvl_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvr_ acnvr
# endif
	extern void acnvr_(float *, float *, int *, float *, int *);
void c_acnvr(float *in, float *out, int *npix, float *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvr_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvrd_ acnvrd
# endif
	extern void acnvrd_(double *, double *, int *, float *, int *);
void c_acnvrd(double *in, double *out, int *npix, float *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvrd_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvri_ acnvri
# endif
	extern void acnvri_(int *, int *, int *, float *, int *);
void c_acnvri(int *in, int *out, int *npix, float *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvri_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvrl_ acnvrl
# endif
	extern void acnvrl_(int *, int *, int *, float *, int *);
void c_acnvrl(int *in, int *out, int *npix, float *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvrl_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvrr_ acnvrr
# endif
	extern void acnvrr_(float *, float *, int *, float *, int *);
void c_acnvrr(float *in, float *out, int *npix, float *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvrr_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvrs_ acnvrs
# endif
	extern void acnvrs_(short *, short *, int *, float *, int *);
void c_acnvrs(short *in, short *out, int *npix, float *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvrs_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define acnvs_ acnvs
# endif
	extern void acnvs_(short *, short *, int *, short *, int *);
void c_acnvs(short *in, short *out, int *npix, short *kernel, int *knpix) {
	clear_cvoserr();
	xerpsh_();
	acnvs_(in, out, npix, kernel, knpix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivd_ adivd
# endif
	extern void adivd_(double *, double *, double *, int *);
void c_adivd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivi_ adivi
# endif
	extern void adivi_(int *, int *, int *, int *);
void c_adivi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivkd_ adivkd
# endif
	extern void adivkd_(double *, double *, double *, int *);
void c_adivkd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivki_ adivki
# endif
	extern void adivki_(int *, int *, int *, int *);
void c_adivki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivkl_ adivkl
# endif
	extern void adivkl_(int *, int *, int *, int *);
void c_adivkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivkr_ adivkr
# endif
	extern void adivkr_(float *, float *, float *, int *);
void c_adivkr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivks_ adivks
# endif
	extern void adivks_(short *, short *, short *, int *);
void c_adivks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivkx_ adivkx
# endif
	extern void adivkx_(Complex *, Complex *, Complex *, int *);
void c_adivkx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivl_ adivl
# endif
	extern void adivl_(int *, int *, int *, int *);
void c_adivl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivr_ adivr
# endif
	extern void adivr_(float *, float *, float *, int *);
void c_adivr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivs_ adivs
# endif
	extern void adivs_(short *, short *, short *, int *);
void c_adivs(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivs_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adivx_ adivx
# endif
	extern void adivx_(Complex *, Complex *, Complex *, int *);
void c_adivx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	adivx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define adotd_ adotd
# endif
	extern double adotd_(double *, double *, int *);
double c_adotd(double *a, double *b, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = adotd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define adoti_ adoti
# endif
	extern float adoti_(int *, int *, int *);
float c_adoti(int *a, int *b, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = adoti_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define adotl_ adotl
# endif
	extern double adotl_(int *, int *, int *);
double c_adotl(int *a, int *b, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = adotl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define adotr_ adotr
# endif
	extern float adotr_(float *, float *, int *);
float c_adotr(float *a, float *b, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = adotr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define adots_ adots
# endif
	extern float adots_(short *, short *, int *);
float c_adots(short *a, short *b, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = adots_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define adotx_ adotx
# endif
	extern float adotx_(Complex *, Complex *, int *);
float c_adotx(Complex *a, Complex *b, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = adotx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aexpd_ aexpd
# endif
	extern void aexpd_(double *, double *, double *, int *);
void c_aexpd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpi_ aexpi
# endif
	extern void aexpi_(int *, int *, int *, int *);
void c_aexpi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpkd_ aexpkd
# endif
	extern void aexpkd_(double *, double *, double *, int *);
void c_aexpkd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpki_ aexpki
# endif
	extern void aexpki_(int *, int *, int *, int *);
void c_aexpki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpkl_ aexpkl
# endif
	extern void aexpkl_(int *, int *, int *, int *);
void c_aexpkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpkr_ aexpkr
# endif
	extern void aexpkr_(float *, float *, float *, int *);
void c_aexpkr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpks_ aexpks
# endif
	extern void aexpks_(short *, short *, short *, int *);
void c_aexpks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpkx_ aexpkx
# endif
	extern void aexpkx_(Complex *, Complex *, Complex *, int *);
void c_aexpkx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpl_ aexpl
# endif
	extern void aexpl_(int *, int *, int *, int *);
void c_aexpl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpr_ aexpr
# endif
	extern void aexpr_(float *, float *, float *, int *);
void c_aexpr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexps_ aexps
# endif
	extern void aexps_(short *, short *, short *, int *);
void c_aexps(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexps_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aexpx_ aexpx
# endif
	extern void aexpx_(Complex *, Complex *, Complex *, int *);
void c_aexpx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aexpx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define afftrr_ afftrr
# endif
	extern void afftrr_(float *, float *, float *, float *, int *);
void c_afftrr(float *sr, float *si, float *fr, float *fi, int *npix) {
	clear_cvoserr();
	xerpsh_();
	afftrr_(sr, si, fr, fi, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define afftrx_ afftrx
# endif
	extern void afftrx_(float *, Complex *, int *);
void c_afftrx(float *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	afftrx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define afftxr_ afftxr
# endif
	extern void afftxr_(float *, float *, float *, float *, int *);
void c_afftxr(float *sr, float *si, float *fr, float *fi, int *npix) {
	clear_cvoserr();
	xerpsh_();
	afftxr_(sr, si, fr, fi, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define afftxx_ afftxx
# endif
	extern void afftxx_(Complex *, Complex *, int *);
void c_afftxx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	afftxx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define agltd_ agltd
# endif
	extern void agltd_(double *, double *, int *, double *, double *, double *, double *, int *);
void c_agltd(double *a, double *b, int *npix, double *low, double *high, double *kmul, double *kadd, int *nrange) {
	clear_cvoserr();
	xerpsh_();
	agltd_(a, b, npix, low, high, kmul, kadd, nrange);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aglti_ aglti
# endif
	extern void aglti_(int *, int *, int *, int *, int *, float *, float *, int *);
void c_aglti(int *a, int *b, int *npix, int *low, int *high, float *kmul, float *kadd, int *nrange) {
	clear_cvoserr();
	xerpsh_();
	aglti_(a, b, npix, low, high, kmul, kadd, nrange);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define agltl_ agltl
# endif
	extern void agltl_(int *, int *, int *, int *, int *, double *, double *, int *);
void c_agltl(int *a, int *b, int *npix, int *low, int *high, double *kmul, double *kadd, int *nrange) {
	clear_cvoserr();
	xerpsh_();
	agltl_(a, b, npix, low, high, kmul, kadd, nrange);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define agltr_ agltr
# endif
	extern void agltr_(float *, float *, int *, float *, float *, float *, float *, int *);
void c_agltr(float *a, float *b, int *npix, float *low, float *high, float *kmul, float *kadd, int *nrange) {
	clear_cvoserr();
	xerpsh_();
	agltr_(a, b, npix, low, high, kmul, kadd, nrange);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aglts_ aglts
# endif
	extern void aglts_(short *, short *, int *, short *, short *, float *, float *, int *);
void c_aglts(short *a, short *b, int *npix, short *low, short *high, float *kmul, float *kadd, int *nrange) {
	clear_cvoserr();
	xerpsh_();
	aglts_(a, b, npix, low, high, kmul, kadd, nrange);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define agltx_ agltx
# endif
	extern void agltx_(Complex *, Complex *, int *, Complex *, Complex *, float *, float *, int *);
void c_agltx(Complex *a, Complex *b, int *npix, Complex *low, Complex *high, float *kmul, float *kadd, int *nrange) {
	clear_cvoserr();
	xerpsh_();
	agltx_(a, b, npix, low, high, kmul, kadd, nrange);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ahgmd_ ahgmd
# endif
	extern void ahgmd_(double *, int *, int *, int *, double *, double *);
void c_ahgmd(double *data, int *npix, int *hgm, int *nbins, double *z1, double *z2) {
	clear_cvoserr();
	xerpsh_();
	ahgmd_(data, npix, hgm, nbins, z1, z2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ahgmi_ ahgmi
# endif
	extern void ahgmi_(int *, int *, int *, int *, int *, int *);
void c_ahgmi(int *data, int *npix, int *hgm, int *nbins, int *z1, int *z2) {
	clear_cvoserr();
	xerpsh_();
	ahgmi_(data, npix, hgm, nbins, z1, z2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ahgml_ ahgml
# endif
	extern void ahgml_(int *, int *, int *, int *, int *, int *);
void c_ahgml(int *data, int *npix, int *hgm, int *nbins, int *z1, int *z2) {
	clear_cvoserr();
	xerpsh_();
	ahgml_(data, npix, hgm, nbins, z1, z2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ahgmr_ ahgmr
# endif
	extern void ahgmr_(float *, int *, int *, int *, float *, float *);
void c_ahgmr(float *data, int *npix, int *hgm, int *nbins, float *z1, float *z2) {
	clear_cvoserr();
	xerpsh_();
	ahgmr_(data, npix, hgm, nbins, z1, z2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ahgms_ ahgms
# endif
	extern void ahgms_(short *, int *, int *, int *, short *, short *);
void c_ahgms(short *data, int *npix, int *hgm, int *nbins, short *z1, short *z2) {
	clear_cvoserr();
	xerpsh_();
	ahgms_(data, npix, hgm, nbins, z1, z2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ahivd_ ahivd
# endif
	extern double ahivd_(double *, int *);
double c_ahivd(double *a, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = ahivd_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define ahivi_ ahivi
# endif
	extern int ahivi_(int *, int *);
int c_ahivi(int *a, int *npix) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = ahivi_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define ahivl_ ahivl
# endif
	extern int ahivl_(int *, int *);
int c_ahivl(int *a, int *npix) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = ahivl_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define ahivr_ ahivr
# endif
	extern float ahivr_(float *, int *);
float c_ahivr(float *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = ahivr_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define ahivs_ ahivs
# endif
	extern short ahivs_(short *, int *);
short c_ahivs(short *a, int *npix) {
	short rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = ahivs_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define ahivx_ ahivx
# endif
	extern Complex ahivx_(Complex *, int *);
Complex c_ahivx(Complex *a, int *npix) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = ahivx_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aiftrr_ aiftrr
# endif
	extern void aiftrr_(float *, float *, float *, float *, int *);
void c_aiftrr(float *fr, float *fi, float *sr, float *si, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aiftrr_(fr, fi, sr, si, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aiftrx_ aiftrx
# endif
	extern void aiftrx_(Complex *, float *, int *);
void c_aiftrx(Complex *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aiftrx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aiftxr_ aiftxr
# endif
	extern void aiftxr_(float *, float *, float *, float *, int *);
void c_aiftxr(float *fr, float *fi, float *sr, float *si, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aiftxr_(fr, fi, sr, si, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aiftxx_ aiftxx
# endif
	extern void aiftxx_(Complex *, Complex *, int *);
void c_aiftxx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aiftxx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aimgd_ aimgd
# endif
	extern void aimgd_(Complex *, double *, int *);
void c_aimgd(Complex *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aimgd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aimgi_ aimgi
# endif
	extern void aimgi_(Complex *, int *, int *);
void c_aimgi(Complex *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aimgi_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aimgl_ aimgl
# endif
	extern void aimgl_(Complex *, int *, int *);
void c_aimgl(Complex *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aimgl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aimgr_ aimgr
# endif
	extern void aimgr_(Complex *, float *, int *);
void c_aimgr(Complex *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aimgr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aimgs_ aimgs
# endif
	extern void aimgs_(Complex *, short *, int *);
void c_aimgs(Complex *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aimgs_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alimd_ alimd
# endif
	extern void alimd_(double *, int *, double *, double *);
void c_alimd(double *a, int *npix, double *minval, double *maxval) {
	clear_cvoserr();
	xerpsh_();
	alimd_(a, npix, minval, maxval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alimi_ alimi
# endif
	extern void alimi_(int *, int *, int *, int *);
void c_alimi(int *a, int *npix, int *minval, int *maxval) {
	clear_cvoserr();
	xerpsh_();
	alimi_(a, npix, minval, maxval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aliml_ aliml
# endif
	extern void aliml_(int *, int *, int *, int *);
void c_aliml(int *a, int *npix, int *minval, int *maxval) {
	clear_cvoserr();
	xerpsh_();
	aliml_(a, npix, minval, maxval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alimr_ alimr
# endif
	extern void alimr_(float *, int *, float *, float *);
void c_alimr(float *a, int *npix, float *minval, float *maxval) {
	clear_cvoserr();
	xerpsh_();
	alimr_(a, npix, minval, maxval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alims_ alims
# endif
	extern void alims_(short *, int *, short *, short *);
void c_alims(short *a, int *npix, short *minval, short *maxval) {
	clear_cvoserr();
	xerpsh_();
	alims_(a, npix, minval, maxval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alimx_ alimx
# endif
	extern void alimx_(Complex *, int *, Complex *, Complex *);
void c_alimx(Complex *a, int *npix, Complex *minval, Complex *maxval) {
	clear_cvoserr();
	xerpsh_();
	alimx_(a, npix, minval, maxval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alovd_ alovd
# endif
	extern double alovd_(double *, int *);
double c_alovd(double *a, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = alovd_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define alovi_ alovi
# endif
	extern int alovi_(int *, int *);
int c_alovi(int *a, int *npix) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = alovi_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define alovl_ alovl
# endif
	extern int alovl_(int *, int *);
int c_alovl(int *a, int *npix) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = alovl_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define alovr_ alovr
# endif
	extern float alovr_(float *, int *);
float c_alovr(float *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = alovr_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define alovs_ alovs
# endif
	extern short alovs_(short *, int *);
short c_alovs(short *a, int *npix) {
	short rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = alovs_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define alovx_ alovx
# endif
	extern Complex alovx_(Complex *, int *);
Complex c_alovx(Complex *a, int *npix) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = alovx_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define altad_ altad
# endif
	extern void altad_(double *, double *, int *, double *, double *);
void c_altad(double *a, double *b, int *npix, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	altad_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altai_ altai
# endif
	extern void altai_(int *, int *, int *, float *, float *);
void c_altai(int *a, int *b, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	altai_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altal_ altal
# endif
	extern void altal_(int *, int *, int *, double *, double *);
void c_altal(int *a, int *b, int *npix, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	altal_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altar_ altar
# endif
	extern void altar_(float *, float *, int *, float *, float *);
void c_altar(float *a, float *b, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	altar_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altas_ altas
# endif
	extern void altas_(short *, short *, int *, float *, float *);
void c_altas(short *a, short *b, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	altas_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altax_ altax
# endif
	extern void altax_(Complex *, Complex *, int *, float *, float *);
void c_altax(Complex *a, Complex *b, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	altax_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altmd_ altmd
# endif
	extern void altmd_(double *, double *, int *, double *, double *);
void c_altmd(double *a, double *b, int *npix, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	altmd_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altmi_ altmi
# endif
	extern void altmi_(int *, int *, int *, float *, float *);
void c_altmi(int *a, int *b, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	altmi_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altml_ altml
# endif
	extern void altml_(int *, int *, int *, double *, double *);
void c_altml(int *a, int *b, int *npix, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	altml_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altmr_ altmr
# endif
	extern void altmr_(float *, float *, int *, float *, float *);
void c_altmr(float *a, float *b, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	altmr_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altms_ altms
# endif
	extern void altms_(short *, short *, int *, float *, float *);
void c_altms(short *a, short *b, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	altms_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altmx_ altmx
# endif
	extern void altmx_(Complex *, Complex *, int *, float *, float *);
void c_altmx(Complex *a, Complex *b, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	altmx_(a, b, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altrd_ altrd
# endif
	extern void altrd_(double *, double *, int *, double *, double *, double *);
void c_altrd(double *a, double *b, int *npix, double *k1, double *k2, double *k3) {
	clear_cvoserr();
	xerpsh_();
	altrd_(a, b, npix, k1, k2, k3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altri_ altri
# endif
	extern void altri_(int *, int *, int *, float *, float *, float *);
void c_altri(int *a, int *b, int *npix, float *k1, float *k2, float *k3) {
	clear_cvoserr();
	xerpsh_();
	altri_(a, b, npix, k1, k2, k3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altrl_ altrl
# endif
	extern void altrl_(int *, int *, int *, double *, double *, double *);
void c_altrl(int *a, int *b, int *npix, double *k1, double *k2, double *k3) {
	clear_cvoserr();
	xerpsh_();
	altrl_(a, b, npix, k1, k2, k3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altrr_ altrr
# endif
	extern void altrr_(float *, float *, int *, float *, float *, float *);
void c_altrr(float *a, float *b, int *npix, float *k1, float *k2, float *k3) {
	clear_cvoserr();
	xerpsh_();
	altrr_(a, b, npix, k1, k2, k3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altrs_ altrs
# endif
	extern void altrs_(short *, short *, int *, float *, float *, float *);
void c_altrs(short *a, short *b, int *npix, float *k1, float *k2, float *k3) {
	clear_cvoserr();
	xerpsh_();
	altrs_(a, b, npix, k1, k2, k3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define altrx_ altrx
# endif
	extern void altrx_(Complex *, Complex *, int *, float *, float *, float *);
void c_altrx(Complex *a, Complex *b, int *npix, float *k1, float *k2, float *k3) {
	clear_cvoserr();
	xerpsh_();
	altrx_(a, b, npix, k1, k2, k3);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aluid_ aluid
# endif
	extern void aluid_(double *, double *, float *, int *);
void c_aluid(double *a, double *b, float *x, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aluid_(a, b, x, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aluii_ aluii
# endif
	extern void aluii_(int *, int *, float *, int *);
void c_aluii(int *a, int *b, float *x, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aluii_(a, b, x, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aluil_ aluil
# endif
	extern void aluil_(int *, int *, float *, int *);
void c_aluil(int *a, int *b, float *x, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aluil_(a, b, x, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aluir_ aluir
# endif
	extern void aluir_(float *, float *, float *, int *);
void c_aluir(float *a, float *b, float *x, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aluir_(a, b, x, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aluis_ aluis
# endif
	extern void aluis_(short *, short *, float *, int *);
void c_aluis(short *a, short *b, float *x, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aluis_(a, b, x, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alutd_ alutd
# endif
	extern void alutd_(int *, double *, int *, double *);
void c_alutd(int *a, double *b, int *npix, double *lut) {
	clear_cvoserr();
	xerpsh_();
	alutd_(a, b, npix, lut);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aluti_ aluti
# endif
	extern void aluti_(int *, int *, int *, int *);
void c_aluti(int *a, int *b, int *npix, int *lut) {
	clear_cvoserr();
	xerpsh_();
	aluti_(a, b, npix, lut);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alutl_ alutl
# endif
	extern void alutl_(int *, int *, int *, int *);
void c_alutl(int *a, int *b, int *npix, int *lut) {
	clear_cvoserr();
	xerpsh_();
	alutl_(a, b, npix, lut);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define alutr_ alutr
# endif
	extern void alutr_(int *, float *, int *, float *);
void c_alutr(int *a, float *b, int *npix, float *lut) {
	clear_cvoserr();
	xerpsh_();
	alutr_(a, b, npix, lut);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aluts_ aluts
# endif
	extern void aluts_(short *, short *, int *, short *);
void c_aluts(short *a, short *b, int *npix, short *lut) {
	clear_cvoserr();
	xerpsh_();
	aluts_(a, b, npix, lut);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amagd_ amagd
# endif
	extern void amagd_(double *, double *, double *, int *);
void c_amagd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amagd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amagi_ amagi
# endif
	extern void amagi_(int *, int *, int *, int *);
void c_amagi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amagi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amagl_ amagl
# endif
	extern void amagl_(int *, int *, int *, int *);
void c_amagl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amagl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amagr_ amagr
# endif
	extern void amagr_(float *, float *, float *, int *);
void c_amagr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amagr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amags_ amags
# endif
	extern void amags_(short *, short *, short *, int *);
void c_amags(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amags_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amagx_ amagx
# endif
	extern void amagx_(Complex *, Complex *, Complex *, int *);
void c_amagx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amagx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amapd_ amapd
# endif
	extern void amapd_(double *, double *, int *, double *, double *, double *, double *);
void c_amapd(double *a, double *b, int *npix, double *a1, double *a2, double *b1, double *b2) {
	clear_cvoserr();
	xerpsh_();
	amapd_(a, b, npix, a1, a2, b1, b2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amapi_ amapi
# endif
	extern void amapi_(int *, int *, int *, int *, int *, int *, int *);
void c_amapi(int *a, int *b, int *npix, int *a1, int *a2, int *b1, int *b2) {
	clear_cvoserr();
	xerpsh_();
	amapi_(a, b, npix, a1, a2, b1, b2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amapl_ amapl
# endif
	extern void amapl_(int *, int *, int *, int *, int *, int *, int *);
void c_amapl(int *a, int *b, int *npix, int *a1, int *a2, int *b1, int *b2) {
	clear_cvoserr();
	xerpsh_();
	amapl_(a, b, npix, a1, a2, b1, b2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amapr_ amapr
# endif
	extern void amapr_(float *, float *, int *, float *, float *, float *, float *);
void c_amapr(float *a, float *b, int *npix, float *a1, float *a2, float *b1, float *b2) {
	clear_cvoserr();
	xerpsh_();
	amapr_(a, b, npix, a1, a2, b1, b2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaps_ amaps
# endif
	extern void amaps_(short *, short *, int *, short *, short *, short *, short *);
void c_amaps(short *a, short *b, int *npix, short *a1, short *a2, short *b1, short *b2) {
	clear_cvoserr();
	xerpsh_();
	amaps_(a, b, npix, a1, a2, b1, b2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxd_ amaxd
# endif
	extern void amaxd_(double *, double *, double *, int *);
void c_amaxd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxi_ amaxi
# endif
	extern void amaxi_(int *, int *, int *, int *);
void c_amaxi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxkd_ amaxkd
# endif
	extern void amaxkd_(double *, double *, double *, int *);
void c_amaxkd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxki_ amaxki
# endif
	extern void amaxki_(int *, int *, int *, int *);
void c_amaxki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxkl_ amaxkl
# endif
	extern void amaxkl_(int *, int *, int *, int *);
void c_amaxkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxkr_ amaxkr
# endif
	extern void amaxkr_(float *, float *, float *, int *);
void c_amaxkr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxks_ amaxks
# endif
	extern void amaxks_(short *, short *, short *, int *);
void c_amaxks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxkx_ amaxkx
# endif
	extern void amaxkx_(Complex *, Complex *, Complex *, int *);
void c_amaxkx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxl_ amaxl
# endif
	extern void amaxl_(int *, int *, int *, int *);
void c_amaxl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxr_ amaxr
# endif
	extern void amaxr_(float *, float *, float *, int *);
void c_amaxr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxs_ amaxs
# endif
	extern void amaxs_(short *, short *, short *, int *);
void c_amaxs(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxs_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amaxx_ amaxx
# endif
	extern void amaxx_(Complex *, Complex *, Complex *, int *);
void c_amaxx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amaxx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed3d_ amed3d
# endif
	extern void amed3d_(double *, double *, double *, double *, int *);
void c_amed3d(double *a, double *b, double *c, double *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed3d_(a, b, c, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed3i_ amed3i
# endif
	extern void amed3i_(int *, int *, int *, int *, int *);
void c_amed3i(int *a, int *b, int *c, int *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed3i_(a, b, c, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed3l_ amed3l
# endif
	extern void amed3l_(int *, int *, int *, int *, int *);
void c_amed3l(int *a, int *b, int *c, int *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed3l_(a, b, c, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed3r_ amed3r
# endif
	extern void amed3r_(float *, float *, float *, float *, int *);
void c_amed3r(float *a, float *b, float *c, float *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed3r_(a, b, c, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed3s_ amed3s
# endif
	extern void amed3s_(short *, short *, short *, short *, int *);
void c_amed3s(short *a, short *b, short *c, short *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed3s_(a, b, c, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed4d_ amed4d
# endif
	extern void amed4d_(double *, double *, double *, double *, double *, int *);
void c_amed4d(double *a, double *b, double *c, double *d, double *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed4d_(a, b, c, d, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed4i_ amed4i
# endif
	extern void amed4i_(int *, int *, int *, int *, int *, int *);
void c_amed4i(int *a, int *b, int *c, int *d, int *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed4i_(a, b, c, d, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed4l_ amed4l
# endif
	extern void amed4l_(int *, int *, int *, int *, int *, int *);
void c_amed4l(int *a, int *b, int *c, int *d, int *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed4l_(a, b, c, d, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed4r_ amed4r
# endif
	extern void amed4r_(float *, float *, float *, float *, float *, int *);
void c_amed4r(float *a, float *b, float *c, float *d, float *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed4r_(a, b, c, d, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed4s_ amed4s
# endif
	extern void amed4s_(short *, short *, short *, short *, short *, int *);
void c_amed4s(short *a, short *b, short *c, short *d, short *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed4s_(a, b, c, d, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed5d_ amed5d
# endif
	extern void amed5d_(double *, double *, double *, double *, double *, double *, int *);
void c_amed5d(double *a, double *b, double *c, double *d, double *e, double *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed5d_(a, b, c, d, e, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed5i_ amed5i
# endif
	extern void amed5i_(int *, int *, int *, int *, int *, int *, int *);
void c_amed5i(int *a, int *b, int *c, int *d, int *e, int *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed5i_(a, b, c, d, e, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed5l_ amed5l
# endif
	extern void amed5l_(int *, int *, int *, int *, int *, int *, int *);
void c_amed5l(int *a, int *b, int *c, int *d, int *e, int *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed5l_(a, b, c, d, e, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed5r_ amed5r
# endif
	extern void amed5r_(float *, float *, float *, float *, float *, float *, int *);
void c_amed5r(float *a, float *b, float *c, float *d, float *e, float *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed5r_(a, b, c, d, e, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amed5s_ amed5s
# endif
	extern void amed5s_(short *, short *, short *, short *, short *, short *, int *);
void c_amed5s(short *a, short *b, short *c, short *d, short *e, short *m, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amed5s_(a, b, c, d, e, m, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amedd_ amedd
# endif
	extern double amedd_(double *, int *);
double c_amedd(double *a, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = amedd_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define amedi_ amedi
# endif
	extern int amedi_(int *, int *);
int c_amedi(int *a, int *npix) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = amedi_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define amedl_ amedl
# endif
	extern int amedl_(int *, int *);
int c_amedl(int *a, int *npix) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = amedl_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define amedr_ amedr
# endif
	extern float amedr_(float *, int *);
float c_amedr(float *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = amedr_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define ameds_ ameds
# endif
	extern short ameds_(short *, int *);
short c_ameds(short *a, int *npix) {
	short rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = ameds_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define amedx_ amedx
# endif
	extern Complex amedx_(Complex *, int *);
Complex c_amedx(Complex *a, int *npix) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = amedx_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define amgsd_ amgsd
# endif
	extern void amgsd_(double *, double *, double *, int *);
void c_amgsd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amgsd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amgsi_ amgsi
# endif
	extern void amgsi_(int *, int *, int *, int *);
void c_amgsi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amgsi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amgsl_ amgsl
# endif
	extern void amgsl_(int *, int *, int *, int *);
void c_amgsl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amgsl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amgsr_ amgsr
# endif
	extern void amgsr_(float *, float *, float *, int *);
void c_amgsr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amgsr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amgss_ amgss
# endif
	extern void amgss_(short *, short *, short *, int *);
void c_amgss(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amgss_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amgsx_ amgsx
# endif
	extern void amgsx_(Complex *, Complex *, Complex *, int *);
void c_amgsx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amgsx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amind_ amind
# endif
	extern void amind_(double *, double *, double *, int *);
void c_amind(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amind_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amini_ amini
# endif
	extern void amini_(int *, int *, int *, int *);
void c_amini(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amini_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminkd_ aminkd
# endif
	extern void aminkd_(double *, double *, double *, int *);
void c_aminkd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminki_ aminki
# endif
	extern void aminki_(int *, int *, int *, int *);
void c_aminki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminkl_ aminkl
# endif
	extern void aminkl_(int *, int *, int *, int *);
void c_aminkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminkr_ aminkr
# endif
	extern void aminkr_(float *, float *, float *, int *);
void c_aminkr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminks_ aminks
# endif
	extern void aminks_(short *, short *, short *, int *);
void c_aminks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminkx_ aminkx
# endif
	extern void aminkx_(Complex *, Complex *, Complex *, int *);
void c_aminkx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminl_ aminl
# endif
	extern void aminl_(int *, int *, int *, int *);
void c_aminl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminr_ aminr
# endif
	extern void aminr_(float *, float *, float *, int *);
void c_aminr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amins_ amins
# endif
	extern void amins_(short *, short *, short *, int *);
void c_amins(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amins_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aminx_ aminx
# endif
	extern void aminx_(Complex *, Complex *, Complex *, int *);
void c_aminx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aminx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodd_ amodd
# endif
	extern void amodd_(double *, double *, double *, int *);
void c_amodd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodi_ amodi
# endif
	extern void amodi_(int *, int *, int *, int *);
void c_amodi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodkd_ amodkd
# endif
	extern void amodkd_(double *, double *, double *, int *);
void c_amodkd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodki_ amodki
# endif
	extern void amodki_(int *, int *, int *, int *);
void c_amodki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodkl_ amodkl
# endif
	extern void amodkl_(int *, int *, int *, int *);
void c_amodkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodkr_ amodkr
# endif
	extern void amodkr_(float *, float *, float *, int *);
void c_amodkr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodks_ amodks
# endif
	extern void amodks_(short *, short *, short *, int *);
void c_amodks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodl_ amodl
# endif
	extern void amodl_(int *, int *, int *, int *);
void c_amodl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amodr_ amodr
# endif
	extern void amodr_(float *, float *, float *, int *);
void c_amodr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amodr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amods_ amods
# endif
	extern void amods_(short *, short *, short *, int *);
void c_amods(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amods_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovd_ amovd
# endif
	extern void amovd_(double *, double *, int *);
void c_amovd(double *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovi_ amovi
# endif
	extern void amovi_(int *, int *, int *);
void c_amovi(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovi_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovkd_ amovkd
# endif
	extern void amovkd_(double *, double *, int *);
void c_amovkd(double *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovkd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovki_ amovki
# endif
	extern void amovki_(int *, int *, int *);
void c_amovki(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovki_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovkl_ amovkl
# endif
	extern void amovkl_(int *, int *, int *);
void c_amovkl(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovkl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovkr_ amovkr
# endif
	extern void amovkr_(float *, float *, int *);
void c_amovkr(float *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovkr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovks_ amovks
# endif
	extern void amovks_(short *, short *, int *);
void c_amovks(short *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovks_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovkx_ amovkx
# endif
	extern void amovkx_(Complex *, Complex *, int *);
void c_amovkx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovkx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovl_ amovl
# endif
	extern void amovl_(int *, int *, int *);
void c_amovl(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovr_ amovr
# endif
	extern void amovr_(float *, float *, int *);
void c_amovr(float *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovs_ amovs
# endif
	extern void amovs_(short *, short *, int *);
void c_amovs(short *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovs_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amovx_ amovx
# endif
	extern void amovx_(Complex *, Complex *, int *);
void c_amovx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amovx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amuld_ amuld
# endif
	extern void amuld_(double *, double *, double *, int *);
void c_amuld(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amuld_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amuli_ amuli
# endif
	extern void amuli_(int *, int *, int *, int *);
void c_amuli(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amuli_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amulkd_ amulkd
# endif
	extern void amulkd_(double *, double *, double *, int *);
void c_amulkd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amulkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amulki_ amulki
# endif
	extern void amulki_(int *, int *, int *, int *);
void c_amulki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amulki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amulkl_ amulkl
# endif
	extern void amulkl_(int *, int *, int *, int *);
void c_amulkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amulkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amulkr_ amulkr
# endif
	extern void amulkr_(float *, float *, float *, int *);
void c_amulkr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amulkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amulks_ amulks
# endif
	extern void amulks_(short *, short *, short *, int *);
void c_amulks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amulks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amulkx_ amulkx
# endif
	extern void amulkx_(Complex *, Complex *, Complex *, int *);
void c_amulkx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amulkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amull_ amull
# endif
	extern void amull_(int *, int *, int *, int *);
void c_amull(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amull_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amulr_ amulr
# endif
	extern void amulr_(float *, float *, float *, int *);
void c_amulr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amulr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amuls_ amuls
# endif
	extern void amuls_(short *, short *, short *, int *);
void c_amuls(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amuls_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define amulx_ amulx
# endif
	extern void amulx_(Complex *, Complex *, Complex *, int *);
void c_amulx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	amulx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anegd_ anegd
# endif
	extern void anegd_(double *, double *, int *);
void c_anegd(double *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anegd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anegi_ anegi
# endif
	extern void anegi_(int *, int *, int *);
void c_anegi(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anegi_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anegl_ anegl
# endif
	extern void anegl_(int *, int *, int *);
void c_anegl(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anegl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anegr_ anegr
# endif
	extern void anegr_(float *, float *, int *);
void c_anegr(float *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anegr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anegs_ anegs
# endif
	extern void anegs_(short *, short *, int *);
void c_anegs(short *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anegs_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anegx_ anegx
# endif
	extern void anegx_(Complex *, Complex *, int *);
void c_anegx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anegx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anoti_ anoti
# endif
	extern void anoti_(int *, int *, int *);
void c_anoti(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anoti_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anotl_ anotl
# endif
	extern void anotl_(int *, int *, int *);
void c_anotl(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anotl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anots_ anots
# endif
	extern void anots_(short *, short *, int *);
void c_anots(short *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	anots_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apkxd_ apkxd
# endif
	extern void apkxd_(double *, double *, Complex *, int *);
void c_apkxd(double *a, double *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apkxd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apkxi_ apkxi
# endif
	extern void apkxi_(int *, int *, Complex *, int *);
void c_apkxi(int *a, int *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apkxi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apkxl_ apkxl
# endif
	extern void apkxl_(int *, int *, Complex *, int *);
void c_apkxl(int *a, int *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apkxl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apkxr_ apkxr
# endif
	extern void apkxr_(float *, float *, Complex *, int *);
void c_apkxr(float *a, float *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apkxr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apkxs_ apkxs
# endif
	extern void apkxs_(short *, short *, Complex *, int *);
void c_apkxs(short *a, short *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apkxs_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apkxx_ apkxx
# endif
	extern void apkxx_(Complex *, Complex *, Complex *, int *);
void c_apkxx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apkxx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apold_ apold
# endif
	extern double apold_(double *, double *, int *);
double c_apold(double *x, double *coeff, int *ncoeff) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = apold_(x, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define apolr_ apolr
# endif
	extern float apolr_(float *, float *, int *);
float c_apolr(float *x, float *coeff, int *ncoeff) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = apolr_(x, coeff, ncoeff);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define apowd_ apowd
# endif
	extern void apowd_(double *, int *, double *, int *);
void c_apowd(double *a, int *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowi_ apowi
# endif
	extern void apowi_(int *, int *, int *, int *);
void c_apowi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowkd_ apowkd
# endif
	extern void apowkd_(double *, int *, double *, int *);
void c_apowkd(double *a, int *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowki_ apowki
# endif
	extern void apowki_(int *, int *, int *, int *);
void c_apowki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowkl_ apowkl
# endif
	extern void apowkl_(int *, int *, int *, int *);
void c_apowkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowkr_ apowkr
# endif
	extern void apowkr_(float *, int *, float *, int *);
void c_apowkr(float *a, int *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowks_ apowks
# endif
	extern void apowks_(short *, int *, short *, int *);
void c_apowks(short *a, int *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowkx_ apowkx
# endif

/*
cslocum - removed this function because apowkx_ does not exist, resulting in
errors building this file (apowkx is not being built in IRAF, and c_apowkx does
not appear to be called anywhere)

	extern void apowkx_(Complex *, int *, Complex *, int *);
void c_apowkx(Complex *a, int *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}
*/

# if defined(NO_UNDERSCORE)
# define apowl_ apowl
# endif
	extern void apowl_(int *, int *, int *, int *);
void c_apowl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowr_ apowr
# endif
	extern void apowr_(float *, int *, float *, int *);
void c_apowr(float *a, int *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apows_ apows
# endif
	extern void apows_(short *, int *, short *, int *);
void c_apows(short *a, int *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apows_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define apowx_ apowx
# endif
	extern void apowx_(Complex *, int *, Complex *, int *);
void c_apowx(Complex *a, int *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	apowx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aravd_ aravd
# endif
	extern int aravd_(double *, int *, double *, double *, double *);
int c_aravd(double *a, int *npix, double *mean, double *sigma, double *ksig) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aravd_(a, npix, mean, sigma, ksig);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aravi_ aravi
# endif
	extern int aravi_(int *, int *, float *, float *, float *);
int c_aravi(int *a, int *npix, float *mean, float *sigma, float *ksig) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aravi_(a, npix, mean, sigma, ksig);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aravl_ aravl
# endif
	extern int aravl_(int *, int *, double *, double *, double *);
int c_aravl(int *a, int *npix, double *mean, double *sigma, double *ksig) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aravl_(a, npix, mean, sigma, ksig);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aravr_ aravr
# endif
	extern int aravr_(float *, int *, float *, float *, float *);
int c_aravr(float *a, int *npix, float *mean, float *sigma, float *ksig) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aravr_(a, npix, mean, sigma, ksig);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aravs_ aravs
# endif
	extern int aravs_(short *, int *, float *, float *, float *);
int c_aravs(short *a, int *npix, float *mean, float *sigma, float *ksig) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aravs_(a, npix, mean, sigma, ksig);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aravx_ aravx
# endif
	extern int aravx_(Complex *, int *, float *, float *, float *);
int c_aravx(Complex *a, int *npix, float *mean, float *sigma, float *ksig) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aravx_(a, npix, mean, sigma, ksig);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define arcpd_ arcpd
# endif
	extern void arcpd_(double *, double *, double *, int *);
void c_arcpd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	arcpd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arcpi_ arcpi
# endif
	extern void arcpi_(int *, int *, int *, int *);
void c_arcpi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	arcpi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arcpl_ arcpl
# endif
	extern void arcpl_(int *, int *, int *, int *);
void c_arcpl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	arcpl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arcpr_ arcpr
# endif
	extern void arcpr_(float *, float *, float *, int *);
void c_arcpr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	arcpr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arcps_ arcps
# endif
	extern void arcps_(short *, short *, short *, int *);
void c_arcps(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	arcps_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arcpx_ arcpx
# endif
	extern void arcpx_(Complex *, Complex *, Complex *, int *);
void c_arcpx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	arcpx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define argtd_ argtd
# endif
	extern void argtd_(double *, int *, double *, double *);
void c_argtd(double *a, int *npix, double *ceil, double *newval) {
	clear_cvoserr();
	xerpsh_();
	argtd_(a, npix, ceil, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define argti_ argti
# endif
	extern void argti_(int *, int *, int *, int *);
void c_argti(int *a, int *npix, int *ceil, int *newval) {
	clear_cvoserr();
	xerpsh_();
	argti_(a, npix, ceil, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define argtl_ argtl
# endif
	extern void argtl_(int *, int *, int *, int *);
void c_argtl(int *a, int *npix, int *ceil, int *newval) {
	clear_cvoserr();
	xerpsh_();
	argtl_(a, npix, ceil, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define argtr_ argtr
# endif
	extern void argtr_(float *, int *, float *, float *);
void c_argtr(float *a, int *npix, float *ceil, float *newval) {
	clear_cvoserr();
	xerpsh_();
	argtr_(a, npix, ceil, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define argts_ argts
# endif
	extern void argts_(short *, int *, short *, short *);
void c_argts(short *a, int *npix, short *ceil, short *newval) {
	clear_cvoserr();
	xerpsh_();
	argts_(a, npix, ceil, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define argtx_ argtx
# endif
	extern void argtx_(Complex *, int *, Complex *, Complex *);
void c_argtx(Complex *a, int *npix, Complex *ceil, Complex *newval) {
	clear_cvoserr();
	xerpsh_();
	argtx_(a, npix, ceil, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arltd_ arltd
# endif
	extern void arltd_(double *, int *, double *, double *);
void c_arltd(double *a, int *npix, double *floor, double *newval) {
	clear_cvoserr();
	xerpsh_();
	arltd_(a, npix, floor, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arlti_ arlti
# endif
	extern void arlti_(int *, int *, int *, int *);
void c_arlti(int *a, int *npix, int *floor, int *newval) {
	clear_cvoserr();
	xerpsh_();
	arlti_(a, npix, floor, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arltl_ arltl
# endif
	extern void arltl_(int *, int *, int *, int *);
void c_arltl(int *a, int *npix, int *floor, int *newval) {
	clear_cvoserr();
	xerpsh_();
	arltl_(a, npix, floor, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arltr_ arltr
# endif
	extern void arltr_(float *, int *, float *, float *);
void c_arltr(float *a, int *npix, float *floor, float *newval) {
	clear_cvoserr();
	xerpsh_();
	arltr_(a, npix, floor, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arlts_ arlts
# endif
	extern void arlts_(short *, int *, short *, short *);
void c_arlts(short *a, int *npix, short *floor, short *newval) {
	clear_cvoserr();
	xerpsh_();
	arlts_(a, npix, floor, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arltx_ arltx
# endif
	extern void arltx_(Complex *, int *, Complex *, Complex *);
void c_arltx(Complex *a, int *npix, Complex *floor, Complex *newval) {
	clear_cvoserr();
	xerpsh_();
	arltx_(a, npix, floor, newval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aseld_ aseld
# endif
	extern void aseld_(double *, double *, double *, int *, int *);
void c_aseld(double *a, double *b, double *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aseld_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aseli_ aseli
# endif
	extern void aseli_(int *, int *, int *, int *, int *);
void c_aseli(int *a, int *b, int *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aseli_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aselkd_ aselkd
# endif
	extern void aselkd_(double *, double *, double *, int *, int *);
void c_aselkd(double *a, double *b, double *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aselkd_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aselki_ aselki
# endif
	extern void aselki_(int *, int *, int *, int *, int *);
void c_aselki(int *a, int *b, int *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aselki_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aselkl_ aselkl
# endif
	extern void aselkl_(int *, int *, int *, int *, int *);
void c_aselkl(int *a, int *b, int *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aselkl_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aselkr_ aselkr
# endif
	extern void aselkr_(float *, float *, float *, int *, int *);
void c_aselkr(float *a, float *b, float *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aselkr_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aselks_ aselks
# endif
	extern void aselks_(short *, short *, short *, int *, int *);
void c_aselks(short *a, short *b, short *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aselks_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aselkx_ aselkx
# endif
	extern void aselkx_(Complex *, Complex *, Complex *, int *, int *);
void c_aselkx(Complex *a, Complex *b, Complex *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aselkx_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asell_ asell
# endif
	extern void asell_(int *, int *, int *, int *, int *);
void c_asell(int *a, int *b, int *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asell_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aselr_ aselr
# endif
	extern void aselr_(float *, float *, float *, int *, int *);
void c_aselr(float *a, float *b, float *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aselr_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asels_ asels
# endif
	extern void asels_(short *, short *, short *, int *, int *);
void c_asels(short *a, short *b, short *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asels_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aselx_ aselx
# endif
	extern void aselx_(Complex *, Complex *, Complex *, int *, int *);
void c_aselx(Complex *a, Complex *b, Complex *c, int *sel, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aselx_(a, b, c, sel, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asokd_ asokd
# endif
	extern double asokd_(double *, int *, int *);
double c_asokd(double *a, int *npix, int *ksel) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asokd_(a, npix, ksel);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asoki_ asoki
# endif
	extern int asoki_(int *, int *, int *);
int c_asoki(int *a, int *npix, int *ksel) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asoki_(a, npix, ksel);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asokl_ asokl
# endif
	extern int asokl_(int *, int *, int *);
int c_asokl(int *a, int *npix, int *ksel) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asokl_(a, npix, ksel);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asokr_ asokr
# endif
	extern float asokr_(float *, int *, int *);
float c_asokr(float *a, int *npix, int *ksel) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asokr_(a, npix, ksel);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asoks_ asoks
# endif
	extern short asoks_(short *, int *, int *);
short c_asoks(short *a, int *npix, int *ksel) {
	short rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asoks_(a, npix, ksel);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asokx_ asokx
# endif
	extern Complex asokx_(Complex *, int *, int *);
Complex c_asokx(Complex *a, int *npix, int *ksel) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asokx_(a, npix, ksel);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asrtd_ asrtd
# endif
	extern void asrtd_(double *, double *, int *);
void c_asrtd(double *a, double *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asrtd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asrti_ asrti
# endif
	extern void asrti_(int *, int *, int *);
void c_asrti(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asrti_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asrtl_ asrtl
# endif
	extern void asrtl_(int *, int *, int *);
void c_asrtl(int *a, int *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asrtl_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asrtr_ asrtr
# endif
	extern void asrtr_(float *, float *, int *);
void c_asrtr(float *a, float *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asrtr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asrts_ asrts
# endif
	extern void asrts_(short *, short *, int *);
void c_asrts(short *a, short *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asrts_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asrtx_ asrtx
# endif
	extern void asrtx_(Complex *, Complex *, int *);
void c_asrtx(Complex *a, Complex *b, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asrtx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define assqd_ assqd
# endif
	extern double assqd_(double *, int *);
double c_assqd(double *a, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = assqd_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define assqi_ assqi
# endif
	extern float assqi_(int *, int *);
float c_assqi(int *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = assqi_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define assql_ assql
# endif
	extern double assql_(int *, int *);
double c_assql(int *a, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = assql_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define assqr_ assqr
# endif
	extern float assqr_(float *, int *);
float c_assqr(float *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = assqr_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define assqs_ assqs
# endif
	extern float assqs_(short *, int *);
float c_assqs(short *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = assqs_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define assqx_ assqx
# endif
	extern Complex assqx_(Complex *, int *);
Complex c_assqx(Complex *a, int *npix) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = assqx_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asubd_ asubd
# endif
	extern void asubd_(double *, double *, double *, int *);
void c_asubd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubi_ asubi
# endif
	extern void asubi_(int *, int *, int *, int *);
void c_asubi(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubkd_ asubkd
# endif
	extern void asubkd_(double *, double *, double *, int *);
void c_asubkd(double *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubkd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubki_ asubki
# endif
	extern void asubki_(int *, int *, int *, int *);
void c_asubki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubkl_ asubkl
# endif
	extern void asubkl_(int *, int *, int *, int *);
void c_asubkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubkr_ asubkr
# endif
	extern void asubkr_(float *, float *, float *, int *);
void c_asubkr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubkr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubks_ asubks
# endif
	extern void asubks_(short *, short *, short *, int *);
void c_asubks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubkx_ asubkx
# endif
	extern void asubkx_(Complex *, Complex *, Complex *, int *);
void c_asubkx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubkx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubl_ asubl
# endif
	extern void asubl_(int *, int *, int *, int *);
void c_asubl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubr_ asubr
# endif
	extern void asubr_(float *, float *, float *, int *);
void c_asubr(float *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubs_ asubs
# endif
	extern void asubs_(short *, short *, short *, int *);
void c_asubs(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubs_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asubx_ asubx
# endif
	extern void asubx_(Complex *, Complex *, Complex *, int *);
void c_asubx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asubx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asumd_ asumd
# endif
	extern double asumd_(double *, int *);
double c_asumd(double *a, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asumd_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asumi_ asumi
# endif
	extern float asumi_(int *, int *);
float c_asumi(int *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asumi_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asuml_ asuml
# endif
	extern double asuml_(int *, int *);
double c_asuml(int *a, int *npix) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asuml_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asumr_ asumr
# endif
	extern float asumr_(float *, int *);
float c_asumr(float *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asumr_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asums_ asums
# endif
	extern float asums_(short *, int *);
float c_asums(short *a, int *npix) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asums_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asumx_ asumx
# endif
	extern Complex asumx_(Complex *, int *);
Complex c_asumx(Complex *a, int *npix) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asumx_(a, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aupxd_ aupxd
# endif
	extern void aupxd_(Complex *, double *, double *, int *);
void c_aupxd(Complex *a, double *b, double *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aupxd_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aupxi_ aupxi
# endif
	extern void aupxi_(Complex *, int *, int *, int *);
void c_aupxi(Complex *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aupxi_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aupxl_ aupxl
# endif
	extern void aupxl_(Complex *, int *, int *, int *);
void c_aupxl(Complex *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aupxl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aupxr_ aupxr
# endif
	extern void aupxr_(Complex *, float *, float *, int *);
void c_aupxr(Complex *a, float *b, float *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aupxr_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aupxs_ aupxs
# endif
	extern void aupxs_(Complex *, short *, short *, int *);
void c_aupxs(Complex *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aupxs_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aupxx_ aupxx
# endif
	extern void aupxx_(Complex *, Complex *, Complex *, int *);
void c_aupxx(Complex *a, Complex *b, Complex *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	aupxx_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define aveqd_ aveqd
# endif
	extern Bool aveqd_(double *, double *, int *);
Bool c_aveqd(double *a, double *b, int *npix) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aveqd_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aveqi_ aveqi
# endif
	extern Bool aveqi_(int *, int *, int *);
Bool c_aveqi(int *a, int *b, int *npix) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aveqi_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aveql_ aveql
# endif
	extern Bool aveql_(int *, int *, int *);
Bool c_aveql(int *a, int *b, int *npix) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aveql_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aveqr_ aveqr
# endif
	extern Bool aveqr_(float *, float *, int *);
Bool c_aveqr(float *a, float *b, int *npix) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aveqr_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aveqs_ aveqs
# endif
	extern Bool aveqs_(short *, short *, int *);
Bool c_aveqs(short *a, short *b, int *npix) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aveqs_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define aveqx_ aveqx
# endif
	extern Bool aveqx_(Complex *, Complex *, int *);
Bool c_aveqx(Complex *a, Complex *b, int *npix) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = aveqx_(a, b, npix);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define awsud_ awsud
# endif
	extern void awsud_(double *, double *, double *, int *, double *, double *);
void c_awsud(double *a, double *b, double *c, int *npix, double *k1, double *k2) {
	clear_cvoserr();
	xerpsh_();
	awsud_(a, b, c, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define awsui_ awsui
# endif
	extern void awsui_(int *, int *, int *, int *, float *, float *);
void c_awsui(int *a, int *b, int *c, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	awsui_(a, b, c, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define awsul_ awsul
# endif
	extern void awsul_(int *, int *, int *, int *, float *, float *);
void c_awsul(int *a, int *b, int *c, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	awsul_(a, b, c, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define awsur_ awsur
# endif
	extern void awsur_(float *, float *, float *, int *, float *, float *);
void c_awsur(float *a, float *b, float *c, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	awsur_(a, b, c, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define awsus_ awsus
# endif
	extern void awsus_(short *, short *, short *, int *, float *, float *);
void c_awsus(short *a, short *b, short *c, int *npix, float *k1, float *k2) {
	clear_cvoserr();
	xerpsh_();
	awsus_(a, b, c, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define awsux_ awsux
# endif
	extern void awsux_(Complex *, Complex *, Complex *, int *, Complex *, Complex *);
void c_awsux(Complex *a, Complex *b, Complex *c, int *npix, Complex *k1, Complex *k2) {
	clear_cvoserr();
	xerpsh_();
	awsux_(a, b, c, npix, k1, k2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define awvgd_ awvgd
# endif
	extern int awvgd_(double *, int *, double *, double *, double *, double *);
int c_awvgd(double *a, int *npix, double *mean, double *sigma, double *lcut, double *hcut) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = awvgd_(a, npix, mean, sigma, lcut, hcut);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define awvgi_ awvgi
# endif
	extern int awvgi_(int *, int *, float *, float *, float *, float *);
int c_awvgi(int *a, int *npix, float *mean, float *sigma, float *lcut, float *hcut) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = awvgi_(a, npix, mean, sigma, lcut, hcut);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define awvgl_ awvgl
# endif
	extern int awvgl_(int *, int *, double *, double *, double *, double *);
int c_awvgl(int *a, int *npix, double *mean, double *sigma, double *lcut, double *hcut) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = awvgl_(a, npix, mean, sigma, lcut, hcut);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define awvgr_ awvgr
# endif
	extern int awvgr_(float *, int *, float *, float *, float *, float *);
int c_awvgr(float *a, int *npix, float *mean, float *sigma, float *lcut, float *hcut) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = awvgr_(a, npix, mean, sigma, lcut, hcut);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define awvgs_ awvgs
# endif
	extern int awvgs_(short *, int *, float *, float *, float *, float *);
int c_awvgs(short *a, int *npix, float *mean, float *sigma, float *lcut, float *hcut) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = awvgs_(a, npix, mean, sigma, lcut, hcut);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define awvgx_ awvgx
# endif
	extern int awvgx_(Complex *, int *, float *, float *, float *, float *);
int c_awvgx(Complex *a, int *npix, float *mean, float *sigma, float *lcut, float *hcut) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = awvgx_(a, npix, mean, sigma, lcut, hcut);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define axori_ axori
# endif
	extern void axori_(int *, int *, int *, int *);
void c_axori(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	axori_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define axorki_ axorki
# endif
	extern void axorki_(int *, int *, int *, int *);
void c_axorki(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	axorki_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define axorkl_ axorkl
# endif
	extern void axorkl_(int *, int *, int *, int *);
void c_axorkl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	axorkl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define axorks_ axorks
# endif
	extern void axorks_(short *, short *, short *, int *);
void c_axorks(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	axorks_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define axorl_ axorl
# endif
	extern void axorl_(int *, int *, int *, int *);
void c_axorl(int *a, int *b, int *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	axorl_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define axors_ axors
# endif
	extern void axors_(short *, short *, short *, int *);
void c_axors(short *a, short *b, short *c, int *npix) {
	clear_cvoserr();
	xerpsh_();
	axors_(a, b, c, npix);
	if (xerpoi_())
	    set_cvoserr();
}

