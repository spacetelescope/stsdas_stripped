# include <ximinterp.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define arbpix_ arbpix
# endif
	extern void arbpix_(float *, float *, int *, int *, int *);
void c_arbpix(float *datain, float *dataout, int *npts, int *interp_type, int *boundary_type) {
	clear_cvoserr();
	xerpsh_();
	arbpix_(datain, dataout, npts, interp_type, boundary_type);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iibadx_ iibadx
# endif
	extern float iibadx_(float *, int *, int *, int *);
float c_ii_badpix(float *datain, int *npix, int *index, int *interp_type) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = iibadx_(datain, npix, index, interp_type);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define iinewx_ iinewx
# endif
	extern float iinewx_(float *, float *, float *, int *, int *, int *);
float c_ii_newpix(float *x, float *xarray, float *data, int *npts, int *index, int *interp_type) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = iinewx_(x, xarray, data, npts, index, interp_type);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define iibadc_ iibadc
# endif
	extern void iibadc_(float *, float *, int *, int *, int *, float *, float *);
void c_ii_badsinc(float *datain, float *dataout, int *npts, int *nsinc, int *ntaper, float *staper, float *min_bdx) {
	clear_cvoserr();
	xerpsh_();
	iibadc_(datain, dataout, npts, nsinc, ntaper, staper, min_bdx);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arider_ arider
# endif
	extern void arider_(float *, float *, int *, float *, int *, int *);
void c_arider(float *x, float *datain, int *npix, float *derivs, int *nder, int *interp_type) {
	clear_cvoserr();
	xerpsh_();
	arider_(x, datain, npix, derivs, nder, interp_type);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define arievl_ arievl
# endif
	extern float arievl_(float *, float *, int *, int *);
float c_arieval(float *x, float *datain, int *npts, int *interp_type) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = arievl_(x, datain, npts, interp_type);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asider_ asider
# endif
	extern void asider_(IRAFPointer *, float *, float *, int *);
void c_asider(IRAFPointer *asi, float *x, float *der, int *nder) {
	clear_cvoserr();
	xerpsh_();
	asider_(asi, x, der, nder);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asievl_ asievl
# endif
	extern float asievl_(IRAFPointer *, float *);
float c_asieval(IRAFPointer *asi, float *x) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asievl_(asi, x);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asifit_ asifit
# endif
	extern void asifit_(IRAFPointer *, float *, int *);
void c_asifit(IRAFPointer *asi, float *datain, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asifit_(asi, datain, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asifre_ asifre
# endif
	extern void asifre_(IRAFPointer *);
void c_asifree(IRAFPointer *asi) {
	clear_cvoserr();
	xerpsh_();
	asifre_(asi);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asigei_ asigei
# endif
	extern int asigei_(IRAFPointer *, int *);
int c_asigeti(IRAFPointer *asi, int *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asigei_(asi, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asigrl_ asigrl
# endif
	extern float asigrl_(IRAFPointer *, float *, float *);
float c_asigrl(IRAFPointer *asi, float *a, float *b) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = asigrl_(asi, a, b);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define asiint_ asiint
# endif
	extern void asiint_(IRAFPointer *, int *);
void c_asiinit(IRAFPointer *asi, int *interp_type) {
	clear_cvoserr();
	xerpsh_();
	asiint_(asi, interp_type);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asiree_ asiree
# endif
	extern void asiree_(IRAFPointer *, float *);
void c_asirestore(IRAFPointer *asi, float *interpolant) {
	clear_cvoserr();
	xerpsh_();
	asiree_(asi, interpolant);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asisae_ asisae
# endif
	extern void asisae_(IRAFPointer *, float *);
void c_asisave(IRAFPointer *asi, float *interpolant) {
	clear_cvoserr();
	xerpsh_();
	asisae_(asi, interpolant);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define asiver_ asiver
# endif
	extern void asiver_(IRAFPointer *, float *, float *, int *);
void c_asivector(IRAFPointer *asi, float *x, float *y, int *npix) {
	clear_cvoserr();
	xerpsh_();
	asiver_(asi, x, y, npix);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define ii1dig_ ii1dig
# endif
	extern float ii1dig_(float *, float *, float *, int *);
float c_ii_1dinteg(float *coeff, float *a, float *b, int *interp_type) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = ii1dig_(coeff, a, b, interp_type);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define iigetf_ iigetf
# endif
	extern void iigetf_(float *, int *, float *, int *);
void c_ii_getpcoeff(float *coeff, int *index, float *pcoeff, int *interp_type) {
	clear_cvoserr();
	xerpsh_();
	iigetf_(coeff, index, pcoeff, interp_type);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iisinl_ iisinl
# endif
	extern void iisinl_(float *, float *, float *, float *, int *, int *, int *, float *, float *);
void c_ii_sincigrl(float *a, float *b, float *sum, float *data, int *npix, int *nsinc, int *ntaper, float *staper, float *mindx) {
	clear_cvoserr();
	xerpsh_();
	iisinl_(a, b, sum, data, npix, nsinc, ntaper, staper, mindx);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iibint_ iibint
# endif
	extern void iibint_(float *, int *, int *, float *, float *, float *, int *);
void c_ii_binearest(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	iibint_(coeff, first_point, len_coeff, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iibilr_ iibilr
# endif
	extern void iibilr_(float *, int *, int *, float *, float *, float *, int *);
void c_ii_bilinear(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	iibilr_(coeff, first_point, len_coeff, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iibip3_ iibip3
# endif
	extern void iibip3_(float *, int *, int *, float *, float *, float *, int *);
void c_ii_bipoly3(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	iibip3_(coeff, first_point, len_coeff, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iibip5_ iibip5
# endif
	extern void iibip5_(float *, int *, int *, float *, float *, float *, int *);
void c_ii_bipoly5(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	iibip5_(coeff, first_point, len_coeff, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iibis3_ iibis3
# endif
	extern void iibis3_(float *, int *, int *, float *, float *, float *, int *);
void c_ii_bispline3(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	iibis3_(coeff, first_point, len_coeff, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iineat_ iineat
# endif
	extern void iineat_(float *, float *, int *, float *);
void c_ii_nearest(float *x, float *y, int *npts, float *data) {
	clear_cvoserr();
	xerpsh_();
	iineat_(x, y, npts, data);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iilinr_ iilinr
# endif
	extern void iilinr_(float *, float *, int *, float *);
void c_ii_linear(float *x, float *y, int *npts, float *data) {
	clear_cvoserr();
	xerpsh_();
	iilinr_(x, y, npts, data);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iipol3_ iipol3
# endif
	extern void iipol3_(float *, float *, int *, float *);
void c_ii_poly3(float *x, float *y, int *npts, float *data) {
	clear_cvoserr();
	xerpsh_();
	iipol3_(x, y, npts, data);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iipol5_ iipol5
# endif
	extern void iipol5_(float *, float *, int *, float *);
void c_ii_poly5(float *x, float *y, int *npts, float *data) {
	clear_cvoserr();
	xerpsh_();
	iipol5_(x, y, npts, data);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iispl3_ iispl3
# endif
	extern void iispl3_(float *, float *, int *, float *);
void c_ii_spline3(float *x, float *y, int *npts, float *bcoeff) {
	clear_cvoserr();
	xerpsh_();
	iispl3_(x, y, npts, bcoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iisinc_ iisinc
# endif
	extern void iisinc_(float *, float *, int *, float *, int *, int *, int *, float *, float *);
void c_ii_sinc(float *x, float *y, int *npts, float *data, int *npix, int *nsinc, int *ntaper, float *staper, float *mindx) {
	clear_cvoserr();
	xerpsh_();
	iisinc_(x, y, npts, data, npix, nsinc, ntaper, staper, mindx);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iigrnt_ iigrnt
# endif
	extern void iigrnt_(float *, int *, int *, float *, float *, float *, int *, int *, int *);
void c_ii_grnearest(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *nxpts, int *nypts, int *len_zfit) {
	clear_cvoserr();
	xerpsh_();
	iigrnt_(coeff, first_point, len_coeff, x, y, zfit, nxpts, nypts, len_zfit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iigrlr_ iigrlr
# endif
	extern void iigrlr_(float *, int *, int *, float *, float *, float *, int *, int *, int *);
void c_ii_grlinear(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *nxpts, int *nypts, int *len_zfit) {
	clear_cvoserr();
	xerpsh_();
	iigrlr_(coeff, first_point, len_coeff, x, y, zfit, nxpts, nypts, len_zfit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iigrp3_ iigrp3
# endif
	extern void iigrp3_(float *, int *, int *, float *, float *, float *, int *, int *, int *);
void c_ii_grpoly3(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *nxpts, int *nypts, int *len_zfit) {
	clear_cvoserr();
	xerpsh_();
	iigrp3_(coeff, first_point, len_coeff, x, y, zfit, nxpts, nypts, len_zfit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iigrp5_ iigrp5
# endif
	extern void iigrp5_(float *, int *, int *, float *, float *, float *, int *, int *, int *);
void c_ii_grpoly5(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *nxpts, int *nypts, int *len_zfit) {
	clear_cvoserr();
	xerpsh_();
	iigrp5_(coeff, first_point, len_coeff, x, y, zfit, nxpts, nypts, len_zfit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iigrs3_ iigrs3
# endif
	extern void iigrs3_(float *, int *, int *, float *, float *, float *, int *, int *, int *);
void c_ii_grspline3(float *coeff, int *first_point, int *len_coeff, float *x, float *y, float *zfit, int *nxpts, int *nypts, int *len_zfit) {
	clear_cvoserr();
	xerpsh_();
	iigrs3_(coeff, first_point, len_coeff, x, y, zfit, nxpts, nypts, len_zfit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iapcp3_ iapcp3
# endif
	extern void iapcp3_(float *, float *, int *, float *);
void c_ia_pcpoly3(float *x, float *datain, int *npts, float *pcoeff) {
	clear_cvoserr();
	xerpsh_();
	iapcp3_(x, datain, npts, pcoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iapcp5_ iapcp5
# endif
	extern void iapcp5_(float *, float *, int *, float *);
void c_ia_pcpoly5(float *x, float *datain, int *npts, float *pcoeff) {
	clear_cvoserr();
	xerpsh_();
	iapcp5_(x, datain, npts, pcoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iapcs3_ iapcs3
# endif
	extern void iapcs3_(float *, float *, int *, float *);
void c_ia_pcspline3(float *x, float *datain, int *npts, float *pcoeff) {
	clear_cvoserr();
	xerpsh_();
	iapcs3_(x, datain, npts, pcoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iisinr_ iisinr
# endif
	extern void iisinr_(float *, float *, int *, float *, int *, int *, int *, float *, float *);
void c_ii_sincder(float *x, float *der, int *nder, float *data, int *npix, int *nsinc, int *ntaper, float *staper, float *mindx) {
	clear_cvoserr();
	xerpsh_();
	iisinr_(x, der, nder, data, npix, nsinc, ntaper, staper, mindx);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iipcp3_ iipcp3
# endif
	extern void iipcp3_(float *, int *, int *, float *, int *);
void c_ii_pcpoly3(float *coeff, int *index, int *len_coeff, float *pcoeff, int *len_pcoeff) {
	clear_cvoserr();
	xerpsh_();
	iipcp3_(coeff, index, len_coeff, pcoeff, len_pcoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iipcp5_ iipcp5
# endif
	extern void iipcp5_(float *, int *, int *, float *, int *);
void c_ii_pcpoly5(float *coeff, int *index, int *len_coeff, float *pcoeff, int *len_pcoeff) {
	clear_cvoserr();
	xerpsh_();
	iipcp5_(coeff, index, len_coeff, pcoeff, len_pcoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iipcs3_ iipcs3
# endif
	extern void iipcs3_(float *, int *, int *, float *, int *);
void c_ii_pcspline3(float *coeff, int *index, int *len_coeff, float *pcoeff, int *len_pcoeff) {
	clear_cvoserr();
	xerpsh_();
	iipcs3_(coeff, index, len_coeff, pcoeff, len_pcoeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iipolp_ iipolp
# endif
	extern float iipolp_(float *, float *, int *, float *);
float c_ii_polterp(float *x, float *y, int *n, float *x0) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = iipolp_(x, y, n, x0);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define iisple_ iisple
# endif
	extern void iisple_(float *, float *, int *);
void c_ii_spline(float *bcoeff, float *diag, int *npts) {
	clear_cvoserr();
	xerpsh_();
	iisple_(bcoeff, diag, npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iispld_ iispld
# endif
	extern void iispld_(float *, float *, int *, int *, int *, int *);
void c_ii_spline2d(float *data, float *coeff, int *nxpix, int *nvectors, int *len_data, int *len_coeff) {
	clear_cvoserr();
	xerpsh_();
	iispld_(data, coeff, nxpix, nvectors, len_data, len_coeff);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mrider_ mrider
# endif
	extern void mrider_(float *, float *, float *, int *, int *, int *, float *, int *, int *, int *, int *);
void c_mrider(float *x, float *y, float *datain, int *nxpix, int *nypix, int *len_datain, float *der, int *nxder, int *nyder, int *len_der, int *interp_type) {
	clear_cvoserr();
	xerpsh_();
	mrider_(x, y, datain, nxpix, nypix, len_datain, der, nxder, nyder, len_der, interp_type);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mrievl_ mrievl
# endif
	extern float mrievl_(float *, float *, float *, int *, int *, int *, int *);
float c_mrieval(float *x, float *y, float *datain, int *nxpix, int *nypix, int *len_datain, int *interp_type) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mrievl_(x, y, datain, nxpix, nypix, len_datain, interp_type);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define msider_ msider
# endif
	extern void msider_(IRAFPointer *, float *, float *, float *, int *, int *, int *);
void c_msider(IRAFPointer *msi, float *x, float *y, float *der, int *nxder, int *nyder, int *len_der) {
	clear_cvoserr();
	xerpsh_();
	msider_(msi, x, y, der, nxder, nyder, len_der);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define msievl_ msievl
# endif
	extern float msievl_(IRAFPointer *, float *, float *);
float c_msieval(IRAFPointer *msi, float *x, float *y) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = msievl_(msi, x, y);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define msifit_ msifit
# endif
	extern void msifit_(IRAFPointer *, float *, int *, int *, int *);
void c_msifit(IRAFPointer *msi, float *datain, int *nxpix, int *nypix, int *len_datain) {
	clear_cvoserr();
	xerpsh_();
	msifit_(msi, datain, nxpix, nypix, len_datain);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define msifre_ msifre
# endif
	extern void msifre_(IRAFPointer *);
void c_msifree(IRAFPointer *msi) {
	clear_cvoserr();
	xerpsh_();
	msifre_(msi);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define msigei_ msigei
# endif
	extern int msigei_(IRAFPointer *, int *);
int c_msigeti(IRAFPointer *msi, int *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = msigei_(msi, param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define msigrd_ msigrd
# endif
	extern void msigrd_(IRAFPointer *, float *, float *, float *, int *, int *, int *);
void c_msigrid(IRAFPointer *msi, float *x, float *y, float *zfit, int *nx, int *ny, int *len_zfit) {
	clear_cvoserr();
	xerpsh_();
	msigrd_(msi, x, y, zfit, nx, ny, len_zfit);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define msigrl_ msigrl
# endif
	extern float msigrl_(IRAFPointer *, float *, float *, int *);
float c_msigrl(IRAFPointer *msi, float *x, float *y, int *npts) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = msigrl_(msi, x, y, npts);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define iifins_ iifins
# endif
	extern void iifins_(float *, float *, int *, float *, float *, float *, float *, int *, int *);
void c_ii_find_limits(float *x, float *y, int *npts, float *x1lim, float *x2lim, float *ymin, float *ymax, int *nylmin, int *nylmax) {
	clear_cvoserr();
	xerpsh_();
	iifins_(x, y, npts, x1lim, x2lim, ymin, ymax, nylmin, nylmax);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define iipycp_ iipycp
# endif
	extern int iipycp_(float *, float *, float *, float *, int *, float *, float *);
int c_ii_pyclip(float *xver, float *yver, float *xintr, float *yintr, int *nver, float *lx, float *ld) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = iipycp_(xver, yver, xintr, yintr, nver, lx, ld);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define iialix_ iialix
# endif
	extern void iialix_(float *, int *, float *, int *, float *, int *);
void c_ii_alimrix(float *x, int *npts, float *xmin, int *nxmin, float *xmax, int *nxmax) {
	clear_cvoserr();
	xerpsh_();
	iialix_(x, npts, xmin, nxmin, xmax, nxmax);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define msiint_ msiint
# endif
	extern void msiint_(IRAFPointer *, int *);
void c_msiinit(IRAFPointer *msi, int *interp_type) {
	clear_cvoserr();
	xerpsh_();
	msiint_(msi, interp_type);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define msiree_ msiree
# endif
	extern void msiree_(IRAFPointer *, float *);
void c_msirestore(IRAFPointer *msi, float *interpolant) {
	clear_cvoserr();
	xerpsh_();
	msiree_(msi, interpolant);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define msisae_ msisae
# endif
	extern void msisae_(IRAFPointer *, float *);
void c_msisave(IRAFPointer *msi, float *interpolant) {
	clear_cvoserr();
	xerpsh_();
	msisae_(msi, interpolant);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define msisql_ msisql
# endif
	extern float msisql_(IRAFPointer *, float *, float *, float *, float *);
float c_msisqgrl(IRAFPointer *msi, float *x1, float *x2, float *y1, float *y2) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = msisql_(msi, x1, x2, y1, y2);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define msiver_ msiver
# endif
	extern void msiver_(IRAFPointer *, float *, float *, float *, int *);
void c_msivector(IRAFPointer *msi, float *x, float *y, float *zfit, int *npts) {
	clear_cvoserr();
	xerpsh_();
	msiver_(msi, x, y, zfit, npts);
	if (xerpoi_())
	    set_cvoserr();
}

