# include <xmwcs.h>
# include <c_iraf_priv.h>
# include <stdlib.h>
# if defined(NO_UNDERSCORE)
# define mwopen_ mwopen
# endif
	extern IRAFPointer mwopen_(IRAFPointer *, int *);
IRAFPointer c_mw_open(IRAFPointer bufptr, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwopen_(&bufptr, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwopem_ mwopem
# endif
	extern IRAFPointer mwopem_(IRAFPointer *);
IRAFPointer c_mw_openim(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwopem_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwnewy_ mwnewy
# endif
	extern IRAFPointer mwnewy_(IRAFPointer *);
IRAFPointer c_mw_newcopy(IRAFPointer o_mw) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwnewy_(&o_mw);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwcloe_ mwcloe
# endif
	extern void mwcloe_(IRAFPointer *);
void c_mw_close(IRAFPointer *mw) {
	clear_cvoserr();
	xerpsh_();
	mwcloe_(mw);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwload_ mwload
# endif
	extern void mwload_(IRAFPointer *, IRAFPointer *);
void c_mw_load(IRAFPointer mw, IRAFPointer bp) {
	clear_cvoserr();
	xerpsh_();
	mwload_(&mw, &bp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwsave_ mwsave
# endif
	extern int mwsave_(IRAFPointer *, IRAFPointer *, int *);
int c_mw_save(IRAFPointer o_mw, IRAFPointer *bp, int *buflen) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwsave_(&o_mw, bp, buflen);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwloam_ mwloam
# endif
	extern void mwloam_(IRAFPointer *, IRAFPointer *);
void c_mw_loadim(IRAFPointer mw, IRAFPointer im) {
	clear_cvoserr();
	xerpsh_();
	mwloam_(&mw, &im);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwsavm_ mwsavm
# endif
	extern void mwsavm_(IRAFPointer *, IRAFPointer *);
void c_mw_saveim(IRAFPointer mw, IRAFPointer im) {
	clear_cvoserr();
	xerpsh_();
	mwsavm_(&mw, &im);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwsctn_ mwsctn
# endif
	extern IRAFPointer mwsctn_(IRAFPointer *, short *, short *, int *);
IRAFPointer c_mw_sctran(IRAFPointer mw, char *system1, char *system2, int axbits) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwsctn_(&mw, char2iraf(system1,1), char2iraf(system2,2), &axbits);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwgctd_ mwgctd
# endif
	extern int mwgctd_(IRAFPointer *, double *, double *, int *, int *, int *);
int c_mw_gctrand(IRAFPointer a_ct, double *o_ltm, double *o_ltv, int *axtype1, int *axtype2, int maxdim) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwgctd_(&a_ct, o_ltm, o_ltv, axtype1, axtype2, &maxdim);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwgctr_ mwgctr
# endif
	extern int mwgctr_(IRAFPointer *, float *, float *, int *, int *, int *);
int c_mw_gctranr(IRAFPointer a_ct, float *o_ltm, float *o_ltv, int *axtype1, int *axtype2, int maxdim) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwgctr_(&a_ct, o_ltm, o_ltv, axtype1, axtype2, &maxdim);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwctfe_ mwctfe
# endif
	extern void mwctfe_(IRAFPointer *);
void c_mw_ctfree(IRAFPointer *ct) {
	clear_cvoserr();
	xerpsh_();
	mwctfe_(ct);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwc1td_ mwc1td
# endif
	extern double mwc1td_(IRAFPointer *, double *);
double c_mw_c1trand(IRAFPointer a_ct, double x) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwc1td_(&a_ct, &x);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwc1tr_ mwc1tr
# endif
	extern float mwc1tr_(IRAFPointer *, float *);
float c_mw_c1tranr(IRAFPointer a_ct, float x) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwc1tr_(&a_ct, &x);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwv1td_ mwv1td
# endif
	extern void mwv1td_(IRAFPointer *, double *, double *, int *);
void c_mw_v1trand(IRAFPointer a_ct, double *x1, double *x2, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwv1td_(&a_ct, x1, x2, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwv1tr_ mwv1tr
# endif
	extern void mwv1tr_(IRAFPointer *, float *, float *, int *);
void c_mw_v1tranr(IRAFPointer a_ct, float *x1, float *x2, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwv1tr_(&a_ct, x1, x2, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwc2td_ mwc2td
# endif
	extern void mwc2td_(IRAFPointer *, double *, double *, double *, double *);
void c_mw_c2trand(IRAFPointer a_ct, double x1, double y1, double *x2, double *y2) {
	clear_cvoserr();
	xerpsh_();
	mwc2td_(&a_ct, &x1, &y1, x2, y2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwc2tr_ mwc2tr
# endif
	extern void mwc2tr_(IRAFPointer *, float *, float *, float *, float *);
void c_mw_c2tranr(IRAFPointer a_ct, float x1, float y1, float *x2, float *y2) {
	clear_cvoserr();
	xerpsh_();
	mwc2tr_(&a_ct, &x1, &y1, x2, y2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwv2td_ mwv2td
# endif
	extern void mwv2td_(IRAFPointer *, double *, double *, double *, double *, int *);
void c_mw_v2trand(IRAFPointer a_ct, double *x1, double *y1, double *x2, double *y2, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwv2td_(&a_ct, x1, y1, x2, y2, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwv2tr_ mwv2tr
# endif
	extern void mwv2tr_(IRAFPointer *, float *, float *, float *, float *, int *);
void c_mw_v2tranr(IRAFPointer a_ct, float *x1, float *y1, float *x2, float *y2, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwv2tr_(&a_ct, x1, y1, x2, y2, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwctrd_ mwctrd
# endif
	extern void mwctrd_(IRAFPointer *, double *, double *, int *);
void c_mw_ctrand(IRAFPointer a_ct, double *p1, double *p2, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwctrd_(&a_ct, p1, p2, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwctrr_ mwctrr
# endif
	extern void mwctrr_(IRAFPointer *, float *, float *, int *);
void c_mw_ctranr(IRAFPointer a_ct, float *p1, float *p2, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwctrr_(&a_ct, p1, p2, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwvtrd_ mwvtrd
# endif
	extern void mwvtrd_(IRAFPointer *, double *, double *, int *, int *);
void c_mw_vtrand(IRAFPointer ct, double *v1, double *v2, int ndim, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwvtrd_(&ct, v1, v2, &ndim, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwvtrr_ mwvtrr
# endif
	extern void mwvtrr_(IRAFPointer *, float *, float *, int *, int *);
void c_mw_vtranr(IRAFPointer ct, float *v1, float *v2, int ndim, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwvtrr_(&ct, v1, v2, &ndim, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwsltd_ mwsltd
# endif
	extern void mwsltd_(IRAFPointer *, double *, double *, int *);
void c_mw_sltermd(IRAFPointer mw, double *ltm, double *ltv, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwsltd_(&mw, ltm, ltv, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwsltr_ mwsltr
# endif
	extern void mwsltr_(IRAFPointer *, float *, float *, int *);
void c_mw_sltermr(IRAFPointer mw, float *ltm, float *ltv, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwsltr_(&mw, ltm, ltv, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgltd_ mwgltd
# endif
	extern void mwgltd_(IRAFPointer *, double *, double *, int *);
void c_mw_gltermd(IRAFPointer mw, double *ltm, double *ltv, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwgltd_(&mw, ltm, ltv, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgltr_ mwgltr
# endif
	extern void mwgltr_(IRAFPointer *, float *, float *, int *);
void c_mw_gltermr(IRAFPointer mw, float *ltm, float *ltv, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwgltr_(&mw, ltm, ltv, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwtrad_ mwtrad
# endif
	extern void mwtrad_(IRAFPointer *, double *, double *, double *, int *);
void c_mw_translated(IRAFPointer mw, double *ltv_1, double *ltm, double *ltv_2, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwtrad_(&mw, ltv_1, ltm, ltv_2, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwtrar_ mwtrar
# endif
	extern void mwtrar_(IRAFPointer *, float *, float *, float *, int *);
void c_mw_translater(IRAFPointer mw, float *ltv_1, float *ltm, float *ltv_2, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwtrar_(&mw, ltv_1, ltm, ltv_2, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwrote_ mwrote
# endif
	extern void mwrote_(IRAFPointer *, float *, float *, int *);
void c_mw_rotate(IRAFPointer mw, float theta, float *center, int axbits) {
	clear_cvoserr();
	xerpsh_();
	mwrote_(&mw, &theta, center, &axbits);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwscae_ mwscae
# endif
	extern void mwscae_(IRAFPointer *, float *, int *);
void c_mw_scale(IRAFPointer mw, float *scale, int axbits) {
	clear_cvoserr();
	xerpsh_();
	mwscae_(&mw, scale, &axbits);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwshit_ mwshit
# endif
	extern void mwshit_(IRAFPointer *, float *, int *);
void c_mw_shift(IRAFPointer mw, float *shift, int axbits) {
	clear_cvoserr();
	xerpsh_();
	mwshit_(&mw, shift, &axbits);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwnewm_ mwnewm
# endif
	extern void mwnewm_(IRAFPointer *, short *, int *);
void c_mw_newsystem(IRAFPointer mw, char *system, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwnewm_(&mw, char2iraf(system,1), &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgsym_ mwgsym
# endif
	extern void mwgsym_(IRAFPointer *, short *, int *);
void c_mw_gsystem(IRAFPointer mw, char *outstr, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	mwgsym_(&mw, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define mwssym_ mwssym
# endif
	extern void mwssym_(IRAFPointer *, short *);
void c_mw_ssystem(IRAFPointer mw, char *system) {
	clear_cvoserr();
	xerpsh_();
	mwssym_(&mw, char2iraf(system,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgaxp_ mwgaxp
# endif
	extern void mwgaxp_(IRAFPointer *, int *, int *, int *);
void c_mw_gaxmap(IRAFPointer mw, int *axno, int *axval, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwgaxp_(&mw, axno, axval, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwsaxp_ mwsaxp
# endif
	extern void mwsaxp_(IRAFPointer *, int *, int *, int *);
void c_mw_saxmap(IRAFPointer mw, int *axno, int *axval, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwsaxp_(&mw, axno, axval, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgwtd_ mwgwtd
# endif
	extern void mwgwtd_(IRAFPointer *, double *, double *, double *, int *);
void c_mw_gwtermd(IRAFPointer mw, double *r, double *w, double *cd, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwgwtd_(&mw, r, w, cd, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgwtr_ mwgwtr
# endif
	extern void mwgwtr_(IRAFPointer *, float *, float *, float *, int *);
void c_mw_gwtermr(IRAFPointer mw, float *r, float *w, float *cd, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwgwtr_(&mw, r, w, cd, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwswtd_ mwswtd
# endif
	extern void mwswtd_(IRAFPointer *, double *, double *, double *, int *);
void c_mw_swtermd(IRAFPointer mw, double *r, double *w, double *cd, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwswtd_(&mw, r, w, cd, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwswtr_ mwswtr
# endif
	extern void mwswtr_(IRAFPointer *, float *, float *, float *, int *);
void c_mw_swtermr(IRAFPointer mw, float *r, float *w, float *cd, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwswtr_(&mw, r, w, cd, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwswte_ mwswte
# endif
	extern void mwswte_(IRAFPointer *, int *, int *, short *, short *);
void c_mw_swtype(IRAFPointer mw, int *axis, int naxes, char *wtype, char *wattr) {
	clear_cvoserr();
	xerpsh_();
	mwswte_(&mw, axis, &naxes, char2iraf(wtype,1), char2iraf(wattr,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgwsd_ mwgwsd
# endif
	extern void mwgwsd_(IRAFPointer *, int *, double *, double *, int *);
void c_mw_gwsampd(IRAFPointer mw, int axis, double *pv, double *wv, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwgwsd_(&mw, &axis, pv, wv, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgwsr_ mwgwsr
# endif
	extern void mwgwsr_(IRAFPointer *, int *, float *, float *, int *);
void c_mw_gwsampr(IRAFPointer mw, int axis, float *pv, float *wv, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwgwsr_(&mw, &axis, pv, wv, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwswsd_ mwswsd
# endif
	extern void mwswsd_(IRAFPointer *, int *, double *, double *, int *);
void c_mw_swsampd(IRAFPointer mw, int axis, double *pv, double *wv, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwswsd_(&mw, &axis, pv, wv, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwswsr_ mwswsr
# endif
	extern void mwswsr_(IRAFPointer *, int *, float *, float *, int *);
void c_mw_swsampr(IRAFPointer mw, int axis, float *pv, float *wv, int npts) {
	clear_cvoserr();
	xerpsh_();
	mwswsr_(&mw, &axis, pv, wv, &npts);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwgwas_ mwgwas
# endif
	extern void mwgwas_(IRAFPointer *, int *, short *, short *, int *);
void c_mw_gwattrs(IRAFPointer mw, int axis, char *attribute, char *valstr, int maxch) {
	CH2I_chk_buffer(1,SZ_ATNAME);
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	mwgwas_(&mw, &axis, char2iraf(attribute,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(attribute,SZ_ATNAME,1);
	iraf2char(valstr,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define mwswas_ mwswas
# endif
	extern void mwswas_(IRAFPointer *, int *, short *, short *);
void c_mw_swattrs(IRAFPointer mw, int axis, char *attribute, char *valstr) {
	clear_cvoserr();
	xerpsh_();
	mwswas_(&mw, &axis, char2iraf(attribute,1), char2iraf(valstr,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwinvd_ mwinvd
# endif
	extern void mwinvd_(double *, double *, int *);
void c_mw_invertd(double *o_ltm, double *n_ltm, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwinvd_(o_ltm, n_ltm, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwinvr_ mwinvr
# endif
	extern void mwinvr_(float *, float *, int *);
void c_mw_invertr(float *o_ltm, float *n_ltm, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwinvr_(o_ltm, n_ltm, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwmmud_ mwmmud
# endif
	extern void mwmmud_(double *, double *, double *, int *);
void c_mw_mmuld(double *a, double *b, double *c, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwmmud_(a, b, c, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwmmur_ mwmmur
# endif
	extern void mwmmur_(float *, float *, float *, int *);
void c_mw_mmulr(float *a, float *b, float *c, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwmmur_(a, b, c, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwvmud_ mwvmud
# endif
	extern void mwvmud_(double *, double *, double *, int *);
void c_mw_vmuld(double *a, double *b, double *c, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwvmud_(a, b, c, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwvmur_ mwvmur
# endif
	extern void mwvmur_(float *, float *, float *, int *);
void c_mw_vmulr(float *a, float *b, float *c, int ndim) {
	clear_cvoserr();
	xerpsh_();
	mwvmur_(a, b, c, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwseti_ mwseti
# endif
	extern void mwseti_(IRAFPointer *, int *, int *);
void c_mw_seti(IRAFPointer mw, int param, int value) {
	clear_cvoserr();
	xerpsh_();
	mwseti_(&mw, &param, &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define mwstai_ mwstai
# endif
	extern int mwstai_(IRAFPointer *, int *);
int c_mw_stati(IRAFPointer mw, int param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = mwstai_(&mw, &param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define mwshow_ mwshow
# endif
	extern void mwshow_(IRAFPointer *, int *, int *);
void c_mw_show(IRAFPointer mw, int fd, int what) {
	clear_cvoserr();
	xerpsh_();
	mwshow_(&mw, &fd, &what);
	if (xerpoi_())
	    set_cvoserr();
}

