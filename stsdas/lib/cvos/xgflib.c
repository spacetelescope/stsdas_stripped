# include <xgflib.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define gfaddr_ gfaddr
# endif
	extern void gfaddr_(IRAFPointer *, short *, int *, int *, short *, short *);
void c_gf_addpar(IRAFPointer im, char *pname, int dtype, int plen, char *pval, char *pcomm) {
	clear_cvoserr();
	xerpsh_();
	gfaddr_(&im, char2iraf(pname,1), &dtype, &plen, char2iraf(pval,2), char2iraf(pcomm,3));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gfdelr_ gfdelr
# endif
	extern void gfdelr_(IRAFPointer *, short *);
void c_gf_delpar(IRAFPointer im, char *pname) {
	clear_cvoserr();
	xerpsh_();
	gfdelr_(&im, char2iraf(pname,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gfgcot_ gfgcot
# endif
	extern int gfgcot_(IRAFPointer *);
int c_gf_gcount(IRAFPointer im) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gfgcot_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gfgeis_ gfgeis
# endif
	extern Bool gfgeis_(IRAFPointer *);
Bool c_gf_geis(IRAFPointer im) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gfgeis_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gfgfid_ gfgfid
# endif
	extern int gfgfid_(IRAFPointer *, short *);
int c_gf_gfind(IRAFPointer im, char *keyword) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gfgfid_(&im, char2iraf(keyword,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gfgstl_ gfgstl
# endif
	extern int gfgstl_(IRAFPointer *, short *);
int c_gf_gstfval(IRAFPointer im, char *keyword) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gfgstl_(&im, char2iraf(keyword,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gfmap_ gfmap
# endif
	extern IRAFPointer gfmap_(short *, int *, IRAFPointer *);
IRAFPointer c_gf_map(char *image, int acmode, IRAFPointer oldim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = gfmap_(char2iraf(image,1), &acmode, &oldim);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define gfoper_ gfoper
# endif
	extern void gfoper_(IRAFPointer *, int *, float *, float *, IRAFPointer *);
void c_gf_opengr(IRAFPointer *im, int gn, float *datamin, float *datamax, IRAFPointer imt) {
	clear_cvoserr();
	xerpsh_();
	gfoper_(im, &gn, datamin, datamax, &imt);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define gfpstl_ gfpstl
# endif
	extern void gfpstl_(IRAFPointer *, short *, int *);
void c_gf_pstfval(IRAFPointer im, char *keyword, int value) {
	clear_cvoserr();
	xerpsh_();
	gfpstl_(&im, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

