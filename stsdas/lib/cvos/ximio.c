# include <ximio.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define immap_ immap
# endif
	extern IRAFPointer immap_(short *, int *, IRAFPointer *);
IRAFPointer c_immap(char *imspec, int acmode, IRAFPointer hdr_arg) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = immap_(char2iraf(imspec,1), &acmode, &hdr_arg);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imunmp_ imunmp
# endif
	extern void imunmp_(IRAFPointer *);
void c_imunmap(IRAFPointer im) {
	clear_cvoserr();
	xerpsh_();
	imunmp_(&im);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imgl1d_ imgl1d
# endif
	extern IRAFPointer imgl1d_(IRAFPointer *);
double * c_imgl1d(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl1d_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl1i_ imgl1i
# endif
	extern IRAFPointer imgl1i_(IRAFPointer *);
int * c_imgl1i(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl1i_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl1l_ imgl1l
# endif
	extern IRAFPointer imgl1l_(IRAFPointer *);
int * c_imgl1l(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl1l_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl1r_ imgl1r
# endif
	extern IRAFPointer imgl1r_(IRAFPointer *);
float * c_imgl1r(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl1r_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl1s_ imgl1s
# endif
	extern IRAFPointer imgl1s_(IRAFPointer *);
short * c_imgl1s(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl1s_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl1x_ imgl1x
# endif
	extern IRAFPointer imgl1x_(IRAFPointer *);
Complex * c_imgl1x(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl1x_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl2d_ imgl2d
# endif
	extern IRAFPointer imgl2d_(IRAFPointer *, int *);
double * c_imgl2d(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl2d_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl2i_ imgl2i
# endif
	extern IRAFPointer imgl2i_(IRAFPointer *, int *);
int * c_imgl2i(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl2i_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl2l_ imgl2l
# endif
	extern IRAFPointer imgl2l_(IRAFPointer *, int *);
int * c_imgl2l(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl2l_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl2r_ imgl2r
# endif
	extern IRAFPointer imgl2r_(IRAFPointer *, int *);
float * c_imgl2r(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl2r_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl2s_ imgl2s
# endif
	extern IRAFPointer imgl2s_(IRAFPointer *, int *);
short * c_imgl2s(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl2s_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl2x_ imgl2x
# endif
	extern IRAFPointer imgl2x_(IRAFPointer *, int *);
Complex * c_imgl2x(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl2x_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl3d_ imgl3d
# endif
	extern IRAFPointer imgl3d_(IRAFPointer *, int *, int *);
double * c_imgl3d(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl3d_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl3i_ imgl3i
# endif
	extern IRAFPointer imgl3i_(IRAFPointer *, int *, int *);
int * c_imgl3i(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl3i_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl3l_ imgl3l
# endif
	extern IRAFPointer imgl3l_(IRAFPointer *, int *, int *);
int * c_imgl3l(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl3l_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl3r_ imgl3r
# endif
	extern IRAFPointer imgl3r_(IRAFPointer *, int *, int *);
float * c_imgl3r(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl3r_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl3s_ imgl3s
# endif
	extern IRAFPointer imgl3s_(IRAFPointer *, int *, int *);
short * c_imgl3s(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl3s_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgl3x_ imgl3x
# endif
	extern IRAFPointer imgl3x_(IRAFPointer *, int *, int *);
Complex * c_imgl3x(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgl3x_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl1d_ impl1d
# endif
	extern IRAFPointer impl1d_(IRAFPointer *);
double * c_impl1d(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl1d_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl1i_ impl1i
# endif
	extern IRAFPointer impl1i_(IRAFPointer *);
int * c_impl1i(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl1i_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl1l_ impl1l
# endif
	extern IRAFPointer impl1l_(IRAFPointer *);
int * c_impl1l(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl1l_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl1r_ impl1r
# endif
	extern IRAFPointer impl1r_(IRAFPointer *);
float * c_impl1r(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl1r_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl1s_ impl1s
# endif
	extern IRAFPointer impl1s_(IRAFPointer *);
short * c_impl1s(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl1s_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl1x_ impl1x
# endif
	extern IRAFPointer impl1x_(IRAFPointer *);
Complex * c_impl1x(IRAFPointer im) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl1x_(&im);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl2d_ impl2d
# endif
	extern IRAFPointer impl2d_(IRAFPointer *, int *);
double * c_impl2d(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl2d_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl2i_ impl2i
# endif
	extern IRAFPointer impl2i_(IRAFPointer *, int *);
int * c_impl2i(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl2i_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl2l_ impl2l
# endif
	extern IRAFPointer impl2l_(IRAFPointer *, int *);
int * c_impl2l(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl2l_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl2r_ impl2r
# endif
	extern IRAFPointer impl2r_(IRAFPointer *, int *);
float * c_impl2r(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl2r_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl2s_ impl2s
# endif
	extern IRAFPointer impl2s_(IRAFPointer *, int *);
short * c_impl2s(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl2s_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl2x_ impl2x
# endif
	extern IRAFPointer impl2x_(IRAFPointer *, int *);
Complex * c_impl2x(IRAFPointer im, int linenum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl2x_(&im, &linenum);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl3d_ impl3d
# endif
	extern IRAFPointer impl3d_(IRAFPointer *, int *, int *);
double * c_impl3d(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl3d_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl3i_ impl3i
# endif
	extern IRAFPointer impl3i_(IRAFPointer *, int *, int *);
int * c_impl3i(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl3i_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl3l_ impl3l
# endif
	extern IRAFPointer impl3l_(IRAFPointer *, int *, int *);
int * c_impl3l(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl3l_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl3r_ impl3r
# endif
	extern IRAFPointer impl3r_(IRAFPointer *, int *, int *);
float * c_impl3r(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl3r_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl3s_ impl3s
# endif
	extern IRAFPointer impl3s_(IRAFPointer *, int *, int *);
short * c_impl3s(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl3s_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impl3x_ impl3x
# endif
	extern IRAFPointer impl3x_(IRAFPointer *, int *, int *);
Complex * c_impl3x(IRAFPointer im, int line, int band) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impl3x_(&im, &line, &band);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgnld_ imgnld
# endif
	extern int imgnld_(IRAFPointer *, IRAFPointer *, int *);
int c_imgnld(IRAFPointer imdes, double **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = imgnld_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (double *)((double *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgnli_ imgnli
# endif
	extern int imgnli_(IRAFPointer *, IRAFPointer *, int *);
int c_imgnli(IRAFPointer imdes, int **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = imgnli_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (int *)((int *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgnll_ imgnll
# endif
	extern int imgnll_(IRAFPointer *, IRAFPointer *, int *);
int c_imgnll(IRAFPointer imdes, int **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = imgnll_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (int *)((int *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgnlr_ imgnlr
# endif
	extern int imgnlr_(IRAFPointer *, IRAFPointer *, int *);
int c_imgnlr(IRAFPointer imdes, float **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = imgnlr_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (float *)((float *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgnls_ imgnls
# endif
	extern int imgnls_(IRAFPointer *, IRAFPointer *, int *);
int c_imgnls(IRAFPointer imdes, short **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = imgnls_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (short *)((short *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgnlx_ imgnlx
# endif
	extern int imgnlx_(IRAFPointer *, IRAFPointer *, int *);
int c_imgnlx(IRAFPointer imdes, Complex **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = imgnlx_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (Complex *)((Complex *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define impnld_ impnld
# endif
	extern int impnld_(IRAFPointer *, IRAFPointer *, int *);
int c_impnld(IRAFPointer imdes, double **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = impnld_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (double *)((double *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define impnli_ impnli
# endif
	extern int impnli_(IRAFPointer *, IRAFPointer *, int *);
int c_impnli(IRAFPointer imdes, int **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = impnli_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (int *)((int *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define impnll_ impnll
# endif
	extern int impnll_(IRAFPointer *, IRAFPointer *, int *);
int c_impnll(IRAFPointer imdes, int **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = impnll_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (int *)((int *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define impnlr_ impnlr
# endif
	extern int impnlr_(IRAFPointer *, IRAFPointer *, int *);
int c_impnlr(IRAFPointer imdes, float **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = impnlr_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (float *)((float *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define impnls_ impnls
# endif
	extern int impnls_(IRAFPointer *, IRAFPointer *, int *);
int c_impnls(IRAFPointer imdes, short **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = impnls_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (short *)((short *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define impnlx_ impnlx
# endif
	extern int impnlx_(IRAFPointer *, IRAFPointer *, int *);
int c_impnlx(IRAFPointer imdes, Complex **lineptr, int *v) {
	int rtn;
	IRAFPointer IRAF_lineptr;
	IRAF_lineptr = 0;
	clear_cvoserr();
	xerpsh_();
	rtn = impnlx_(&imdes, &IRAF_lineptr, v);
	if (xerpoi_())
	    set_cvoserr();
	*lineptr = (Complex *)((Complex *)mem_ + IRAF_lineptr - 1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgs1d_ imgs1d
# endif
	extern IRAFPointer imgs1d_(IRAFPointer *, int *, int *);
double * c_imgs1d(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs1d_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs1i_ imgs1i
# endif
	extern IRAFPointer imgs1i_(IRAFPointer *, int *, int *);
int * c_imgs1i(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs1i_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs1l_ imgs1l
# endif
	extern IRAFPointer imgs1l_(IRAFPointer *, int *, int *);
int * c_imgs1l(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs1l_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs1r_ imgs1r
# endif
	extern IRAFPointer imgs1r_(IRAFPointer *, int *, int *);
float * c_imgs1r(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs1r_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs1s_ imgs1s
# endif
	extern IRAFPointer imgs1s_(IRAFPointer *, int *, int *);
short * c_imgs1s(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs1s_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs1x_ imgs1x
# endif
	extern IRAFPointer imgs1x_(IRAFPointer *, int *, int *);
Complex * c_imgs1x(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs1x_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs2d_ imgs2d
# endif
	extern IRAFPointer imgs2d_(IRAFPointer *, int *, int *, int *, int *);
double * c_imgs2d(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs2d_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs2i_ imgs2i
# endif
	extern IRAFPointer imgs2i_(IRAFPointer *, int *, int *, int *, int *);
int * c_imgs2i(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs2i_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs2l_ imgs2l
# endif
	extern IRAFPointer imgs2l_(IRAFPointer *, int *, int *, int *, int *);
int * c_imgs2l(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs2l_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs2r_ imgs2r
# endif
	extern IRAFPointer imgs2r_(IRAFPointer *, int *, int *, int *, int *);
float * c_imgs2r(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs2r_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs2s_ imgs2s
# endif
	extern IRAFPointer imgs2s_(IRAFPointer *, int *, int *, int *, int *);
short * c_imgs2s(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs2s_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs2x_ imgs2x
# endif
	extern IRAFPointer imgs2x_(IRAFPointer *, int *, int *, int *, int *);
Complex * c_imgs2x(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs2x_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs3d_ imgs3d
# endif
	extern IRAFPointer imgs3d_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
double * c_imgs3d(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs3d_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs3i_ imgs3i
# endif
	extern IRAFPointer imgs3i_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
int * c_imgs3i(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs3i_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs3l_ imgs3l
# endif
	extern IRAFPointer imgs3l_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
int * c_imgs3l(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs3l_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs3r_ imgs3r
# endif
	extern IRAFPointer imgs3r_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
float * c_imgs3r(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs3r_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs3s_ imgs3s
# endif
	extern IRAFPointer imgs3s_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
short * c_imgs3s(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs3s_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imgs3x_ imgs3x
# endif
	extern IRAFPointer imgs3x_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
Complex * c_imgs3x(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgs3x_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps1d_ imps1d
# endif
	extern IRAFPointer imps1d_(IRAFPointer *, int *, int *);
double * c_imps1d(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps1d_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps1i_ imps1i
# endif
	extern IRAFPointer imps1i_(IRAFPointer *, int *, int *);
int * c_imps1i(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps1i_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps1l_ imps1l
# endif
	extern IRAFPointer imps1l_(IRAFPointer *, int *, int *);
int * c_imps1l(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps1l_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps1r_ imps1r
# endif
	extern IRAFPointer imps1r_(IRAFPointer *, int *, int *);
float * c_imps1r(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps1r_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps1s_ imps1s
# endif
	extern IRAFPointer imps1s_(IRAFPointer *, int *, int *);
short * c_imps1s(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps1s_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps1x_ imps1x
# endif
	extern IRAFPointer imps1x_(IRAFPointer *, int *, int *);
Complex * c_imps1x(IRAFPointer im, int x1, int x2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps1x_(&im, &x1, &x2);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps2d_ imps2d
# endif
	extern IRAFPointer imps2d_(IRAFPointer *, int *, int *, int *, int *);
double * c_imps2d(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps2d_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps2i_ imps2i
# endif
	extern IRAFPointer imps2i_(IRAFPointer *, int *, int *, int *, int *);
int * c_imps2i(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps2i_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps2l_ imps2l
# endif
	extern IRAFPointer imps2l_(IRAFPointer *, int *, int *, int *, int *);
int * c_imps2l(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps2l_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps2r_ imps2r
# endif
	extern IRAFPointer imps2r_(IRAFPointer *, int *, int *, int *, int *);
float * c_imps2r(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps2r_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps2s_ imps2s
# endif
	extern IRAFPointer imps2s_(IRAFPointer *, int *, int *, int *, int *);
short * c_imps2s(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps2s_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps2x_ imps2x
# endif
	extern IRAFPointer imps2x_(IRAFPointer *, int *, int *, int *, int *);
Complex * c_imps2x(IRAFPointer im, int x1, int x2, int y1, int y2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps2x_(&im, &x1, &x2, &y1, &y2);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps3d_ imps3d
# endif
	extern IRAFPointer imps3d_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
double * c_imps3d(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps3d_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps3i_ imps3i
# endif
	extern IRAFPointer imps3i_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
int * c_imps3i(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps3i_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps3l_ imps3l
# endif
	extern IRAFPointer imps3l_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
int * c_imps3l(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps3l_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps3r_ imps3r
# endif
	extern IRAFPointer imps3r_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
float * c_imps3r(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps3r_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps3s_ imps3s
# endif
	extern IRAFPointer imps3s_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
short * c_imps3s(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps3s_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imps3x_ imps3x
# endif
	extern IRAFPointer imps3x_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
Complex * c_imps3x(IRAFPointer im, int x1, int x2, int y1, int y2, int z1, int z2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imps3x_(&im, &x1, &x2, &y1, &y2, &z1, &z2);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imggsd_ imggsd
# endif
	extern IRAFPointer imggsd_(IRAFPointer *, int *, int *, int *);
double * c_imggsd(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imggsd_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imggsi_ imggsi
# endif
	extern IRAFPointer imggsi_(IRAFPointer *, int *, int *, int *);
int * c_imggsi(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imggsi_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imggsl_ imggsl
# endif
	extern IRAFPointer imggsl_(IRAFPointer *, int *, int *, int *);
int * c_imggsl(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imggsl_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imggsr_ imggsr
# endif
	extern IRAFPointer imggsr_(IRAFPointer *, int *, int *, int *);
float * c_imggsr(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imggsr_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imggss_ imggss
# endif
	extern IRAFPointer imggss_(IRAFPointer *, int *, int *, int *);
short * c_imggss(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imggss_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imggsx_ imggsx
# endif
	extern IRAFPointer imggsx_(IRAFPointer *, int *, int *, int *);
Complex * c_imggsx(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imggsx_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impgsd_ impgsd
# endif
	extern IRAFPointer impgsd_(IRAFPointer *, int *, int *, int *);
double * c_impgsd(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impgsd_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (double *)((double *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impgsi_ impgsi
# endif
	extern IRAFPointer impgsi_(IRAFPointer *, int *, int *, int *);
int * c_impgsi(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impgsi_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impgsl_ impgsl
# endif
	extern IRAFPointer impgsl_(IRAFPointer *, int *, int *, int *);
int * c_impgsl(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impgsl_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (int *)((int *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impgsr_ impgsr
# endif
	extern IRAFPointer impgsr_(IRAFPointer *, int *, int *, int *);
float * c_impgsr(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impgsr_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (float *)((float *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impgss_ impgss
# endif
	extern IRAFPointer impgss_(IRAFPointer *, int *, int *, int *);
short * c_impgss(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impgss_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define impgsx_ impgsx
# endif
	extern IRAFPointer impgsx_(IRAFPointer *, int *, int *, int *);
Complex * c_impgsx(IRAFPointer imdes, int *vs, int *ve, int ndim) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = impgsx_(&imdes, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
	return (Complex *)((Complex *)mem_ + rtn - 1);
}

# if defined(NO_UNDERSCORE)
# define imfluh_ imfluh
# endif
	extern void imfluh_(IRAFPointer *);
void c_imflush(IRAFPointer imdes) {
	clear_cvoserr();
	xerpsh_();
	imfluh_(&imdes);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imaccs_ imaccs
# endif
	extern int imaccs_(short *, int *);
int c_ximaccess(char *image, int acmode) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imaccs_(char2iraf(image,1), &acmode);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imcopy_ imcopy
# endif
	extern void imcopy_(short *, short *);
void c_imcopy(char *old, char *newname) {
	clear_cvoserr();
	xerpsh_();
	imcopy_(char2iraf(old,1), char2iraf(newname,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imdele_ imdele
# endif
	extern void imdele_(short *);
void c_imdelete(char *image) {
	clear_cvoserr();
	xerpsh_();
	imdele_(char2iraf(image,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imrene_ imrene
# endif
	extern void imrene_(short *, short *);
void c_imrename(char *old, char *newname) {
	clear_cvoserr();
	xerpsh_();
	imrene_(char2iraf(old,1), char2iraf(newname,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imgsen_ imgsen
# endif
	extern void imgsen_(short *, short *, int *);
void c_imgsection(char *imspec, char *section, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	imgsen_(char2iraf(imspec,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(section,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define imgime_ imgime
# endif
	extern void imgime_(short *, short *, int *);
void c_imgimage(char *imspec, char *image, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	imgime_(char2iraf(imspec,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(image,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define imgclr_ imgclr
# endif
	extern void imgclr_(short *, short *, int *);
void c_imgcluster(char *imspec, char *cluster, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	imgclr_(char2iraf(imspec,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(cluster,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define imgetb_ imgetb
# endif
	extern Bool imgetb_(IRAFPointer *, short *);
Bool c_imgetb(IRAFPointer im, char *key) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgetb_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgetc_ imgetc
# endif
	extern short imgetc_(IRAFPointer *, short *);
char c_imgetc(IRAFPointer im, char *key) {
	char rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = (char)imgetc_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgetd_ imgetd
# endif
	extern double imgetd_(IRAFPointer *, short *);
double c_imgetd(IRAFPointer im, char *key) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgetd_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgeti_ imgeti
# endif
	extern int imgeti_(IRAFPointer *, short *);
int c_imgeti(IRAFPointer im, char *key) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgeti_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgetl_ imgetl
# endif
	extern int imgetl_(IRAFPointer *, short *);
int c_imgetl(IRAFPointer im, char *key) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgetl_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgetr_ imgetr
# endif
	extern float imgetr_(IRAFPointer *, short *);
float c_imgetr(IRAFPointer im, char *key) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgetr_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgets_ imgets
# endif
	extern short imgets_(IRAFPointer *, short *);
short c_imgets(IRAFPointer im, char *key) {
	short rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgets_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgstr_ imgstr
# endif
	extern void imgstr_(IRAFPointer *, short *, short *, int *);
void c_imgstr(IRAFPointer im, char *key, char *outstr, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	imgstr_(&im, char2iraf(key,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define imputb_ imputb
# endif
	extern void imputb_(IRAFPointer *, short *, Bool *);
void c_imputb(IRAFPointer im, char *key, Bool bval) {
	clear_cvoserr();
	xerpsh_();
	imputb_(&im, char2iraf(key,1), &bval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imputd_ imputd
# endif
	extern void imputd_(IRAFPointer *, short *, double *);
void c_imputd(IRAFPointer im, char *key, double dval) {
	clear_cvoserr();
	xerpsh_();
	imputd_(&im, char2iraf(key,1), &dval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imputh_ imputh
# endif
	extern void imputh_(IRAFPointer *, short *, short *);
void c_imputh(IRAFPointer im, char *key, char *text) {
	clear_cvoserr();
	xerpsh_();
	imputh_(&im, char2iraf(key,1), char2iraf(text,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imputi_ imputi
# endif
	extern void imputi_(IRAFPointer *, short *, int *);
void c_imputi(IRAFPointer im, char *key, int ival) {
	clear_cvoserr();
	xerpsh_();
	imputi_(&im, char2iraf(key,1), &ival);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imputl_ imputl
# endif
	extern void imputl_(IRAFPointer *, short *, int *);
void c_imputl(IRAFPointer im, char *key, int lval) {
	clear_cvoserr();
	xerpsh_();
	imputl_(&im, char2iraf(key,1), &lval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imputr_ imputr
# endif
	extern void imputr_(IRAFPointer *, short *, float *);
void c_imputr(IRAFPointer im, char *key, float rval) {
	clear_cvoserr();
	xerpsh_();
	imputr_(&im, char2iraf(key,1), &rval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imputs_ imputs
# endif
	extern void imputs_(IRAFPointer *, short *, short *);
void c_imputs(IRAFPointer im, char *key, short value) {
	clear_cvoserr();
	xerpsh_();
	imputs_(&im, char2iraf(key,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define impstr_ impstr
# endif
	extern void impstr_(IRAFPointer *, short *, short *);
void c_impstr(IRAFPointer im, char *key, char *value) {
	clear_cvoserr();
	xerpsh_();
	impstr_(&im, char2iraf(key,1), char2iraf(value,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imaddb_ imaddb
# endif
	extern void imaddb_(IRAFPointer *, short *, Bool *);
void c_imaddb(IRAFPointer im, char *key, Bool value) {
	clear_cvoserr();
	xerpsh_();
	imaddb_(&im, char2iraf(key,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imaddd_ imaddd
# endif
	extern void imaddd_(IRAFPointer *, short *, double *);
void c_imaddd(IRAFPointer im, char *key, double value) {
	clear_cvoserr();
	xerpsh_();
	imaddd_(&im, char2iraf(key,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imaddi_ imaddi
# endif
	extern void imaddi_(IRAFPointer *, short *, int *);
void c_imaddi(IRAFPointer im, char *key, int value) {
	clear_cvoserr();
	xerpsh_();
	imaddi_(&im, char2iraf(key,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imaddl_ imaddl
# endif
	extern void imaddl_(IRAFPointer *, short *, int *);
void c_imaddl(IRAFPointer im, char *key, int value) {
	clear_cvoserr();
	xerpsh_();
	imaddl_(&im, char2iraf(key,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imaddr_ imaddr
# endif
	extern void imaddr_(IRAFPointer *, short *, float *);
void c_imaddr(IRAFPointer im, char *key, float value) {
	clear_cvoserr();
	xerpsh_();
	imaddr_(&im, char2iraf(key,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imadds_ imadds
# endif
	extern void imadds_(IRAFPointer *, short *, short *);
void c_imadds(IRAFPointer im, char *key, short value) {
	clear_cvoserr();
	xerpsh_();
	imadds_(&im, char2iraf(key,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imastr_ imastr
# endif
	extern void imastr_(IRAFPointer *, short *, short *);
void c_imastr(IRAFPointer im, char *key, char *value) {
	clear_cvoserr();
	xerpsh_();
	imastr_(&im, char2iraf(key,1), char2iraf(value,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imaddf_ imaddf
# endif
	extern void imaddf_(IRAFPointer *, short *, short *);
void c_imaddf(IRAFPointer im, char *key, char *datatype) {
	clear_cvoserr();
	xerpsh_();
	imaddf_(&im, char2iraf(key,1), char2iraf(datatype,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imdelf_ imdelf
# endif
	extern void imdelf_(IRAFPointer *, short *);
void c_imdelf(IRAFPointer im, char *key) {
	clear_cvoserr();
	xerpsh_();
	imdelf_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imaccf_ imaccf
# endif
	extern int imaccf_(IRAFPointer *, short *);
int c_imaccf(IRAFPointer im, char *key) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imaccf_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgfte_ imgfte
# endif
	extern int imgfte_(IRAFPointer *, short *);
int c_imgftype(IRAFPointer im, char *key) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgfte_(&im, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imofns_ imofns
# endif
	extern IRAFPointer imofns_(IRAFPointer *, short *);
IRAFPointer c_imofnls(IRAFPointer im, char *pattern) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imofns_(&im, char2iraf(pattern,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imofnu_ imofnu
# endif
	extern IRAFPointer imofnu_(IRAFPointer *, short *);
IRAFPointer c_imofnlu(IRAFPointer im, char *pattern) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imofnu_(&im, char2iraf(pattern,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgnfn_ imgnfn
# endif
	extern int imgnfn_(IRAFPointer *, short *, int *);
int c_imgnfn(IRAFPointer fn, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = imgnfn_(&fn, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imcfnl_ imcfnl
# endif
	extern void imcfnl_(IRAFPointer *);
void c_imcfnl(IRAFPointer fn) {
	clear_cvoserr();
	xerpsh_();
	imcfnl_(&fn);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imtopn_ imtopn
# endif
	extern IRAFPointer imtopn_(short *);
IRAFPointer c_imtopen(char *pattern) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imtopn_(char2iraf(pattern,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imtlen_ imtlen
# endif
	extern int imtlen_(IRAFPointer *);
int c_imtlen(IRAFPointer imt) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imtlen_(&imt);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imtrew_ imtrew
# endif
	extern void imtrew_(IRAFPointer *);
void c_imtrew(IRAFPointer imt) {
	clear_cvoserr();
	xerpsh_();
	imtrew_(&imt);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imtgem_ imtgem
# endif
	extern int imtgem_(IRAFPointer *, short *, int *);
int c_imtgetim(IRAFPointer imt, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = imtgem_(&imt, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imtcle_ imtcle
# endif
	extern void imtcle_(IRAFPointer *);
void c_imtclose(IRAFPointer imt) {
	clear_cvoserr();
	xerpsh_();
	imtcle_(&imt);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imtopp_ imtopp
# endif
	extern IRAFPointer imtopp_(short *);
IRAFPointer c_imtopenp(char *param) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imtopp_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imtrgm_ imtrgm
# endif
	extern int imtrgm_(IRAFPointer *, int *, short *, int *);
int c_imtrgetim(IRAFPointer imt, int index, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = imtrgm_(&imt, &index, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imtmae_ imtmae
# endif
	extern int imtmae_(short *, short *, int *);
int c_imt_mapname(char *fnt, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = imtmae_(char2iraf(fnt,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgcte_ imgcte
# endif
	extern int imgcte_(IRAFPointer *);
int c_imgctime(IRAFPointer imptr) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgcte_(&imptr);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imghiy_ imghiy
# endif
	extern void imghiy_(IRAFPointer *, short *, int *);
void c_imghistory(IRAFPointer imptr, char *str, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	imghiy_(&imptr, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(str,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define imglie_ imglie
# endif
	extern int imglie_(IRAFPointer *);
int c_imglimtime(IRAFPointer imptr) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imglie_(&imptr);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgmax_ imgmax
# endif
	extern float imgmax_(IRAFPointer *);
float c_imgmax(IRAFPointer imptr) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgmax_(&imptr);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgmin_ imgmin
# endif
	extern float imgmin_(IRAFPointer *);
float c_imgmin(IRAFPointer imptr) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgmin_(&imptr);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgmte_ imgmte
# endif
	extern int imgmte_(IRAFPointer *);
int c_imgmtime(IRAFPointer imptr) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgmte_(&imptr);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgndm_ imgndm
# endif
	extern int imgndm_(IRAFPointer *);
int c_imgndim(IRAFPointer imptr) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgndm_(&imptr);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imglen_ imglen
# endif
	extern int imglen_(IRAFPointer *, int *);
int c_imglen(IRAFPointer imptr, int axis) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imglen_(&imptr, &axis);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgpie_ imgpie
# endif
	extern void imgpie_(IRAFPointer *, short *, int *);
void c_imgpixfile(IRAFPointer imptr, char *str, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	imgpie_(&imptr, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(str,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define imgtyx_ imgtyx
# endif
	extern int imgtyx_(IRAFPointer *);
int c_imgtypepix(IRAFPointer imptr) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgtyx_(&imptr);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define imgtie_ imgtie
# endif
	extern void imgtie_(IRAFPointer *, short *, int *);
void c_imgtitle(IRAFPointer imptr, char *str, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	imgtie_(&imptr, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(str,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define impcte_ impcte
# endif
	extern void impcte_(IRAFPointer *, int *);
void c_impctime(IRAFPointer imptr, int val) {
	clear_cvoserr();
	xerpsh_();
	impcte_(&imptr, &val);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imphiy_ imphiy
# endif
	extern void imphiy_(IRAFPointer *, short *);
void c_imphistory(IRAFPointer imptr, char *str) {
	clear_cvoserr();
	xerpsh_();
	imphiy_(&imptr, char2iraf(str,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define implie_ implie
# endif
	extern void implie_(IRAFPointer *, int *);
void c_implimtime(IRAFPointer imptr, int val) {
	clear_cvoserr();
	xerpsh_();
	implie_(&imptr, &val);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define impmax_ impmax
# endif
	extern void impmax_(IRAFPointer *, float *);
void c_impmax(IRAFPointer imptr, float val) {
	clear_cvoserr();
	xerpsh_();
	impmax_(&imptr, &val);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define impmin_ impmin
# endif
	extern void impmin_(IRAFPointer *, float *);
void c_impmin(IRAFPointer imptr, float val) {
	clear_cvoserr();
	xerpsh_();
	impmin_(&imptr, &val);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define impmte_ impmte
# endif
	extern void impmte_(IRAFPointer *, int *);
void c_impmtime(IRAFPointer imptr, int val) {
	clear_cvoserr();
	xerpsh_();
	impmte_(&imptr, &val);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define impndm_ impndm
# endif
	extern void impndm_(IRAFPointer *, int *);
void c_impndim(IRAFPointer imptr, int val) {
	clear_cvoserr();
	xerpsh_();
	impndm_(&imptr, &val);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define implen_ implen
# endif
	extern void implen_(IRAFPointer *, int *, int *);
void c_implen(IRAFPointer imptr, int axis, int val) {
	clear_cvoserr();
	xerpsh_();
	implen_(&imptr, &axis, &val);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imppie_ imppie
# endif
	extern void imppie_(IRAFPointer *, short *);
void c_imppixfile(IRAFPointer imptr, char *str) {
	clear_cvoserr();
	xerpsh_();
	imppie_(&imptr, char2iraf(str,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imptyx_ imptyx
# endif
	extern void imptyx_(IRAFPointer *, int *);
void c_imptypepix(IRAFPointer imptr, int val) {
	clear_cvoserr();
	xerpsh_();
	imptyx_(&imptr, &val);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imptie_ imptie
# endif
	extern void imptie_(IRAFPointer *, short *);
void c_imptitle(IRAFPointer imptr, char *str) {
	clear_cvoserr();
	xerpsh_();
	imptie_(&imptr, char2iraf(str,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define imgusa_ imgusa
# endif
	extern IRAFPointer imgusa_(IRAFPointer *);
short * c_imguserarea(IRAFPointer imptr) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = imgusa_(&imptr);
	if (xerpoi_())
	    set_cvoserr();
	return (short *)((short *)mem_ + rtn - 1);
}

