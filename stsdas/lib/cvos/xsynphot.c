# include <xsynphot.h>
# include <c_iraf_priv.h>
# include <stdlib.h>
# if defined(NO_UNDERSCORE)
# define compbd_ compbd
# endif
	extern void compbd_(short *, int *, short *, short *, int *, IRAFPointer *, IRAFPointer *);
void c_compband(char *command, int iw, char *graphtab, char *comptab, int nwave, IRAFPointer *wave, IRAFPointer *band) {
	clear_cvoserr();
	xerpsh_();
	compbd_(char2iraf(command,1), &iw, char2iraf(graphtab,2), char2iraf(comptab,3), &nwave, wave, band);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define compsc_ compsc
# endif
	extern void compsc_(short *, int *, short *, short *, int *, IRAFPointer *, IRAFPointer *, short *);
void c_compspec(char *command, int iw, char *graphtab, char *comptab, int nwave, IRAFPointer *wave, IRAFPointer *spec, char *form) {
	CH2I_chk_buffer(4,15);
	clear_cvoserr();
	xerpsh_();
	compsc_(char2iraf(command,1), &iw, char2iraf(graphtab,2), char2iraf(comptab,3), &nwave, wave, spec, CH2I_buffer[4]);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(form,15,4);
}

# if defined(NO_UNDERSCORE)
# define inisyb_ inisyb
# endif
	extern void inisyb_();
void c_inisyntab() {
	clear_cvoserr();
	xerpsh_();
	inisyb_();
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clssyb_ clssyb
# endif
	extern void clssyb_();
void c_clssyntab() {
	clear_cvoserr();
	xerpsh_();
	clssyb_();
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define syncoe_ syncoe
# endif
	extern void syncoe_(short *, int *, int *);
void c_syncompile(char *expr, int *pcode, int maxcode) {
	clear_cvoserr();
	xerpsh_();
	syncoe_(char2iraf(expr,1), pcode, &maxcode);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define syncac_ syncac
# endif
	extern void syncac_(int *, int *, IRAFPointer *, int *, float *, short *, short *, float *, int *);
void c_syncalc(int *pcode, int maxcode, IRAFPointer getvar, int nwave, float *wave, char *graphtab, char *comptab, float *output, int *units) {
	clear_cvoserr();
	xerpsh_();
	syncac_(pcode, &maxcode, &getvar, &nwave, wave, char2iraf(graphtab,1), char2iraf(comptab,2), output, units);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define calcre_ calcre
# endif
	extern void calcre_(int *, int *, short *, short *, float *, float *);
void c_calcrange(int *pcode, int maxcode, char *graphtab, char *comptab, float *minwave, float *maxwave) {
	clear_cvoserr();
	xerpsh_();
	calcre_(pcode, &maxcode, char2iraf(graphtab,1), char2iraf(comptab,2), minwave, maxwave);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define getphx_ getphx
# endif
	extern void getphx_(short *, short *, short *, short *, int *, float *);
void c_getphotx(char *mode, char *graphtab, char *comptab, char *path, int mxpath, float *phot) {
	CH2I_chk_buffer(4,mxpath);
	clear_cvoserr();
	xerpsh_();
	getphx_(char2iraf(mode,1), char2iraf(graphtab,2), char2iraf(comptab,3), CH2I_buffer[4], &mxpath, phot);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(path,mxpath,4);
}

# if defined(NO_UNDERSCORE)
# define getbax_ getbax
# endif
	extern void getbax_(short *, short *, short *, Bool *, int *, float *, float *, float *);
void c_getbandx(char *mode, char *graphtab, char *comptab, Bool logspace, int nwave, float *wave, float *thruput, float *thruerr) {
	clear_cvoserr();
	xerpsh_();
	getbax_(char2iraf(mode,1), char2iraf(graphtab,2), char2iraf(comptab,3), &logspace, &nwave, wave, thruput, thruerr);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define evalbx_ evalbx
# endif
	extern void evalbx_(short *, int *, float *, short *, short *, float *, float *);
void c_evalbandx(char *mode, int nwave, float *wave, char *graphtab, char *comptab, float *thruput, float *thruerr) {
	clear_cvoserr();
	xerpsh_();
	evalbx_(char2iraf(mode,1), &nwave, wave, char2iraf(graphtab,2), char2iraf(comptab,3), thruput, thruerr);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define phopar_ phopar
# endif
	extern void phopar_(int *, float *, float *, float *);
void c_phopar(int nwave, float *wave, float *thruput, float *phot) {
	clear_cvoserr();
	xerpsh_();
	phopar_(&nwave, wave, thruput, phot);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define anytog_ anytog
# endif
	extern int anytog_(short *, float *, int *);
int c_anytoang(char *units, float *wave, int nwave) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = anytog_(char2iraf(units,1), wave, &nwave);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define angtoy_ angtoy
# endif
	extern int angtoy_(short *, float *, int *);
int c_angtoany(char *units, float *wave, int nwave) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = angtoy_(char2iraf(units,1), wave, &nwave);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define anytot_ anytot
# endif
	extern int anytot_(short *, int *, float *, float *);
int c_anytophot(char *units, int nwave, float *wave, float *flux) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = anytot_(char2iraf(units,1), &nwave, wave, flux);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define photty_ photty
# endif
	extern int photty_(short *, int *, float *, float *);
int c_phottoany(char *units, int nwave, float *wave, float *flux) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = photty_(char2iraf(units,1), &nwave, wave, flux);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define avglam_ avglam
# endif
	extern float avglam_(int *, float *, float *);
float c_avglam(int nwave, float *wave, float *thruput) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = avglam_(&nwave, wave, thruput);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define pivlam_ pivlam
# endif
	extern float pivlam_(int *, float *, float *);
float c_pivlam(int nwave, float *wave, float *thruput) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = pivlam_(&nwave, wave, thruput);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rmslam_ rmslam
# endif
	extern float rmslam_(int *, float *, float *);
float c_rmslam(int nwave, float *wave, float *thruput) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rmslam_(&nwave, wave, thruput);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define fwhmlm_ fwhmlm
# endif
	extern float fwhmlm_(int *, float *, float *);
float c_fwhmlam(int nwave, float *wave, float *thruput) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = fwhmlm_(&nwave, wave, thruput);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define funit_ funit
# endif
	extern float funit_(float *, int *, float *, float *);
float c_funit(float area, int nwave, float *wave, float *thruput) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = funit_(&area, &nwave, wave, thruput);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define getvea_ getvea
# endif
	extern void getvea_(int *, float *, float *);
void c_getvega(int nwave, float *wave, float *spec) {
	clear_cvoserr();
	xerpsh_();
	getvea_(&nwave, wave, spec);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define listph_ listph
# endif
	extern void listph_(short *, short *, short *, short *, int *);
void c_listpath(char *mode, char *graphtab, char *comptab, char *path, int mxpath) {
	CH2I_chk_buffer(4,mxpath);
	clear_cvoserr();
	xerpsh_();
	listph_(char2iraf(mode,1), char2iraf(graphtab,2), char2iraf(comptab,3), CH2I_buffer[4], &mxpath);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(path,mxpath,4);
}

# if defined(NO_UNDERSCORE)
# define graffs_ graffs
# endif
	extern void graffs_(short *, short *, short *, int *, int *, int *, short **);
void c_graffiles(char *mode, char *graphtab, char *comptab, int maxname, int maxthru, int *nthru, char **thruput) {
	CH2I_chk_buffer(4,(maxname + 1) * maxthru);
	clear_cvoserr();
	xerpsh_();
	graffs_(char2iraf(mode,1), char2iraf(graphtab,2), char2iraf(comptab,3), &maxname, &maxthru, nthru, (short **)CH2I_buffer[4]);
	if (xerpoi_())
	    set_cvoserr();
	iraf2twodchar(thruput,maxname,maxthru,4);
}

