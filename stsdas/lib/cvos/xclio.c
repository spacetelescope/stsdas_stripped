# include <xclio.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define clseti_ clseti
# endif
	extern void clseti_(int *, int *);
void c_clseti(int parameter, int value) {
	clear_cvoserr();
	xerpsh_();
	clseti_(&parameter, &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clstai_ clstai
# endif
	extern int clstai_(int *);
int c_clstati(int parameter) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clstai_(&parameter);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgetb_ clgetb
# endif
	extern Bool clgetb_(short *);
Bool c_clgetb(char *param) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgetb_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgetc_ clgetc
# endif
	extern short clgetc_(short *);
char c_clgetc(char *param) {
	char rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = (char)clgetc_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgetd_ clgetd
# endif
	extern double clgetd_(short *);
double c_clgetd(char *param) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgetd_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgeti_ clgeti
# endif
	extern int clgeti_(short *);
int c_clgeti(char *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgeti_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgetl_ clgetl
# endif
	extern int clgetl_(short *);
int c_clgetl(char *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgetl_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgetr_ clgetr
# endif
	extern float clgetr_(short *);
float c_clgetf(char *param) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgetr_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgetr_ clgetr
# endif
	extern float clgetr_(short *);
float c_clgetr(char *param) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgetr_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgets_ clgets
# endif
	extern short clgets_(short *);
short c_clgets(char *param) {
	short rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgets_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgetx_ clgetx
# endif
	extern Complex clgetx_(short *);
Complex c_clgetx(char *param) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgetx_(char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clputb_ clputb
# endif
	extern void clputb_(short *, Bool *);
void c_clputb(char *param, Bool bval) {
	clear_cvoserr();
	xerpsh_();
	clputb_(char2iraf(param,1), &bval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clputc_ clputc
# endif
	extern void clputc_(short *, short *);
void c_clputc(char *param, char cval) {
	short short_cval;
	short_cval = cval;
	clear_cvoserr();
	xerpsh_();
	clputc_(char2iraf(param,1), &short_cval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clputd_ clputd
# endif
	extern void clputd_(short *, double *);
void c_clputd(char *param, double dval) {
	clear_cvoserr();
	xerpsh_();
	clputd_(char2iraf(param,1), &dval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clputi_ clputi
# endif
	extern void clputi_(short *, int *);
void c_clputi(char *param, int value) {
	clear_cvoserr();
	xerpsh_();
	clputi_(char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clputs_ clputs
# endif
	extern void clputs_(short *, short *);
void c_clputs(char *param, short value) {
	clear_cvoserr();
	xerpsh_();
	clputs_(char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clputl_ clputl
# endif
	extern void clputl_(short *, int *);
void c_clputl(char *param, int lval) {
	clear_cvoserr();
	xerpsh_();
	clputl_(char2iraf(param,1), &lval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clputr_ clputr
# endif
	extern void clputr_(short *, float *);
void c_clputf(char *param, float rval) {
	clear_cvoserr();
	xerpsh_();
	clputr_(char2iraf(param,1), &rval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clputr_ clputr
# endif
	extern void clputr_(short *, float *);
void c_clputr(char *param, float rval) {
	clear_cvoserr();
	xerpsh_();
	clputr_(char2iraf(param,1), &rval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clputx_ clputx
# endif
	extern void clputx_(short *, Complex *);
void c_clputx(char *param, Complex xval) {
	clear_cvoserr();
	xerpsh_();
	clputx_(char2iraf(param,1), &xval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clgstr_ clgstr
# endif
	extern void clgstr_(short *, short *, int *);
void c_clgstr(char *param, char *outstr, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	clgstr_(char2iraf(param,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define clpstr_ clpstr
# endif
	extern void clpstr_(short *, short *);
void c_clpstr(char *param, char *value) {
	clear_cvoserr();
	xerpsh_();
	clpstr_(char2iraf(param,1), char2iraf(value,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clglpb_ clglpb
# endif
	extern int clglpb_(short *, Bool *);
int c_clglpb(char *param, Bool *bval) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clglpb_(char2iraf(param,1), bval);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglpc_ clglpc
# endif
	extern int clglpc_(short *, short *);
int c_clglpc(char *param, char *cval) {
	int rtn;
	short short_cval;
	short_cval = *cval;
	clear_cvoserr();
	xerpsh_();
	rtn = clglpc_(char2iraf(param,1), &short_cval);
	if (xerpoi_())
	    set_cvoserr();
	*cval = short_cval;
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglpd_ clglpd
# endif
	extern int clglpd_(short *, double *);
int c_clglpd(char *param, double *dval) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clglpd_(char2iraf(param,1), dval);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglpi_ clglpi
# endif
	extern int clglpi_(short *, int *);
int c_clglpi(char *param, int *ival) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clglpi_(char2iraf(param,1), ival);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglpl_ clglpl
# endif
	extern int clglpl_(short *, int *);
int c_clglpl(char *param, int *lval) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clglpl_(char2iraf(param,1), lval);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglpr_ clglpr
# endif
	extern int clglpr_(short *, float *);
int c_clglpf(char *param, float *rval) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clglpr_(char2iraf(param,1), rval);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglpr_ clglpr
# endif
	extern int clglpr_(short *, float *);
int c_clglpr(char *param, float *rval) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clglpr_(char2iraf(param,1), rval);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglps_ clglps
# endif
	extern int clglps_(short *, short *);
int c_clglps(char *param, short *sval) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clglps_(char2iraf(param,1), sval);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglpx_ clglpx
# endif
	extern int clglpx_(short *, Complex *);
int c_clglpx(char *param, Complex *xval) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clglpx_(char2iraf(param,1), xval);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clglsr_ clglsr
# endif
	extern int clglsr_(short *, short *, int *);
int c_clglstr(char *param, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = clglsr_(char2iraf(param,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgcur_ clgcur
# endif
	extern int clgcur_(short *, float *, float *, int *, int *, short *, int *);
int c_clgcur(char *param, float *wx, float *wy, int *wcs, int *key, char *strval, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = clgcur_(char2iraf(param,1), wx, wy, wcs, key, CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(strval,maxch,2);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgkey_ clgkey
# endif
	extern int clgkey_(short *, int *, short *, int *);
int c_clgkey(char *param, int *key, char *strval, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = clgkey_(char2iraf(param,1), key, CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(strval,maxch,2);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgwrd_ clgwrd
# endif
	extern int clgwrd_(short *, short *, int *, short *);
int c_clgwrd(char *param, char *keyword, int maxchar, char *dictionary) {
	int rtn;
	CH2I_chk_buffer(2,maxchar);
	clear_cvoserr();
	xerpsh_();
	rtn = clgwrd_(char2iraf(param,1), CH2I_buffer[2], &maxchar, char2iraf(dictionary,3));
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(keyword,maxchar,2);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clopst_ clopst
# endif
	extern IRAFPointer clopst_(short *);
IRAFPointer c_clopset(char *pset) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clopst_(char2iraf(pset,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clcpst_ clcpst
# endif
	extern void clcpst_(IRAFPointer *);
void c_clcpset(IRAFPointer pp) {
	clear_cvoserr();
	xerpsh_();
	clcpst_(&pp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clgpsb_ clgpsb
# endif
	extern Bool clgpsb_(IRAFPointer *, short *);
Bool c_clgpsetb(IRAFPointer pp, char *parname) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgpsb_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgpsc_ clgpsc
# endif
	extern short clgpsc_(IRAFPointer *, short *);
char c_clgpsetc(IRAFPointer pp, char *parname) {
	char rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = (char)clgpsc_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgpsd_ clgpsd
# endif
	extern double clgpsd_(IRAFPointer *, short *);
double c_clgpsetd(IRAFPointer pp, char *parname) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgpsd_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgpsi_ clgpsi
# endif
	extern int clgpsi_(IRAFPointer *, short *);
int c_clgpseti(IRAFPointer pp, char *parname) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgpsi_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgpsl_ clgpsl
# endif
	extern int clgpsl_(IRAFPointer *, short *);
int c_clgpsetl(IRAFPointer pp, char *parname) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgpsl_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgpsr_ clgpsr
# endif
	extern float clgpsr_(IRAFPointer *, short *);
float c_clgpsetf(IRAFPointer pp, char *parname) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgpsr_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgpsr_ clgpsr
# endif
	extern float clgpsr_(IRAFPointer *, short *);
float c_clgpsetr(IRAFPointer pp, char *parname) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgpsr_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgpss_ clgpss
# endif
	extern short clgpss_(IRAFPointer *, short *);
short c_clgpsets(IRAFPointer pp, char *parname) {
	short rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgpss_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clgpsx_ clgpsx
# endif
	extern Complex clgpsx_(IRAFPointer *, short *);
Complex c_clgpsetx(IRAFPointer pp, char *parname) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = clgpsx_(&pp, char2iraf(parname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define clppsb_ clppsb
# endif
	extern void clppsb_(IRAFPointer *, short *, Bool *);
void c_clppsetb(IRAFPointer pp, char *parname, Bool bval) {
	clear_cvoserr();
	xerpsh_();
	clppsb_(&pp, char2iraf(parname,1), &bval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clppsc_ clppsc
# endif
	extern void clppsc_(IRAFPointer *, short *, short *);
void c_clppsetc(IRAFPointer pp, char *parname, char cval) {
	short short_cval;
	short_cval = cval;
	clear_cvoserr();
	xerpsh_();
	clppsc_(&pp, char2iraf(parname,1), &short_cval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clppsd_ clppsd
# endif
	extern void clppsd_(IRAFPointer *, short *, double *);
void c_clppsetd(IRAFPointer pp, char *parname, double dval) {
	clear_cvoserr();
	xerpsh_();
	clppsd_(&pp, char2iraf(parname,1), &dval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clppsi_ clppsi
# endif
	extern void clppsi_(IRAFPointer *, short *, int *);
void c_clppseti(IRAFPointer pp, char *parname, int ival) {
	clear_cvoserr();
	xerpsh_();
	clppsi_(&pp, char2iraf(parname,1), &ival);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clppsl_ clppsl
# endif
	extern void clppsl_(IRAFPointer *, short *, int *);
void c_clppsetl(IRAFPointer pp, char *parname, int lval) {
	clear_cvoserr();
	xerpsh_();
	clppsl_(&pp, char2iraf(parname,1), &lval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clppsr_ clppsr
# endif
	extern void clppsr_(IRAFPointer *, short *, float *);
void c_clppsetf(IRAFPointer pp, char *parname, float rval) {
	clear_cvoserr();
	xerpsh_();
	clppsr_(&pp, char2iraf(parname,1), &rval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clppsr_ clppsr
# endif
	extern void clppsr_(IRAFPointer *, short *, float *);
void c_clppsetr(IRAFPointer pp, char *parname, float rval) {
	clear_cvoserr();
	xerpsh_();
	clppsr_(&pp, char2iraf(parname,1), &rval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clppss_ clppss
# endif
	extern void clppss_(IRAFPointer *, short *, short *);
void c_clppsets(IRAFPointer pp, char *parname, short sval) {
	clear_cvoserr();
	xerpsh_();
	clppss_(&pp, char2iraf(parname,1), &sval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clppsx_ clppsx
# endif
	extern void clppsx_(IRAFPointer *, short *, Complex *);
void c_clppsetx(IRAFPointer pp, char *parname, Complex xval) {
	clear_cvoserr();
	xerpsh_();
	clppsx_(&pp, char2iraf(parname,1), &xval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clgpsa_ clgpsa
# endif
	extern void clgpsa_(IRAFPointer *, short *, short *, int *);
void c_clgpseta(IRAFPointer pp, char *pname, char *outstr, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	clgpsa_(&pp, char2iraf(pname,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define clppsa_ clppsa
# endif
	extern void clppsa_(IRAFPointer *, short *, short *);
void c_clppseta(IRAFPointer pp, char *pname, char *sval) {
	clear_cvoserr();
	xerpsh_();
	clppsa_(&pp, char2iraf(pname,1), char2iraf(sval,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define cllpst_ cllpst
# endif
	extern void cllpst_(IRAFPointer *, int *, short *);
void c_cllpset(IRAFPointer pp, int fd, char *format) {
	clear_cvoserr();
	xerpsh_();
	cllpst_(&pp, &fd, char2iraf(format,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clepst_ clepst
# endif
	extern void clepst_(IRAFPointer *);
void c_clepset(IRAFPointer pp) {
	clear_cvoserr();
	xerpsh_();
	clepst_(&pp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clcmd_ clcmd
# endif
	extern void clcmd_(short *);
void c_clcmd(char *cmd) {
	clear_cvoserr();
	xerpsh_();
	clcmd_(char2iraf(cmd,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define clcmdw_ clcmdw
# endif
	extern void clcmdw_(short *);
void c_clcmdw(char *cmd) {
	clear_cvoserr();
	xerpsh_();
	clcmdw_(char2iraf(cmd,1));
	if (xerpoi_())
	    set_cvoserr();
}

