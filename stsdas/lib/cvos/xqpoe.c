# include <xqpoe.h>
# include <c_iraf_priv.h>
# include <stdlib.h>
/*
** M.D. De La Pena 16 July 1999: This file has been manually modified to
** include the c_qpio_get() and c_qpio_put() routines which are wrappers
** around the c_qpio_getevents() and c_qpio_putevents() routines performing
** datatype conversions as needed by the CXC team at CfA.
*/
# if defined(NO_UNDERSCORE)
# define qppare_ qppare
# endif
	extern void qppare_(short *, short *, int *, short *, int *);
void c_qp_parse(char *qpspec, char *root, int sz_root, char *filter, int sz_filter) {
	CH2I_chk_buffer(2,sz_root);
	CH2I_chk_buffer(3,sz_filter);
	clear_cvoserr();
	xerpsh_();
	qppare_(char2iraf(qpspec,1), CH2I_buffer[2], &sz_root, CH2I_buffer[3], &sz_filter);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(root,sz_root,2);
	iraf2char(filter,sz_filter,3);
}

# if defined(NO_UNDERSCORE)
# define qpaccs_ qpaccs
# endif
	extern int qpaccs_(short *, int *);
int c_qp_access(char *poefile, int mode) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpaccs_(char2iraf(poefile,1), &mode);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpcopy_ qpcopy
# endif
	extern void qpcopy_(short *, short *);
void c_qp_copy(char *o_poefile, char *n_poefile) {
	clear_cvoserr();
	xerpsh_();
	qpcopy_(char2iraf(o_poefile,1), char2iraf(n_poefile,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qprene_ qprene
# endif
	extern void qprene_(short *, short *);
void c_qp_rename(char *o_poefile, char *n_poefile) {
	clear_cvoserr();
	xerpsh_();
	qprene_(char2iraf(o_poefile,1), char2iraf(n_poefile,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qprebd_ qprebd
# endif
	extern void qprebd_(short *);
void c_qp_rebuild(char *poefile) {
	clear_cvoserr();
	xerpsh_();
	qprebd_(char2iraf(poefile,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpdele_ qpdele
# endif
	extern void qpdele_(short *);
void c_qp_delete(char *poefile) {
	clear_cvoserr();
	xerpsh_();
	qpdele_(char2iraf(poefile,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpopen_ qpopen
# endif
	extern IRAFPointer qpopen_(short *, int *, IRAFPointer *);
IRAFPointer c_qp_open(char *poefile, int mode, IRAFPointer o_qp) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpopen_(char2iraf(poefile,1), &mode, &o_qp);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpseti_ qpseti
# endif
	extern void qpseti_(IRAFPointer *, int *, int *);
void c_qp_seti(IRAFPointer qp, int param, int value) {
	clear_cvoserr();
	xerpsh_();
	qpseti_(&qp, &param, &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpstai_ qpstai
# endif
	extern int qpstai_(IRAFPointer *, int *);
int c_qp_stati(IRAFPointer qp, int param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpstai_(&qp, &param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpsync_ qpsync
# endif
	extern void qpsync_(IRAFPointer *);
void c_qp_sync(IRAFPointer qp) {
	clear_cvoserr();
	xerpsh_();
	qpsync_(&qp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpcloe_ qpcloe
# endif
	extern void qpcloe_(IRAFPointer *);
void c_qp_close(IRAFPointer qp) {
	clear_cvoserr();
	xerpsh_();
	qpcloe_(&qp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaddb_ qpaddb
# endif
	extern void qpaddb_(IRAFPointer *, short *, Bool *, short *);
void c_qp_addb(IRAFPointer qp, char *param, Bool value, char *comment) {
	clear_cvoserr();
	xerpsh_();
	qpaddb_(&qp, char2iraf(param,1), &value, char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaddc_ qpaddc
# endif
	extern void qpaddc_(IRAFPointer *, short *, short *, short *);
void c_qp_addc(IRAFPointer qp, char *param, char value, char *comment) {
	short short_value;
	short_value = value;
	clear_cvoserr();
	xerpsh_();
	qpaddc_(&qp, char2iraf(param,1), &short_value, char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaddd_ qpaddd
# endif
	extern void qpaddd_(IRAFPointer *, short *, double *, short *);
void c_qp_addd(IRAFPointer qp, char *param, double value, char *comment) {
	clear_cvoserr();
	xerpsh_();
	qpaddd_(&qp, char2iraf(param,1), &value, char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaddi_ qpaddi
# endif
	extern void qpaddi_(IRAFPointer *, short *, int *, short *);
void c_qp_addi(IRAFPointer qp, char *param, int value, char *comment) {
	clear_cvoserr();
	xerpsh_();
	qpaddi_(&qp, char2iraf(param,1), &value, char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaddl_ qpaddl
# endif
	extern void qpaddl_(IRAFPointer *, short *, int *, short *);
void c_qp_addl(IRAFPointer qp, char *param, int value, char *comment) {
	clear_cvoserr();
	xerpsh_();
	qpaddl_(&qp, char2iraf(param,1), &value, char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaddr_ qpaddr
# endif
	extern void qpaddr_(IRAFPointer *, short *, float *, short *);
void c_qp_addr(IRAFPointer qp, char *param, float value, char *comment) {
	clear_cvoserr();
	xerpsh_();
	qpaddr_(&qp, char2iraf(param,1), &value, char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpadds_ qpadds
# endif
	extern void qpadds_(IRAFPointer *, short *, short *, short *);
void c_qp_adds(IRAFPointer qp, char *param, short value, char *comment) {
	clear_cvoserr();
	xerpsh_();
	qpadds_(&qp, char2iraf(param,1), &value, char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaddx_ qpaddx
# endif
	extern void qpaddx_(IRAFPointer *, short *, Complex *, short *);
void c_qp_addx(IRAFPointer qp, char *param, Complex value, char *comment) {
	clear_cvoserr();
	xerpsh_();
	qpaddx_(&qp, char2iraf(param,1), &value, char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpastr_ qpastr
# endif
	extern void qpastr_(IRAFPointer *, short *, short *, short *);
void c_qp_astr(IRAFPointer qp, char *param, char *value, char *comment) {
	clear_cvoserr();
	xerpsh_();
	qpastr_(&qp, char2iraf(param,1), char2iraf(value,2), char2iraf(comment,3));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpgetb_ qpgetb
# endif
	extern Bool qpgetb_(IRAFPointer *, short *);
Bool c_qp_getb(IRAFPointer qp, char *param) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpgetb_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgetx_ qpgetx
# endif
	extern Complex qpgetx_(IRAFPointer *, short *);
Complex c_qp_getx(IRAFPointer qp, char *param) {
	Complex rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpgetx_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgetc_ qpgetc
# endif
	extern short qpgetc_(IRAFPointer *, short *);
char c_qp_getc(IRAFPointer qp, char *param) {
	char rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = (char)qpgetc_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgetd_ qpgetd
# endif
	extern double qpgetd_(IRAFPointer *, short *);
double c_qp_getd(IRAFPointer qp, char *param) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpgetd_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgeti_ qpgeti
# endif
	extern int qpgeti_(IRAFPointer *, short *);
int c_qp_geti(IRAFPointer qp, char *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpgeti_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgetl_ qpgetl
# endif
	extern int qpgetl_(IRAFPointer *, short *);
int c_qp_getl(IRAFPointer qp, char *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpgetl_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgetr_ qpgetr
# endif
	extern float qpgetr_(IRAFPointer *, short *);
float c_qp_getr(IRAFPointer qp, char *param) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpgetr_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgets_ qpgets
# endif
	extern short qpgets_(IRAFPointer *, short *);
short c_qp_gets(IRAFPointer qp, char *param) {
	short rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpgets_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgstr_ qpgstr
# endif
	extern int qpgstr_(IRAFPointer *, short *, short *, int *);
int c_qp_gstr(IRAFPointer qp, char *param, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = qpgstr_(&qp, char2iraf(param,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpputb_ qpputb
# endif
	extern void qpputb_(IRAFPointer *, short *, Bool *);
void c_qp_putb(IRAFPointer qp, char *param, Bool value) {
	clear_cvoserr();
	xerpsh_();
	qpputb_(&qp, char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpputx_ qpputx
# endif
	extern void qpputx_(IRAFPointer *, short *, Complex *);
void c_qp_putx(IRAFPointer qp, char *param, Complex value) {
	clear_cvoserr();
	xerpsh_();
	qpputx_(&qp, char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpputc_ qpputc
# endif
	extern void qpputc_(IRAFPointer *, short *, short *);
void c_qp_putc(IRAFPointer qp, char *param, char value) {
	short short_value;
	short_value = value;
	clear_cvoserr();
	xerpsh_();
	qpputc_(&qp, char2iraf(param,1), &short_value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpputd_ qpputd
# endif
	extern void qpputd_(IRAFPointer *, short *, double *);
void c_qp_putd(IRAFPointer qp, char *param, double value) {
	clear_cvoserr();
	xerpsh_();
	qpputd_(&qp, char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpputi_ qpputi
# endif
	extern void qpputi_(IRAFPointer *, short *, int *);
void c_qp_puti(IRAFPointer qp, char *param, int value) {
	clear_cvoserr();
	xerpsh_();
	qpputi_(&qp, char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpputl_ qpputl
# endif
	extern void qpputl_(IRAFPointer *, short *, int *);
void c_qp_putl(IRAFPointer qp, char *param, int value) {
	clear_cvoserr();
	xerpsh_();
	qpputl_(&qp, char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpputr_ qpputr
# endif
	extern void qpputr_(IRAFPointer *, short *, float *);
void c_qp_putr(IRAFPointer qp, char *param, float value) {
	clear_cvoserr();
	xerpsh_();
	qpputr_(&qp, char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpputs_ qpputs
# endif
	extern void qpputs_(IRAFPointer *, short *, short *);
void c_qp_puts(IRAFPointer qp, char *param, short value) {
	clear_cvoserr();
	xerpsh_();
	qpputs_(&qp, char2iraf(param,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qppstr_ qppstr
# endif
	extern void qppstr_(IRAFPointer *, short *, short *);
void c_qp_pstr(IRAFPointer qp, char *param, char *strval) {
	clear_cvoserr();
	xerpsh_();
	qppstr_(&qp, char2iraf(param,1), char2iraf(strval,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpread_ qpread
# endif
	extern int qpread_(IRAFPointer *, short *, void *, int *, int *, short *);
int c_qp_read(IRAFPointer qp, char *param, void *buf, int maxelem, int first, char *datatype) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpread_(&qp, char2iraf(param,1), buf, &maxelem, &first, char2iraf(datatype,2));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpwrie_ qpwrie
# endif
	extern void qpwrie_(IRAFPointer *, short *, void *, int *, int *, short *);
void c_qp_write(IRAFPointer qp, char *param, void *buf, int nelem, int first, char *datatype) {
	clear_cvoserr();
	xerpsh_();
	qpwrie_(&qp, char2iraf(param,1), buf, &nelem, &first, char2iraf(datatype,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qppopn_ qppopn
# endif
	extern int qppopn_(IRAFPointer *, short *, int *, int *);
int c_qp_popen(IRAFPointer qp, char *param, int mode, int type) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qppopn_(&qp, char2iraf(param,1), &mode, &type);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qploas_ qploas
# endif
	extern IRAFPointer qploas_(IRAFPointer *);
IRAFPointer c_qp_loadwcs(IRAFPointer qp) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qploas_(&qp);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpsavs_ qpsavs
# endif
	extern void qpsavs_(IRAFPointer *, IRAFPointer *);
void c_qp_savewcs(IRAFPointer qp, IRAFPointer mw) {
	clear_cvoserr();
	xerpsh_();
	qpsavs_(&qp, &mw);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaccf_ qpaccf
# endif
	extern int qpaccf_(IRAFPointer *, short *);
int c_qp_accessf(IRAFPointer qp, char *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpaccf_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpdelf_ qpdelf
# endif
	extern void qpdelf_(IRAFPointer *, short *);
void c_qp_deletef(IRAFPointer qp, char *param) {
	clear_cvoserr();
	xerpsh_();
	qpdelf_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qprenf_ qprenf
# endif
	extern void qprenf_(IRAFPointer *, short *, short *);
void c_qp_renamef(IRAFPointer qp, char *param, char *newname) {
	clear_cvoserr();
	xerpsh_();
	qprenf_(&qp, char2iraf(param,1), char2iraf(newname,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpcopf_ qpcopf
# endif
	extern void qpcopf_(IRAFPointer *, short *, IRAFPointer *, short *);
void c_qp_copyf(IRAFPointer o_qp, char *o_param, IRAFPointer n_qp, char *n_param) {
	clear_cvoserr();
	xerpsh_();
	qpcopf_(&o_qp, char2iraf(o_param,1), &n_qp, char2iraf(n_param,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpaddf_ qpaddf
# endif
	extern void qpaddf_(IRAFPointer *, short *, short *, int *, short *, int *);
void c_qp_addf(IRAFPointer qp, char *param, char *datatype, int maxelem, char *comment, int flags) {
	clear_cvoserr();
	xerpsh_();
	qpaddf_(&qp, char2iraf(param,1), char2iraf(datatype,2), &maxelem, char2iraf(comment,3), &flags);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpquef_ qpquef
# endif
	extern int qpquef_(IRAFPointer *, short *, short *, int *, short *, int *);
int c_qp_queryf(IRAFPointer qp, char *param, char *datatype, int *maxelem, char *comment, int *flags) {
	int rtn;
	CH2I_chk_buffer(2,SZ_DATATYPE);
	CH2I_chk_buffer(3,SZ_COMMENT);
	clear_cvoserr();
	xerpsh_();
	rtn = qpquef_(&qp, char2iraf(param,1), CH2I_buffer[2], maxelem, CH2I_buffer[3], flags);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(datatype,SZ_DATATYPE,2);
	iraf2char(comment,SZ_COMMENT,3);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qplenf_ qplenf
# endif
	extern int qplenf_(IRAFPointer *, short *);
int c_qp_lenf(IRAFPointer qp, char *param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qplenf_(&qp, char2iraf(param,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexpt_ qpexpt
# endif
	extern int qpexpt_(IRAFPointer *, short *, short *, int *);
int c_qp_expandtext(IRAFPointer qp, char *s1, char *s2, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = qpexpt_(&qp, char2iraf(s1,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(s2,maxch,2);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpofns_ qpofns
# endif
	extern IRAFPointer qpofns_(IRAFPointer *, short *);
IRAFPointer c_qp_ofnls(IRAFPointer qp, char *fieldname) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpofns_(&qp, char2iraf(fieldname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpofnu_ qpofnu
# endif
	extern IRAFPointer qpofnu_(IRAFPointer *, short *);
IRAFPointer c_qp_ofnlu(IRAFPointer qp, char *fieldname) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpofnu_(&qp, char2iraf(fieldname,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpofnl_ qpofnl
# endif
	extern IRAFPointer qpofnl_(IRAFPointer *, short *, Bool *);
IRAFPointer c_qp_ofnl(IRAFPointer qp, char *fieldname, Bool sort) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpofnl_(&qp, char2iraf(fieldname,1), &sort);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpgnfn_ qpgnfn
# endif
	extern int qpgnfn_(IRAFPointer *, short *, int *);
int c_qp_gnfn(IRAFPointer fl, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = qpgnfn_(&fl, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qplenl_ qplenl
# endif
	extern int qplenl_(IRAFPointer *);
int c_qp_lenfnl(IRAFPointer fl) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qplenl_(&fl);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpseel_ qpseel
# endif
	extern void qpseel_(IRAFPointer *, int *);
void c_qp_seekfnl(IRAFPointer fl, int pos) {
	clear_cvoserr();
	xerpsh_();
	qpseel_(&fl, &pos);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpcfnl_ qpcfnl
# endif
	extern void qpcfnl_(IRAFPointer *);
void c_qp_cfnl(IRAFPointer fl) {
	clear_cvoserr();
	xerpsh_();
	qpcfnl_(&fl);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpioon_ qpioon
# endif
	extern IRAFPointer qpioon_(IRAFPointer *, short *, int *);
IRAFPointer c_qpio_open(IRAFPointer qp, char *paramex, int mode) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpioon_(&qp, char2iraf(paramex,1), &mode);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpioce_ qpioce
# endif
	extern void qpioce_(IRAFPointer *);
void c_qpio_close(IRAFPointer io) {
	clear_cvoserr();
	xerpsh_();
	qpioce_(&io);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpiose_ qpiose
# endif
	extern void qpiose_(IRAFPointer *, int *, int *, int *);
void c_qpio_setrange(IRAFPointer io, int *vs, int *ve, int ndim) {
	clear_cvoserr();
	xerpsh_();
	qpiose_(&io, vs, ve, &ndim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpioge_ qpioge
# endif
	extern int qpioge_(IRAFPointer *, int *, int *, int *);
int c_qpio_getrange(IRAFPointer io, int *vs, int *ve, int maxdim) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpioge_(&io, vs, ve, &maxdim);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpiosr_ qpiosr
# endif
	extern void qpiosr_(IRAFPointer *, short *);
void c_qpio_setfilter(IRAFPointer io, char *expr) {
	clear_cvoserr();
	xerpsh_();
	qpiosr_(&io, char2iraf(expr,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpiogr_ qpiogr
# endif
	extern int qpiogr_(IRAFPointer *, short *, int *);
int c_qpio_getfilter(IRAFPointer io, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = qpiogr_(&io, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpiosi_ qpiosi
# endif
	extern void qpiosi_(IRAFPointer *, int *, int *);
void c_qpio_seti(IRAFPointer io, int param, int value) {
	clear_cvoserr();
	xerpsh_();
	qpiosi_(&io, &param, &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpiost_ qpiost
# endif
	extern int qpiost_(IRAFPointer *, int *);
int c_qpio_stati(IRAFPointer io, int param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpiost_(&io, &param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpiols_ qpiols
# endif
	extern IRAFPointer qpiols_(IRAFPointer *);
IRAFPointer c_qpio_loadwcs(IRAFPointer io) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpiols_(&io);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpiomx_ qpiomx
# endif
	extern void qpiomx_(IRAFPointer *, short *);
void c_qpio_mkindex(IRAFPointer io, char *key) {
	clear_cvoserr();
	xerpsh_();
	qpiomx_(&io, char2iraf(key,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpiops_ qpiops
# endif
	extern void qpiops_(IRAFPointer *, IRAFPointer *, int *);
void c_qpio_putevents(IRAFPointer io, IRAFPointer *i_ev, int nevents) {
	clear_cvoserr();
	xerpsh_();
	qpiops_(&io, i_ev, &nevents);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define qpiogs_ qpiogs
# endif
	extern int qpiogs_(IRAFPointer *, IRAFPointer *, int *, int *, int *);
int c_qpio_getevents(IRAFPointer io, IRAFPointer *o_ev, int *maskval, int maxev, int *o_nev) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpiogs_(&io, o_ev, maskval, &maxev, o_nev);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

/*
** Special wrapper routines c_qpio_put() and c_qpio_get()
*/
void c_qpio_put(IRAFPointer io, char **buffer, int numrows) {
        int i;
        IRAFPointer *rows;

	rows = (IRAFPointer *) malloc (numrows * sizeof(IRAFPointer));
        for (i = 0; i < numrows; i++) 
	    rows[i] = c_IrafPointer(buffer[i]);
        c_qpio_putevents(io, rows, numrows);

        free (rows);
}

int c_qpio_get(IRAFPointer io, int buffer_size, char **IrafRowPtrs, 
               int maskval, int maxev, int o_nev) {
        int i, rtn;
        IRAFPointer *IrafRowBuffers;

	IrafRowBuffers = (IRAFPointer *) calloc (buffer_size, sizeof(IRAFPointer));
	rtn = c_qpio_getevents(io, IrafRowBuffers, &maskval, maxev, &o_nev);

        for (i = 0; i < o_nev; i++) 
	    IrafRowPtrs[i] = c_IrafString(IrafRowBuffers[i]);

        free (IrafRowBuffers);

        return (rtn);
}
/*
** End special wrappers
*/

# if defined(NO_UNDERSCORE)
# define qpiori_ qpiori
# endif
	extern int qpiori_(IRAFPointer *, int *, int *, int *, int *, int *, int *);
int c_qpio_readpixi(IRAFPointer io, int *obuf, int *vs, int *ve, int ndim, int xblock, int yblock) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpiori_(&io, obuf, vs, ve, &ndim, &xblock, &yblock);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpiors_ qpiors
# endif
	extern int qpiors_(IRAFPointer *, short *, int *, int *, int *, int *, int *);
int c_qpio_readpixs(IRAFPointer io, short *obuf, int *vs, int *ve, int ndim, int xblock, int yblock) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpiors_(&io, obuf, vs, ve, &ndim, &xblock, &yblock);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexon_ qpexon
# endif
	extern IRAFPointer qpexon_(IRAFPointer *, short *);
IRAFPointer c_qpex_open(IRAFPointer qp, char *expr) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpexon_(&qp, char2iraf(expr,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexmr_ qpexmr
# endif
	extern int qpexmr_(IRAFPointer *, short *);
int c_qpex_modfilter(IRAFPointer ex, char *exprlist) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpexmr_(&ex, char2iraf(exprlist,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexgr_ qpexgr
# endif
	extern int qpexgr_(IRAFPointer *, short *, int *);
int c_qpex_getfilter(IRAFPointer ex, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = qpexgr_(&ex, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexge_ qpexge
# endif
	extern int qpexge_(IRAFPointer *, short *, short *, int *);
int c_qpex_getattribute(IRAFPointer ex, char *attribute, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = qpexge_(&ex, char2iraf(attribute,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexad_ qpexad
# endif
	extern int qpexad_(IRAFPointer *, short *, IRAFPointer *, IRAFPointer *, int *);
int c_qpex_attrld(IRAFPointer ex, char *attribute, IRAFPointer *xs, IRAFPointer *xe, int *xlen) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpexad_(&ex, char2iraf(attribute,1), xs, xe, xlen);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexai_ qpexai
# endif
	extern int qpexai_(IRAFPointer *, short *, IRAFPointer *, IRAFPointer *, int *);
int c_qpex_attrli(IRAFPointer ex, char *attribute, IRAFPointer *xs, IRAFPointer *xe, int *xlen) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpexai_(&ex, char2iraf(attribute,1), xs, xe, xlen);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexar_ qpexar
# endif
	extern int qpexar_(IRAFPointer *, short *, IRAFPointer *, IRAFPointer *, int *);
int c_qpex_attrlr(IRAFPointer ex, char *attribute, IRAFPointer *xs, IRAFPointer *xe, int *xlen) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpexar_(&ex, char2iraf(attribute,1), xs, xe, xlen);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexee_ qpexee
# endif
	extern int qpexee_(IRAFPointer *, IRAFPointer *, IRAFPointer *, int *);
int c_qpex_evaluate(IRAFPointer ex, IRAFPointer *i_ev, IRAFPointer *o_ev, int nev) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = qpexee_(&ex, i_ev, o_ev, &nev);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define qpexce_ qpexce
# endif
	extern void qpexce_(IRAFPointer *);
void c_qpex_close(IRAFPointer ex) {
	clear_cvoserr();
	xerpsh_();
	qpexce_(&ex);
	if (xerpoi_())
	    set_cvoserr();
}

