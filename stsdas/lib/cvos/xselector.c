# include <xselector.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define rdselt_ rdselt
# endif
	extern void rdselt_(short *, short *, short *, short *, int *);
void c_rdselect(char *file, char *root, char *rowselect, char *colselect, int maxch) {
	CH2I_chk_buffer(2,maxch);
	CH2I_chk_buffer(3,maxch);
	CH2I_chk_buffer(4,maxch);
	clear_cvoserr();
	xerpsh_();
	rdselt_(char2iraf(file,1), CH2I_buffer[2], CH2I_buffer[3], CH2I_buffer[4], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(root,maxch,2);
	iraf2char(rowselect,maxch,3);
	iraf2char(colselect,maxch,4);
}

# if defined(NO_UNDERSCORE)
# define tcsopn_ tcsopn
# endif
	extern void tcsopn_(IRAFPointer *, short *, IRAFPointer *, int *, int *);
void c_tcs_open(IRAFPointer tp, char *columns, IRAFPointer *descrip, int *ndescrip, int maxdescrip) {
	clear_cvoserr();
	xerpsh_();
	tcsopn_(&tp, char2iraf(columns,1), descrip, ndescrip, &maxdescrip);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tcscle_ tcscle
# endif
	extern void tcscle_(IRAFPointer *, int *);
void c_tcs_close(IRAFPointer *descrip, int ndescrip) {
	clear_cvoserr();
	xerpsh_();
	tcscle_(descrip, &ndescrip);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tcsshe_ tcsshe
# endif
	extern void tcsshe_(IRAFPointer *, int *, int *, int *);
void c_tcs_shape(IRAFPointer descrip, int *length, int *ndim, int maxdimen) {
	clear_cvoserr();
	xerpsh_();
	tcsshe_(&descrip, length, ndim, &maxdimen);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tcstoe_ tcstoe
# endif
	extern int tcstoe_(IRAFPointer *);
int c_tcs_totsize(IRAFPointer descrip) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tcstoe_(&descrip);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define trsopn_ trsopn
# endif
	extern IRAFPointer trsopn_(IRAFPointer *, short *);
IRAFPointer c_trsopen(IRAFPointer tp, char *expr) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = trsopn_(&tp, char2iraf(expr,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define trscle_ trscle
# endif
	extern void trscle_(IRAFPointer *);
void c_trsclose(IRAFPointer trs) {
	clear_cvoserr();
	xerpsh_();
	trscle_(&trs);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define trsevl_ trsevl
# endif
	extern Bool trsevl_(IRAFPointer *, int *, IRAFPointer *);
Bool c_trseval(IRAFPointer tp, int irow, IRAFPointer pcode) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = trsevl_(&tp, &irow, &pcode);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstcoy_ rstcoy
# endif
	extern IRAFPointer rstcoy_(IRAFPointer *);
IRAFPointer c_rst_copy(IRAFPointer set1) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rstcoy_(&set1);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstcre_ rstcre
# endif
	extern IRAFPointer rstcre_(int *, int *);
IRAFPointer c_rst_create(int loval, int hival) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rstcre_(&loval, &hival);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstfre_ rstfre
# endif
	extern void rstfre_(IRAFPointer *);
void c_rst_free(IRAFPointer set) {
	clear_cvoserr();
	xerpsh_();
	rstfre_(&set);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rstadl_ rstadl
# endif
	extern void rstadl_(IRAFPointer *, int *);
void c_rst_addval(IRAFPointer set, int value) {
	clear_cvoserr();
	xerpsh_();
	rstadl_(&set, &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rstdel_ rstdel
# endif
	extern void rstdel_(IRAFPointer *, int *);
void c_rst_delval(IRAFPointer *set, int value) {
	clear_cvoserr();
	xerpsh_();
	rstdel_(set, &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rstadb_ rstadb
# endif
	extern void rstadb_(IRAFPointer *, int *, int *);
void c_rst_addtab(IRAFPointer set, int loval, int nval) {
	clear_cvoserr();
	xerpsh_();
	rstadb_(&set, &loval, &nval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rstdeb_ rstdeb
# endif
	extern void rstdeb_(IRAFPointer *, int *, int *);
void c_rst_deltab(IRAFPointer *set, int loval, int nval) {
	clear_cvoserr();
	xerpsh_();
	rstdeb_(set, &loval, &nval);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define rstand_ rstand
# endif
	extern IRAFPointer rstand_(IRAFPointer *, IRAFPointer *);
IRAFPointer c_rst_and(IRAFPointer set1, IRAFPointer set2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rstand_(&set1, &set2);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstor_ rstor
# endif
	extern IRAFPointer rstor_(IRAFPointer *, IRAFPointer *);
IRAFPointer c_rst_or(IRAFPointer set1, IRAFPointer set2) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rstor_(&set1, &set2);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstnot_ rstnot
# endif
	extern IRAFPointer rstnot_(int *, IRAFPointer *);
IRAFPointer c_rst_not(int nrow, IRAFPointer set1) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rstnot_(&nrow, &set1);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstint_ rstint
# endif
	extern Bool rstint_(IRAFPointer *, int *);
Bool c_rst_inset(IRAFPointer set, int value) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rstint_(&set, &value);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstnem_ rstnem
# endif
	extern int rstnem_(IRAFPointer *);
int c_rst_nelem(IRAFPointer set) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rstnem_(&set);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstrom_ rstrom
# endif
	extern int rstrom_(IRAFPointer *, int *);
int c_rst_rownum(IRAFPointer set, int index) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = rstrom_(&set, &index);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define rstshw_ rstshw
# endif
	extern void rstshw_(IRAFPointer *, short *, int *);
void c_rst_show(IRAFPointer set, char *str, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	rstshw_(&set, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(str,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define tcsrdb_ tcsrdb
# endif
	extern void tcsrdb_(IRAFPointer *, IRAFPointer *, int *, int *, int *, Bool *);
void c_tcs_rdaryb(IRAFPointer tp, IRAFPointer descrip, int irow, int maxbuf, int *nbuf, Bool *buffer) {
	clear_cvoserr();
	xerpsh_();
	tcsrdb_(&tp, &descrip, &irow, &maxbuf, nbuf, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tcsrdt_ tcsrdt
# endif
	extern void tcsrdt_(IRAFPointer *, IRAFPointer *, int *, int *, int *, int *, short **);
void c_tcs_rdaryt(IRAFPointer tp, IRAFPointer descrip, int irow, int maxch, int maxbuf, int *nbuf, char **buffer) {
	CH2I_chk_buffer(1,(maxch + 1) * maxbuf);
	clear_cvoserr();
	xerpsh_();
	tcsrdt_(&tp, &descrip, &irow, &maxch, &maxbuf, nbuf, (short **)CH2I_buffer[1]);
	if (xerpoi_())
	    set_cvoserr();
	iraf2twodchar(buffer,maxch,maxbuf,1);
}

# if defined(NO_UNDERSCORE)
# define tcsrdd_ tcsrdd
# endif
	extern void tcsrdd_(IRAFPointer *, IRAFPointer *, int *, int *, int *, double *);
void c_tcs_rdaryd(IRAFPointer tp, IRAFPointer descrip, int irow, int maxbuf, int *nbuf, double *buffer) {
	clear_cvoserr();
	xerpsh_();
	tcsrdd_(&tp, &descrip, &irow, &maxbuf, nbuf, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tcsrdi_ tcsrdi
# endif
	extern void tcsrdi_(IRAFPointer *, IRAFPointer *, int *, int *, int *, int *);
void c_tcs_rdaryi(IRAFPointer tp, IRAFPointer descrip, int irow, int maxbuf, int *nbuf, int *buffer) {
	clear_cvoserr();
	xerpsh_();
	tcsrdi_(&tp, &descrip, &irow, &maxbuf, nbuf, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tcsrdr_ tcsrdr
# endif
	extern void tcsrdr_(IRAFPointer *, IRAFPointer *, int *, int *, int *, float *);
void c_tcs_rdaryr(IRAFPointer tp, IRAFPointer descrip, int irow, int maxbuf, int *nbuf, float *buffer) {
	clear_cvoserr();
	xerpsh_();
	tcsrdr_(&tp, &descrip, &irow, &maxbuf, nbuf, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tcsrds_ tcsrds
# endif
	extern void tcsrds_(IRAFPointer *, IRAFPointer *, int *, int *, int *, short *);
void c_tcs_rdarys(IRAFPointer tp, IRAFPointer descrip, int irow, int maxbuf, int *nbuf, short *buffer) {
	clear_cvoserr();
	xerpsh_();
	tcsrds_(&tp, &descrip, &irow, &maxbuf, nbuf, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

