# include <xtables.h>
# include <c_iraf_priv.h>
# include <stdlib.h>
# if defined(NO_UNDERSCORE)
# define tbtopn_ tbtopn
# endif
	extern IRAFPointer tbtopn_(short *, int *, IRAFPointer *);
IRAFPointer c_tbtopn(char *tablename, int iomode, IRAFPointer oldname) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbtopn_(char2iraf(tablename,1), &iomode, &oldname);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbtcre_ tbtcre
# endif
	extern void tbtcre_(IRAFPointer *);
void c_tbtcre(IRAFPointer tp) {
	clear_cvoserr();
	xerpsh_();
	tbtcre_(&tp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbtclo_ tbtclo
# endif
	extern void tbtclo_(IRAFPointer *);
void c_tbtclo(IRAFPointer tp) {
	clear_cvoserr();
	xerpsh_();
	tbtclo_(&tp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcdef_ tbcdef
# endif
	extern void tbcdef_(IRAFPointer *, IRAFPointer *, short **, short **, short **, int *, int *, int *);
void c_tbcdef(IRAFPointer tp, IRAFPointer *colptr, char **colname, char **colunits, char **colfmt, int *datatype, int *lenarray, int numcols) {
	clear_cvoserr();
	xerpsh_();
	tbcdef_(&tp, colptr, twodchar2iraf(colname,SZ_COLNAME,numcols,1), twodchar2iraf(colunits,SZ_COLUNITS,numcols,2), twodchar2iraf(colfmt,SZ_COLFMT,numcols,3), datatype, lenarray, &numcols);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcde1_ tbcde1
# endif
	extern void tbcde1_(IRAFPointer *, IRAFPointer *, short *, short *, short *, int *, int *);
void c_tbcdef1(IRAFPointer tp, IRAFPointer *colptr, char *colname, char *colunits, char *colfmt, int datatype, int nelem) {
	clear_cvoserr();
	xerpsh_();
	tbcde1_(&tp, colptr, char2iraf(colname,1), char2iraf(colunits,2), char2iraf(colfmt,3), &datatype, &nelem);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcfnd_ tbcfnd
# endif
	extern void tbcfnd_(IRAFPointer *, short **, IRAFPointer *, int *);
void c_tbcfnd(IRAFPointer tp, char **colname, IRAFPointer *colptr, int numcols) {
	clear_cvoserr();
	xerpsh_();
	tbcfnd_(&tp, twodchar2iraf(colname,SZ_COLNAME,numcols,1), colptr, &numcols);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcfn1_ tbcfn1
# endif
	extern void tbcfn1_(IRAFPointer *, short *, IRAFPointer *);
void c_tbcfnd1(IRAFPointer tp, char *colname, IRAFPointer *colptr) {
	clear_cvoserr();
	xerpsh_();
	tbcfn1_(&tp, char2iraf(colname,1), colptr);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcinf_ tbcinf
# endif
	extern void tbcinf_(IRAFPointer *, int *, short *, short *, short *, int *, int *, int *);
void c_tbcinf(IRAFPointer colptr, int *colnum, char *colname, char *colunits, char *colfmt, int *datatype, int *lendata, int *lenfmt) {
	CH2I_chk_buffer(1,SZ_COLNAME);
	CH2I_chk_buffer(2,SZ_COLUNITS);
	CH2I_chk_buffer(3,SZ_COLFMT);
	clear_cvoserr();
	xerpsh_();
	tbcinf_(&colptr, colnum, CH2I_buffer[1], CH2I_buffer[2], CH2I_buffer[3], datatype, lendata, lenfmt);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(colname,SZ_COLNAME,1);
	iraf2char(colunits,SZ_COLUNITS,2);
	iraf2char(colfmt,SZ_COLFMT,3);
}

# if defined(NO_UNDERSCORE)
# define tbcigi_ tbcigi
# endif
	extern int tbcigi_(IRAFPointer *, int *);
int c_tbcigi(IRAFPointer cptr, int get_what) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbcigi_(&cptr, &get_what);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbciga_ tbciga
# endif
	extern void tbciga_(IRAFPointer *, IRAFPointer *, int *, int *, int *);
void c_tbciga(IRAFPointer tp, IRAFPointer cp, int *ndim, int *axlen, int maxdim) {
	clear_cvoserr();
	xerpsh_();
	tbciga_(&tp, &cp, ndim, axlen, &maxdim);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcigt_ tbcigt
# endif
	extern void tbcigt_(IRAFPointer *, int *, short *, int *);
void c_tbcigt(IRAFPointer cptr, int get_what, char *outstr, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	tbcigt_(&cptr, &get_what, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define tbpset_ tbpset
# endif
	extern void tbpset_(IRAFPointer *, int *, int *);
void c_tbpset(IRAFPointer tp, int setwhat, int value) {
	clear_cvoserr();
	xerpsh_();
	tbpset_(&tp, &setwhat, &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbpsta_ tbpsta
# endif
	extern int tbpsta_(IRAFPointer *, int *);
int c_tbpsta(IRAFPointer tp, int param) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbpsta_(&tp, &param);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbcdes_ tbcdes
# endif
	extern IRAFPointer tbcdes_(IRAFPointer *, IRAFPointer *);
IRAFPointer c_tbcdes(IRAFPointer tp, IRAFPointer cp) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbcdes_(&tp, &cp);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbsirw_ tbsirw
# endif
	extern void tbsirw_(IRAFPointer *, int *, int *);
void c_tbsirow(IRAFPointer tp, int selrow, int *rownum) {
	clear_cvoserr();
	xerpsh_();
	tbsirw_(&tp, &selrow, rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbegtb_ tbegtb
# endif
	extern void tbegtb_(IRAFPointer *, IRAFPointer *, int *, Bool *);
void c_tbegtb(IRAFPointer tp, IRAFPointer cptr, int rownum, Bool *buffer) {
	clear_cvoserr();
	xerpsh_();
	tbegtb_(&tp, &cptr, &rownum, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbegtd_ tbegtd
# endif
	extern void tbegtd_(IRAFPointer *, IRAFPointer *, int *, double *);
void c_tbegtd(IRAFPointer tp, IRAFPointer cptr, int rownum, double *buffer) {
	clear_cvoserr();
	xerpsh_();
	tbegtd_(&tp, &cptr, &rownum, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbegtr_ tbegtr
# endif
	extern void tbegtr_(IRAFPointer *, IRAFPointer *, int *, float *);
void c_tbegtr(IRAFPointer tp, IRAFPointer cptr, int rownum, float *buffer) {
	clear_cvoserr();
	xerpsh_();
	tbegtr_(&tp, &cptr, &rownum, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbegti_ tbegti
# endif
	extern void tbegti_(IRAFPointer *, IRAFPointer *, int *, int *);
void c_tbegti(IRAFPointer tp, IRAFPointer cptr, int rownum, int *buffer) {
	clear_cvoserr();
	xerpsh_();
	tbegti_(&tp, &cptr, &rownum, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbegts_ tbegts
# endif
	extern void tbegts_(IRAFPointer *, IRAFPointer *, int *, short *);
void c_tbegts(IRAFPointer tp, IRAFPointer cptr, int rownum, short *buffer) {
	clear_cvoserr();
	xerpsh_();
	tbegts_(&tp, &cptr, &rownum, buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbegtt_ tbegtt
# endif
	extern void tbegtt_(IRAFPointer *, IRAFPointer *, int *, short *, int *);
void c_tbegtt(IRAFPointer tp, IRAFPointer cptr, int rownum, char *buffer, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	tbegtt_(&tp, &cptr, &rownum, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(buffer,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define tbeptb_ tbeptb
# endif
	extern void tbeptb_(IRAFPointer *, IRAFPointer *, int *, Bool *);
void c_tbeptb(IRAFPointer tp, IRAFPointer cptr, int rownum, Bool buffer) {
	clear_cvoserr();
	xerpsh_();
	tbeptb_(&tp, &cptr, &rownum, &buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbeptd_ tbeptd
# endif
	extern void tbeptd_(IRAFPointer *, IRAFPointer *, int *, double *);
void c_tbeptd(IRAFPointer tp, IRAFPointer cptr, int rownum, double buffer) {
	clear_cvoserr();
	xerpsh_();
	tbeptd_(&tp, &cptr, &rownum, &buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbeptr_ tbeptr
# endif
	extern void tbeptr_(IRAFPointer *, IRAFPointer *, int *, float *);
void c_tbeptr(IRAFPointer tp, IRAFPointer cptr, int rownum, float buffer) {
	clear_cvoserr();
	xerpsh_();
	tbeptr_(&tp, &cptr, &rownum, &buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbepti_ tbepti
# endif
	extern void tbepti_(IRAFPointer *, IRAFPointer *, int *, int *);
void c_tbepti(IRAFPointer tp, IRAFPointer cptr, int rownum, int buffer) {
	clear_cvoserr();
	xerpsh_();
	tbepti_(&tp, &cptr, &rownum, &buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbepts_ tbepts
# endif
	extern void tbepts_(IRAFPointer *, IRAFPointer *, int *, short *);
void c_tbepts(IRAFPointer tp, IRAFPointer cptr, int rownum, short buffer) {
	clear_cvoserr();
	xerpsh_();
	tbepts_(&tp, &cptr, &rownum, &buffer);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbeptt_ tbeptt
# endif
	extern void tbeptt_(IRAFPointer *, IRAFPointer *, int *, short *);
void c_tbeptt(IRAFPointer tp, IRAFPointer cptr, int rownum, char *buffer) {
	clear_cvoserr();
	xerpsh_();
	tbeptt_(&tp, &cptr, &rownum, char2iraf(buffer,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbagtd_ tbagtd
# endif
	extern int tbagtd_(IRAFPointer *, IRAFPointer *, int *, double *, int *, int *);
int c_tbagtd(IRAFPointer tp, IRAFPointer cp, int row, double *buffer, int first, int nelem) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbagtd_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbagtr_ tbagtr
# endif
	extern int tbagtr_(IRAFPointer *, IRAFPointer *, int *, float *, int *, int *);
int c_tbagtr(IRAFPointer tp, IRAFPointer cp, int row, float *buffer, int first, int nelem) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbagtr_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbagti_ tbagti
# endif
	extern int tbagti_(IRAFPointer *, IRAFPointer *, int *, int *, int *, int *);
int c_tbagti(IRAFPointer tp, IRAFPointer cp, int row, int *buffer, int first, int nelem) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbagti_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbagts_ tbagts
# endif
	extern int tbagts_(IRAFPointer *, IRAFPointer *, int *, short *, int *, int *);
int c_tbagts(IRAFPointer tp, IRAFPointer cp, int row, short *buffer, int first, int nelem) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbagts_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbagtb_ tbagtb
# endif
	extern int tbagtb_(IRAFPointer *, IRAFPointer *, int *, Bool *, int *, int *);
int c_tbagtb(IRAFPointer tp, IRAFPointer cp, int row, Bool *buffer, int first, int nelem) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbagtb_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbagtt_ tbagtt
# endif
	extern int tbagtt_(IRAFPointer *, IRAFPointer *, int *, short **, int *, int *, int *);
int c_tbagtt(IRAFPointer tp, IRAFPointer cp, int row, char **cbuf, int maxch, int first, int nelem) {
	int rtn;
	CH2I_chk_buffer(1,(maxch + 1) * nelem);
	clear_cvoserr();
	xerpsh_();
	rtn = tbagtt_(&tp, &cp, &row, (short **)CH2I_buffer[1], &maxch, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
	iraf2twodchar(cbuf,maxch,nelem,1);
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbaptd_ tbaptd
# endif
	extern void tbaptd_(IRAFPointer *, IRAFPointer *, int *, double *, int *, int *);
void c_tbaptd(IRAFPointer tp, IRAFPointer cp, int row, double *buffer, int first, int nelem) {
	clear_cvoserr();
	xerpsh_();
	tbaptd_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbaptr_ tbaptr
# endif
	extern void tbaptr_(IRAFPointer *, IRAFPointer *, int *, float *, int *, int *);
void c_tbaptr(IRAFPointer tp, IRAFPointer cp, int row, float *buffer, int first, int nelem) {
	clear_cvoserr();
	xerpsh_();
	tbaptr_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbapti_ tbapti
# endif
	extern void tbapti_(IRAFPointer *, IRAFPointer *, int *, int *, int *, int *);
void c_tbapti(IRAFPointer tp, IRAFPointer cp, int row, int *buffer, int first, int nelem) {
	clear_cvoserr();
	xerpsh_();
	tbapti_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbapts_ tbapts
# endif
	extern void tbapts_(IRAFPointer *, IRAFPointer *, int *, short *, int *, int *);
void c_tbapts(IRAFPointer tp, IRAFPointer cp, int row, short *buffer, int first, int nelem) {
	clear_cvoserr();
	xerpsh_();
	tbapts_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbaptb_ tbaptb
# endif
	extern void tbaptb_(IRAFPointer *, IRAFPointer *, int *, Bool *, int *, int *);
void c_tbaptb(IRAFPointer tp, IRAFPointer cp, int row, Bool *buffer, int first, int nelem) {
	clear_cvoserr();
	xerpsh_();
	tbaptb_(&tp, &cp, &row, buffer, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbaptt_ tbaptt
# endif
	extern void tbaptt_(IRAFPointer *, IRAFPointer *, int *, short **, int *, int *, int *);
void c_tbaptt(IRAFPointer tp, IRAFPointer cp, int row, char **cbuf, int maxch, int first, int nelem) {
	clear_cvoserr();
	xerpsh_();
	tbaptt_(&tp, &cp, &row, twodchar2iraf(cbuf,maxch,nelem,1), &maxch, &first, &nelem);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrgtb_ tbrgtb
# endif
	extern void tbrgtb_(IRAFPointer *, IRAFPointer *, Bool *, Bool *, int *, int *);
void c_tbrgtb(IRAFPointer tp, IRAFPointer *cp, Bool *buffer, Bool *nullflag, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrgtb_(&tp, cp, buffer, nullflag, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrgtd_ tbrgtd
# endif
	extern void tbrgtd_(IRAFPointer *, IRAFPointer *, double *, Bool *, int *, int *);
void c_tbrgtd(IRAFPointer tp, IRAFPointer *cp, double *buffer, Bool *nullflag, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrgtd_(&tp, cp, buffer, nullflag, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrgtr_ tbrgtr
# endif
	extern void tbrgtr_(IRAFPointer *, IRAFPointer *, float *, Bool *, int *, int *);
void c_tbrgtr(IRAFPointer tp, IRAFPointer *cp, float *buffer, Bool *nullflag, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrgtr_(&tp, cp, buffer, nullflag, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrgti_ tbrgti
# endif
	extern void tbrgti_(IRAFPointer *, IRAFPointer *, int *, Bool *, int *, int *);
void c_tbrgti(IRAFPointer tp, IRAFPointer *cp, int *buffer, Bool *nullflag, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrgti_(&tp, cp, buffer, nullflag, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrgts_ tbrgts
# endif
	extern void tbrgts_(IRAFPointer *, IRAFPointer *, short *, Bool *, int *, int *);
void c_tbrgts(IRAFPointer tp, IRAFPointer *cp, short *buffer, Bool *nullflag, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrgts_(&tp, cp, buffer, nullflag, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrgtt_ tbrgtt
# endif
	extern void tbrgtt_(IRAFPointer *, IRAFPointer *, short **, Bool *, int *, int *, int *);
void c_tbrgtt(IRAFPointer tp, IRAFPointer *cp, char **buffer, Bool *nullflag, int lenstr, int numcols, int rownum) {
	CH2I_chk_buffer(1,(lenstr + 1) * numcols);
	clear_cvoserr();
	xerpsh_();
	tbrgtt_(&tp, cp, (short **)CH2I_buffer[1], nullflag, &lenstr, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
	iraf2twodchar(buffer,lenstr,numcols,1);
}

# if defined(NO_UNDERSCORE)
# define tbrptb_ tbrptb
# endif
	extern void tbrptb_(IRAFPointer *, IRAFPointer *, Bool *, int *, int *);
void c_tbrptb(IRAFPointer tp, IRAFPointer *cp, Bool *buffer, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrptb_(&tp, cp, buffer, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrptd_ tbrptd
# endif
	extern void tbrptd_(IRAFPointer *, IRAFPointer *, double *, int *, int *);
void c_tbrptd(IRAFPointer tp, IRAFPointer *cp, double *buffer, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrptd_(&tp, cp, buffer, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrptr_ tbrptr
# endif
	extern void tbrptr_(IRAFPointer *, IRAFPointer *, float *, int *, int *);
void c_tbrptr(IRAFPointer tp, IRAFPointer *cp, float *buffer, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrptr_(&tp, cp, buffer, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrpti_ tbrpti
# endif
	extern void tbrpti_(IRAFPointer *, IRAFPointer *, int *, int *, int *);
void c_tbrpti(IRAFPointer tp, IRAFPointer *cp, int *buffer, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrpti_(&tp, cp, buffer, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrpts_ tbrpts
# endif
	extern void tbrpts_(IRAFPointer *, IRAFPointer *, short *, int *, int *);
void c_tbrpts(IRAFPointer tp, IRAFPointer *cp, short *buffer, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrpts_(&tp, cp, buffer, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrptt_ tbrptt
# endif
	extern void tbrptt_(IRAFPointer *, IRAFPointer *, short **, int *, int *, int *);
void c_tbrptt(IRAFPointer tp, IRAFPointer *cp, char **buffer, int lenstr, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrptt_(&tp, cp, twodchar2iraf(buffer,lenstr,numcols,1), &lenstr, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcgtb_ tbcgtb
# endif
	extern void tbcgtb_(IRAFPointer *, IRAFPointer *, Bool *, Bool *, int *, int *);
void c_tbcgtb(IRAFPointer tp, IRAFPointer cp, Bool *buffer, Bool *nullflag, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcgtb_(&tp, &cp, buffer, nullflag, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcgtd_ tbcgtd
# endif
	extern void tbcgtd_(IRAFPointer *, IRAFPointer *, double *, Bool *, int *, int *);
void c_tbcgtd(IRAFPointer tp, IRAFPointer cp, double *buffer, Bool *nullflag, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcgtd_(&tp, &cp, buffer, nullflag, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcgtr_ tbcgtr
# endif
	extern void tbcgtr_(IRAFPointer *, IRAFPointer *, float *, Bool *, int *, int *);
void c_tbcgtr(IRAFPointer tp, IRAFPointer cp, float *buffer, Bool *nullflag, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcgtr_(&tp, &cp, buffer, nullflag, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcgti_ tbcgti
# endif
	extern void tbcgti_(IRAFPointer *, IRAFPointer *, int *, Bool *, int *, int *);
void c_tbcgti(IRAFPointer tp, IRAFPointer cp, int *buffer, Bool *nullflag, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcgti_(&tp, &cp, buffer, nullflag, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcgts_ tbcgts
# endif
	extern void tbcgts_(IRAFPointer *, IRAFPointer *, short *, Bool *, int *, int *);
void c_tbcgts(IRAFPointer tp, IRAFPointer cp, short *buffer, Bool *nullflag, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcgts_(&tp, &cp, buffer, nullflag, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcgtt_ tbcgtt
# endif
	extern void tbcgtt_(IRAFPointer *, IRAFPointer *, short **, Bool *, int *, int *, int *);
void c_tbcgtt(IRAFPointer tp, IRAFPointer cp, char **buffer, Bool *nullflag, int lenstr, int firstrow, int lastrow) {
	CH2I_chk_buffer(1,(lenstr + 1) * (lastrow-firstrow+1));
	clear_cvoserr();
	xerpsh_();
	tbcgtt_(&tp, &cp, (short **)CH2I_buffer[1], nullflag, &lenstr, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
	iraf2twodchar(buffer,lenstr,(lastrow-firstrow+1),1);
}

# if defined(NO_UNDERSCORE)
# define tbcptb_ tbcptb
# endif
	extern void tbcptb_(IRAFPointer *, IRAFPointer *, Bool *, int *, int *);
void c_tbcptb(IRAFPointer tp, IRAFPointer cp, Bool *buffer, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcptb_(&tp, &cp, buffer, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcptd_ tbcptd
# endif
	extern void tbcptd_(IRAFPointer *, IRAFPointer *, double *, int *, int *);
void c_tbcptd(IRAFPointer tp, IRAFPointer cp, double *buffer, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcptd_(&tp, &cp, buffer, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcptr_ tbcptr
# endif
	extern void tbcptr_(IRAFPointer *, IRAFPointer *, float *, int *, int *);
void c_tbcptr(IRAFPointer tp, IRAFPointer cp, float *buffer, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcptr_(&tp, &cp, buffer, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcpti_ tbcpti
# endif
	extern void tbcpti_(IRAFPointer *, IRAFPointer *, int *, int *, int *);
void c_tbcpti(IRAFPointer tp, IRAFPointer cp, int *buffer, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcpti_(&tp, &cp, buffer, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcpts_ tbcpts
# endif
	extern void tbcpts_(IRAFPointer *, IRAFPointer *, short *, int *, int *);
void c_tbcpts(IRAFPointer tp, IRAFPointer cp, short *buffer, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcpts_(&tp, &cp, buffer, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcptt_ tbcptt
# endif
	extern void tbcptt_(IRAFPointer *, IRAFPointer *, short **, int *, int *, int *);
void c_tbcptt(IRAFPointer tp, IRAFPointer cp, char **buffer, int lenstr, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbcptt_(&tp, &cp, twodchar2iraf(buffer,lenstr,(lastrow-firstrow+1),1), &lenstr, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrudf_ tbrudf
# endif
	extern void tbrudf_(IRAFPointer *, IRAFPointer *, int *, int *);
void c_tbrudf(IRAFPointer tp, IRAFPointer *colptr, int numcols, int rownum) {
	clear_cvoserr();
	xerpsh_();
	tbrudf_(&tp, colptr, &numcols, &rownum);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhgtb_ tbhgtb
# endif
	extern Bool tbhgtb_(IRAFPointer *, short *);
Bool c_tbhgtb(IRAFPointer tp, char *keyword) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbhgtb_(&tp, char2iraf(keyword,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbhgtd_ tbhgtd
# endif
	extern double tbhgtd_(IRAFPointer *, short *);
double c_tbhgtd(IRAFPointer tp, char *keyword) {
	double rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbhgtd_(&tp, char2iraf(keyword,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbhgti_ tbhgti
# endif
	extern int tbhgti_(IRAFPointer *, short *);
int c_tbhgti(IRAFPointer tp, char *keyword) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbhgti_(&tp, char2iraf(keyword,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbhgtr_ tbhgtr
# endif
	extern float tbhgtr_(IRAFPointer *, short *);
float c_tbhgtr(IRAFPointer tp, char *keyword) {
	float rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbhgtr_(&tp, char2iraf(keyword,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbhgtt_ tbhgtt
# endif
	extern void tbhgtt_(IRAFPointer *, short *, short *, int *);
void c_tbhgtt(IRAFPointer tp, char *keyword, char *text, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	tbhgtt_(&tp, char2iraf(keyword,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(text,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define tbhadb_ tbhadb
# endif
	extern void tbhadb_(IRAFPointer *, short *, Bool *);
void c_tbhadb(IRAFPointer tp, char *keyword, Bool value) {
	clear_cvoserr();
	xerpsh_();
	tbhadb_(&tp, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhadd_ tbhadd
# endif
	extern void tbhadd_(IRAFPointer *, short *, double *);
void c_tbhadd(IRAFPointer tp, char *keyword, double value) {
	clear_cvoserr();
	xerpsh_();
	tbhadd_(&tp, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhadi_ tbhadi
# endif
	extern void tbhadi_(IRAFPointer *, short *, int *);
void c_tbhadi(IRAFPointer tp, char *keyword, int value) {
	clear_cvoserr();
	xerpsh_();
	tbhadi_(&tp, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhadr_ tbhadr
# endif
	extern void tbhadr_(IRAFPointer *, short *, float *);
void c_tbhadr(IRAFPointer tp, char *keyword, float value) {
	clear_cvoserr();
	xerpsh_();
	tbhadr_(&tp, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhadt_ tbhadt
# endif
	extern void tbhadt_(IRAFPointer *, short *, short *);
void c_tbhadt(IRAFPointer tp, char *keyword, char *text) {
	clear_cvoserr();
	xerpsh_();
	tbhadt_(&tp, char2iraf(keyword,1), char2iraf(text,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhptb_ tbhptb
# endif
	extern void tbhptb_(IRAFPointer *, short *, Bool *);
void c_tbhptb(IRAFPointer tp, char *keyword, Bool value) {
	clear_cvoserr();
	xerpsh_();
	tbhptb_(&tp, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhptd_ tbhptd
# endif
	extern void tbhptd_(IRAFPointer *, short *, double *);
void c_tbhptd(IRAFPointer tp, char *keyword, double value) {
	clear_cvoserr();
	xerpsh_();
	tbhptd_(&tp, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhpti_ tbhpti
# endif
	extern void tbhpti_(IRAFPointer *, short *, int *);
void c_tbhpti(IRAFPointer tp, char *keyword, int value) {
	clear_cvoserr();
	xerpsh_();
	tbhpti_(&tp, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhptr_ tbhptr
# endif
	extern void tbhptr_(IRAFPointer *, short *, float *);
void c_tbhptr(IRAFPointer tp, char *keyword, float value) {
	clear_cvoserr();
	xerpsh_();
	tbhptr_(&tp, char2iraf(keyword,1), &value);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhptt_ tbhptt
# endif
	extern void tbhptt_(IRAFPointer *, short *, short *);
void c_tbhptt(IRAFPointer tp, char *keyword, char *text) {
	clear_cvoserr();
	xerpsh_();
	tbhptt_(&tp, char2iraf(keyword,1), char2iraf(text,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhcal_ tbhcal
# endif
	extern void tbhcal_(IRAFPointer *, IRAFPointer *);
void c_tbhcal(IRAFPointer itp, IRAFPointer otp) {
	clear_cvoserr();
	xerpsh_();
	tbhcal_(&itp, &otp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbhgnp_ tbhgnp
# endif
	extern void tbhgnp_(IRAFPointer *, int *, short *, int *, short *);
void c_tbhgnp(IRAFPointer tp, int parnum, char *keyword, int *dtype, char *str) {
	CH2I_chk_buffer(1,SZ_KEYWORD);
	CH2I_chk_buffer(2,SZ_PARREC);
	clear_cvoserr();
	xerpsh_();
	tbhgnp_(&tp, &parnum, CH2I_buffer[1], dtype, CH2I_buffer[2]);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(keyword,SZ_KEYWORD,1);
	iraf2char(str,SZ_PARREC,2);
}

# if defined(NO_UNDERSCORE)
# define tbhgcm_ tbhgcm
# endif
	extern void tbhgcm_(IRAFPointer *, short *, short *, int *);
void c_tbhgcm(IRAFPointer tp, char *keyword, char *comment, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	tbhgcm_(&tp, char2iraf(keyword,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(comment,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define tbhpcm_ tbhpcm
# endif
	extern void tbhpcm_(IRAFPointer *, short *, short *);
void c_tbhpcm(IRAFPointer tp, char *keyword, char *comment) {
	clear_cvoserr();
	xerpsh_();
	tbhpcm_(&tp, char2iraf(keyword,1), char2iraf(comment,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbfpri_ tbfpri
# endif
	extern void tbfpri_(short *, short *, int *);
void c_tbfpri(char *intable, char *outtable, int *copied) {
	clear_cvoserr();
	xerpsh_();
	tbfpri_(char2iraf(intable,1), char2iraf(outtable,2), copied);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbtcpy_ tbtcpy
# endif
	extern void tbtcpy_(short *, short *);
void c_tbtcpy(char *intable, char *outtable) {
	clear_cvoserr();
	xerpsh_();
	tbtcpy_(char2iraf(intable,1), char2iraf(outtable,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbtdel_ tbtdel
# endif
	extern void tbtdel_(short *);
void c_tbtdel(char *table) {
	clear_cvoserr();
	xerpsh_();
	tbtdel_(char2iraf(table,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbtren_ tbtren
# endif
	extern void tbtren_(short *, short *);
void c_tbtren(char *intable, char *outtable) {
	clear_cvoserr();
	xerpsh_();
	tbtren_(char2iraf(intable,1), char2iraf(outtable,2));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbtacc_ tbtacc
# endif
	extern int tbtacc_(short *);
int c_tbtacc(char *tablename) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbtacc_(char2iraf(tablename,1));
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbtext_ tbtext
# endif
	extern void tbtext_(short *, short *, int *);
void c_tbtext(char *inname, char *outname, int maxch) {
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	tbtext_(char2iraf(inname,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outname,maxch,2);
}

# if defined(NO_UNDERSCORE)
# define tbtnam_ tbtnam
# endif
	extern void tbtnam_(IRAFPointer *, short *, int *);
void c_tbtnam(IRAFPointer tp, char *tblname, int maxch) {
	CH2I_chk_buffer(1,maxch);
	clear_cvoserr();
	xerpsh_();
	tbtnam_(&tp, CH2I_buffer[1], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(tblname,maxch,1);
}

# if defined(NO_UNDERSCORE)
# define tbtflu_ tbtflu
# endif
	extern void tbtflu_(IRAFPointer *);
void c_tbtflu(IRAFPointer tp) {
	clear_cvoserr();
	xerpsh_();
	tbtflu_(&tp);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbtchs_ tbtchs
# endif
	extern void tbtchs_(IRAFPointer *, int *, int *, int *, int *);
void c_tbtchs(IRAFPointer tp, int maxpar, int maxcols, int rowlen, int allrows) {
	clear_cvoserr();
	xerpsh_();
	tbtchs_(&tp, &maxpar, &maxcols, &rowlen, &allrows);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrcpy_ tbrcpy
# endif
	extern void tbrcpy_(IRAFPointer *, IRAFPointer *, int *, int *);
void c_tbrcpy(IRAFPointer itp, IRAFPointer otp, int iselrow, int oselrow) {
	clear_cvoserr();
	xerpsh_();
	tbrcpy_(&itp, &otp, &iselrow, &oselrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrcsc_ tbrcsc
# endif
	extern void tbrcsc_(IRAFPointer *, IRAFPointer *, IRAFPointer *, IRAFPointer *, int *, int *, int *);
void c_tbrcsc(IRAFPointer itp, IRAFPointer otp, IRAFPointer *icp, IRAFPointer *ocp, int irow, int orow, int ncols) {
	clear_cvoserr();
	xerpsh_();
	tbrcsc_(&itp, &otp, icp, ocp, &irow, &orow, &ncols);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrswp_ tbrswp
# endif
	extern void tbrswp_(IRAFPointer *, int *, int *);
void c_tbrswp(IRAFPointer tp, int row1, int row2) {
	clear_cvoserr();
	xerpsh_();
	tbrswp_(&tp, &row1, &row2);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbtsrt_ tbtsrt
# endif
	extern void tbtsrt_(IRAFPointer *, int *, IRAFPointer *, Bool *, int *, int *);
void c_tbtsrt(IRAFPointer tp, int numcols, IRAFPointer *colptr, Bool fold, int nindex, int *index) {
	clear_cvoserr();
	xerpsh_();
	tbtsrt_(&tp, &numcols, colptr, &fold, &nindex, index);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrdel_ tbrdel
# endif
	extern void tbrdel_(IRAFPointer *, int *, int *);
void c_tbrdel(IRAFPointer tp, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbrdel_(&tp, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbrnll_ tbrnll
# endif
	extern void tbrnll_(IRAFPointer *, int *, int *);
void c_tbrnll(IRAFPointer tp, int firstrow, int lastrow) {
	clear_cvoserr();
	xerpsh_();
	tbrnll_(&tp, &firstrow, &lastrow);
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcnam_ tbcnam
# endif
	extern void tbcnam_(IRAFPointer *, IRAFPointer *, short *);
void c_tbcnam(IRAFPointer tp, IRAFPointer cp, char *colname) {
	clear_cvoserr();
	xerpsh_();
	tbcnam_(&tp, &cp, char2iraf(colname,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcfmt_ tbcfmt
# endif
	extern void tbcfmt_(IRAFPointer *, IRAFPointer *, short *);
void c_tbcfmt(IRAFPointer tp, IRAFPointer cp, char *colfmt) {
	clear_cvoserr();
	xerpsh_();
	tbcfmt_(&tp, &cp, char2iraf(colfmt,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcnit_ tbcnit
# endif
	extern void tbcnit_(IRAFPointer *, IRAFPointer *, short *);
void c_tbcnit(IRAFPointer tp, IRAFPointer cp, char *colunits) {
	clear_cvoserr();
	xerpsh_();
	tbcnit_(&tp, &cp, char2iraf(colunits,1));
	if (xerpoi_())
	    set_cvoserr();
}

# if defined(NO_UNDERSCORE)
# define tbcnum_ tbcnum
# endif
	extern IRAFPointer tbcnum_(IRAFPointer *, int *);
IRAFPointer c_tbcnum(IRAFPointer tp, int colnum) {
	IRAFPointer rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = tbcnum_(&tp, &colnum);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define tbpare_ tbpare
# endif
	extern int tbpare_(short *, short *, short *, int *, int *);
int c_tbparse(char *tablename, char *fname, char *extname, int maxch, int *hdu) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	CH2I_chk_buffer(3,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = tbpare_(char2iraf(tablename,1), CH2I_buffer[2], CH2I_buffer[3], &maxch, hdu);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(fname,maxch,2);
	iraf2char(extname,maxch,3);
	return rtn;
}

