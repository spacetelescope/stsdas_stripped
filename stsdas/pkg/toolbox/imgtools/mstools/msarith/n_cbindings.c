# include <c_iraf_priv.h>

/*  These are temporary C bindings for:

     iraf$pkg/xtools/isdir.x
     fio$fnldir.x

      Revision history:
      ---------------
      01 Mar 96  -  Implementation (IB)

*/

#ifdef NO_UNDERSCORE
	extern int isdiry(short *, short *, int *);
#else
	extern int isdiry_(short *, short *, int *);
#endif
int c_isdirectory(char *param, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
#ifdef NO_UNDERSCORE
	rtn = isdiry(char2iraf(param,1), CH2I_buffer[2], &maxch);
#else
	rtn = isdiry_(char2iraf(param,1), CH2I_buffer[2], &maxch);
#endif
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
	return rtn;
}

#ifdef NO_UNDERSCORE
	extern int fnldir(short *, short *, int *);
#else
	extern int fnldir_(short *, short *, int *);
#endif
int c_fnldir(char *param, char *outstr, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
#ifdef NO_UNDERSCORE
	rtn = fnldir(char2iraf(param,1), CH2I_buffer[2], &maxch);
#else
	rtn = fnldir_(char2iraf(param,1), CH2I_buffer[2], &maxch);
#endif
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(outstr,maxch,2);
	return rtn;
}


/*  The float and double clget and clput routines in cvos$xclio.c do not
 *  work properly. They return/put garbage. But if we rename and compile 
 *  them locally they work.
 *
 *                                                         IB, 21/10/96
 */
# if defined(NATIVE_IRAF)
# if defined(NO_UNDERSCORE)
# define clgetr_ clgetr
# endif

          extern float clgetr_(short *);
float l_clgetf(char *param) {
        float rtn;
        clear_cvoserr();
        xerpsh_();
        rtn = clgetr_(char2iraf(param,1));
        if (xerpoi_())
            set_cvoserr();
        return rtn;
}
# endif

