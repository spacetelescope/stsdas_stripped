# include <xxtools.h>
# include <c_iraf_priv.h>
# include <stdlib.h>

# if defined(NO_UNDERSCORE)
# define decods_ decods
# endif
	extern int decods_(short *, int *, int *, int *);
int c_decode_ranges(char *range_string, int *ranges, int max_ranges, int *nvalues) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = decods_(char2iraf(range_string,1), ranges, &max_ranges, nvalues);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define getner_ getner
# endif
	extern int getner_(int *, int *);
int c_get_next_number(int *ranges, int *number) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = getner_(ranges, number);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define getprr_ getprr
# endif
	extern int getprr_(int *, int *);
int c_get_previous_number(int *ranges, int *number) {
	int rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = getprr_(ranges, number);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define isinre_ isinre
# endif
	extern Bool isinre_(int *, int *);
Bool c_is_in_range(int *ranges, int number) {
	Bool rtn;
	clear_cvoserr();
	xerpsh_();
	rtn = isinre_(ranges, &number);
	if (xerpoi_())
	    set_cvoserr();
	return rtn;
}

# if defined(NO_UNDERSCORE)
# define isdiry_ isdiry
# endif
	extern int isdiry_(short *, short *, int *);
int c_isdirectory(char *vfn, char *pathname, int maxch) {
	int rtn;
	CH2I_chk_buffer(2,maxch);
	clear_cvoserr();
	xerpsh_();
	rtn = isdiry_(char2iraf(vfn,1), CH2I_buffer[2], &maxch);
	if (xerpoi_())
	    set_cvoserr();
	iraf2char(pathname,maxch,2);
	return rtn;
}

