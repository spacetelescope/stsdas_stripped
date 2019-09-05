# include "c_iraf_priv.h"
# include <c_iraf.h>
# include <stdlib.h>

/*  The float and double clget and clput routines in cvos$xclio.c do not
 *  work properly. They return/put garbage. But if we rename and compile 
 *  them locally they work.
 *
 *                                                         IB, 14/10/96
 */

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

# if defined(NO_UNDERSCORE)
# define clppsr_ clppsr
# endif
        extern void clppsr_(IRAFPointer *, short *, float *);
void l_clppsetf(IRAFPointer pp, char *parname, float rval) {
        clear_cvoserr();
        xerpsh_();
        clppsr_(&pp, char2iraf(parname,1), &rval);
        if (xerpoi_())
            set_cvoserr();
}


