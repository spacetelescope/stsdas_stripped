/*
** Allen Farris - Original Implementation.
** M.D. De La Pena 27 January 1998 - Removed setting of NO_UNDERSCORE macro.
** M.D. De La Pena 09 March 1998 - Use NO_UNDERSCORE to set xercom properly.
** P.E. Hodge and M.D. De La Pena 13 August 1998 - Simplied call to errget
** to avoid segmentation violation on SGI.  Also, replaced calls to 
** iraf2char in set_cvoserr and erract with for loop to fill error message.
** M.D. De La Pena 13 January 1999 - Modified iraf2twodchar and twodchar2iraf
** routines to handle properly an array of strings.
** M.D. De La Pena 01 April 1999 - Modified c_iraf_priv to use exit(-1) for
** fatal IRAF exceptions to solve problems with host level C tasks.
** M.D. De La Pena 12 April 1999 - Further modified c_iraf_priv.c erract() to
** call fflush() and _exit() to ensure that the process exits.  Instances of
** segmentation violation on the Digital Unix machine did not abort with exit().
** M.D. De La Pena 23 April 1999 - The CVOS erract() routine has been renamed
** ceract() to avoid conflicts with the IRAF erract() routine.  The ceract()
** routine is only to be used for C programs compiled as host level tasks.
** Added name under NO_UNDERSCORE check.
** M.D. De La Pena 27 May 1999 - Redefine _exit to exit for VMS machines.
** M.D. De La Pena 07 July 1999 - Added void parameter to appropriate functions.
** M.D. De La Pena 16 July 1999 - Added function definitions to allow
** conversion between IRAF Pointer <-> String.  Functions written by 
** programmers at CXC for use in the XQPOE library.  Cast functions to void 
** when the return value is not needed.  Added the CVOS VERSION indicator.
** M.D. De La Pena 03 Sept 1999 - Made sure c_IrafString() and c_IrafPointer
** are using ints.  Updated version.
** M.D. De La Pena 03 Nov 1999 - Changed all calls from printf() to fprintf()
** to resolve problems with ^C for C tasks running on Solaris under IRAF 2.11.2.
** Updated to Version 3.3.
** M.D. De La Pena 16 May 2000 - Added c_clgetr, c_clputr, c_clglpr,
** c_clgpsetr, and c_clppsetr bindings; the corresponding "f" routines 
** (e.g., c_clgetf) already exist.  Added IRAF_SZ_FNAME macro to the c_iraf.h 
** file.  Added the _STDIO_TAG_H and _FILEDEFED macros to avoid compilation 
** problems under Solaris 2.8 to the CVOS version of the stdio.h file.
** Updated to Version 3.4.
** M.D. De La Pena 09 April 2001 - Added the c_clcmd and c_clcmdw bindings to
** xclio.  Removed "irafos" from mkpkg files and references to VMS.
*/

/* 
** String defined to allow determination of the CVOS library version 
** from the library file (*.a) or the executable using the library.
*/
const char CVOS_VERSION[] = {"CVOS Version 3.5 (10-April-2001)"};

# if defined(__VMS)
# define _exit exit
# if defined(__ALPHA)
# pragma extern_model common_block
# else
# pragma extern_model common_block shr
# endif
# endif

# if defined (NO_UNDERSCORE)
# define xercom_ xercom
# define ceract_ ceract
# endif

# include <stdlib.h>
# include <string.h>

/* global flags to indicate CL or host execution */
int iraf_native_task = 0;
int os_host_task = 1;

/* initailization of IRAF data sizes */
int IRAFSize[13] = { 0, 4, 2, 2, 4, 4, 4, 8, 8, 4, 4, 2, 2 };

/* Error Handling */
static short iraf_errmsg[256];
static int cvos_errcode = 0;
static int cvosmsglen = 255;
static char cvos_errmsg[256] = { '\0' };
typedef void (*c_IRAFErrHandler)(void);
c_IRAFErrHandler errhandler[32];
static int max_err_handlers = 32;
static int cvos_errtop = -1;
int cvos_irafinit = 0;

/* Char String Conversion (buffer #0 is reserved for error handling */
# define CH2I_no_buffer 4
# define CH2I_buffer_increment 512
short *CH2I_buffer[CH2I_no_buffer + 1] = { iraf_errmsg, 0, 0, 0, 0 };
short CH2I_buffersize[CH2I_no_buffer + 1] = { 256, 0, 0, 0, 0 };

void CH2I_chk_buffer(int i, int size);
short *char2iraf(char *source, int i);
void iraf2char(char *target, int maxsize, int i);

# if defined (NO_UNDERSCORE)
# define errget_ errget
# define mem_ mem
# endif

# if defined(__cplusplus)
extern "C" {
# endif
extern int errget_(short *, int *);
# if defined(__cplusplus)
}
# endif

extern char mem_[];
extern int errget_(short *, int *);

# include <stdio.h>
void clear_cvoserr(void) { cvos_errcode = 0; cvos_errmsg[0] = '\0'; }
char *c_iraferrmsg(void) { return cvos_errmsg; }
int c_iraferr(void) { return cvos_errcode; }

void set_cvoserr(void) {
	int i;
	cvos_errcode = errget_(iraf_errmsg, &cvosmsglen);
	if (cvos_errcode == 0)
	    cvos_errcode = 99; /* in case the error_code was reset to 0 */
	for (i = 0;  i < cvosmsglen;  i++) {
	    cvos_errmsg[i] = (char)iraf_errmsg[i];
	    if (iraf_errmsg[i] == 0)
		break;
	}
	if (cvos_errtop > -1 && errhandler[cvos_errtop] != 0)
	    errhandler[cvos_errtop]();
}

int c_pusherr(c_IRAFErrHandler x) {
	if (cvos_errtop == (max_err_handlers - 1)) return -1;
	++cvos_errtop;
	errhandler[cvos_errtop] = x;
	return cvos_errtop + 1;	
}

int c_poperr(void) {
	if (cvos_errtop == -1) return -1;
	--cvos_errtop;
	return cvos_errtop + 1;
}

void CH2I_chk_buffer(int i, int size) {
	++size; /* allow for null */
	if (size >= CH2I_buffersize[i]) {
	    free(CH2I_buffer[i]);
	    CH2I_buffersize[i] = ((size / CH2I_buffer_increment) + 1) * 
						CH2I_buffer_increment;
	    CH2I_buffer[i] = (short *)calloc(CH2I_buffersize[i],sizeof(short));
	    if (CH2I_buffer[i] == 0)
		exit(-1);
	}
}

short *char2iraf(char *source, int i) {
	int n;
	short *t;
	n = strlen(source) + 1;
	CH2I_chk_buffer(i,n);
	t = CH2I_buffer[i];
	while (n-- && *source)
	    *t++ = *source++;
	*t = 0;
	return CH2I_buffer[i];
}

void iraf2char(char *target, int maxsize, int i) {
	short *s;
	s = CH2I_buffer[i];
	while (maxsize-- && *s)
	    *target++ = (char)*s++;
	*target = 0;
}

short **twodchar2iraf(char **ptr_source, int d1, int d2, int n) {
	int i, j;
	char *source, *s;
	short *target, *t;
	source = ptr_source[0];
	++d1; /* allow for null */
	CH2I_chk_buffer(n,(d1 * d2));
	target = CH2I_buffer[n];
	for (i = 0; i < d2; ++i, target += d1, source += d1, source = ptr_source[i]) {
	    for (j = 0, t = target, s = source; j < d1 && *s; ++j)
		*t++ = *s++;
	    *t = 0;
	}
	return (short**)CH2I_buffer[n];
}

void iraf2twodchar(char **ptr_target, int d1, int d2, int n) {
	char *target, *t;
	short *source, *s;
	int i, j;
        target = ptr_target[0];
	++d1; /* allow for null */
	source = CH2I_buffer[n];

	for (i = 0; i < d2; ++i, target += d1, target = ptr_target[i], 
             source += d1) {
	    for (j = 0, t = target, s = source; j < d1 && *s; ++j)
		*t++ = (char)*s++;
	    *t = '\0';
	}
}

/*
** IRAF Pointer to String conversion functions written by programmers at
** CXC for the XQPOE library.
*/

/* Defined in IRAF libc.h */
#define Memc  (((short* )mem_)-1)

char* c_IrafString( int offset ) {
	char* ptr;
	ptr = (char*)&(Memc[offset]);
	return (ptr);
}

int c_IrafPointer( void* addr ) {
	int ptr;
	ptr = (int)( ((short*)addr)-((short*)mem_-1) );
	return (ptr);
}

/*                                                                  *
 * The CVOS version of the IRAF erract() routine is called ceract.  *
 * This routine must be used for C programs compiled as host level  *
 * tasks.  This routine should not be used for C programs compiled  *
 * as IRAF native tasks; the IRAF erract() will be invoked in this  *
 * case.  MDD  23 April 1999                                        *
 *                                                                  */
# include <stdio.h>
# include <unistd.h>

void ceract_(int *severity) {
	int n, i;
	extern int xercom_;
	int *xerflg = &xercom_;
	int *nhandlers = &xercom_ + 2;

	if (*severity == 0) { /* not an error condition */
	    *xerflg = 0;
	    return;
	}
	if (*nhandlers > 0) {
	    if (*severity == 2 || *severity == 3) { /* ERROR or WARN */
	        *xerflg = 1;
	        return;
	    } else if (*severity == 1) { /* FATAL */
		cvos_errcode = errget_(iraf_errmsg, &cvosmsglen);
	        for (i = 0;  i < cvosmsglen;  i++) {
	            cvos_errmsg[i] = (char)iraf_errmsg[i];
	            if (iraf_errmsg[i] == 0)
		        break;
	        }
		(void) fprintf(stderr,"IRAF fatal error: %s (%d)\nAborting...\n",
                              cvos_errmsg,cvos_errcode);
                /*
                  Call fflush to flush output streams which will be closed 
                  upon process termination.  Exit via (_exit(-1)) to the host 
                  environment.
                */
                (void) fflush (NULL);
		_exit  (-1);
	    }
	} else if (*severity == 3) { /* WARN */
	    n = errget_(iraf_errmsg, &cvosmsglen);
	    for (i = 0;  i < cvosmsglen;  i++) {
	        cvos_errmsg[i] = (char)iraf_errmsg[i];
	        if (iraf_errmsg[i] == 0)
	           break;
	    }
	    (void) fprintf(stderr,"IRAF warning: %s (%d)\n",cvos_errmsg,n);
	    return;
	}
	cvos_errcode = errget_(iraf_errmsg, &cvosmsglen);
	for (i = 0;  i < cvosmsglen;  i++) {
	    cvos_errmsg[i] = (char)iraf_errmsg[i];
	    if (iraf_errmsg[i] == 0)
		break;
	}
	(void) fprintf(stderr,"IRAF error: %s (%d)\nNo user-posted exception handler!  Aborting...\n",
		cvos_errmsg,cvos_errcode);
        /*
          Call fflush to flush output streams which will be closed upon
          process termination.  Exit via (_exit(-1)) to the host environment.
        */
        (void) fflush (NULL);
	_exit  (-1);
}
