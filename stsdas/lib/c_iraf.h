/*
** Allen Farris - Original Implementation.
** M.D. De La Pena 27 January 1998 - Removed setting of NO_UNDERSCORE macro.
** M.D. De La Pena 26 February 1998 - IRAFPointer is an "int" instead of "long".
**   and NULL is 0 and not 0L.
** M.D. De La Pena 07 January 1999 - Added a macro definition for the version.
**   Used in irafinit.c (c_irafinit) which is called by all C programs 
**   whether compiled as native IRAF or as host level tasks.  Updated SZ_LINE
**   from 161 to 1023.
** M.D. De La Pena 02 April 1999 - Updated to v2.1 for change to erract() in
**   c_iraf_priv.c.  Fatal IRAF errors activate exit(-1).
** M.D. De La Pena 12 April 1999 - Updated to v2.2 for additional changes to
**   erract() in c_iraf_priv.c.  Routine calls fflush() and _exit() to ensure
**   process aborts.
** M.D. De La Pena 26 April 1999 - Updated to v3.0 for version which 
**   accommodates exception handling for both host-level and C programs which
**   have been compiled as native IRAF tasks.
** M.D. De La Pena 27 May 1999 - Updated to v3.1 for changes to accommodate 
**   VMS.
** M.D. De La Pena 07 July 1999 - Added "void" to the appropriate function
**   prototypes.  Moved the clear_cvoserr() function to this public file 
**   from c_iraf_priv.h.  Moved CVOS Version indicator to c_iraf_priv.c.
** M.D. De La Pena 19 January 2000 - Added IRAF_SZ_FNAME macro.
*/
# ifndef C_IRAF
# define C_IRAF

# if defined(__VMS)
# if defined(__ALPHA)
# pragma extern_model common_block
# else
# pragma extern_model common_block shr
# endif
# endif

/*
** Public interface declarations for CVOS IRAF interface
*/
# ifndef BOOL_
# define BOOL_
enum Bool_ { False = 0, True = 1 };
typedef enum Bool_ Bool;
# endif
struct IRAFComplex { 
	float re; 
	float im; 
};
typedef struct IRAFComplex Complex;
typedef int IRAFPointer;

# define IRAF_EOF -2

# define IRAF_NULL 0
# define IRAF_SZ_LINE 1023
# define IRAF_SZ_FNAME 255

/* These must be the same as in $/unix/hlib/iraf.h */
enum IRAFTypes { IRAF_NOTYPE = 0, IRAF_BOOL = 1, IRAF_CHAR = 2, 
	IRAF_SHORT = 3, IRAF_INT = 4, IRAF_LONG = 5, IRAF_REAL = 6, 
	IRAF_DOUBLE = 7, IRAF_COMPLEX = 8, IRAF_POINTER = 9, 
	IRAF_STRUCT = 10, IRAF_USHORT = 11, IRAF_UBYTE = 12 };
typedef enum IRAFTypes IRAFType;

extern int IRAFSize[13];


/* These must be the same as in $iraf/unix/hlib/iraf.h */
enum IRAFIOModes { IRAF_NOMODE = 0, IRAF_READ_ONLY = 1, 
	IRAF_READ_WRITE = 2, IRAF_WRITE_ONLY = 3, IRAF_APPEND = 4,
	IRAF_NEW_FILE = 5, IRAF_TEMP_FILE = 6, IRAF_NEW_COPY = 7 };
typedef enum IRAFIOModes IRAFIOMode;
# define IRAF_NEW_IMAGE IRAF_NEW_FILE

/* The IRAF initialization routine */
void c_irafinit(int argc, char **argv);

/* CVOS error handling routines */
void clear_cvoserr(void);
char *c_iraferrmsg(void);
int c_iraferr(void);
typedef void (*c_IRAFErrHandler)(void);
int c_pusherr(c_IRAFErrHandler);
int c_poperr(void);

extern int iraf_native_task;
extern int os_host_task;
/* Macros to run a C program as a native IRAF task */

# if defined(NO_UNDERSCORE)

# define IRAFTASK(taskname) \
int argc; char **argv; \
void main(int m_argc, char **m_argv) { \
	void irafcmain(int, char **); \
	argc = m_argc; \
	argv = m_argv; \
	iraf_native_task = 1; \
	irafcmain(m_argc,m_argv); \
} \
int sysruk(short *task) { \
	void taskname(); char *p = #taskname; short *t = task; \
	if (task[0] == '?') { printf("    %s\n",#taskname); return 0; } \
	while (*p) if (*p == *t) { ++p; ++t; } else break; \
	if (*p || *t) return -1; \
	taskname(); \
	fflush(stdout); \
	return 0; \
} \
void taskname()

# else

# define IRAFTASK(taskname) \
int argc; char **argv; \
void main(int m_argc, char **m_argv) { \
	void irafcmain(int, char **); \
	argc = m_argc; \
	argv = m_argv; \
	iraf_native_task = 1; \
	irafcmain(m_argc,m_argv); \
} \
int sysruk_(short *task) { \
	void taskname(); char *p = #taskname; short *t = task; \
	if (task[0] == '?') { printf("    %s\n",#taskname); return 0; } \
	while (*p) if (*p == *t) { ++p; ++t; } else break; \
	if (*p || *t) return -1; \
	taskname(); \
	fflush(stdout); \
	return 0; \
} \
void taskname()

# endif

# endif
