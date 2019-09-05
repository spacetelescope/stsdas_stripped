/* 
** 07 July 1999 - M.D. De La Pena: Moved clear_cvoserr(void) to c_iraf.h to 
**    allow public access.   Added void parameter to set_cvoserr, xerpsh, and
**    xerpoi functions. Added new functions, c_IrafString and c_IrafPointer.
** 03 Sept 1999 - M.D. De La Pena: Changed "long" to "int" on QPOE functions.
*/
# ifndef C_IRAF_PRIV
# define C_IRAF_PRIV

# if defined(__VMS)
# if defined(__ALPHA)
# pragma extern_model common_block
# else
# pragma extern_model common_block shr
# endif
# endif

/* Error Handling */
/*void clear_cvoserr(void);*/
void set_cvoserr(void);

/* Char String Conversion */
# define CH2I_no_buffer 4
# define CH2I_buffer_increment 512
short *CH2I_buffer[CH2I_no_buffer + 1];
short CH2I_buffersize[CH2I_no_buffer + 1];

void CH2I_chk_buffer(int i, int size);
short *char2iraf(char *source, int i);
void iraf2char(char *target, int maxsize, int i);
short **twodchar2iraf(char **source, int d1, int d2, int n);
void iraf2twodchar(char **target, int d1, int d2, int n);
char* c_IrafString(int offset);
int c_IrafPointer(void *addr);

# if defined(NO_UNDERSCORE)
# define xerpsh_ xerpsh
# define xerpoi_ xerpoi
# define errget_ errget
# define mem_ mem
# endif

# if defined(__cplusplus)
extern "C" {
# endif
extern char mem_[];
extern void xerpsh_(void);
extern int xerpoi_(void);
extern int errget_(short *, int *);
# if defined(__cplusplus)
}
# endif

# endif
