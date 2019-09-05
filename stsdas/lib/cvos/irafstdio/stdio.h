/*
** This is a modified version of stdio.h to implement the interface to IRAF.
** This definition is compatible with IRAF version 2.12.
**
** Allen Farris
** April 24, 1997
**
** M.D. De La Pena
** 17 February 1998 - added macro _FPOS_T
**
** M.D. De La Pena
** 09 March 1998 - Modified the definition of FIOCOM depending upon 
**                 NO_UNDERSCORE.
**
** M.D. De La Pena
** 26 June 1998 - Added a check around definition of TMP_MAX.  Also, decreased
**                the value from 17567 to 16384 to be the "lowest common
**                denominator" for all ported machines.
** M.D. De La Pena
** 15 May 2000 - Added the _STDIO_TAG_H and _FILEDEFED macros to avoid
**               compilation problems under Solaris 2.8.
**
** P.E. Hodge
** 10 June 2002 - Changed the value of FIO_MAXFD from 256 to 4096,
**                for compatibility with IRAF 2.12.
**
** Note:
**	IRAF does not implement the following ANSI C STDIO functions:
**		tmpfile
**		setvbuf (and associated defines _IOFBF, _IOLBF, _IONBF)
**		vfprintf
**		vprintf
**		vsprintf
**		fgetpos
**		fsetpos
** These names have been redefined so as to produce link errors if they
** are used.
*/

#ifndef _STDIO_H
#define	_STDIO_H

/* Defines needed to circumvent conflicts with definitions in stdio_tag.h and
 * floatingpoint.h on Solaris 2.8 systems.
 */
#define _STDIO_TAG_H
#define _FILEDEFED

#ifdef	__cplusplus
extern "C" {
#endif

/* Special IRAF defines */
# if defined (NO_UNDERSCORE)
# define fiocom_ fiocom
# endif
#define FIOCOM		fiocom_
#define FIO_MAXFD	4096

#define tmpfile		NoIRAF_tmpfile
#define	fclose		u_fclose
#define	fflush		u_fflush
#define	fopen		u_fopen
#define	freopen		u_freopen
#define	setbuf		u_setbuf
#define setvbuf		NoIRAF_setvbuf
#define	fprintf		u_fprintf
#define	fscanf		u_fscanf
#define	printf		u_printf
#define	scanf		u_scanf
#define	sprintf		u_sprintf
#define	sscanf		u_sscanf
#define vfprintf	NoIRAF_vfprintf
#define vprintf		NoIRAF_vprintf
#define vsprintf	NoIRAF_vsprintf
#define	fgetc		u_fgetc
#define	fgets		u_fgets
#define	fputc		u_fputc
#define	fputs		u_fputs
#define	gets		u_gets
#define	puts		u_puts
#define	ungetc		u_ungetc
#define	fread		u_fread
#define	fwrite		u_fwrite
#define fgetpos		NoIRAF_fgetpos
#define	fseek		u_fseek
#define fsetpos		NoIRAF_fsetpos
#define	ftell		u_ftell
#define	rewind		u_rewind
#define	perror		u_perror

#define	XCHAR		short
#define	XINT		int
#define	XLONG		int

#define _FEOF		010
#define _FERR		020
#define _FFLUSH		0100

#define	MEMCOM		mem_
extern	char		MEMCOM[];
#define	Memc		(((XCHAR *)MEMCOM)-1)
/* End -- Special IRAF defines */

/* M.D. De La Pena 17 Feb 98: set up _FPOS_T macro */

#ifndef _FPOS_T
#define _FPOS_T
typedef long	fpos_t;
#endif

#ifndef NULL
#define NULL 0
#endif

#define	BUFSIZ	1024

#define _NFILE	FIO_MAXFD

#ifndef EOF
#define	EOF	(-1)
#endif

#define	FOPEN_MAX	_NFILE

#define	FILENAME_MAX    1024	/* max # of characters in a path name */

#define	SEEK_SET	0
#define	SEEK_CUR	1
#define	SEEK_END	2

/*
** M.D. De La Pena 26 June 1998: Added check on TMP_MAX and decreased 
** value from 17567 to 16384 to accommodate the IBM AIX system.
*/
#ifndef TMP_MAX
#define	TMP_MAX		16384
#endif

#define	L_tmpnam	25	/* (sizeof(P_tmpdir) + 15) */

/*
** IRAF's definition of the stdio implementation
*/
#define	FILE		struct _iobuf

/* Filler space is defined here to reduce clutter in the struct def below.
 */
#define	_F1		_filler1[_NFILE-1]
#define	_F2		_filler2[_NFILE-1]
#define	_F3		_filler3[_NFILE-1]
#define	_F4		_filler4[_NFILE-1]
#define	_F5		_filler5[_NFILE-1]
#define	_F6		_filler6[_NFILE-1]
#define	_F7		_filler7[_NFILE-1]

/* The _iobuf structure is the C version of the FIO common /fiocom/, which
 * contains all the FIO buffer and i/o pointers.  Each structure field is
 * maintained in the common as an array of length _NFILE, hence in terms of
 * C the structures are interleaved.  The file pointers are indices into
 * the array Memc, an array of XCHAR.
 */
struct _iobuf {
	XLONG	_boffset, _F1;		/* XCHAR file offset of buffer	*/
	XINT	_bufptr,  _F2;		/* buffer pointer		*/
	XINT	_buftop,  _F3;		/* pointer to top of buffer + 1	*/
	XINT	_iop,     _F4;		/* pointer to next XCHAR	*/
	XINT	_itop,    _F5;		/* call filbuf when _iop >=	*/
	XINT	_otop,    _F6;		/* call flsbuf when _iop >=	*/
	XINT	_fiodes,  _F7;		/* FIO file descriptor		*/
	XINT	_fflags;		/* bit flags			*/
};

extern	int FIOCOM[];			/* the FIO common		*/

/* Convert FILE pointers to and from FIO integer file descriptors.
 */
#define	FDTOFP(fd)	((FILE *)(&FIOCOM[(fd)-1]))
#define	FPTOFD(fp)	((int *)(fp) - FIOCOM + 1)

#define	stdin		(FDTOFP(3))
#define	stdout		(FDTOFP(4))
#define	stderr		(FDTOFP(5))

/* 
** The list of functions defined in ANSI-C stdio.
*/

extern int	remove(const char *);
extern int	rename(const char *, const char *);
extern FILE	*tmpfile(void);
extern char	*tmpnam(char *);
extern int	fclose(FILE *);
extern int	fflush(FILE *);
extern FILE	*fopen(const char *, const char *);
extern FILE	*freopen(const char *, const char *, FILE *);
extern void	setbuf(FILE *, char *);
extern int	setvbuf(FILE *, char *, int, unsigned int);
extern int	fprintf(FILE *, const char *, ...);
extern int	fscanf(FILE *, const char *, ...);
extern int	printf(const char *, ...);
extern int	scanf(const char *, ...);
extern int	sprintf(char *, const char *, ...);
extern int	sscanf(const char *, const char *, ...);
extern int	vfprintf(FILE *, const char *, void *);
extern int	vprintf(const char *, void *);
extern int	vsprintf(char *, const char *, void *);
extern int	fgetc(FILE *);
extern char	*fgets(char *, int, FILE *);
extern int	fputc(int, FILE *);
extern int	fputs(const char *, FILE *);
extern char	*gets(char *);
extern int	puts(const char *);
extern int	ungetc(int, FILE *);
extern unsigned int fread(void *, unsigned int, unsigned int, FILE *);
extern unsigned int fwrite(const void *, unsigned int, unsigned int, FILE *);
extern int	fgetpos(FILE *, fpos_t *);
extern int	fseek(FILE *, long, int);
extern int	fsetpos(FILE *, const fpos_t *);
extern long	ftell(FILE *);
extern void	rewind(FILE *);
extern void	perror(const char *);


/* 
 * IRAF I/O macros
 *
 * I/O macro defines.  I/O is assumed to be sequential, i.e., we do not check
 * for _iop < _bufptr.  This is consistent with UNIX usage.  The getc and putc
 * macros are quite efficient despite their complex appearance.
 */
#define	getchar()	fgetc(stdin)
#define	getc(fp) \
(((fp)->_iop >= (fp)->_itop) ? c_filbuf((fp)) : Memc[(fp)->_iop++] & 0377)

#define	putchar(ch)	fputc((ch),stdout)
#define	putc(ch,fp) \
(((fp)->_iop >= (fp)->_otop || ((ch) == '\n' && (fp)->_fflags&_FFLUSH)) ? \
c_flsbuf((unsigned)(ch),(fp)) : ((int)(Memc[(fp)->_iop++] = (unsigned)(ch))))

#define	fileno(fp)	(FPTOFD((fp)))
#define	feof(fp)	((fp)->_fflags & _FEOF)
#define	ferror(fp)	((fp)->_fflags & _FERR)
#define	clearerr(fp)	((fp)->_fflags &= ~_FERR)


#ifdef	__cplusplus
}
#endif

#endif	/* _STDIO_H */
