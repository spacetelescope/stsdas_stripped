/*   
 * Definitions and structures for IRAF/VMS Kernel.
 */

#define VMS4		/* VMS Version 4.xx */
#define NOKNET		/* no networking in kernel (see iraflibc:knames.h) */
#define NODEBUG		/* DEBUG prints VMS error messages, statuses, etc. */

#define import_knames	/* Standard IRAF definitions */
#define import_spp
#include <iraf.h>

#define MAXOFILES	70
#define MAXNODENAME	62
#define MAXDISKNAME	255
#define MAXDIRNAME	255
#define MAXRECLEN	1024		/* for text files */
#define SZ_DISKBLOCK	512
#define PAGE_SIZE	512
				/* optimum and maximum buffer sizes 	*/
#define TX_OPTBUFSIZE	MAXRECLEN 	/* text file 			*/
#define TX_MAXBUFSIZE	MAXRECLEN
#define BF_OPTBUFSIZE	32768		/* binary file, max is 65K-512	*/
#define BF_MAXBUFSIZE	65024
#define SF_OPTBUFSIZE	32768		/* static file 			*/
#define SF_MAXBUFSIZE	65024
#define TY_OPTBUFSIZE	SZ_LINE		/* terminal 			*/
#define TY_MAXBUFSIZE	65535
#define PL_OPTBUFSIZE	1440		/* plotter (NCAR)		*/
#define PL_MAXBUFSIZE	64800
#define LP_OPTBUFSIZE	4096		/* line printer			*/
#define LP_MAXBUFSIZE	65024
#define	KS_OPTBUFSIZE	4096		/* KS i/o 			*/
#define	KS_MAXBUFSIZE	65024

#define STATIC_FILE	14
#define LPRINT_FILE	15

#define CHAN_TYPE 	0		/* Channel table types */
#define RAB_TYPE  	1

#define PRINT_QUEUE	0
#define BATCH_QUEUE	1

#define MAXCPROC 	10		/* Max connected subprocesses   */
#define MAXDPROC 	4		/* Max detached (sub and batch) */

/* Event flag definitions
 */
#define GENERALEF	0		/* General use 		*/
#define IOEF		1		/* I/O completion 	*/
#define SYSCALLEF	2		/* System service calls */
#define PROCEF		3		/* Process termination	*/

/* The following constant is used to distinguish between VMS exceptions and 
 *  IRAF exceptions in some instances; used by ZINTPR and ZXWHEN.  (Bits used 
 *  are set aside by DEC for customer use, so as not to conflict with VMS 
 *  codes.) 
 */
#define VMS_SIGNAL_CONSTANT  ((1 << 27) | (1 << 15))

/* Structure definitions for use within IRAF/VMS kernel routines.
 */
struct iosb {			/* I/O status block definition for QIOs */
	short io_status;
	short io_count;	
	long  io_device;
};

struct item_list {		/* Item List, used by some system services */
	short  it_len;
	short  it_code;
	char  *it_buff;
	short *it_blen;
};

struct chan_info {		/* Channel Information */
	short ch_chan;			/* channel number 		*/
	short ch_pending;		/* number of requests pending 	*/
	short ch_iomode;		/* type of i/o request (rd,wr) 	*/
	short ch_iosize;		/* size of transfer 		*/
	char  *ch_iobuf;		/* pointer to i/o buffer 	*/
	XINT  ch_fmode;			/* access mode 			*/
	XINT  ch_status;		/* channel status 		*/
	XINT  ch_count;			/* channel count 		*/
	XINT  ch_size;			/* size for files 		*/
	XINT  ch_blksiz;		/* device block size 		*/
	short ch_maxrecsize;		/* maximum record size 		*/
	short ch_rawmode;		/* raw mode in effect?		*/
	XINT  ch_offset;		/* current offset in file 	*/
	struct iosb ch_iosb;		/* IOSB for current operation 	*/
	struct fio_buffer *ch_buffer;	/* pointer to I/O buffer 	*/
};

/* The following structure is for buffering some text file I/O if
 * the record does not fit in the user's buffer (RMS will not give
 * you the rest of record on next SYS$GET call). 
 */
struct fio_buffer {		/* Structure for text file I/O */
	struct fio_buffer *bf_next;	/* next free buffer 		*/
	char   *bf_ptr;			/* buffer pointer 		*/
	int    bf_size;			/* buffer size 			*/
	int    bf_off;			/* current offset into buffer 	*/
	char   bf_flags;		/* buffer flags 		*/
	char   bf_block;		/* block number for buffer 	*/
};

#define IOSB		struct iosb
#define ITEM_LIST	struct item_list
#define CHANNEL		struct chan_info
#define FIO_BUFFER	struct fio_buffer
#define DESCRIPTOR	struct dsc$descriptor_s		/* in <descrip.h> */

/* ZLOCVA style pointer to address conversions.
 */
#define	ADDR_TO_LOC(addr)	(((int)((XCHAR *)(addr)))>>(sizeof(XCHAR)-1))
#define	LOC_TO_ADDR(loc,type)	((type *)((XCHAR *)((loc)<<(sizeof(XCHAR)-1))))

/*  Magtape table structure; used by ZFIOMT and CLTAPE routines.
 */
#define MAXTAPES  10

struct  mttab  {
	char	device_name[20];	/* magtape device name (eg. MSA0:) */
	int	density[4];		/* tape densities (800,1600,...)   */
	int	channel;		/* channel number if in use 	   */
};

/* Internal functions.
 */
char	*_osfn();			/* IRAF osfn to VMS/FAB osfn	   */
