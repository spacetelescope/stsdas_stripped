/* Main include file for NICMOS tasks. Contains definitions that
** are generic to many tasks.
**
** H.Bushouse	11-May-2000	Initial implementation.
** H.Bushouse	27-Sep-2005	Added cv_errchk routine.
*/

# include <c_iraf.h>
# include <hstio.h>

# define	SZ_NAME		256	/* File name maximum size            */
# define	SZ_HISTORY	160	/* HISTORY record maximum size       */
# define	SZ_TIMESTAMP	35	/* Maximum size of time stamp string */
# define	SZ_OUTLINE	160	/* STDOUT line maximum size          */
# define	SZ_KW		8	/* Maximum header keyword length     */

# define	NIC_XSIZE	256	/* Expected NICMOS axis sizes.       */
# define	NIC_YSIZE	256
# define	NIC_QSIZE	128	/* Expected NICMOS quarant size.     */

/* NICMOS data quality bits */
# define	N_RS             1      /* Reed-Solomon                      */
# define	N_PLIN           2      /* Poor linearity correction         */
# define	N_PDARK          4      /* Poor dark correction              */
# define	N_PFF            8      /* Poor flat-field correction        */
# define	N_GROT          16      /* Grot                              */
# define	N_DEFECT        32      /* Defective pixel                   */
# define	N_SATUR         64      /* Saturated pixel                   */
# define	N_TELEM        128      /* Telemetry problem                 */
# define	N_BADPIX       256      /* Bad pixel determ. by calibration  */
# define	N_CRAY         512      /* Cosmic ray                        */
# define	N_SOURCE      1024      /* Source                            */
# define	N_ZREAD       2048      /* Zero-read signal correction       */
# define	N_USER1       4096      /* User 1                            */
# define	N_USER2       8192      /* User 2                            */

/* data quality pset name */
# define	DQPAR       "nicdqpar"

/* statistics regions pset name */
# define	STATREGIONS "statregions"

/* NICMOS quadrant pixel limits (zero indexed) */
static int QXI[4] = {  0,128,  0,128};
static int QXF[4] = {127,255,127,255};
static int QYI[4] = {  0,  0,128,128};
static int QYF[4] = {127,127,255,255};

/* This definition must be put in here since IRAF's libc has *no* fabs */
#ifdef __STDC__
#define __P(p)  p
#else
#define __P(p)  ()
#endif
extern double fabs __P((double));

/* Message text string */
extern	char	MsgText[SZ_OUTLINE];

/* Messaging functions. */
void n_message    (char *);
void n_warn       (char *);
void n_error      (char *);
void n_IRAFerror  ();
void errchk	  ();
void cv_errchk    ();

