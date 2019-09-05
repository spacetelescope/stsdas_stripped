/* Main include file for the PEDSUB task.
**
** H.Bushouse	09-May-1999	Implementation.
** H.Bushouse	11-May-2000	Separated from nicmos.h.
** H.Bushouse	27-Sep-2005	Added CVOS error handling (v1.1).
*/

# include "../lib/nicmos.h"

/* Task version number */
# define	VERSION		"1.1 (30Sep2005)"

# define BINF			0.10
# define NBIN			60
# define BINMAX			3.0
# define FTOL			1.0e-4
# define MMin_1_Default		6
# define MMin_2_Default		1
# define MMax_1_Default		8
# define MMax_2_Default		2

/* Image filter types */
enum FilterTypes_ {NONE, MEDIAN, MASK};
typedef enum FilterTypes_ FilterTypes;

/* This structure stores the general task parameters. */
typedef struct {
	short		BitMask;
	char		FlatName[SZ_NAME];
	Bool		KeepLog;
	char		LogName[SZ_NAME];
	FilterTypes	Filter;
	char		FilterName[SZ_NAME];
	int		MMin;
	int		MMax;
	Bool		DoRefine;
	int		Nrefine;
	float		PedStep;
	int		Morder;
	Bool		EqQuads;
	Bool		EqFlat;
	int		EqOrder;
	int		EqPix1, EqPix2;
	int		qx1[4], qx2[4],	qy1[4], qy2[4];
	int		statlim[4];
	float		PedValue[4];
	float		DCValue[4];
	int		quad;
	float		exptime;
} PedInfo;

/* Some useful macros */
# define MIN(a,b)	(a < b ? a : b)
# define MAX(a,b)	(a > b ? a : b)

