/* Main include file for the PEDSKY task.
**
** H.Bushouse	04-May-1999	Implementation.
** H.Bushouse	11-May-2000	Separated from nicmos.h.
** H.Bushouse	27-Sep-2005	Added CVOS error handling (v1.1).
*/

# include "../lib/nicmos.h"

/* Task version number */
# define	VERSION		"1.1 (30Sep2005)"

/* Sky subtraction modes */
enum SkyModes_ {NONE, CONSTANT, ITER, QUICK};
typedef enum SkyModes_ SkyModes;

/* This structure stores the general task parameters. */
typedef struct {
	short		BitMask;
	SkyModes	SkyMode;
	char		SkyModeName[SZ_NAME];
	float		SkyValue;
	float		SkySmin;
	float		SkySmax;
	Bool		keepFlags;
	char		FlatName[SZ_NAME];
	Bool		doRingMedian;
	float		RingInner;
	float		RingOuter;
	float		rms;
	int		MaxIter;
	float		Tolerance;
	float		PedValue[4];
	int		qx1[4], qx2[4];
	int		qy1[4], qy2[4];
	int		statlim[4];
} TaskInfo;

