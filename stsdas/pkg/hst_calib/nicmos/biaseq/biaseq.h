/* Main include file for the BIASEQ task.
**
** H.Bushouse	25-Mar-1999	Initial implementation.
** H.Bushouse	02-May-2000	Renamed task from nbiasfix to biaseq. Prepared
**				initial public release of v1.0.
** H.Bushouse	27-Sep-2005	Added CVOS error handling (v1.1).
*/

# include "../lib/nicmos.h"

# define	VERSION		"1.1 (30Sep2005)"

# define	MAX_RANGES	26	/* Max number of sky samps           */

/* This structure stores the general task parameters. */
typedef struct {
	int	nskys;
	int	skysamps[26];
	char	skylist[SZ_NAME];
	int	nlow;
	int	nhigh;
	short	bitmask;
	Bool	fitJumps;
	int	jmp_filt;
	float	jmp_thresh;
	Bool	keepSky;
	Bool	keepBias;
	Bool	keepJump;
	int	camera;
	int	nimsets;
} BiasInfo;

