# TWOST.COM -- Common used by the two-sample tests in the Astronomical
# Survival analysis package

double	xy[MAX_ASDAT]
int	id1[MAX_ASDAT]
int	id2[MAX_ASDAT]

int	n
int	n1
int	n2
int	ncen
int	isign
int	ifull
int	lo

common	/f/ xy, id1, id2
common	/g/ n, n1, n2, ncen, isign, ifull, lo
