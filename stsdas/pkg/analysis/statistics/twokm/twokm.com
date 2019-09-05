# TWOKM.COM -- Common used by the Schmitt binning method in the Astronomical
# Survival analysis package

define	MAX_BINS	40	# Largest number of bins allowed

double	x[MAX_ASDAT]
double	y[MAX_ASDAT]
int	np[MAX_ASDAT]

double	xb[MAX_BINS]
double	yb[MAX_BINS]

double	f[MAX_BINS,MAX_BINS]
int	n[MAX_BINS,MAX_BINS]
int	n1[MAX_BINS,MAX_BINS]
int	n2[MAX_BINS,MAX_BINS]
int	n3[MAX_BINS,MAX_BINS]
int	n4[MAX_BINS,MAX_BINS]
int	n5[MAX_BINS,MAX_BINS]
int	n6[MAX_BINS,MAX_BINS]
int	n7[MAX_BINS,MAX_BINS]
int	n8[MAX_BINS,MAX_BINS]

int	nc1
int	nc2
int	nc3
int	nc4
int	nc5
int	nc6
int	nc7
int	nc8

common	/a1/ x, y, np
common	/b1/ xb, yb
common	/e1/ f, n, n1, n2, n3
common	/e2/ n4, n5, n6, n7, n8
common	/c1/ nc1, nc2, nc3, nc4, nc5, nc6, nc7, nc8
