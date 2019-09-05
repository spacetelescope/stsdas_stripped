procedure prj_const (pl_cnst, a, d)

include	"skymap.h"

pointer	pl_cnst
double	a, d

begin
	# Chart center
	CEN_RA(pl_cnst)  = a
	CEN_DEC(pl_cnst) = d

	SIN_A(pl_cnst) = sin (a)
	COS_A(pl_cnst) = cos (a)
	SIN_D(pl_cnst) = sin (d)
	COS_D(pl_cnst) = cos (d)

	COSA_SIND(pl_cnst) = COS_A(pl_cnst) * SIN_D(pl_cnst)
	SINA_SIND(pl_cnst) = SIN_A(pl_cnst) * SIN_D(pl_cnst)
	COSA_COSD(pl_cnst) = COS_A(pl_cnst) * COS_D(pl_cnst)
	SINA_COSD(pl_cnst) = SIN_A(pl_cnst) * COS_D(pl_cnst)
end
