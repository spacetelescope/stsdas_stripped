procedure cat_title (gp, title)

#  Draw the plot title at the upper left corner of the legend viewport.

include <gset.h>
include	"skymap.h"

pointer	gp
char	title[ARB]

real	x, y
short	txtspc

string	ttlfmt	"v=t;h=r;s=1;u=180"

begin
	call gseti  (gp, G_WCS, LEGEND_WCS)

	# Force proportional spacing (only works with psikern)
	txtspc = 1
	call gescape (gp, 12, txtspc, 1)

	x = 0.95;  y = 0.5
	call gtext  (gp, x, y, title, ttlfmt)
end
