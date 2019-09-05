procedure star_key (gp, star_str)

include <gset.h>
include	"skymap.h"

pointer	gp
pointer	star_str			# Stars structure

real	dm
real 	mag
real	x, y
char	magstr[6]
real	faint, bright
int	ns
int	i
real	minmag
int	color
short	txtspc

string	magfmt	"v=c;h=r;s=1.0"

define	NTRY	13
double	try[NTRY]
data	try /0.01,  0.02,  0.025,  0.05, 
	     0.1,   0.2,   0.25,   0.5,
	     1.0,   2.0,   2.5,    5.0, 
	    10.0/

char	fmt[5]
char	prc[NTRY]
data	prc /'2',   '2',   '3',   '2', 
	     '1',   '1',   '2',   '1',
	     '1',   '0',   '1',   '0',
	     '0', 0/

int	strlen()
double	round_up()

begin
	call gseti (gp, G_WCS, STAR_KEY_WCS)
	call gseti (gp, G_PLTYPE, GL_SOLID)

	# Force proportional spacing (only works with psikern)
	txtspc = 1
	call gescape (gp, 12, txtspc, 1)

	bright = BRIGHT_PLOT_MAG(star_str)
	faint  = FAINT_PLOT_MAG(star_str)

	dm = faint - bright
	for (i = 1;  int (dm/try[i]) > NUM_STARS;  i = i + 1)
	    ns = i

	dm = try[i]
	minmag = round_up (double (bright), double (dm))

	y = 0.0
	call strcpy ("%6.2f", fmt, 5)
	fmt[4] = prc[i]
	call gsetr (gp, G_PLWIDTH, 1.0)

	for (mag = minmag;  mag <= faint;  mag = mag + dm) {
	    y = y + 1.0
	    call sprintf (magstr, 6, fmt)
		call pargr (mag)
	    if (magstr[strlen (magstr)] == '.')
		magstr[strlen (magstr)] = EOS
	    call gtext (gp, 0.46, y, magstr, magfmt)
#	    call gtext (gp, 0.15, y, magstr, magfmt)
	}

#  These alternate left coordinates are to move the key closer to the
#  right edge of the main viewport.  This looks better on PostScript
#  output and leaves room on the right for other stuff, but doesn't work
#  for SGI or other text.

	x = 0.55
#	x = 0.25
	y = 0.0

	color = COLOR_INDEX(star_str)

	for (mag = minmag;  mag <= faint;  mag = mag + dm) {
	    y = y + 1.0

	    switch (SYMBOL_STYLE(star_str)) {
	    case FILLED_STAR:
		call filled_star (gp, star_str, x, y, mag, color)

	    case ERASE_STAR:
		call erase_star (gp, star_str, x, y, mag, color)

	    case EFILLED_STAR:
		call efilled_star (gp, star_str, x, y, mag, color)

	    default:
		call open_star (gp, star_str, x, y, mag, color,
		    SYMBOL_STYLE(star_str))
	    }
	}
end
