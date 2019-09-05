include "pixpos.h"
define	MAX_TRY			10

# Copyright restrictions apply - see stsdas$copyright.stsdas 

# PIXAUTO -- Automatically fit pixel position withou image cursor

procedure pixauto (par, pos, im, frm)

pointer par		# i: Parameter descriptor
pointer	pos		# i: Star position descriptor
pointer	im		# i: Image descriptor
pointer	frm		# i: Coordinate frame descriptor
#--
double	solution[MAX_SOLUTION,2]
int	boxsize, nterm, maxstar, nstar, istar, ntry, junk
pointer	sp, xold, yold
real	xnew, ynew

string	badcolumn  "Could not read column from pos descriptor (%s)"

int	rdrcol_pos(), wrtival_pos(), wrtrval_pos()

begin
	call rdint_param (par, "nterm", nterm)
	call rdint_param (par, "boxsize", boxsize)

	call rdipar_pos (pos, "nstar", maxstar)

	call smark (sp)
	call salloc (xold, maxstar, TY_REAL)
	call salloc (yold, maxstar, TY_REAL)

	if (rdrcol_pos (pos, "xold", FLAG_OUT, nstar, 
			Memr[xold], maxstar) == ERR)
	    call pixerror (badcolumn, "xold")

	if (rdrcol_pos (pos, "yold", FLAG_OUT, nstar, 
			Memr[yold], maxstar) == ERR)
	    call pixerror (badcolumn, "yold")

	do istar = 1, nstar {

	    call starcenter (im, Memr[xold+istar-1], Memr[yold+istar-1], 
			     boxsize, MAX_TRY, ntry, xnew, ynew)
	    if (ntry < MAX_TRY) {
		junk = wrtival_pos (pos, "flag", FLAG_OUT, istar, FLAG_IN)

		junk = wrtrval_pos (pos, "xnew", FLAG_IN, istar, xnew)
		junk = wrtrval_pos (pos, "ynew", FLAG_IN, istar, ynew)
	    }
	}

	call solve_fit (pos, frm, nterm, MAX_SOLUTION, solution)
	call list_fit (pos, frm, MAX_SOLUTION, solution)
	call stats_fit (pos, frm, MAX_SOLUTION, solution)
	call wrt_coord (par, pos, frm, MAX_SOLUTION, solution)
	call sfree (sp)

end

