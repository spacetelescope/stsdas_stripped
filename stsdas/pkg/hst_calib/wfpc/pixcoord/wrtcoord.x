include	"pixpos.h"
include	"pixframe.h"

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# WRT_COORD -- Write the pixel coordinate positions to the coordinate file
#
# B.Simon	17-Jul-90	Original

procedure wrt_coord (par, pos, frm, maxsol, solution)

pointer	par			#  i: Parameter descriptor
pointer	pos			#  i: Star position descriptor
pointer	frm			#  i: Coordinate frame descriptor
int	maxsol			#  i: Length of solution vector
double	solution[maxsol,2]	# io: Solution vector (cleared on exit)
#--
bool	found
int	fd, root, trip, istar, maxstar, nstar
pointer	sp, ct, coord, image, cluster, fname
pointer	xold, yold, xfit, yfit, xphys, yphys, name

string	badaccess  "Output file already exists (%s). Enter new file name."
string	badopen    "Could not open output file (%s). Enter new file name."
string	badcolumn  "Could not read column from pos descriptor (%s)"

int	fnldir(), fnroot(), strncmp(), strlen(), gstrcpy(), access(), open()
int	rddcol_pos(), rdtcol_pos()
pointer	mw_sctran()

begin
	call putflag_param (par, "coord", NO)

	call smark (sp)
	call salloc (coord, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	call rdipar_pos (pos, "nstar", maxstar)

	call salloc (xold, maxstar, TY_DOUBLE)
	call salloc (yold, maxstar, TY_DOUBLE)
	call salloc (xfit, maxstar, TY_DOUBLE)
	call salloc (yfit, maxstar, TY_DOUBLE)
	call salloc (xphys, maxstar, TY_DOUBLE)
	call salloc (yphys, maxstar, TY_DOUBLE)
	call salloc (name, maxstar, TY_INT)

	found = false
	while (! found) {

	    # See if output file name is default and create file name if it is

	    call rdstr_param (par, "coordfile", Memc[coord], SZ_FNAME)
	    root = fnldir (Memc[coord], Memc[fname], SZ_FNAME)

	    if (strncmp ("default", Memc[coord+root], 7) != 0 &&
		strlen (Memc[coord]) != root) {
		call strcpy (Memc[coord], Memc[fname], SZ_FNAME)
		found = access (Memc[fname], READ_ONLY, TEXT_FILE) == NO

	    } else {
		call rdstr_param (par, "image", Memc[image], SZ_FNAME)
		call imgcluster (Memc[image], Memc[cluster], SZ_FNAME)

		root = gstrcpy (Memc[coord], Memc[fname], root)		
		root = fnroot (Memc[cluster], Memc[fname+root], 
			       SZ_FNAME-root) + root

		# Try each version of default name until one is found that
		# does not exist

		for (trip = 1; ! found && trip < 100; trip = trip + 1) {
		    call sprintf (Memc[fname+root], SZ_FNAME-root, ".coo.%d")
		    call pargi (trip)

		    found = access (Memc[fname], READ_ONLY, TEXT_FILE) == NO
		}
	    }

	    # Ask user for new output filename if there is a problem

	    if (! found) {
		call query_param (par, "coordfile", badaccess)

	    } else {
		iferr (fd = open (Memc[fname], WRITE_ONLY, TEXT_FILE)) {
		    found = false
		    call query_param (par, "coordfile", badopen)
		}
	    }
	}

	# Get old star positions 

	if (rddcol_pos (pos, "xold", FLAG_OUT, nstar, 
			Memd[xold], maxstar) == ERR)
	    call pixerror (badcolumn, "xold")

	if (rddcol_pos (pos, "yold", FLAG_OUT, nstar, 
			Memd[yold], maxstar) == ERR)
	    call pixerror (badcolumn, "yold")

	if (rdtcol_pos (pos, "name", FLAG_OUT, nstar, 
			Memi[name], maxstar) == ERR)
	    call pixerror (badcolumn, "name")

	# Compute fitted star positions

	call apply_fit (frm, maxsol, solution, Memd[xold], Memd[yold], 
			Memd[xfit], Memd[yfit], nstar)

	# Convert star positions from logical to physical frame

	ct = mw_sctran (FRM_MWCS(frm), "logical", "physical", 3)
	call mw_v2trand (ct, Memd[xfit], Memd[yfit], 
			 Memd[xphys], Memd[yphys], nstar)

	do istar = 1, nstar {
	    call fprintf (fd, "%8.1f %8.1f %s\n")
	    call pargd (Memd[xphys+istar-1])
	    call pargd (Memd[yphys+istar-1])
	    call pargstr (Memc[Memi[name+istar-1]])
	}

	call aclrd (solution, 2*maxsol)
	call mw_ctfree (ct)
	call close (fd)
	call sfree (sp)
end
