.help
lgrlist -- List individual file names and group numbers.

Print a list of individual file names making up a group format image.
This is needed because the IRAF image-name/section syntax does not
permit wild-cards in the "cluster" (group member) number.

Task input consists of a root image name and a range of group members.
The group members list (the 'members' parameter) is given in ranges
format.  The output consists of the root image name with a group
member (cluster) specifier enclosed in square brackets.

The list is written to STDOUT, therefore may be piped to another task
or redirected to a file.

(This is based on stplot.grlist, which does not support an input list.)

17 September 1992  Z.G. Levay
.endhelp

include <ctype.h>

define	LOW	1
define	MEDIUM	2
define	HIGH	3

procedure t_lgrlist ()

pointer	sp
pointer	imroot, inimnm, imsect
pointer	templt
int	nclust				# Number of group members (clusters)
pointer	imname
int	im				# Input image descriptor
pointer	clist
int	clust
int	nvals
pointer	list
pointer	inlist
bool	incl
pointer	itmpl
int	nrngs
int	verbos
pointer	verbdict, verb			# Verbosity selection dictionary

int	immap(), imaccf(), imgeti(), imtgetim()
int	decode_ranges(), get_next_number(), clgwrd()
pointer	imtopen()
bool	clgetb(), imgetb(), streq()

begin
	call smark (sp)

	call salloc (inlist, SZ_FNAME, TY_CHAR)
	call salloc (inimnm, SZ_FNAME, TY_CHAR)
	call salloc (imsect, SZ_FNAME, TY_CHAR)
	call salloc (imroot, SZ_FNAME, TY_CHAR)
	call salloc (templt, SZ_LINE,  TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	# The input image list
	call clgstr ("image", Memc[inlist], SZ_FNAME)

	# The ranges template
	call clgstr ("members", Memc[templt], SZ_LINE)

	# Strip leading white-space
	for (itmpl = templt;
	     IS_WHITE(Memc[itmpl])&&Memc[itmpl]!=EOS;
	     itmpl = itmpl + 1)
	    ;

	if (streq (Memc[itmpl], "*"))
	    # Permit "*" wildcard
	    Memc[itmpl] = EOS

	# Include non-group files?
	incl = clgetb ("inclusive")

	call salloc (verbdict, SZ_LINE,  TY_CHAR)
	call salloc (verb, SZ_LINE,  TY_CHAR)

	# Get the verbosity
	call clgstr ("verbosity.p_min", Memc[verbdict], SZ_LINE)
	verbos = clgwrd ("verbosity", Memc[verb], SZ_LINE, Memc[verbdict])

	if (verbos == HIGH) {
	    call eprintf (" # %s %s\n")
		call pargstr (Memc[inlist])
		call pargstr (Memc[templt])
	}

	# Open the image "template" list
	list = imtopen (Memc[inlist])

	while (imtgetim (list, Memc[inimnm], SZ_FNAME) > 0) {
	    # For each image in the list

	    if (verbos == HIGH) {
		call eprintf (" # %s\n")
		    call pargstr (Memc[inimnm])
	    }

	    call imgsection (Memc[inimnm], Memc[imsect], SZ_FNAME)
	    call imgimage (Memc[inimnm], Memc[imroot], SZ_FNAME)

	    im = immap (Memc[imroot], READ_ONLY, 0)

	    if (imaccf (im, "GROUPS") == NO ||
		!imgetb (im, "GROUPS") ||
		imaccf (im, "GCOUNT") == NO) {


		if (verbos == MEDIUM) {
		    call eprintf (" # Image not in group format: %s\n")
			call pargstr (Memc[inimnm])
		}

		if (incl) {
		    call printf ("%s\n")
			call pargstr (Memc[inimnm])
		}

	    } else {
		# Group format image
		# Find the number of clusters (group members)
		nclust = imgeti (im, "GCOUNT")
		nrngs = nclust + 1

		call imunmap (im)

		call malloc (clist, 3*nrngs, TY_INT)

		if (decode_ranges (Memc[itmpl], Memi[clist],
		    nrngs, nvals) == ERR && nvals == 0) {

		    call eprintf ("# %s\n")
			call pargstr (Memc[itmpl])

		    call error (0, "Bad range")
		}


		if (verbos == HIGH) {
		    call eprintf (" # %s %d\n")
			call pargstr (Memc[itmpl])
			call pargi (nclust)
		}

		clust = 0

		while (get_next_number (Memi[clist], clust) != EOF) {
		    # For each cluster member

		    if (clust > nclust)
			break

		    # Write to output list
		    call printf ("%s[%d]%s\n")
			call pargstr (Memc[imroot])
			call pargi (clust)
			call pargstr (Memc[imsect])
		}
	
		call mfree (clist, TY_INT)

	    }  # Group format?

	}  # For each image in input list

	call sfree (sp)
end
