procedure dunlearn

begin
	char msg = ""

	if (!deftask("tsort"))    msg = msg // " ttools"
	if (!deftask("rotate"))   msg = msg // " images"
	if (!deftask("crosscor")) msg = msg // " fourier"
	if (!deftask("pickfile")) msg = msg // " imgtools"
	if (!deftask("imcntr"))   msg = msg // " proto"
	if (!deftask("nfit1d"))   msg = msg // " fitting"

	if (strlen(msg) > 0) {
	    printf ("Please, load packages: %s\n", msg)
	    bye
	}

	# avshift
	unlearn delete
	unlearn tsort

	# crossdriz
	unlearn countfile
	unlearn crosscor
	unlearn hedit
	unlearn imgets
	unlearn imcopy
	unlearn imdelete
	unlearn pickfile
	unlearn taperedge

	# offsets
	unlearn files
	unlearn gcopy
	unlearn hselect

	# rotfind, shiftfind
	unlearn controlpars
	unlearn copy
	unlearn errorpars
	unlearn gfit1d
	unlearn hedit
	unlearn nfit1d
	unlearn n2gaussfit
	unlearn samplepars
	unlearn sections
	unlearn tabpar
	unlearn tinfo
	unlearn tgausspars
	unlearn userpars

	unlearn avshift
	unlearn blot
	unlearn cdriz
	unlearn crossdriz
	unlearn drizzle
	unlearn precor
	unlearn imextreme
	unlearn offsets
	unlearn rotfind
	unlearn shiftfind
	unlearn sky
	unlear ogsky
end
