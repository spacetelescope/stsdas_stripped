procedure contrib()
string	mode="al"

begin

	set redshift   = "contrib$redshift/"
	set acoadd     = "contrib$acoadd/"
	set plucy     = "contrib$acoadd/"
	set spfitpkg     = "contrib$spfitpkg/"
	set slitless     = "contrib$slitless/"
	set vla     = "contrib$vla/"

	package contrib

	task acoadd	= "acoadd$x_acoadd.e"
	task plucy	= "plucy$x_plucy.e"
	task $redshift.pkg	= "redshift$redshift.cl"
#	task specfit	= "specfit$x_specfit.e"
	task spfitpkg.pkg	= "spfitpkg$spfitpkg.cl"
	task slitless   = "slitless$slitless.e"
	task vla.pkg	= "vla$vla.cl"

	# Write the welcome message
	type contrib$contrib.msg

	clbye
end
