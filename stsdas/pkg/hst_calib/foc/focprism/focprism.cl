procedure focprism()
string mode="al"

begin
	package	focprism

	task	objcalib,
		prismsim = "focprism$x_focprism.e"

	task	simprism    = "focprism$simprism.cl"

	# Pset for names of dispersion files.
	task	dispfiles = "focprism$dispfiles.par"

	hidetask prismsim

	clbye()
end
