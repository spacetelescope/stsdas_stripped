# TOOLBOX.CL -- General Tools Package
# Created: R.L. Williamson, 14-Jul-1993
#

procedure toolbox()
string	mode="al"

begin
	set convfile    = "toolbox$convfile/"
	set headers     = "toolbox$headers/"
	set imgtools    = "toolbox$imgtools/"
	set tools       = "toolbox$tools/"
	set ttools       = "tables$pkg/ttools/"

	package toolbox

	task convfile.pkg = "convfile$convfile.cl"
	task headers.pkg = "headers$headers.cl"
	task imgtools.pkg = "imgtools$imgtools.cl"
	task tools.pkg = "tools$tools.cl"
	task ttools.pkg = "ttools$ttools.cl"

	cl()
end
