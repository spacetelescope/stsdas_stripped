# PLAYPEN.CL -- Script to set up tasks in the PLAYPEN package
#
#  11-Jan-94 by RAShaw	Added `nebular' package
#  08-Jal-97 by RAShaw	Deleted `nebular' package

procedure playpen()

bool  pmsg= yes		# Print playpen message when loading?
string	mode="al"

begin
	package playpen

	task	bwfilter,
		edge,
		fill,
		geo2mag,
		hstpos,
		hsubtract,
		ils,
		immean,
		jimage,
		lgrlist		= "playpen$x_playpen.e"

	task    ilspars          = "playpen$ilspars.par"

	task	saolpr	= "playpen$saolpr.cl"

# Print the Welcome banner, but only if the user wants it
if (pmsg) type "playpen$playpen.msg"
	
	clbye
end
