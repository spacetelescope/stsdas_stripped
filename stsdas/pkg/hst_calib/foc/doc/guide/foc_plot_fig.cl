procedure foc_plot_fig (figure )

string	figure   = "" 		{prompt=">figure number to plot for FOC guide"}
file	device   = "stdgraph" 	{prompt=">plotting device"}
file	metacode = "uparm$vdm" 	{prompt=">metacode file id device=file"}
bool	append   = no		{prompt=">append to metacode file"}

begin

string figcom

	# Note that we must assume a .cl file, rather than calling
	#  igi directly because some figs may need say contour or graph.
	if ( strlen(figure) != 0 ) {
	    figcom = "cl <foc$doc/guide/fig"//figure//".cl"
	    print ( figcom)
	    print ( figcom ) | cl ()
	} else {
	    print ( "usage: foc_plot_fig fignumber")
	    print ( "  e.g. foc_plot_fig 1.1_5    ")
	    print ( "    to plot figure 5 of section 1.1")
	}

end
