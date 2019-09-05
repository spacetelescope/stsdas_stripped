# SDISPLAY.CL -- Script to set up tasks in the SDISPLAY package

procedure sdisplay()

string	mode="al"

begin
	package sdisplay

	task	compass,
		hltorgb,
                im2gki,
		mklut,
		imdisp_pos,
		overlap,
		sclean	  = "sdisplay$x_sdisplay.e"

	task	disconlab = "sdisplay$disconlab.cl"
	task	mosaic_display = "sdisplay$mosaic_display.cl"
	task	pltcmap = "sdisplay$pltcmap.cl"

	clbye
end
