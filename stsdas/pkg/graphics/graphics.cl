#} GRAPHICS.CL - Package script for GRAPHICS Package
procedure graphics()
string	mode="al"

begin
	set sdisplay = "graphics$sdisplay/"
	set stplot = "graphics$stplot/"

	package graphics

	task sdisplay.pkg = "sdisplay$sdisplay.cl"
	task stplot.pkg = "stplot$stplot.cl"

	clbye()
end
