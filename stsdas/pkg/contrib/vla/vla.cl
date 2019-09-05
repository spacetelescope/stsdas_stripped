
procedure vla()
string	mode="al"

begin
	package vla

	task	intensity	= "contrib$vla/intensity.cl"
	task	smooth		= "contrib$vla/smooth.cl"
	task    velocity	= "contrib$vla/velocity.cl"

	cl()

end
