procedure tools()
string	mode="al"

begin

	package tools


	task ddiff,
	     epoch,
             fparse,
	     mkapropos,
	     tepoch,
	     tprecess,
	     uniqfile,
             newredshift,
	     uniqid,
	     uniqname,
	     uniqtab	= "tools$x_tools.e"

	task base2dec = "tools$base2dec.cl"
	task dec2base = "tools$dec2base.cl"

	cl()
end
