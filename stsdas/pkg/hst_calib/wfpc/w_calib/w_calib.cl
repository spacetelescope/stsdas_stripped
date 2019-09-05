#  w_calib.cl

procedure w_calib()
string	mode="al"

begin
	package w_calib


	task	flagflat,
                mka2d,
	        mkphottb,
		normclip,
		psfextr,
		streakflat = "w_calib$x_w_calib.e"

	task	sharp = "w_calib$x_sharp.e"
	
	cl()

end
