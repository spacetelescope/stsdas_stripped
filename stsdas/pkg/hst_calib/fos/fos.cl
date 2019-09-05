procedure fos()
string	mode="al"

begin

	set spec_polar = "fos$spec_polar/"
	package fos

	task 	spec_polar.pkg = "spec_polar$spec_polar.cl"

	task	aperlocy,
		apscale,
                bspec,
                countspec,
		deaccum,
		fitoffsety,
                gimpcor,
		unwrap,
		waveoffset = "fos$x_fos.e"

        # Psets
	task	h13b = "fos$h13b.par"
	task	h16b = "fos$h16b.par"
	task	h16r = "fos$h16r.par"
	task	h19b = "fos$h19b.par"
	task	h19r = "fos$h19r.par"
	task	h27b = "fos$h27b.par"
	task	h27r = "fos$h27r.par"
	task	h40b = "fos$h40b.par"
	task	h40r = "fos$h40r.par"
	task	h57b = "fos$h57b.par"
	task	h57r = "fos$h57r.par"
	task	h65b = "fos$h65b.par"
	task	h65r = "fos$h65r.par"
	task	h78r = "fos$h78r.par"
	task	instpars = "fos$instpars.par"

        # Hidden tasks
	hidetask instpars
	hidetask h13b
	hidetask h16b
	hidetask h16r
	hidetask h19b
	hidetask h19r
	hidetask h27b
	hidetask h27r
	hidetask h40b
	hidetask h40r
	hidetask h57b
	hidetask h57r
	hidetask h65b
	hidetask h65r
	hidetask h78r

	task calfos = "fos$x_calfos.e"
	task addnewkeys= "fos$addnewkeys.cl"

        task foswcorr = "fos$foswcorr.cl"

	task grspec  = "graphics$stplot/grspec.cl"
	task grlist  = "graphics$stplot/x_stplot.e"
	hidetask grlist

	task yd2p = "fos$yd2p.cl"
	task yddintplot = "fos$yddintplot.cl"
	task yfluxcal = "fos$yfluxcal.cl"
	task ymkmu = fos$ymkmu.cl
	task yp2d = "fos$yp2d.cl"
	task ypeakup = "fos$ypeakup.cl"
	task yratio = "fos$yratio.cl"
	task yv2v3_calculate = "fos$yv2v3_calculate.cl"

	cl()

end
