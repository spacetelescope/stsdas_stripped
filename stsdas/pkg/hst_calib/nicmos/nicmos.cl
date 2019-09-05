procedure nicmos()

string	mode="al"

begin
	package nicmos

	task	asnexpand    = "nicmos$asnexpand/x_asnexpand.e"
	task	biaseq       = "nicmos$biaseq/x_biaseq.e"
	task 	calnica      = "nicmos$calnica/calnica.cl"
	task	calnicb      = "nicmos$calnicb/calnicb.cl"
	task	iterstat     = "nicmos$iterstat.cl"
	task	markdq	     = "nicmos$x_nicmos.e"
	task	mosdisplay   = "nicmos$mosdisplay.cl"
	task	ndisplay     = "nicmos$ndisplay.cl"
	task	nicpipe      = "nicmos$nicpipe.cl"
	task	pedsky	     = "nicmos$pedsky/x_pedsky.e"
	task	pedsub	     = "nicmos$pedsub/x_pedsub.e"
	task	pstack	     = "nicmos$pstack.cl"
	task	pstats	     = "nicmos$pstats.cl"

        pyexecute("nicmos$rnlincor_iraf.py",tasknames="rnlincor")
        pyexecute("nicmos$puftcorr_iraf.py",tasknames="puftcorr")
        pyexecute("nicmos$saaclean_iraf.py",tasknames="saaclean")
        pyexecute("nicmos$CalTempFromBias_iraf.py",tasknames="CalTempFromBias")
        pyexecute("nicmos$nic_rem_persist_iraf.py",tasknames="nic_rem_persist")

	task	sampcum	     = "nicmos$sampcum.cl"
	task	sampdiff     = "nicmos$sampdiff.cl"
	task	sampinfo     = "nicmos$sampinfo.cl"

	task	statregions  = "nicmos$statregions.par"

	# These live in other packages but are refered in here
	# just for convenience.
	task	nicdqpar      = "ctools$msstreakflat/nicdqpar.par"

	hidetask	nicdqpar

	clbye()
end
