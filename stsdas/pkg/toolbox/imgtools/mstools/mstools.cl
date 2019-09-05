procedure mstools()

string	mode="al"

begin
	set msstatistics	= "mstools$msstatistics/"
	set msarith		= "mstools$msarith/"

	package mstools

	task mssplit 		= "mstools$mssplit.cl"
	task msjoin 		= "mstools$msjoin.cl"
	task mscombine 		= "mstools$mscombine.cl"
	task bloathdu 		= "mstools$bloathdu.cl"
	hidetask bloathdu
	task msarith 		= "msarith$x_msarith.e"
	task mscopy 		= "mstools$mscopy/x_mscopy.e"
	task msstatistics 	= "msstatistics$x_msstatistics.e"

	task    egstp           = "msstatistics$egstp.par"
	task    dqbits          = "msstatistics$dqbits.par"
	task    nsstatpar       = "msstatistics$nsstatpar.par"
	task    acsdqpar	= "msstatistics$acsdqpar.par"
	task    nicdqpar        = "msstatistics$nicdqpar.par"
	task    stisdqpar       = "msstatistics$stisdqpar.par"
	task    wfdqpar         = "msstatistics$wfdqpar.par"
	task    wfc3dqpar         = "msstatistics$wfc3dqpar.par"
	task    cosdqpar         = "msstatistics$cosdqpar.par"

# FITS tools
	task	  ecdel		= "mstools$ecdel.cl"
	task	  ecextract	= "mstools$ecextract.cl"
	task	  extdel	= "mstools$extdel.cl"
	task	  msdel		= "mstools$msdel.cl"
	task	  mssort	= "mstools$mssort.cl"

clbye()
end
