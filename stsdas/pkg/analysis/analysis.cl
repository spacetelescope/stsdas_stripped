# ANALYSIS.CL -- The STScI data analysis suite of packages
# Created: R.L. Williamson, 14-Jul-1993
# 06-Jun-97 - Add dither package (I. Busko)
# 08-Jul-97 - Add nebular package (R.A. Shaw)


procedure analysis()
string	mode="al"

begin
	set dither     = "analysis$dither/"
	set fitting    = "analysis$fitting/"
	set fourier    = "analysis$fourier/"
	set gasp       = "analysis$gasp/"
	set isophote   = "analysis$isophote/"
	set nebular    = "analysis$nebular/"
	set at_data    = "nebular$atomic_data/"
	set restore    = "analysis$restore/"
	set statistics = "analysis$statistics/"
	set slitless   = "analysis$slitless/"

	package analysis

	task dither.pkg 	= "dither$dither.cl"
	task fitting.pkg 	= "fitting$fitting.cl"
	task fourier.pkg 	= "fourier$fourier.cl"
	task gasp.pkg 		= "gasp$gasp.cl"
	task isophote.pkg 	= "isophote$isophote.cl"
	task nebular.pkg	= "nebular$nebular.cl"
	task restore.pkg 	= "restore$restore.cl"
	task statistics.pkg 	= "statistics$statistics.cl"
	task slitless.pkg 	= "slitless$slitless.cl"

	cl()
end
