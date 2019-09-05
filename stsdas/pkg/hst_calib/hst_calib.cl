# HST_CALIB.CL -- The STScI HST data analysis suite of packages
# Created: R.L. Williamson, 14-Jul-1993
#

procedure hst_calib()
string mode="al"

begin

	set acs 	= "hst_calib$acs/"
	set hstcos 	= "hst_calib$hstcos/"
	set ctools	= "hst_calib$ctools/"
	set foc		= "hst_calib$foc/"
	set fos		= "hst_calib$fos/"
	set hrs		= "hst_calib$hrs/"
	set nicmos	= "hst_calib$nicmos/"
	set paperprod	= "hst_calib$paperprod/"
	set stis	= "hst_calib$stis/"
	set synphot	= "hst_calib$synphot/"
	set wfc3	= "hst_calib$wfc3/"
	set wfpc	= "hst_calib$wfpc/"

	package hst_calib

	
	task	acs.pkg		= "acs$acs.cl"
	task	hstcos.pkg		= "hstcos$hstcos.cl"
	task	ctools.pkg	= "ctools$ctools.cl"
	task	foc.pkg		= "foc$foc.cl"
	task	fos.pkg		= "fos$fos.cl"
	task	hrs.pkg		= "hrs$hrs.cl"
	task	nicmos.pkg	= "nicmos$nicmos.cl"
	task	paperprod.pkg	= "paperprod$paperprod.cl"
	task	stis.pkg	= "stis$stis.cl"
	task	synphot.pkg	= "synphot$synphot.cl"
	task	wfc3.pkg	= "wfc3$wfc3.cl"
	task	wfpc.pkg	= "wfpc$wfpc.cl"

# Implicitly load ctools and synphot
ctools
synphot

	cl()

end
