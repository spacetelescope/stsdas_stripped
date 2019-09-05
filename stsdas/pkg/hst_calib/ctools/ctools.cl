procedure ctools()
string  mode="al"

begin
	set msstreakflat = "ctools$msstreakflat/"
	#set msbadpix     = "ctools$msbadpix/"
	#set msreadnoise  = "ctools$msreadnoise/"
	set splice       = "ctools$splice/"

        package ctools

	task eng2tab,
	     groupmod,
	     hstephem,
	     mkmultispec,
	     mkweight,
	     north,	
	     poffsets,
	     pprofile,
	     rcombine,
	     rdsaa,
	     resample,
	     specalign,
	     vac2air	= "ctools$x_ctools.e"
 
        task getcal,
             modcal,
             putcal    = "ctools$x_calpar.e"
              
 
        task chcalpar    = "ctools$chcalpar.cl"
 
        task ckwfoc    = "ctools$ckwfoc.par"
        task ckwfos    = "ctools$ckwfos.par"
        task ckwhrs    = "ctools$ckwhrs.par"
        task ckwhsp    = "ctools$ckwhsp.par"
        task ckwwfp2   = "ctools$ckwwfp2.par"
        task ckwwfpc   = "ctools$ckwwfpc.par"
        task ckwnicmos = "ctools$ckwnicmos.par"
        task ckwstis1  = "ctools$ckwstis1.par"
        task ckwstis2  = "ctools$ckwstis2.par"
        task ckwstis3  = "ctools$ckwstis3.par"
        task ckwstis4  = "ctools$ckwstis4.par"
        task ckwacs1  = "ctools$ckwacs1.par"
        task ckwacs2  = "ctools$ckwacs2.par"
 
        task keywords  = "ctools$keywords.par"
	hide keywords
	
        task fwplot      = "ctools$fwplot.cl"
        task rapidlook   = "ctools$rapidlook.cl"
        task tomultispec = "ctools$tomultispec.cl"

	task msstreakflat = "msstreakflat$x_msstreakflat.e"
	task nstreakpar   = "msstreakflat$nstreakpar.par"
	task wstreakpar   = "msstreakflat$wstreakpar.par"
	task nicdqpar     = "msstreakflat$nicdqpar.par"
	task wfdqpar      = "msstreakflat$wfdqpar.par"
	#task msbadpix     = "msbadpix$x_msbadpix.e"
	#task msreadnoise  = "msreadnoise$x_msreadnoise.e"

	task splice  = "splice$x_splice.e"
	task fweight = "splice$x_fweight.e"
	task pweight = "splice$x_pweight.e"
	task sflux   = "splice$sflux.cl"

        cl()
end
