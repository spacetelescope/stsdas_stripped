procedure dither ()

string	mode="al"

begin
    set drizzle	= "dither$drizzle/"
    set mdrizzle = "dither$"
    set nicmos = "stsdas$pkg/hst_calib/nicmos/"

    package dither

    task gprep	= "dither$gprep.cl"
    task precor	= "dither$precor.cl"
    task sky	= "dither$sky.cl"
    task crossdriz	= "dither$crossdriz.cl"
    task offsets	= "dither$offsets.cl"
    task rotfind	= "dither$rotfind.cl"
    task qzap	= "dither$qzap.cl"
    task shiftfind	= "dither$shiftfind.cl"
    task avshift	= "dither$avshift.cl"
    task $dunlearn	= "dither$dunlearn.cl"

    task loop_blot	= "dither$loop_blot.cl"
    task loop_driz	= "dither$loop_driz.cl"

    task blot	= "drizzle$x_blot.e"
    task drizzle	= "drizzle$x_drizzle.e"
    task tranback	= "drizzle$x_tranback.e"
    task traxy		= "drizzle$x_traxy.e"
    task wtranback	= "drizzle$x_wtranback.e"
    task wtraxy		= "drizzle$x_wtraxy.e"
    task wcs2dr     = "drizzle$x_wcs2dr.e"
    task wdrizzle	= "drizzle$x_wdrizzle.e"
    task wblot  	= "drizzle$x_wblot.e"

    task imextreme	= "dither$x_dither.e"
    task ogsky	= "dither$x_dither.e"

    task cdriz      = "dither$cdriz.par"
    task dq		= "dither$dq.par"
    task wfpc2_chips= "dither$wfpc2_chips.par"

    # was in ditherII
    task fileroot 	= "dither$fileroot.cl"
    task iterstat       = "nicmos$iterstat.cl"
    task minv           = "dither$minv.cl" 
    task filename 	= "dither$filename.cl"
    task driz_cr 	= "dither$driz_cr.cl"
    task deriv 	= "dither$deriv.cl"
    task mask_head 	= "dither$mask_head.cl"
    task blot_mask 	= "dither$blot_mask.cl"
    task cor_shft 	= "dither$cor_shft.cl"

    # added for Drizzle Version 2.6
    task dr2gpar = "drizzle$dr2gpar.par"
    hidetask dr2gpar

    hidetask ogsky
    hidetask fileroot
    hidetask iterstat
    hidetask minv
    hidetask filename
    hidetask blot_mask
    hidetask cor_shft
    hidetask qzap

    type "dither$motd"
    
    # Setup new 'tran' task written by Richard Hook as an IRAF task
    #    This task needs to be added to 'stsdas$python' for now, 
    #    since it relies on IRAF tasks for its header access.
    #
    pyexecute("dither$tran_iraf.py",tasknames="tran")
    
    
    cl()
end
