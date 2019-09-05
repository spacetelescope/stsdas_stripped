procedure testpp()
string mode = "al"

begin
	#set 		pp = "stlocal$pkg/testpp/"
	set 		pp = "devpp$"

	package devpp
	task		pplist,
				autopi,
				affix_mod,
				pp_roots,
				pp_fits,
				pp_banner,
				pr_parts,
				t_gsbar,
				t_dithchop,
				t_compass,
				t_cdcompass,
				t_gethist,
				t_opeakup,
				opp_calib,
				opp_obsum,
				jpp_obsum,
				jpp_thumbs,
				jpp_prods,
				jpp_calib,
				t_oms,
				t_o1drange,
				npp_exp,
				pp_pdfbook,
				pp_pdfsection,
				upp_obsum,
				upp_image,
				xpp_obsum,
				xpp_image,
				ypp_calib,
				ypp_obsum,
				ypp_imdsp,
				ypp_image = 	"pp$x_pp.e"

	task		pp_dads = 	"pp$pp_dads.cl"

	task		pp_fos = 	"pp$pp_fos.cl"
	task		pp_acs = 	"pp$pp_acs.cl"
	task		pp_ghrs = 	"pp$pp_ghrs.cl"
	task		pp_stis = 	"pp$pp_stis.cl"
	task		pp_nicmos = 	"pp$pp_nicmos.cl"
	task        pp_wfpc2 =      "pp$pp_wfpc2.cl"
	task        pp_foc =        "pp$pp_foc.cl"

	task		zpp = 		"pp$zpp/zpp.cl"
	task		pp_igi = 	"pp$pp_igi.cl"
	task		ppcover = 	"pp$ppcover.cl"
	task		ppend = 	"pp$ppend.cl"
	task		ppdirbox = 	"pp$ppdirbox.cl"

	task		jpp_exp = 		"pp$jpp/jpp_exp.cl"
	task		jpp_targ = 		"pp$jpp/jpp_targ.cl"
	task		jpp_expsum = 	"pp$jpp/jpp_expsum.cl"
	task		jpp_jitter = 	"pp$jpp/jpp_jitter.cl"
	task		jpp_acq = 		"pp$jpp/jpp_acq.cl"
	task		jpp_accum = 	"pp$jpp/jpp_accum.cl"

	task		opp_exp = 	"pp$opp/opp_exp.cl"
	task		opp_expsum = 	"pp$opp/opp_expsum.cl"
	task		opp_acq = 	"pp$opp/opp_acq.cl"
	task		opp_peakup = 	"pp$opp/opp_peakup.cl"
	task		opp_hist = 	"pp$opp/opp_hist.cl"
	task		opp_jitter = 	"pp$opp/opp_jitter.cl"
	task		opp_accum = 	"pp$opp/opp_accum.cl"
	task		opp_2dsp = 	"pp$opp/opp_2dsp.cl"
	task		opp_1dsp = 	"pp$opp/opp_1dsp.cl"

	task		ypaccrapid = 	"pp$ypp/ypaccrapid.cl"   
	task		ypacqbin =	"pp$ypp/ypacqbin.cl"     
	task		ypacqpeak =	"pp$ypp/ypacqpeak.cl"    
	task		ypbanner =	"pp$ypp/ypbanner.cl"     
	task		yppeak =	"pp$ypp/yppeak.cl"
	task		yppolar = 	"pp$ypp/yppolar.cl"   

	hidetask pp_fos, pp_ghrs, pp_nicmos, pp_stis, pp_wfpc2, pp_foc, pp_acs

	hidetask pp_igi, ppcover, ppend, ppdirbox, pplist, autopi
    hidetask pp_pdfbook,pp_pdfsection
	hidetask affix_mod, pp_banner, pp_roots, pp_fits, pr_parts
	hidetask t_gsbar, t_compass, t_cdcompass, t_gethist, t_oms

	hidetask npp_exp
	hidetask t_dithchop

	hidetask jpp_jitter, jpp_acq, jpp_accum, jpp_exp, jpp_expsum
	hidetask jpp_obsum, jpp_prods, jpp_thumbs, jpp_calib, jpp_targ

	hidetask opp_2dsp, opp_1dsp, opp_accum, opp_exp, opp_expsum
	hidetask opp_acq, opp_peakup
	hidetask opp_obsum, opp_calib
	hidetask opp_hist, opp_jitter
	hidetask t_opeakup, t_o1drange

	hidetask upp_obsum, upp_image

	hidetask xpp_obsum, xpp_image

	hidetask ypaccrapid, ypacqbin, ypacqpeak, ypbanner, yppeak, yppolar
	hidetask ypp_calib, ypp_obsum, ypp_imdsp, ypp_image

	hidetask zpp

	# Implicitly load fos.
	hst_calib
	fos
 
	cl()
end
