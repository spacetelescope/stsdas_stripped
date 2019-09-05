procedure stfov (input, ra, dec, apertures, center_ap, orient)

file input 	{"", prompt = "input image"}
string ra 	{"", prompt = "right ascension of the center aperture"}
string dec 	{"", prompt = "declination of the center aperture"}
string apertures 	{"sm3b", prompt = "apertures to draw (launch, sm1, sm2, sm3b, ... )"}
string center_ap 	{"ota", prompt = "aperture to center on"}
real orient 	{0., prompt = "roll angle ('orient' in the proposal)"}
string color 	{"blue", min = "red|green|blue|yellow|white", prompt = "aperture/label color (red|green|blue|yellow|white)"}
bool append	{no, prompt = "append to existing chart?"}
bool interactive 	{no, prompt = "interactive mode? (start imexamine)"}

string mode = "al"

begin

file 	ifile
string	dumra, dumdec
string	icenter
string	apers
real	roll
file	igifile
string	acolor
bool	inter, btmp, app
real	mag

	# assign CL parameters to local parameters
	ifile = input
	dumra = ra
	dumdec = dec
	apers = apertures
	icenter = center_ap
	roll = orient
	acolor = color
	app = append
	inter = interactive

	# pre-defined apertures
	if (apers == "launch") {
            apers = "^v*,^w???$,x??zlrg,^z?$,^y??$,^fgs"
	} else if (apers == "sm93" || apers == "sm1") {
            apers = "^u???$,xk??zlrg,^zk?$,^yk??$,^fgs"
	} else if (apers == "sm97" || apers == "sm2") {
            apers = "^u???$,xk??zlrg,ofuv,nic?fix,^fgs"
	} else if (apers == "sm2004" || apers == "sm3b") {
            apers = "^u???$,jwfcfix,jhrcfix,ofuv,nic?fix,^fgs"
	} else if (apers == "wfpc2") {
            apers = "^u???$"
	} else if (apers == "acs") {
            apers = "jwfcfix,jhrcfix"
        }

	# populate the world coordiante of the DSS image
        keypar (ifile, "CRVAL1", silent=yes)
        if (keypar.found) {
	} else {

            # need the gasp package
            if (!defpac("gasp")) {
                print ("Error: need to load the gasp package.")
                bye ()
            }
	    makewcs (ifile, verbose = no)
	}

	# run disconlab with yellow labels and green apertures
	wlpars.major_grid = no
	wlpars.minor_grid = no
	wlpars.label_size = 0.7

	if (app) {
	    disconlab.disp = no
	    disconlab.label = no
	} else {
	    display.erase = yes
	    display.border_erase = yes
	    disconlab.disp = yes
	    disconlab.label = yes
	}

	# make sure siaper use the default value of sky_project 3/3/99 JCHsu
	btmp = siaper.sky_project
	siaper.sky_project = yes

	disconlab (ifile, cont = no, limage = "", doapers = yes, 
		   lablcol = acolor, aimage = "", apercol = acolor, 
		   apers = apers, center = icenter, 
		   ra = dumra, dec = dumdec, roll = roll, 
		   left = 0.1, right = 0.9, bottom = 0.1, top = 0.9)

	siaper.sky_project = btmp

	# label center aperture and orient
	if (!app) {
	    igifile = mktemp ("tmp$IGI")
	    printf ("reset, fontset hard, vpage 0 1 0 1, expand 0.7\n", 
			>> igifile)
	    printf ("move 0.75 0.97, label 'orient = %0.1f'\n", roll, 
			>> igifile)
	    printf ("move 0.75 1.0, label 'center aper: %s'\n", icenter, 
			>> igifile)
            igi (initcmd="", wlpars="", usewcs=no, wcspars="", 
		 device="imd"//acolor, append=yes, debug=no, cursor="", 
		 < igifile)
	    delete (igifile, verify = no, >& "dev$null")
	}

	# interactive mode
	if (inter) {
            keypar (ifile, "EXPOSURE", silent=yes)
            if (keypar.found)
                mag = 28. + log10(real(keypar.value)/60.) * 2.5
            else {
                printf ("Keyword EXPOSURE not found in %s\n", ifile)
                mag = 0.
            }

	    rimexam.magzero = mag
	    printf ("Magnitude zero point is set at %0.1f\n", mag)
	    print ("(move the cursor to a star and hit the a key, hit the q key when it is done.)")
	    imexamine 
	}
end
