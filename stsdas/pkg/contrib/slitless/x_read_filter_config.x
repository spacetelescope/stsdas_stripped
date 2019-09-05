procedure read_filter_config(configname,instrument,filtername, maxx1, maxx2, ccx, ccy, lambda_zero, lambda_step, angle)
# Read parameters corresponding to filtername on instrument
char configname[SZ_LINE],instrument[SZ_LINE], filtername[SZ_LINE]
char finstrument[SZ_LINE], ffiltername[SZ_LINE]
int maxx1, maxx2
real ccx, ccy
real lambda_zero, lambda_step,angle
int fmaxx1, fmaxx2
real fcx, fcy
real flambda_zero, flambda_step,fangle
pointer fd,open()
bool streq(), found
int fscan()

begin
	call printf("Reading the GRISM parameter file.\n")
	found = false
	fd = open(configname, READ_ONLY, TEXT_FILE)
	while (fscan(fd,"%s %s %d %d %f %f %f %f %f")!=EOF) {
		call gargwrd(finstrument,SZ_LINE)
		call gargwrd(ffiltername,SZ_LINE)
		call gargi(fmaxx1)
		call gargi(fmaxx2)
		call gargr(fcx)
		call gargr(fcy)
		call gargr(flambda_zero)
		call gargr(flambda_step)
		call gargr(fangle)


		if (streq(instrument,finstrument) & streq(filtername,ffiltername)) {
			call printf("Filter found in configuration file\n")
			maxx1 = fmaxx1
			maxx2 = fmaxx2
			ccx = fcx
			ccy = fcy
			lambda_zero = flambda_zero
			lambda_step = flambda_step
			angle = fangle/180.0*3.1415
			found = true
		}

	}
	if (!found) {
		call printf("Could not get information on this filter (%s) from the configuration file (%s)\n")
			call pargstr(filtername)
			call pargstr(configname)
			stop
	}


end
