procedure zwavecal (input, output)

file	input {prompt="GHRS wavelength observations"}
file	output {prompt="CCR6-formatted table of wavelength solution coefficients"}
file	wave {"",prompt="Wavelength solution images"}
string	defwavext {"c0h",prompt="Default wavelength solution image extension"}
file	fittab {"",prompt="Rootname of tables containing fit evaluations"}
bool	interactive {no,prompt="Interactively examine line finding process"}
pset	dvpar {"",prompt="Graphic device parameters"}
pset	findpars {"",prompt="Parameters related to the line-finding process"}
pset	fitpars {"",prompt="Parameters related to the line-fitting process"}

bool	verbose {no,prompt="Print additional information."}
string	Version {"1Sep95",prompt="Date of Installation"}

string	*aper_list
string	*cpos_list

begin
        # Declarations
	string	aper
	file	aper_file
	int	cpos
	file	cpos_file
        string	expr
	file	hsel_file
        file	in_file
        file	pinput
	file	poutput
	string	sx
	file	tmerge_file, tmerges_file
        file	tmp_file
	file	wavefit_file
        file	waveid_file
	file	zavg_file

	# Get interactive parameters
	pinput = input
	poutput = output


	# Get the deciding header parameters.
	if (verbose)
	    print ("zwavecal: Examining header parameters.")
	hsel_file = mktemp ("tmp$ZwCaL_hsel_file")
	hselect (pinput, "$I,carpos,aperture", "yes", > hsel_file)

        # Get the list of apertures.
	if (verbose)
	    print ("zwavecal: Finding apertures.")
	aper_file = mktemp ("tmp$ZwCaL_aper_file")
        tproject (hsel_file, aper_file, "c3", uniq=yes)

        # For each aperture, solve for wavelengths.
	tmerges_file = mktemp ("tmp$ZwCaL_tmerges_file")
        aper_list = aper_file
        while (fscan (aper_list, aper) != EOF) {

            # Find all the carrousel positions.
	    if (verbose)
		print ("zwavecal: Finding all carrousel positions.")
	    cpos_file = mktemp ("tmp$ZwCaL_cpos_file")
            tproject (hsel_file, cpos_file, "c2", uniq=yes)

            # For each carrousel position, solve for wavelengths.
            cpos_list = cpos_file
            while (fscan (cpos_list, cpos) != EOF) {
		print ("zwavecal: aperture "//aper//" carrousel position = "//cpos)
		
                # Get list of images containing the (aperture,carpos)
		if (verbose)
		    print ("zwavecal: Looking for unique aperture/carrousel values.")
                expr = "c2="//cpos//" && c3=\""//aper//"\""
		tmp_file = mktemp ("tmp$ZwCaL_tmp_file")
		in_file = mktemp ("tmp$ZwCaL_in_file")
                tselect (hsel_file, tmp_file, expr)
                tproject (tmp_file, in_file, "c1", uniq=no)

                # Identify the wavelengths.
		if (verbose)
		    print ("zwavecal: Identifying lines.")
		waveid_file = mktemp ("tmp$ZwCaL_waveid_file")
                zwaveid ("@"//in_file, waveid_file, wave=wave,
			interactive=interactive, defwavext=defwavext)

                # Fit the wavelengths.
		if (verbose)
		    print ("zwavecal: Fitting lines.")
		wavefit_file = mktemp ("tmp$ZwCaL_wavefit_file")
		sx = ""
		if (fittab != "")
		    sx = fittab//"_"//cpos
                zwavefit (waveid_file, wavefit_file, fittab=sx)

		# Collect the temperature information.
		if (verbose)
		    print ("zwavecal: Collecting temperature information.")
		zavg_file = mktemp ("tmp$ZwCaL_zavg_file")
		zavgtemp ("@"//in_file, zavg_file)

		# Create the CCR6 table.
		if (verbose)
		    print ("zwavecal: Merging results into output table.")
		tmerge_file = mktemp ("tmp$ZwCaL_tmerge_file")
		tmerge (wavefit_file//","//zavg_file, tmerge_file, "merge",
			allcols=yes, tbltype="default", allrows=100,
			extracol=0)
		print (tmerge_file, >> tmerges_file)
            }
        }

	# Create the final CCR6 table.
	if (verbose)
	    print ("zwavecal: Creating output table.")
	tmerge ("@"//tmerges_file, poutput, "append", allcols=yes,
		tbltype="default", allrows=100, extracol=0)

	# Clean up tmp directory.
	delete ("tmp$ZwCaL*", go_ahead=yes, verify=no, >& "dev$null")
end
#---------------------------------------------------------------------------
# End of zwavecal
#---------------------------------------------------------------------------
