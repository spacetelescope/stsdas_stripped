include "ypp.h"

# do the FOS calibration summary page

procedure yppcalib ()

char    rootname[SZ_FNAME]
char    output[SZ_FNAME]
char	ftype[SZ_FNAME]

pointer	im
char    rootid[SZ_FNAME]
char	ref[SZ_PED, MAX_EXT]
char	ped[SZ_PED, MAX_EXT]
char    fname[SZ_FNAME]
char    c1h_ext[SZ_EXT]
char    d0h_ext[SZ_EXT]
char	refname[SZ_LINE]
char    linenum[SZ_LINENUM]
char    propid[SZ_LINE]
int	fd
int	nref
real	yoff
real	x1, x2, x3, x4, x5, y0

int	open()
int	access()
pointer	immap()
bool	streq()
#------------------------------------------------------------------------------
begin
        # read parameters
        call clgstr ("rootname", rootname, SZ_LINE)
        call clgstr ("output", output, SZ_LINE)
        call clgstr ("fits", ftype, SZ_FNAME)
 
        # construct necessary file name extensions
        if (streq(ftype,"fits")) {
            call strcpy ("_c1f.fits", c1h_ext, SZ_EXT)
        } else {
            call strcpy (".c1h", c1h_ext, SZ_EXT)
        }

        # construct file names
        call strcpy (rootname, fname, SZ_FNAME)
        call strcat (c1h_ext, fname, SZ_FNAME)

	if (access (fname, 0, 0) == NO) {
            if (streq(ftype,"fits")) {
                call strcpy ("_d0f.fits", d0h_ext, SZ_EXT)
            } else {
                call strcpy (".d0h", d0h_ext, SZ_EXT)
            }
            call strcpy (rootname, fname, SZ_FNAME)
            call strcat (d0h_ext, fname, SZ_FNAME)
	} 
	if (streq(ftype,"fits") ) call strcat("[0]",fname, SZ_FNAME)
	im = immap (fname, READ_ONLY, 0)
 
        # read keywords
        call imgstr (im, "LINENUM", linenum, SZ_LINENUM)
        call imgstr (im, "PROPOSID", propid, SZ_LINE)
        call imgstr (im, "ROOTNAME", rootid, SZ_LINE)

	# retrieve reference file pedigree 
	call pp_pedigree (im, ref, ped, nref)

        # open the output file
        fd = open (output, NEW_FILE, TEXT_FILE)

	# Start a new page.
        call pp_erase (fd)

        # draw the banner
        call obs_banner (fd, linenum, rootid, propid, "FOS", yoff)

	call fprintf (
		fd, "expand .75; limits 0 80 32 0; vpage 0.05 0.95 0.05 0.87\n")

	# locations markers
	x1 = 0.
	x2 = 9.
	x3 = 18.
	x4 = 35.
	x5 = 60.
	y0 = 2.

	# Headings.
	call fprintf (fd, "move 40 0; justify 2; label 'Calibration Status'\n")
	call pp_label (fd, x1+3., y0, "Flag")
	call pp_label (fd, x2+2., y0, "State")
	call pp_label (fd, x3+6., y0, "Reference File")
	call pp_label (fd, x4+8., y0, "Pedigree")
	call pp_label (fd, x5+9., y0, "Description")
	call fprintf (fd, "move 0 3; draw 80 3 \n")
	y0 = y0 + 2.

	# Values
	call fprintf (fd, "justify 3 \n")
	call pp_label (fd, x1, y0, "ERR_CORR")
	call pp_keywd (fd, x2, y0, im, "err_corr")
	call pp_label (fd, x5, y0, "Propogated Error Computation")

	call pp_label (fd, x1, y0+1., "CNT_CORR")
	call pp_keywd (fd, x2, y0+1., im, "cnt_corr")
        iferr (call imgstr (im, "ddthfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+1., refname)
	call get_ped  (fd, x4, y0+1., refname, ref, ped, nref)

	call pp_label (fd, x5, y0+1., "Disabled Diode Correction")
        iferr (call imgstr (im, "dq1hfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+2., refname)
	call get_ped  (fd, x4, y0+2., refname, ref, ped, nref)
        iferr (call imgstr (im, "dq2hfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+3., refname)
	call get_ped  (fd, x4, y0+3., refname, ref, ped, nref)

	call pp_label (fd, x1, y0+4., "OFF_CORR")
	call pp_keywd (fd, x2, y0+4., im, "off_corr")
        iferr (call imgstr (im, "ccs7", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+4., refname)
	call get_ped  (fd, x4, y0+4., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+4., "GIMP Correction")

	call pp_label (fd, x1, y0+5., "PPC_CORR")
	call pp_keywd (fd, x2, y0+5., im, "ppc_corr")
        iferr (call imgstr (im, "ccg2", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+5., refname)
	call get_ped  (fd, x4, y0+5., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+5., "Paired Pulse Correction")

	call pp_label (fd, x1, y0+6, "BAC_CORR")
	call pp_keywd (fd, x2, y0+6, im, "bac_corr")
        iferr (call imgstr (im, "bachfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+6., refname)
	call get_ped  (fd, x4, y0+6., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+6, "Background Subtraction")
        iferr (call imgstr (im, "ccs3", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+7., refname)
	call get_ped  (fd, x4, y0+7., refname, ref, ped, nref)
        iferr (call imgstr (im, "ccs9", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+8., refname)
	call get_ped  (fd, x4, y0+8., refname, ref, ped, nref)

	call pp_label (fd, x1, y0+9., "GMF_CORR")
	call pp_keywd (fd, x2, y0+9., im, "gmf_corr")
        iferr (call imgstr (im, "ccs8", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+9., refname)
	call get_ped  (fd, x4, y0+9., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+9., "Scale Reference Background")

	call pp_label (fd, x1, y0+10., "SCT_CORR")
	call pp_keywd (fd, x2, y0+10., im, "sct_corr")
        iferr (call imgstr (im, "ccs9", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+10., refname)
	call get_ped  (fd, x4, y0+10., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+10., "Scattered Light Correction")

	call pp_label (fd, x1, y0+11., "FLT_CORR")
	call pp_keywd (fd, x2, y0+11., im, "flt_corr")
        iferr (call imgstr (im, "fl1hfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+11., refname)
	call get_ped  (fd, x4, y0+11., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+11., "Flat-field Removal")
        iferr (call imgstr (im, "fl2hfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+12., refname)
	call get_ped  (fd, x4, y0+12., refname, ref, ped, nref)

	call pp_label (fd, x1, y0+13., "SKY_CORR")
	call pp_keywd (fd, x2, y0+13., im, "sky_corr")
        iferr (call imgstr (im, "ccs0", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+13., refname)
	call get_ped  (fd, x4, y0+13., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+13., "Sky Subtraction")
        iferr (call imgstr (im, "ccs2", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+14., refname)
	call get_ped  (fd, x4, y0+14., refname, ref, ped, nref)
        iferr (call imgstr (im, "ccs3", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+15., refname)
	call get_ped  (fd, x4, y0+15., refname, ref, ped, nref)
        iferr (call imgstr (im, "ccs5", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+16., refname)
	call get_ped  (fd, x4, y0+16., refname, ref, ped, nref)

	call pp_label (fd, x1, y0+17., "WAV_CORR")
	call pp_keywd (fd, x2, y0+17., im, "wav_corr")
        iferr (call imgstr (im, "ccs6", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+17., refname)
	call get_ped  (fd, x4, y0+17., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+17., "Wavelength Scale Determination")

	call pp_label (fd, x1, y0+18., "FLX_CORR")
	call pp_keywd (fd, x2, y0+18., im, "flx_corr")
        iferr (call imgstr (im, "iv1hfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+18., refname)
	call get_ped  (fd, x4, y0+18., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+18., "Flux Scale Generation")
        iferr (call imgstr (im, "iv2hfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+19., refname)
	call get_ped  (fd, x4, y0+19., refname, ref, ped, nref)

	call pp_label (fd, x1, y0+20., "APR_CORR")
	call pp_keywd (fd, x2, y0+20., im, "apr_corr")
        iferr (call imgstr (im, "ccsa", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+20., refname)
	call get_ped  (fd, x4, y0+20., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+20., "Aperture Throughput Correction")
        iferr (call imgstr (im, "ccsb", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+21., refname)
	call get_ped  (fd, x4, y0+21., refname, ref, ped, nref)
        iferr (call imgstr (im, "ccsc", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+22., refname)
	call get_ped  (fd, x4, y0+22., refname, ref, ped, nref)

	call pp_label (fd, x1, y0+23., "AIS_CORR")
	call pp_keywd (fd, x2, y0+23., im, "ais_corr")
        iferr (call imgstr (im, "aishfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+23., refname)
	call get_ped  (fd, x4, y0+23., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+23., "AIS Flux Scale Generation")

	call pp_label (fd, x1, y0+24., "TIM_CORR")
	call pp_keywd (fd, x2, y0+24., im, "tim_corr")
        iferr (call imgstr (im, "ccsd", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+24., refname)
	call get_ped  (fd, x4, y0+24., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+24., "Time Changes in Sensitivity")

	call pp_label (fd, x1, y0+25., "MOD_CORR")
	call pp_keywd (fd, x2, y0+25., im, "mod_corr")
        iferr (call imgstr (im, "rethfile", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+25., refname)
	call get_ped  (fd, x4, y0+25., refname, ref, ped, nref)
	call pp_label (fd, x5, y0+25., "Mode Dependent Corrections")
        iferr (call imgstr (im, "ccs4", refname, SZ_LINE)) refname[1] = EOS
        call pp_label (fd, x3, y0+26., refname)
	call get_ped  (fd, x4, y0+26., refname, ref, ped, nref)

	# close the image and output file
	call imunmap (im)
	call close (fd)
end


procedure get_ped (fd, x, y, refname, ref, ped, nref)

int	fd
real	x, y
char	refname[ARB]
char	ref[SZ_PED, ARB]
char	ped[SZ_PED, ARB]
int	nref

int	i

bool	streq()

begin
	do i = 1, nref {
	    if (streq (refname, ref[1,i])) {
        	call pp_label (fd, x, y, ped[1,i])
		break
	    }
	}
end
