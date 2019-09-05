include <tbset.h>

include	"../nlfit/nlfit.h"
include	"../lib/curfitdef.h"

define	LEN_FIELD	26

# PRFIT -- Format contents of tables generated by the fitting tasks.
# Input is given by a CL parameter, output is on STDOUT. Main procedure 
# is just for taking care of template expansion.

procedure t_prfit ()

char	list[SZ_LINE]
char	table[SZ_PATHNAME]
int	imt

int	i, j, len

int	imtopen(), imtgetim(), imtlen()

begin
	# Expand the input list

	call clgstr ("input", list, SZ_LINE)
	imt = imtopen (list)
	len = imtlen (imt)

	# Print each table

	do i = 1, len {
	    j = imtgetim (imt, table, SZ_PATHNAME)
	    call npr_list (table)
	    if ((i != len) && (len != 1))
	        call printf ("--------------------------------------------------------------------------")
	}

	call imtclose (imt)
end


# NPR_LIST -- List contents of one table.

procedure npr_list (table)

char	table[ARB]

pointer	coeff, cerror, coefflags, tb, sp
char	origfile[SZ_LINE], timestamp[SZ_LINE]
char	function[SZ_LINE], unit[SZ_LINE], str[SZ_LINE]
char	label[4]
int	i, size, row, maxrow, npar, npts
int	lf, nlf
real	rms, chisq, xmin, xmax
bool	errors

pointer	opendb()
int	tbpsta(), strdic()
bool	streq()

begin
	size = SZ_LINE
	call printf ("\ntable file:  %s\n\n")
	    call pargstr (table)

	# Open table and scan it.
	tb = opendb (table)
	maxrow = tbpsta (tb, TBL_NROWS)

	do row = 1, maxrow {

	    # read one table line
	    call smark (sp)
	    call getfit (table, row, origfile, timestamp, function, unit, 
			 SZ_LINE, npar, npts, rms, chisq, xmin, xmax, 
			 coeff, cerror, coefflags)

	    # get function type 
	    lf   = strdic (function, str, SZ_LINE, FUNCTIONS)
	    nlf  = strdic (function, str, SZ_LINE, NLFUNCTIONS)

	    # verify if coefficient errors are present 
	    errors = false
	    do i = 1, npar
	        if (Memr[cerror+i-1] != 0.) errors = true

	    # print

	    call printf ("Original file:  %s\nTime:           %s\n")
	        call pargstr (origfile)
	        call pargstr (timestamp)
	    call printf ("Row:            %d\n")
	        call pargi (row)
	    call printf ("Function:  %s %27tchi-square: %6.4g %53trms: %g\n")
	        call pargstr (function)
	        call pargr (chisq)
	        call pargr (rms)

	    if ((nlf != 0) && (unit[1] == '*'))
	        call strcpy ("Auto", unit, SZ_LINE)
	    else if ((lf != 0) || streq (function, PSPOLY))
	        call strcpy ("unknown", unit, SZ_LINE)
	    call printf ("Units:     %s %27tnpts:   %d\n")
	        call pargstr (unit)
	        call pargi (npts)

	    if (lf != 0) {
	        call printf ("Normalization interval (xmin,xmax): %g, %g\n")
	            call pargr (xmin)
	            call pargr (xmax)
	    }

	    if ( (nlf == GAUSSIANS) || (nlf == CGAUSS)) {
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "Zero", Memr[coeff+NL_GA],
					Memr[cerror+NL_GA], errors)
	        call npr_addfield (str, size, 2, "Slope", Memr[coeff+NL_GB],
					Memr[cerror+NL_GB], errors)
	        str[2 * LEN_FIELD]   = '\n'
	        str[2 * LEN_FIELD+1] = EOS
	        call printf ("%s")
	            call pargstr (str)
	        do i = 1, (npar-2)/3 {
	            call npr_clr (str, size)
	            call npr_addfield (str, size, 2, "ampl", 
					Memr[coeff+NL_GAMPL(i)],
					Memr[cerror+NL_GAMPL(i)], errors)
	            call npr_addfield (str, size, 1, "pos", 
					Memr[coeff+NL_GCENT(i)],
					Memr[cerror+NL_GCENT(i)], errors)
	            call npr_addfield (str, size, 3, "fwhm", 
					Memr[coeff+NL_GFWHM(i)],
					Memr[cerror+NL_GFWHM(i)], errors)
	            call printf ("%s")
	                call pargstr (str)
	        }

	    } else if (nlf == TWODGAUSS) {
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "Ampl.", 
				   Memr[coeff+NL_G2AMPL], 
				   Memr[cerror+NL_G2AMPL], errors)
	        call npr_addfield (str, size, 2, "X", 
				   Memr[coeff+NL_G2XC], 
				   Memr[cerror+NL_G2XC], errors)
	        call npr_addfield (str, size, 3, "FWHM", 
				   Memr[coeff+NL_G2FWHM], 
				   Memr[cerror+NL_G2FWHM], errors)
	        call printf ("%s")
	            call pargstr (str)

	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "Back.", 
				   Memr[coeff+NL_G2A], 
				   Memr[cerror+NL_G2A], errors)
	        call npr_addfield (str, size, 2, "Y", 
				   Memr[coeff+NL_G2YC], 
				   Memr[cerror+NL_G2YC], errors)
	        call npr_addfield (str, size, 3, "Ellip.", 
				   Memr[coeff+NL_G2ELL], 
				   Memr[cerror+NL_G2ELL], errors)
	        call printf ("%s")
	            call pargstr (str)

	        call npr_clr (str, size)
	        call npr_addfield (str, size, 3, "Ang.", 
				   Memr[coeff+NL_G2TETA]/3.1415927*180., 
				   Memr[cerror+NL_G2TETA]/3.1415927*180., errors)
	        call printf ("%s")
	            call pargstr (str)

	    } else if (nlf == POWERLAW) {
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "index", Memr[coeff+NL_PINDEX], 
				   Memr[cerror+NL_PINDEX], errors)
	        call npr_addfield (str, size, 2, "ampl", Memr[coeff+NL_PAMPL],
				   Memr[cerror+NL_PAMPL], errors)
	        call npr_addfield (str, size, 3, "ref", Memr[coeff+NL_PREF],
				   Memr[cerror+NL_PREF], errors)
	        call printf ("%s")
	            call pargstr (str)

	    } else if (nlf == BBODY) {
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "temp", Memr[coeff+NL_BTEMP], 
				   Memr[cerror+NL_BTEMP], errors)
	        call npr_addfield (str, size, 2, "ampl", Memr[coeff+NL_BAMPL],
				   Memr[cerror+NL_BAMPL], errors)
	        call npr_addfield (str, size, 3, "ref", Memr[coeff+NL_BREF],
				   Memr[cerror+NL_BREF], errors)
	        call printf ("%s")
	            call pargstr (str)

	    } else if (nlf == TWOBBODY) {
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "temp1", Memr[coeff+NL_TTEMP1], 
				   Memr[cerror+NL_TTEMP1], errors)
	        call npr_addfield (str, size, 2, "ampl1", Memr[coeff+NL_TAMPL1],
				   Memr[cerror+NL_TAMPL1], errors)
	        call npr_addfield (str, size, 3, "ref1", Memr[coeff+NL_TREF1],
				   Memr[cerror+NL_TREF1], errors)
	        call printf ("%s")
	            call pargstr (str)
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "temp2", Memr[coeff+NL_TTEMP2], 
				   Memr[cerror+NL_TTEMP2], errors)
	        call npr_addfield (str, size, 2, "ampl2", Memr[coeff+NL_TAMPL2],
				   Memr[cerror+NL_TAMPL2], errors)
	        call npr_addfield (str, size, 3, "ref2", Memr[coeff+NL_TREF2],
				   Memr[cerror+NL_TREF2], errors)
	        call printf ("%s")
	            call pargstr (str)

	    } else if (nlf == COMPOSITE) {
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "index", Memr[coeff+NL_CINDEX], 
				   Memr[cerror+NL_CINDEX], errors)
	        call npr_addfield (str, size, 2, "pampl", Memr[coeff+NL_CPAMPL],
				   Memr[cerror+NL_CPAMPL], errors)
	        call npr_addfield (str, size, 3, "pref", Memr[coeff+NL_CPREF],
				   Memr[cerror+NL_CPREF], errors)
	        call printf ("%s")
	            call pargstr (str)
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "temp", Memr[coeff+NL_CTEMP], 
				   Memr[cerror+NL_CTEMP], errors)
	        call npr_addfield (str, size, 2, "bampl", Memr[coeff+NL_CBAMPL],
				   Memr[cerror+NL_CBAMPL], errors)
	        call npr_addfield (str, size, 3, "bref", Memr[coeff+NL_CBREF],
				   Memr[cerror+NL_CBREF], errors)
	        call printf ("%s")
	            call pargstr (str)

	    } else if (nlf == GALPROF) {
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "se", Memr[coeff+NL_GBME], 
				   Memr[cerror+NL_GBME], errors)
	        call npr_addfield (str, size, 2, "re", Memr[coeff+NL_GBRE],
				   Memr[cerror+NL_GBRE], errors)
	        call npr_addfield (str, size, 3, "backg", Memr[coeff+NL_GBACKGR],
				   Memr[cerror+NL_GBACKGR], errors)
	        call printf ("%s")
	            call pargstr (str)
	        call npr_clr (str, size)
	        call npr_addfield (str, size, 1, "s0", Memr[coeff+NL_GDM0], 
				   Memr[cerror+NL_GDM0], errors)
	        call npr_addfield (str, size, 2, "r0", Memr[coeff+NL_GDR0],
				   Memr[cerror+NL_GDR0], errors)
	        call npr_addfield (str, size, 3, "s1", Memr[coeff+NL_GDRH],
				   Memr[cerror+NL_GDRH], errors)
	        call printf ("%s")
	            call pargstr (str)

	    } else if (nlf == USER) {
	        do i = 0, npar-1 {
	            call npr_clr (str, size)
	            call sprintf (label, 4, "c%d")
	                call pargi (i+1) 
	            call npr_addfield (str, size, 1, label, 
					Memr[coeff+i], Memr[cerror+i], errors)
	            str[LEN_FIELD]   = '\n'
	            str[LEN_FIELD+1] = EOS
	            call printf ("%s")
	                call pargstr (str)
	        }

	    } else if ((lf == CV_SP3) || (lf == CV_SP1)) {
	        do i = 0, npar-1 {
	            call npr_clr (str, size)
	            call sprintf (label, 4, "s%d")
	                call pargi (i) 
	            call npr_addfield (str, size, 1, label, 
					Memr[coeff+i], Memr[cerror+i], errors)
	            str[LEN_FIELD]   = '\n'
	            str[LEN_FIELD+1] = EOS
	            call printf ("%s")
	                call pargstr (str)
	        }

	    } else if ((lf == CV_CHEB) || (lf == CV_LEG) ||
			streq (function, PSPOLY)) {
	        do i = 0, npar-1 {
	            call npr_clr (str, size)
	            call sprintf (label, 4, "a%d")
	                call pargi (i) 
	            call npr_addfield (str, size, 1, label, 
					Memr[coeff+i], Memr[cerror+i], errors)
	            str[LEN_FIELD]   = '\n'
	            str[LEN_FIELD+1] = EOS
	            call printf ("%s")
	                call pargstr (str)
	        }
	    }
	    call printf ("\n")
	    call sfree (sp)
	}
	call tbtclo (tb)
end


# NPR_ADDFIELD --  Add a formatted field to an output line kept on a
# string. The line is divided in three fields of 26 columns each.

procedure npr_addfield (str, size, field, name, value, error, errflag)

char	str[ARB]		# string to be updated
int	size			# string size
int	field			# field number
char	name[ARB]
real	value
real	error
bool	errflag			# true -> errors present on table

char	tempstr[SZ_LINE]
int	strlen()

begin
	call sprintf (tempstr, SZ_LINE, " %-5.5s =%9.4g")
	    call pargstr (name)
	    call pargr (value)

	if (errflag) {
	    if (error == 0.) {
	        call sprintf (tempstr[1+strlen (tempstr)], SZ_LINE, "( fixed )")
	    } else {
	        call sprintf (tempstr[1+strlen (tempstr]), SZ_LINE, "(%7.2g)")
	            call pargr (error)
	    }
	}

	call strcpy (tempstr, str[(field-1)*LEN_FIELD], size)
	if (field == 3) {
	    str[(field-1)*LEN_FIELD + strlen (tempstr)]    = '\n'
	    str[(field-1)*LEN_FIELD + strlen (tempstr)+ 1] = EOS
	} else
	    str[(field-1)*LEN_FIELD + strlen (tempstr)] = ' '	# Clobber EOS
end



# NPR_CLR -- Clears output line.

procedure npr_clr (str, size)

char	str[ARB]
int	size, i

begin
	do i = 1, size-1
	    str[i] = ' '
	str[size] = EOS
end

