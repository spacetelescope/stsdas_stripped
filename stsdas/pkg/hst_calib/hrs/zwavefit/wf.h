#---------------------------------------------------------------------------
.help wf.h 23Mar95 source
.ih
NAME
wf.h -- Definitions for the Wavefit object.
.endhelp
#---------------------------------------------------------------------------
#====
# WF structure.
define	WF_X1_PTR	Memi[($1)+0]	# First independent variable
define	WF_X2_PTR	Memi[($1)+1]	# Second independent variable
define	WF_Y_PTR	Memi[($1)+2]	# Dependent variable
define	WF_SIG_PTR	Memi[($1)+3]	# Errors in dependent variable
define	WF_A_PTR	Memi[($1)+4]	# Coefficients
define	WF_AFIT_PTR	Memi[($1)+5]
define	WF_NCOEF	Memi[($1)+6]	# Number of coefficients
define	WF_NDATA	Memi[($1)+7]	# Number of data points
define	WF_NTRY		Memi[($1)+8]	# Number of rejection loops
define	WF_NSIG		Memr[($1)+9]	# Number of sigmas to reject.

# Output table information.
define	WF_OT		Memi[($1)+10]	# Output table descriptor.
define	WF_OT_CP_PTR	Memi[($1)+11]	# Output column descriptors.
define	WF_OT_NROWS	Memi[($1)+12]	# Number of rows in output table.

# Aperture conversion info.
define	WF_GRATING	Memi[($1)+13]	# Grating
define	WF_APER		Memi[($1)+14]	# Aperture
define	WF_CS_PTR	Memi[($1)+15]	# Aperture conversion coefficients.
define	WF_CARPOS	Memi[($1)+16]	# Carrousel position.
define	WF_SZ		17

# Array access
define	WF_X1		Memd[WF_X1_PTR($1)+($2)-1]
define	WF_X2		Memd[WF_X2_PTR($1)+($2)-1]
define	WF_Y		Memd[WF_Y_PTR($1)+($2)-1]
define	WF_SIG		Memd[WF_SIG_PTR($1)+($2)-1]
define	WF_A		Memd[WF_A_PTR($1)+($2)-1]
define	WF_AFIT		Memi[WF_AFIT_PTR($1)+($2)-1]
define	WF_OT_CP	Memi[WF_OT_CP_PTR($1)+($2)-1]
define	WF_CS		Memd[WF_CS_PTR($1)+($2)-1]

# Define number of coefficients for aperture conversion.
define	WF_N_CS		6

# Define grating conversion.  The side 1 gratings must appear first.
define	GRATING_DICT	",ECH-A,G140M,G140L,ECH-B,G160M,G200M,G270M"
define	N_GRATINGS	7

# Define aperture conversion.
define	APER_DICT	",SC1,SC2"

# Define columns for the output table.
define	WF_OT_GRATING_SZ	9
define	WF_OT_NCOLS	(2+WF_NCOEF(wf))
define	WF_OT_COEF_COL	3
define	WF_OT_CARPOS	1
define	WF_OT_GRATING	2
#---------------------------------------------------------------------------
# End of wf.h
#---------------------------------------------------------------------------
