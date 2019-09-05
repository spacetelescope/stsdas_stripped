include	<error.h>
include	"wf.h"

# Define the number of coefficients in use.
define	NCOEF		8

# Memory management
define	Sx		Memc[sx]

#---------------------------------------------------------------------------
.help t_wavefit 23Mar95 source
.ih
NAME
t_wavefit -- Task implementing fitting for the GHRS wavelength solution.
.endhelp
#---------------------------------------------------------------------------
procedure t_wavefit()

# Declarations
bool	clgetb()		# Get boolean-valued parameter.
double	clgetd()		# Get double-valued parameter.
int	clgeti()		# Get integer-valued parameter.
real	clgetr()		# Get real-valued parameter.
int	defcoef[NCOEF,N_GRATINGS]
extern	zdc			# GHRS wavelength function.
int	i
pointer	sp			# Stack pointer.
pointer	sx			# Generic string.
pointer	wf			# Wavefit object.
pointer	wf_alloc()		# Allocate a Wavefit object.

# The default coefficients.  The array defcoef is indexed using the
# grating number assigned by the order of the grating names in the string
# GRATING_DICT.
data	defcoef/YES,YES,YES, NO,YES, NO, NO, NO,	# ECH-A
                YES,YES,YES, NO, NO, NO, NO, NO,	# G140M
                YES,YES,YES, NO, NO, NO, NO, NO,	# G140L
                YES,YES,YES, NO,YES, NO, NO, NO,	# ECH-B
                YES,YES,YES, NO, NO, NO, NO, NO,	# G160M
                YES,YES,YES, NO, NO, NO, NO, NO,	# G200M
                YES,YES,YES, NO, NO, NO, NO, NO/	# G270M
begin
	call smark (sp)
	call salloc (sx, SZ_PATHNAME, TY_CHAR)

        wf = wf_alloc (NCOEF)
        
        # Open table containing data to fit.
        call clgstr ("input", Sx, SZ_PATHNAME)
	call wf_rtab (wf, Sx)

        # Open the output table.
        call clgstr ("output", Sx, SZ_PATHNAME)
	call wf_o_alloc (wf, Sx)

        # Initialize the other fitting parameters
	WF_NTRY(wf) = clgeti ("maxtry")
	WF_NSIG(wf) = clgetr ("nsigma")
	if (clgetb ("defcoef")) {
	    call amovkd (0.0d0, WF_A(wf,1), NCOEF)
	    call amovi (defcoef[1,WF_GRATING(wf)], WF_AFIT(wf,1), NCOEF)
	} else
	    do i = 1, NCOEF {
		call sprintf (Sx, SZ_PATHNAME, "a%d")
		call pargi (i-1)
		WF_A(wf,i) = clgetd (Sx)
		if (IS_INDEFD(WF_A(wf,i))) {
		    WF_AFIT(wf,i) = YES
		    WF_A(wf,i) = 0.d0
		} else
		    WF_AFIT(wf,i) = NO
	    }

        # Read the aperture offset table if specified.
        call clgstr ("offlib", Sx, SZ_PATHNAME)
        iferr (call wf_off_rtab (wf, Sx)) {
            call erract (EA_WARN)
            call eprintf ("wavefit: Wavelength solution cannot be converted to SSA aperture.\n\tIf used with calhrs, set IAC_CORR to 'OMIT'.\n\n")
            WF_CS(wf,1) = INDEFD
        }
        
        # Perform the fitting.
	call clgstr ("fittab", Sx, SZ_PATHNAME)
        call wf_wavefit (wf, Sx, zdc)

        # Write out the coefficients
	call wf_o_wtab (wf)

        # That's all folks.
        call wf_free (wf)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_wavefit
#---------------------------------------------------------------------------
