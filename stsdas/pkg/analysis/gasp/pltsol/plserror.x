include "iraf$pkg/xtools/icfit/icfit.h"
include "pls.h"

# PLS_ERROR -- Compute and error diagnositic information.

procedure pls_error (ic, file, nterm, npts, plate_model,
		     xi_res, eta_res, xcoeff, ycoeff, xsig, ysig,
		     xchisqr, ychisqr)

pointer	ic		# ICFIT pointer
char	file[ARB]	# Output file
int	plate_model[NTERMS_MODEL]
double  xi_res[npts], eta_res[npts]
double	xcoeff[nterm], ycoeff[nterm]
real	xsig[nterm], ysig[nterm]
int	npts, nterm

pointer bas, sp
int	i, j, fd
double	xchisqr, ychisqr

int	open()
errchk	open()

begin
	# Open the output file.
	fd = open (file, APPEND, TEXT_FILE)

	# Print the error analysis.
	call fprintf (fd, "total points = %d\nsample points = %d\n")
	    call pargi (npts)
	    call pargi (IC_NFIT(ic))
	call fprintf (fd, "deleted = %d\n")
	    call pargi (IC_NREJECT(ic))
	call fprintf (fd, "xchisqr= %7.4g   ychisqr=%7.4g\n")
	    call pargd (xchisqr)
	    call pargd (ychisqr)

	call smark (sp)
	call salloc (bas, TERML*NTERMS_MODEL, TY_CHAR)
	call get_model(bas)
	call fprintf (fd, "No  Term %19tXi%33terror%50tEta%65terror\n")
	j = 1
	do i = 1, NTERMS_MODEL {
	   if (plate_model(i) == 1) {
	      call fprintf (fd, "%d%4t%s\t%15.8g\t%10.4e\t%15.8g\t%10.4e\n")
		  call pargi (i)
		  call pargstr (Memc[bas+TERML*(i-1)])
		  call pargd (xcoeff(j))
		  call pargr (xsig(j))
		  call pargd (ycoeff(j))
		  call pargr (ysig(j))
	      j = j + 1	
	   }
	}

	# Free allocated memory.
	call sfree(sp)
	call close (fd)
end
