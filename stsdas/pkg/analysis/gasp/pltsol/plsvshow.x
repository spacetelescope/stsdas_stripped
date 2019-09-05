include "iraf$pkg/xtools/icfit/icfit.h"
include "pls.h"

# PLS_VSHOW -- Type verbose error info

procedure pls_vshow (ic, file, pn, nterm, npts, plate_model,
		     xi_res, eta_res, xcoeff, ycoeff, xsig, ysig,
		     xchisqr, ychisqr)

pointer	ic		# ICFIT pointer
pointer pn		# pls pointer
char	file[ARB]	# Output file
int	plate_model[NTERMS_MODEL]
double  xi_res[npts], eta_res[npts]
double	xcoeff[nterm], ycoeff[nterm]
real	xsig[nterm], ysig[nterm]
int	npts, nterm

char	stime[SZ_LINE]
int	i, j, fd
double	xchisqr, ychisqr
pointer bas

int	open(), clktime()
errchk	open()

begin

	# Open the output file.
	fd = open (file, APPEND, TEXT_FILE)

	# Print the error analysis.
	call cnvtime (clktime(0), stime, SZ_LINE)
	call fprintf (fd, "\n%s\n")
	    call pargstr (stime)

	call fprintf (fd, "total points = %d\nsample points = %d\n")
	    call pargi (npts)
	    call pargi (IC_NFIT(ic))
	call fprintf (fd, "deleted = %d\n")
	    call pargi (IC_NREJECT(ic))
	call fprintf (fd, "xchisqr= %7.4g   ychisqr=%7.4g\n")
	    call pargd (xchisqr)
	    call pargd (ychisqr)

	call malloc (bas, TERML*NTERMS_MODEL, TY_CHAR)
	call get_model (bas)
	call fprintf (fd, "No  Term\tXi\terror\t\t Eta\t\t  error\n")
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

	call fprintf (fd, "		Individual star residuals\n")
	call fprintf (fd, "Star Flag     X(mm)      xi_res     Y(mm)      eta_res\n")
	do i = 0, npts-1 {
	     call fprintf (fd, "%2.2d    %d   %10.3f  %8.2f   %10.3f  %8.2f\n")
	     call pargi (i+1)
	     call pargr (Memr[PW(pn)+i])
	     call pargd (Memd[X_PREF(pn)+i])
	     call pargd (xi_res[i+1])
	     call pargd (Memd[Y_PREF(pn)+i])
	     call pargd (eta_res[i+1])
	}
	call fprintf (fd, "No  Term            value\n")
	do i = 1, NTERMS_MODEL {
	   call fprintf (fd, "%d  %s %22t%d\n")
		call pargi(i)
		call pargstr(Memc[bas+TERML*(i-1)])
		call pargi(plate_model[i])
	}
	call mfree (bas, TY_CHAR)

	call close (fd)
end
