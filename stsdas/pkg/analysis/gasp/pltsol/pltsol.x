include <tbset.h>
include	<error.h>
include <config.h>
include <pkg/gtools.h>
include "pls.h"
include "iraf$pkg/xtools/icfit/icfit.h"

# PLTSOL-- Fit a function to image lines or columns and output an image
# consisting of the fit, the difference, or the ratio.  The fitting parameters
# may be set interactively using the icfit package.

procedure t_pltsol()

int	plate_model[NTERMS_MODEL]	# Flag term model for fit
char	input[SZ_FNAME]			# image name
char	tinput[SZ_FNAME]		# input table name
pointer	ic				# ICFIT pointer
pointer	gt				# GTOOLS pointer
pointer	gp
pointer	gopen()

char	graphics[SZ_FNAME]		# Graphics device
int	i
char	ttemp[SZ_FNAME]
char	extn[MAX_EXTNLEN]
pointer	im, tp, pn
int	tbtopn(), immap(), open()
int	gt_init()
int	clgeti()
int	strncmp(), fnextn(), tbpsta()
int	nchar, ncols, nobjs
bool	iminfo, clgetb()
real	x_pixel_size, y_pixel_size
double	crpix1, crpix2
double	crval1, crval2
double  plate_scale

begin
	
	# get input parameters
	iminfo = clgetb ("iminfo")
	if (iminfo)
	   call clgstr ("infile", input, SZ_LINE)
	call clgstr ("tinput", tinput, SZ_LINE)

	do i = 1, NTERMS_MODEL {
	  call sprintf(ttemp, SZ_LINE, "pmod%d")
	       call pargi(i)
	  plate_model[i] = clgeti (ttemp)
	}
	
	im = -1
	if (iminfo) {
	   im = immap (input, READ_WRITE, 0)
	   call get_impar (im, crpix1, crpix2, crval1, crval2,
			   x_pixel_size, y_pixel_size, plate_scale)
	} else {
	   call get_clpar (crpix1, crpix2, crval1, crval2,
			   x_pixel_size, y_pixel_size, plate_scale)
	   im = -1	# No image is open
	}

	nchar = fnextn (tinput, extn, MAX_EXTNLEN)
	if (strncmp (extn, "tab", 3) == 0) {	  # is stsdas table
	   tp = tbtopn (tinput, READ_ONLY, 0)
	   # read table data and load arrays
           ncols = tbpsta (tp, TBL_NCOLS)
	   nobjs = tbpsta (tp, TBL_NROWS)
	   # allocate fixed and transient memory
	   call pls_alloc (pn, nobjs)
	   call rd_tab (tp, pn, crpix1, crpix2, crval1, crval2, 
		    nobjs, ncols, x_pixel_size, y_pixel_size)
	   call tbtclo (tp)
	} else {
	   iferr { 				# is ascii table
	      tp = open (tinput, READ_ONLY, TEXT_FILE)
	   } then {
	      call eprintf("Table %s does not exist \n")
		 call pargstr (tinput)
	      call erract(EA_FATAL)
	   }
	   # read text table data and load_arrays
	   nobjs = 100       # starting number
	   call pls_alloc (pn, nobjs)
	   call rd_txt (tp, pn, crpix1, crpix2, crval1, crval2, 
		       nobjs, x_pixel_size, y_pixel_size)
	   call close (tp)
	}
	gt = gt_init()
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTMARK, "diamond")

	call clgstr ("graphics", graphics, SZ_FNAME)
	gp = gopen (graphics, NEW_FILE, STDGRAPH)
	call ic_open (ic)
	call icg_fitp (ic, gp, "cursor", gt, im, pn, plate_model, 
		       nobjs, x_pixel_size, y_pixel_size,
		       crpix1, crpix2, crval1, crval2, plate_scale)

	if (iminfo)
	   call imunmap (im)
	call ic_closer (ic)
	call gclose (gp)
	call gt_free (gt)

	call pls_free (pn)
end

procedure ic_close_local (ic)
pointer ic
begin
	if (ic != NULL) {
	# Free memory for the package parameter structure.
	if(IC_RG(ic) != NULL)
	  call mfree(IC_RG(ic))
	if(IC_XFIT(ic) != NULL)
	  call mfree(IC_XFIT(ic))
	if(IC_YFIT(ic) != NULL)
	  call mfree(IC_YFIT(ic))
	if(IC_WTSFIT(ic) != NULL)
	  call mfree(IC_WTSFIT(ic))
	if(IC_REJPTS(ic) != NULL)
	  call mfree(IC_REJPTS(ic))
	call mfree (IC_SAMPLE(ic), TY_CHAR)
	call mfree (IC_LABELS(ic,1), TY_CHAR)
	call mfree (IC_LABELS(ic,2), TY_CHAR)
	call mfree (IC_UNITS(ic,1), TY_CHAR)
	call mfree (IC_UNITS(ic,2), TY_CHAR)
	call mfree (IC_HELP(ic), TY_CHAR)
	call mfree (ic, TY_STRUCT)
	}
end
