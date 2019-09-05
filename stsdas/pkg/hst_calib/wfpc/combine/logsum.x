include	<imhdr.h>
include	<imset.h>
include	<time.h>

#################################################################################
# LOGSUM --	Log information about sum combining.  This routine is taken 	#
#		from the `images.imcombine' package.  				#
#										#
#		Revised:	1/91	 by RAShaw.  Added comments.		#

procedure logsum (in, out, nimages, log, str)

# Calling arguments:
pointer		in[nimages]		# Input images
pointer		out			# Output image
int		nimages			# Number of images to combine
int		log			# Log file descriptor
char		str[ARB]		# Log string (includes combine option)

# Local variables:
pointer		expname			# Name of keyword containing exposure time
real		exposure		# 
pointer		fname			# 
int		i			# Loop counter
pointer		sp			# Pointer to stack memory
pointer		t			# Exposure duration @image
pointer		time			# 

# Functions used:
long		clktime()		# Get current time
real		imgetr()		# Get real FITS keyword from image header

begin
	call smark (sp)
	call salloc (expname, SZ_FNAME, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (t, nimages, TY_REAL)

# Determine the exposure times.
	call clgstr ("expname", Memc[expname], SZ_FNAME)
	if (Memc[expname] != EOS) {
	    exposure = 0.
	    do i = 1, nimages {
	        iferr (Memr[t+i-1] = imgetr (in[i], Memc[expname]))
		    Memr[t+i-1] = 0.
	        exposure = exposure + Memr[t+i-1]
	    }
	    call gf_iaddr (out, "exptime", exposure)
	}

# Append to the logfile if not null.
	call cnvdate (clktime(0), Memc[time], SZ_DATE)
	if (log != NULL) {
	    call fprintf (log, "%s combine: %s\n")
		call pargstr (Memc[time])
		call pargstr (str)
	    if (Memc[expname] != EOS) {
	        call fprintf (log, "  %20s %6s\n")
	            call pargstr ("Images")
	            call pargstr ("Exp")
	        do i = 1, nimages {
		    call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	            call fprintf (log, "  %20s %6.1f\n")
		        call pargstr (Memc[fname])
		        call pargr (Memr[t+i-1])
	        }
	        call fprintf (log, "  -------------------- ------\n")
	        call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call fprintf (log, "  %20s %6.1f\n")
		    call pargstr (Memc[fname])
	            call pargr (exposure)
	    } else {
	        call fprintf (log, "  %20s\n")
	            call pargstr ("Images")
	        do i = 1, nimages {
		    call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	            call fprintf (log, "  %20s\n")
		        call pargstr (Memc[fname])
	        }
	        call fprintf (log, "  --------------------\n")
	        call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call fprintf (log, "  %20s\n")
		    call pargstr (Memc[fname])
	    }
	    call flush (log)
	}

	call sfree (sp)
end
