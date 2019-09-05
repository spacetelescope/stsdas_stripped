include	<gset.h>
include	<error.h>

# List of colon commands.
define	CMDS   "|show|gain|readnoise|scalenoise|group|binsize|ksigreject|clip|\
		|xrange|yrange|write|"

define	SHOW		1	# Show parameters
define	GAIN		2	# Set detector gain
define	READNOISE	3	# Set detector readnoise
define	SCALENOISE	4	# Set detector scalenoise
define	GROUP		5	# Set image group 
define	BINSIZE		6	# Set size of bin along x,y axes
define	KSIGREJECT	7	# Set K-sigma pixel rejection threshold
define	CLIP		8	# Set Chi-squared rejection thresholds
# newline		9
define	XRANGE		10	# Set range of X-axis for plot
define	YRANGE		11	# Set range of Y-axis for plot
define	WRITE		12	# Write parameters

#################################################################################
# NM_COLON -- Interpret/respond to colon command.  				#
#										#
#	6/91	RAShaw	Initial code						#
#	8/91	RAShaw	Added chisqr rejection threshold			#

procedure nm_colon (gp, cmdstr, group, ksigma, xsize, ysize, xmin, xmax, 
			ymin, ymax, newplot)

include	"nmcom.h"

#  Calling arguments:
pointer	gp			# Graphics pointer
char	cmdstr[ARB]		# Colon command
int	group			# Mapped image group
real	ksigma			# Pixel rejection threshold
int	xsize			# Bin size along first axis
int	ysize			# Bin size along second axis
real	xmin, xmax		# Min, max of X-axis to plot
real	ymin, ymax		# Range of Y-axis to plot
char	newplot[ARB]		# Flag for new plot

#  Local variables:
char	cmd[SZ_LINE]		# Input command string
int	ncmd			# CMDS index
int	ival, ival2		# INT input values
real	rval, rval2		# REAL input values 

# Functions called:
int	nscan(), strdic()

begin

#  Scan the command string and get the first word.
	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)
	call strcpy (EOS, newplot, SZ_LINE)

	switch (ncmd) {
	case SHOW:		# :show - show values of all parameters
	    call gargwrd (cmd, SZ_LINE)
	    call gdeactivate (gp, AW_CLEAR)
	    call nm_show (group, ksigma, xsize, ysize)
	    call greactivate (gp, AW_PAUSE)
	case GAIN:		# :gain - set detector gain
	    call gargr (rval)				# Read gain value

#  Show present gain setting
	    if (nscan() == 1) {
		call printf ("gain %g e-/DN \n")
		    call pargr (gain)
	    } else {

#  Set gain if positive
		if (rval > 0.) {
		    gain = rval
		    call strcpy ("model", newplot, SZ_LINE)
		} else {
		    call printf ("gain: %g too small\n")
		    call pargr (rval)
		}
	    }
	case READNOISE:		# :readnoise - set detector readnoise
	    call gargr (rval)				# Read readnoise

#  Show present readnoise
	    if (nscan() == 1) {
		call printf ("readnoise %g e-/DN \n")
		    call pargr (readn)
	    } else {

#  Set readnoise if non-negative
		if (rval >= 0.) {
		    readn = rval
		    call strcpy ("model", newplot, SZ_LINE)
		} else
		    call erract (EA_WARN)
	    }
	case SCALENOISE:	# :scalenoise - set detector scalenoise
	    call gargr (rval)

#  Show present scalenoise
	    if (nscan() == 1) {
	        call printf ("scalenoise %5.2f\n")
		    call pargr (scalen)
	    } else {

#  Set scalenoise if non-negative
		if (rval >= 0.) {
		    scalen = rval
		    call strcpy ("model", newplot, SZ_LINE)
		} else {
		    call printf ("scalenoise: %d too small\n")
		    call pargr (rval)
		}
	    }
	case GROUP:		# :group - set image group
	    call gargi (ival)				# Read group no.

#  Show present image group
	    if (nscan() == 1) {
		call printf ("group %d\n")
		    call pargi (group)
	    } else {

#  Set image group only if different from present value
		if (ival != group) {
		    if (ival > 0 && ival < 5) {
			group = ival
			call strcpy ("data", newplot, SZ_LINE)
		    } else {
			call printf ("group: %d--must be 1 <= group <= 4\n")
			call pargi (ival)
		    }
		}
	    }
	case BINSIZE: 		# :binsize - set size of bin along x,y axes
	    call gargi (ival)				# Read X-size
	    call gargi (ival2)				# Read Y-size

#  Show present X- and Y-sizes
	    if (nscan() == 1) {
	        call printf ("xbinsize: %d  ybinsize: %d\n")
		    call pargi (xsize)
		    call pargi (ysize)

#  Set X-size only (if different from present value)
	    } else if (nscan() == 2) {
		if (ival*ysize >= BINMIN && ival != xsize) {
		    xsize = ival
		    call strcpy ("data", newplot, SZ_LINE)
		} else {
		    call printf ("binsize: %d too small\n")
		    call pargi (ival*ysize)
		}

#  Set both X- and Y-size
	    } else if (nscan() == 3) {
		if (ival*ival2 >= BINMIN) {
		    xsize = ival
		    ysize = ival2
		    call strcpy ("data", newplot, SZ_LINE)
		} else {
		    call printf ("binsize: %d too small\n")
		    call pargi (ival*ival2)
		}
	    }
	case KSIGREJECT:	# :ksigreject - set pixel rejection threshold
	    call gargr (rval)

#  Show pixel rejection threshold
	    if (nscan() == 1) {
		call printf ("ksigma %g\n")
		    call pargr (ksigma)

#  Set new pixel rejection threshold only if different from present value
	    } else {
		if (rval != ksigma) {
		    if (rval > 0.) {
			ksigma = rval
			call strcpy ("data", newplot, SZ_LINE)
		    } else {
			call printf ("ksigma: %g must be > 0.\n")
			call pargr (rval)
		    }
		}
	    }
	case CLIP:	# :clip - set chi-squared rejection threshold
	    call gargr (rval)				# Read ChiLow
	    call gargr (rval2)				# Read ChiHigh

#  Show pixel rejection threshold
	    if (nscan() == 1) {
		call printf ("cliplo: %5g  cliphi: %5g\n")
		    call pargr (cliplo)
		    call pargr (cliphi)

#  Set low pixel rejection threshold only if different from present value
	    } else if (nscan() == 2) {
		if (rval != cliplo) {
		    if (rval > 0.) {
			cliplo = rval
			call strcpy ("plot", newplot, SZ_LINE)
		    } else {
			call printf ("cliplo: %g must be > 0.\n")
			call pargr (rval)
		    }
		}

#  Set low and high pixel rejection thresholds only if different from present 
#  values
	    } else if (nscan() == 3) { 
		if (rval != cliplo) {
		    if (rval > 0.) {
			cliplo = rval
			call strcpy ("plot", newplot, SZ_LINE)
		    } else {
			call printf ("cliplo: %g must be > 0.\n")
			call pargr (rval)
		    }
		} 
		if (rval2 != cliphi) {
		    if (rval2 > 0.) {
			cliphi = rval2
			call strcpy ("plot", newplot, SZ_LINE)
		    } else {
			call printf ("cliphi: %g must be > 0.\n")
			call pargr (rval2)
		    }
		}
	    }
	case XRANGE: 		# :xrange - set min,max for x axis
	    call gargr (rval)				# Read Xmin
	    call gargr (rval2)				# Read Xmax

#  Show present Xrange
	    if (nscan() == 1) {
	        call printf ("xmin: %d  xmax: %d\n")
		    call pargr (xmin)
		    call pargr (xmax)

#  Set Xmin if it exceeds Xmax
	    } else if (nscan() == 2) {
		if (rval >= xmax) {
		    call printf ("xmin: %d must be less than xmax\n")
		    call pargr (rval)
		} else if (rval < -1.e+4) {
		    call printf ("xmin: %d too small\n")
		    call pargr (rval)
		} else {
		    xmin = rval
		    call strcpy ("plot", newplot, SZ_LINE)
		}

#  Set both Xmin and Xmax
	    } else if (nscan() == 3) {
		if (rval < -1.e+4) {
		    call printf ("xmin: %d too small\n")
		    call pargr (rval)
		} else {
		    xmin = rval
		    call strcpy ("plot", newplot, SZ_LINE)
		}
		if (rval >= rval2) {
		    call printf ("xmax: %d must exceed xmin\n")
		    call pargr (rval)
		} else {
		    xmax = rval2
		    call strcpy ("plot", newplot, SZ_LINE)
		}
	    }
	case YRANGE: 		# :yrange - set min,max for y axis
	    call gargr (rval)
	    call gargr (rval2)

#  Show present Yrange
	    if (nscan() == 1) {
	        call printf ("ymin: %d  ymax: %d\n")
		    call pargr (ymin)
		    call pargr (ymax)

#  Set Ymin if it exceeds Xmax
	    } else if (nscan() == 2) {
		if (rval >= ymax) {
		    call printf ("ymin: %d must be less than ymax\n")
		    call pargr (rval)
		} else if (rval <= 0.) {
		    call printf ("ymin: %d too small\n")
		    call pargr (rval)
		} else {
		    ymin = rval
		    call strcpy ("plot", newplot, SZ_LINE)
		}

#  Set both Ymin and Ymax
	    } else if (nscan() == 3) {
		if (rval <= 0.) {
		    call printf ("ymin: %d too small\n")
		    call pargr (rval)
		} else {
		    ymin = rval
		    call strcpy ("plot", newplot, SZ_LINE)
		}
		if (rval >= rval2) {
		    call printf ("ymax: %d must exceed ymin\n")
		    call pargr (rval)
		} else {
		    ymax = rval2
		    call strcpy ("plot", newplot, SZ_LINE)
		}
	    }
	case WRITE: # :write - write gain, readn, scalen to parameter file
	    iferr {
		call clputr ("gain", gain)
		call clputr ("readnoise", readn)
		call clputr ("scalenoise", scalen)
	    } then
		call erract (EA_WARN)

#  Unrecognized command: ring cursor bell
	default:
	    call printf ("\7")
	    call printf ("Invalid colon command\n")
	}
end


#################################################################################
# NM_SHOW --	Show all parameter values.  					#
#										#
#	6/91	RAShaw:	Initial code						#
#	8/91	RAShaw:	Added chi-sqr rejection threshold:			#

procedure nm_show (group, ksigma, xsize, ysize)

include	"nmcom.h"

#  Calling arguments:
int	group			# Mapped image group
real	ksigma			# Pixel rejection threshold
int	xsize			# Bin size along first axis
int	ysize			# Bin size along second axis

begin
	call printf ("Current NOISEMODEL parameter settings:\n\n")

	call printf ("  Gain:        %5.2f e-/DN\n")
	    call pargr (gain)
	call printf ("  Readnoise:   %5.2f DN\n")
	    call pargr (readn)
	call printf ("  Scalenoise:  %5.2f percent\n")
	    call pargr (scalen)
	call printf ("  Image group: %2d\n")
	    call pargi (group)
	call printf ("  KsigReject:  %5.2f\n")
	    call pargr (ksigma)
	call printf ("  ClipHi:      %5.4f\n")
	    call pargr (cliphi)
	call printf ("  ClipLo:      %5.4f\n")
	    call pargr (cliplo)
	call printf ("  XbinSize:  %4d pixels\n")
	    call pargi (xsize)
	call printf ("  YbinSize:  %4d pixels\n\n")
	    call pargi (ysize)
end
