# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<clset.h>

#---------------------------------------------------------------------------
.help fos_dispfit 11Feb96 source
.ih
NAME
fos_dispfit -- The Post Operational Archive FOS dispersion solution fitting took
.ih
DESCRIPTION
The main code description lies in the routine yclfos.  The general help
can be optained from the package-level help.  This routine simply allows
the fos_pix2wav task to return an error status when run standalone.

The pipeline, as per OPR#29606, required that the calxxx tasks return
an error status.  Since IRAF tasks are not the main, there was currently
no way of accomplishing this.  N. Zarate developed platform specific
routines to get a status returned.  This "wrapper" SPP task is necessary
to post the appropriate error handler to make this happen.
.endhelp
#---------------------------------------------------------------------------
procedure fos_dispfit()

begin
	# Setup error handler to return status if run from the host.
	call post_exit_handler

	# Run the main task.
	call ycldsp
end
#---------------------------------------------------------------------------
# End of fos_dispfit
#---------------------------------------------------------------------------