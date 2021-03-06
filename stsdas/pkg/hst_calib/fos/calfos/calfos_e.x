include	<clset.h>

#---------------------------------------------------------------------------
.help calfos 11Feb96 source
.ih
NAME
calfos -- The pipeline calibration task for the FOS
.ih
DESCRIPTION
The main code description lies in the routine yclfos.  The general help
can be optained from the package-level help.  This routine simply allows
the calfos task to return an error status when run standalone.

The pipeline, as per OPR#29606, required that the calxxx tasks return
an error status.  Since IRAF tasks are not the main, there was currently
no way of accomplishing this.  N. Zarate developed platform specific
routines to get a status returned.  This "wrapper" SPP task is necessary
to post the appropriate error handler to make this happen.
.endhelp
#---------------------------------------------------------------------------
procedure calfos()

begin
	# Setup error handler to return status if run from the host.
	call post_exit_handler

	# Run the main task.
	call yclfos
end
#---------------------------------------------------------------------------
# End of calfos
#---------------------------------------------------------------------------
