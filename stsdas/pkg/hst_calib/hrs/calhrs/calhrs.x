include	<clset.h>

#---------------------------------------------------------------------------
.help calhrs 22Mar96 source
.ih
NAME
calhrs -- The pipeline calibration task for the GHRS
.ih
DESCRIPTION
The main code description lies in the routine zclhrs.  The general help
can be optained from the package-level help.  This routine simply allows
the calhrs task to return an error status when run standalone.

The pipeline, as per OPR#29606, required that the calxxx tasks return
an error status.  Since IRAF tasks are not the main, there was currently
no way of accomplishing this.  N. Zarate developed platform specific
routines to get a status returned.  This "wrapper" SPP task is necessary
to post the appropriate error handler to make this happen.
.endhelp
#---------------------------------------------------------------------------
procedure calhrs()

begin
	# Setup error handler to return status if run from the host.
	call post_exit_handler

	# Run the main task.
	call zclhrs
end
#---------------------------------------------------------------------------
# End of calhrs
#---------------------------------------------------------------------------
