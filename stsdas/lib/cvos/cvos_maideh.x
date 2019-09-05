# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
# Source copied from iraf$sys/etc/maideh.x.  The routines have been prepended
# with "cvos_" in order not to conflict with the IRAF routines.  These 
# routines are used for C programs compiled as host level tasks.
# M.D. De La Pena  23 April 1999

include	<xwhen.h>
include	<syserr.h>
include	<fset.h>

define	SZ_ERRMSG	SZ_LINE

# MA_IDEH -- Iraf Main routine which installs the default exception handler.
# A single handler processes all exceptions.

procedure cvos_ideh()

extern	cvos_xstdexh()
int	junk, i, epa_standard_handler
int	exception[4]
data	exception /X_ACV, X_INT, X_ARITH, X_IPC/

begin
	call zlocpr (cvos_xstdexh, epa_standard_handler)
	do i = 1, 4
	    call xwhen (exception[i], epa_standard_handler, junk)

	# Initialize the critical section protection stack.
	call intr_reset()
end


# XSTDEXH -- Standard exception handler.  Unless the user code posts a handler
# for a particular exception, this handler will gain control.

procedure cvos_xstdexh (exception, next_handler)

int	exception			# code for exception
int	next_handler			# EPA of next handler to be called

char	os_errmsg[SZ_ERRMSG]
int	os_errcode

begin
	# Get OS description of the exception.
	call zxgmes (os_errcode, os_errmsg, SZ_ERRMSG)
	call strupk (os_errmsg, os_errmsg, SZ_ERRMSG)

	# Cancel any output and resync awaits.
	call fseti (STDOUT, F_CANCEL, OK)
	call fseti (CLOUT, F_CANCEL, OK)
	call fseti (CLIN, F_CANCEL, OK)

	# Set this here as error() will return immediately if it comes back.
	next_handler = X_IGNORE

	# Take error action.
	switch (exception) {
	case X_ACV:
	    if (os_errcode > 0)
		call cvos_fatal (SYS_XACV, os_errmsg)
	    else
		call cvos_fatal (SYS_XACV, "Access violation")
	case X_ARITH:
	    if (os_errcode > 0)
		call cvos_fatal (SYS_XARITH, os_errmsg)
	    else
		call cvos_fatal (SYS_XARITH, "Arithmetic exception")
	case X_INT:
	    if (os_errcode > 0)
		call cvos_fatal (SYS_XINT, os_errmsg)
	    else
		call cvos_fatal (SYS_XINT, "Keyboard interrupt")
	case X_IPC:
	    call cvos_fatal (SYS_XIPC, "Write to IPC with no reader")

	default:
	    call cvos_fatal (ERR, "Unknown exception")
	}
end
