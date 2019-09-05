include	"wid.h"

#---------------------------------------------------------------------------
.help wid_struct.x 22Feb95 source
.ih
NAME
.nf
wid_alloc	-- Create the WID object
wid_free	-- Destroy the WID object.
.fi
.endhelp
#---------------------------------------------------------------------------
pointer procedure wid_alloc()

pointer	wid			# WID object.

errchk	malloc

begin
	call calloc (wid, WID_SZ, TY_STRUCT)

	return (wid)
end
#---------------------------------------------------------------------------
# End of wid_alloc
#---------------------------------------------------------------------------
procedure wid_free (wid)

pointer	wid			# IO: WID object, NULL on return.

errchk	mfree

begin
	call mfree (wid, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of wid_free
#---------------------------------------------------------------------------
