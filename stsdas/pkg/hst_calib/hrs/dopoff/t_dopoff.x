include	<error.h>
include <tbset.h>

#---------------------------------------------------------------------------
.help dopoff 11Jul95 source
.ih
NAME
dopoff -- Calculate the magnitude of the doppler shift for granularity.
.ih
DESCRIPTION
For the user-level description, see the file dopoff.hlp.  Program
notes, if any, will appear below.
.endhelp
#---------------------------------------------------------------------------
procedure t_dopoff

# Doppler correction.
double  do_dopoff()             # Determine doppler correction.
double  s                       # Shift between current and zero-point.
double  zero                    # Zero-point doppler correction.

# File
pointer od                      # OneD file descriptor.
pointer od_map()                # Open a oned file descriptor.
pointer prev                    # File previously opened.

# Table
pointer file_c                  # Filename column.
pointer gran_c                  # Granularity column.
int     group                   # Current group.
pointer group_c                 # Group number column.
int     n                       # Number rows to read.
int     r                       # Current row.
pointer t                       # Table descriptor.
int     tbpsta()                # Get table parameter.
pointer tbtopn()                # Open a table.

# Misc.
bool    bx, by                  # Generic.
bool    clgetb()                # Get boolean valued parameter.
pointer sp                      # Stack pointer.
int     strlen()                # Get length of string.
bool    strne()                 # Are strings not equal?
pointer sx, sy                  # Generic string.

begin
        call smark (sp)
        call salloc (prev, SZ_PATHNAME, TY_CHAR)
        call salloc (sx, max(SZ_LINE,SZ_PATHNAME), TY_CHAR)
        call salloc (sy, max(SZ_LINE,SZ_PATHNAME), TY_CHAR)

        # Open the table.
        call clgstr ("input", Memc[sx], SZ_PATHNAME)
        t = tbtopn (Memc[sx], READ_WRITE, NULL)

        # See if the granularity column exists.  If it does, check
        # the overwrite flag.  Else, create the column.
        call clgstr ("gran_col", Memc[sx], SZ_LINE)
        call tbcfnd (t, Memc[sx], gran_c, 1)
        if (gran_c != NULL) {
            if (!clgetb ("overwrite")) {
                call sprintf (Memc[sy], SZ_LINE, "dopoff: column %s exists and overwrite=no, cannot modify table")
                call pargstr (Memc[sx])
                call error (1, Memc[sy])
            }
        } else {
            call tbcdef (t, gran_c, Memc[sx], "pixels", "", TY_DOUBLE, 1, 1)
        }

        # Get the file/group columns.
        call clgstr ("file_col", memc[sx], SZ_LINE)
        call tbcfnd (t, Memc[sx], file_c, 1)
        if (file_c == NULL) {
            call sprintf (Memc[sy], SZ_LINE, "dopoff: file column %s doesn't exist")
            call pargstr (Memc[sx])
            call error (1, Memc[sy])
        }
        
        call clgstr ("group_col", memc[sx], SZ_LINE)
        call tbcfnd (t, Memc[sx], group_c, 1)
        if (group_c == NULL) {
            call sprintf (Memc[sy], SZ_LINE, "dopoff: group column %s doesn't exist")
            call pargstr (Memc[sx])
            call error (1, Memc[sy])
        }

        # How many of these do we need to do.
        n = tbpsta (t, TBL_NROWS)
        if (n <= 0)
            call error (1, "dopoff: table is empty")
        
        # Get zero-point reference.  If none is specified, use the
        # first entry in the table.
        call clgstr ("reference", Memc[sx], SZ_PATHNAME)
        if (strlen (Memc[sx]) <= 0) {
            r = 0
            bx = true
            group = 1
            repeat {
                r = r + 1
                call tbrgtt (t, file_c, Memc[sx], bx, SZ_LINE, 1, 1)
                if (!bx)
                    call tbrgti (t, group_c, group, by, 1, 1)
            } until (!bx || r == n)
            if (bx)
                call error (1, "dopoff: no files in table to get reference from")
        } else
            group = 1
        od = od_map (Memc[sx], READ_ONLY, NULL)
        if (group > 1)
            call od_open_group (od, group)
        zero = do_dopoff (od)
        call od_unmap (od)

        # Find the doppler shift for each entry in the table relative
        # to the zero-point reference.  Write the shift in the
        # granularity column.
        call strcpy ("", Memc[prev], SZ_PATHNAME)
        do r = 1, n {

            # Get the image.
            call tbrgtt (t, file_c, Memc[sx], bx, SZ_LINE, 1, r)
            if (bx)
                next
            call tbrgti (t, group_c, group, bx, 1, r)
            if (bx)
                group = 1
            if (strne (Memc[sx], Memc[prev])) {
                call od_unmap (od)
                iferr (od = od_map (Memc[sx], READ_ONLY, NULL)) {
		    call erract (EA_WARN)
                    call eprintf ("dopoff: could not open %s, skipping..\n")
                    call pargstr (Memc[sx])
                    next
                }
                call strcpy (Memc[sx], Memc[prev], SZ_PATHNAME)
            }
            if (group > 1)
                call od_open_group (od, group)
            
            # Find the shift
            s = do_dopoff (od) - zero

            # Write it.
            call tbrptd (t, gran_c, s, 1, r)
        }

        # That's all folks.
        call od_unmap (od)
        call tbtclo (t)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_dopoff
#---------------------------------------------------------------------------
