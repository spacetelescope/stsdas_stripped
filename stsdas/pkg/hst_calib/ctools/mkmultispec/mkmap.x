include <imhdr.h>
include <imio.h>
include <tbset.h>
include "mkms.h"

# Define the default column number to retrieve table data from
define  DEFAULT_COL     1

#---------------------------------------------------------------------------
.help mk_map Mar93 source
.ih
NAME
.nf
mk_map          -- Open a file as either an image or table.
mk_unmap        -- Close the file.
mk_data         -- Retrieve data.

mk_image_map    -- Internal: Map an image.
mk_table_map    -- Internal: Map a table.
.fi
.ih
USAGE
.nf
mkd = mk_map (name, mode)
call mk_unmap (mkd)
call mk_data (mkd, group, data)

call mk_image_map (name, mkd)
call mk_table_map (name, mode, mkd)
.fi
.ih
ARGUMENTS
.ls name (char[ARB] :input)
The name of the file to open.
.le
.ls mode (int :input)
The access mode to open the file in.  Same as the standard IRAF open
modes.
.le
.ls mkd (pointer :input)
The MK_IMIO descriptor.
.le
.ls group (int :input)
The group to retrieve the data from.
.le
.ls data (double :output)
The data retrieved from the file.
.le
.ih
RETURNS
An mk_imio file descriptor containing the image/table descriptor, a flag
indicating whether it is an image or table, and, if a table, the column
descriptor to retrieve the data from.
.ih
DESCRIPTION
This provides a common interface to retrieve one dimensional data from
either an image or a table.  This is vary basic and is not intended to
handle a full i/o interface.  Just need to open, close, and read data.

Added some syntax to the table name specification.  We will allow the
column names/numbers to be specified in a "section" notation.  An
example:

.nf
        tablename[columnname1,...]
.fi

where columnnameX are either names or numbers.  If no column
specification is used, then it is assumed all columns of the table are
wavelengths and will be considered with the appropriate "group" of
multigroup input.
.ih
TO DO
This routine should be generalized a bit more and placed in a common
library.
.endhelp
#---------------------------------------------------------------------------
pointer procedure mk_map(name, mode)

char    name[ARB]               # I:  The file name to open.
int     mode                    # I:  The mode to open the file in.

pointer immap()                 # IMIO open an image.
pointer mkd                     # MK_IMIO descriptor.

errchk  malloc, mk_image_map, mk_table_map

begin
        # Allocate the mk_imio descriptor.
        call malloc (mkd, MK_SZ_IO, TY_STRUCT)
        call malloc (IO_NAME_PTR(mkd), SZ_LINE, TY_CHAR)

        # Check to see if this is an image.  If so, get some pertinent
        # information.
        ifnoerr (IO_FD(mkd) = immap (name, mode, NULL))
            call mk_image_map (name, mkd)

        # Try a table.  If not, punt.
        else
            call mk_table_map (name, mode, mkd)

        # That's all folks.
        return (mkd)
end
#---------------------------------------------------------------------------
# End of mk_map
#---------------------------------------------------------------------------
procedure mk_unmap (mkd)

pointer mkd                     # I:  The MK_IMIO descriptor.

errchk  tbtclo, imunmap, mfree

begin
        if (mkd != NULL) {
            switch (IO_TYPE(mkd)) {
            case TABLE:
                call tbtclo (IO_FD(mkd))
                call mfree (IO_CD_PTR(mkd), TY_POINTER)
            case IMAGE:
                call imunmap (IO_FD(mkd))
            }

            call mfree (IO_NAME_PTR(mkd), TY_CHAR)
            call mfree (mkd, TY_STRUCT)
        }
end
#---------------------------------------------------------------------------
# End of mk_unmap
#---------------------------------------------------------------------------
procedure mk_data (mkd, group, data)

pointer mkd                     # I:  The MK_IMIO descriptor.
int     group                   # I:  Which group to open
double  data[ARB]               # O:  The data.

pointer imgl1d()                # IMIO get line of data.
real    x                       # Generic.
pointer null                    # Null flag array for table IO.

errchk  gf_opengr, imgl1d, malloc, mfree, tbcgtd

begin
        # Check if a file is actually opened.  If not, do nothing.
        if (mkd != NULL) {

            # Get data depending on file type.
            switch (IO_TYPE(mkd)) {
            case TABLE:
                call malloc (null, IO_LEN(mkd), TY_BOOL)
                call tbcgtd (IO_FD(mkd), IO_CD(mkd,group), data, Memb[null],
                             1, IO_LEN(mkd))
                call mfree (null, TY_BOOL)

            case IMAGE:

                # Open the appropriate group.
                if (group <= IO_NGRP(mkd) && IO_NGRP(mkd) > 1)
                    call gf_opengr (IO_FD(mkd), group, x, x, 0)

                # Retrieve the data.
                call amovd (Memd[imgl1d(IO_FD(mkd))], data, IO_LEN(mkd))
            }
        }
end
#---------------------------------------------------------------------------
# End of mk_data
#---------------------------------------------------------------------------
procedure mk_image_map (name, mkd)

char    name[ARB]               # I:  Full specified name.
pointer mkd                     # I:  MK_IMIO descriptor.

int     i                       # Generic.
pointer sp                      # Stack pointer.
pointer xstr                    # Generic.

begin
        call smark (sp)
        call salloc (xstr, SZ_LINE, TY_CHAR)

        # Fill the MK_IMIO descriptor.
        IO_TYPE(mkd) = IMAGE
        IO_CD_PTR(mkd) = NULL
        IO_LEN(mkd) = IM_LEN(IO_FD(mkd),1)
        IO_NGRP(mkd) = max(1,IM_CLSIZE(IO_FD(mkd)))
        call strcpy (IM_HDRFILE(IO_FD(mkd)), IO_NAME(mkd), SZ_LINE)
        
        # WARNING: INTERFACE VIOLATION!!!
        # The image may have a specific group specified.  In this case,
        # the number of groups is supposed to be one.  However, the
        # IM_CLSIZE macro still reports the actual number of groups.
        # The only way to determine whether a specific group has been
        # specified is to parse the file name.  IRAF doesn't provide
        # a parse function to return group specified.  It does provide
        # the routine imparse, but according to IRAF, this is not part
        # of the imio interface.  So, imparse may change, and when it
        # does, this will break.  But it is the only practical option.
        # If the index is greater than 0, implying that a group was
        # specified, then set the number of groups to 1.
        call imparse (name, Memc[xstr], SZ_LINE, Memc[xstr], SZ_LINE,
                      Memc[xstr], SZ_LINE, IO_GRP(mkd), i)
        if (IO_GRP(mkd) > 0)
            IO_NGRP(mkd) = 1
        else
            IO_GRP(mkd) = 1

        # That's all folks.
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of mk_image_map
#---------------------------------------------------------------------------
procedure mk_table_map (name, mode, mkd)

char    name[ARB]               # I:  The specified file name.
int     mode                    # I:  The file access mode.
pointer mkd                     # I:  The MK_IMIO descriptor.

pointer colname                 # Current column name.
int     ctoi()                  # Character to integer conversion.
int     i, j, k                 # Generic.
int     ic                      # Pointer into section list.
pointer section                 # Section specification.
pointer sp                      # Stack pointer.
int     strlen()                # Get length of string.
pointer tbcnum()                # Get number of columns from table.
int     tbpsta()                # Get table information.
pointer tbtopn()                # Open a table.
int     word_count()            # How many words in a string.
int     word_fetch()            # Get next word from string.
pointer xstr                    # Generic.


errchk  tbcnum, tbpsta, tbtopn, word_count, word_fetch

begin
        # Memory.
        call smark (sp)
        call salloc (colname, SZ_LINE, TY_CHAR)
        call salloc (section, SZ_LINE, TY_CHAR)
        call salloc (xstr, SZ_LINE, TY_CHAR)

        # Set what type of file.
        IO_TYPE(mkd) = TABLE

        # Get the base filename and section.
        call asparse (name, IO_NAME(mkd), SZ_LINE, Memc[section], SZ_LINE)

        # Open up and get some parameters.
        IO_FD(mkd) = tbtopn (IO_NAME(mkd), mode, 0)
        IO_LEN(mkd) = tbpsta (IO_FD(mkd), TBL_NROWS)
        IO_GRP(mkd) = 1

        # Now retrieve the columns.  If no columns are specified, then use
        # all the columns.
        if (strlen (Memc[section]) <= 0) {
            IO_NGRP(mkd) = tbpsta (IO_FD(mkd), TBL_NCOLS)
            call malloc (IO_CD_PTR(mkd), IO_NGRP(mkd), TY_POINTER)
            do i = 1, IO_NGRP(mkd) {
                IO_CD(mkd,i) = tbcnum (IO_FD(mkd), i)
                if (IO_CD(mkd,i) == NULL) {
                    call sprintf (Memc[xstr], SZ_LINE, "Cannot open column %d in table %s")
                    call pargi (i)
                    call pargstr (IO_NAME(mkd))
                    call error (1, Memc[xstr])
                }
            }
        } else {
            IO_NGRP(mkd) = word_count (Memc[section])
            call malloc (IO_CD_PTR(mkd), IO_NGRP(mkd), TY_POINTER)
            i = 0
            ic = 1
            while (word_fetch (Memc[section], ic, Memc[colname], SZ_LINE) > 0) {
                i = i + 1
                k = 1
                if (ctoi (Memc[colname], k, j) > 0)
                    IO_CD(mkd,i) = tbcnum (IO_FD(mkd), j)
                else
                    call tbcfnd (IO_FD(mkd), Memc[colname], IO_CD(mkd,i), 1)
            }
            if (IO_CD(mkd,i) == NULL) {
                call sprintf (Memc[xstr], SZ_LINE, "Cannot open column %s in table %s")
                call pargstr (Memc[colname])
                call pargstr (IO_NAME(mkd))
                call error (1, Memc[xstr])
            }
        }
        
        # That's all folks.
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of mk_table_map
#---------------------------------------------------------------------------
#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	06-Aug-92	removed code which deletes commas

# ASPARSE -- Parse a file name specification into file name and section fields
#
#	Syntax:		filename[section]
#
# The [ character must be escaped to be included in the filename.
# This syntax is similar to the image section syntax in imio, but
# is intended to extract variable names or numbers, column names, etc.
# for the Astronomical Survival analysis suite of programs.
# The section field is returned as a string with no leading or trailing 
# brackets.

procedure asparse (filespec, file, sz_file, section, sz_section)

char	filespec[ARB]		# i: full file specification
char	file[sz_file]		# o: receives file name
int	sz_file			# i: max chars in file name
char	section[sz_section]	# o: receives section
int	sz_section		# i: max chars in section name
#--
int	ch, ip, op, right

int	strlen()

begin
	ip = 1
	op = 1

	# Extract file name.  The first (unescaped) [ marks the start of
	# the section field.

	for (ch=filespec[ip];  ch != EOS && ch != '[';  ch=filespec[ip]) {
	    if (ch == '\\' && filespec[ip+1] == '[') {
		file[op] = '\\'
		op = op + 1
		file[op] = '['
		ip = ip + 1
	    } else
		file[op] = ch

	    op = min (sz_file, op + 1)
	    ip = ip + 1
	}

	file[op] = EOS
	section[1]  = EOS

	if (ch == EOS)
	    return

	# If we have a [...] field, copy the section string,
	# removing the brackets, and any commas used as delimiters.

	# Eliminate the leading "["
	ip = ip + 1
	call strcpy (filespec[ip], section, sz_section)

	# Remove the trailing "]"
	right = strlen (section)
	if (section[right] == ']')
	    section[right] = EOS

end
