include <tbset.h>
include "shift_table.h"

# Define memory access for the column description arrays.
define  FMT     Memc[fmt+(($1-1)*(SZ_COLFMT+1))]
define  LEN     Memi[len+$1-1]
define  NAME    Memc[name+(($1-1)*(SZ_COLNAME+1))]
define  TYPE    Memi[type+$1-1]
define  UNITS   Memc[units+(($1-1)*(SZ_COLUNITS+1))]

#---------------------------------------------------------------------------
.help out_table.x Feb93 source
.ih
NAME
.nf
pof_open_output  -- Open and setup the table containing the pixel offsets.
pof_close_output -- Close the table.
pof_write_output -- Write info to the table.
.fi
.ih
USAGE
.nf
pointer = pof_open_output (tname, savewave, zero)
call pof_close_output (t)
call pof_write_output (t, file, group, shift, wave, wgroup, wshift)
.fi
.ih
RETURNS
pof_open_output returns a descriptor of the output
table for POFFSETS.  This is not an STSDAS TABLES descriptor, but one
defined in the include file "shift_table.h".  If no name is specified,
this will be NULL.
.ih
ARGUMENTS
.ls tname (I: char[ARB])
The name of the table to open.  If the table doesn't exist, it is
created.  If it does exist, all the necessary columns are looked for.
If the columns exist, the table is open to append to.  If the columns
do not exist, an error is returned.
.le
.ls savewave (I: bool)
If TRUE, columns for wavelength file names, group numbers, and shifts
will be present in the table.  If FALSE, these columns will not be
created or looked for.
.le
.ls zero (I: char[ARB])
File name of the zero point reference spectrum.  If the table is being
created, this is not used.  However, if the table exists, the value of
the header parameter ZEROPT must match the value of zero.  On exit, zero
will have the value of the header parameter ZEROPT.
.le
.ls t (I: pointer)
The output table descriptor, as returned by pof_open_output.
For routine pof_close_output, the value of NULL is allowed.
.le
.ls file (I: char[ARB])
The file name to place in the "file" column of the output table.
.le
.ls group (I: int)
The group number of the file to place in the "group" column of the
output table.
.le
.ls shift (I: double)
The shift for the file/group to place in the "shift" column of the
output table.
.le
.ls wave (I: char[ARB])
The wavelength file to be placed in the "wave" column of the output
table.
.le
.ls wgroup (I: int)
The group of the wavelength file to be place in the "wgroup" column of
the output table.
.le
.ls wshift (I: int)
The shift of the wavelenght/group file to be place in the "wshift"
column of the output table.
.le
.ih
DESCRIPTION
These routines describe the output from the POFFSETS task.
.endhelp
#---------------------------------------------------------------------------
pointer procedure pof_open_output (tname, savewave, zero)

char    tname[ARB]              # I:  Name of the table to open.
bool    savewave                # I:  True if wavelengths will be written.
char	zero[ARB]		# IO: Name of the zero-point spectrum.

# Column definitions
pointer fmt                     # Print format of each column.
pointer len                     # Size of data elements of each column.
int     n_col                   # Number of columns to define.
pointer name                    # Name of each column.
pointer type                    # Type of each column.
pointer units                   # Units of each column.

# Column operations.
pointer t                       # The table descriptor.
int     tbpsta()                # Get table statistics.
pointer tbtopn()                # Open the table.

# Misc.
int     btoi()                  # Convert boolean to integer.
bool    bx                      # Generic.
int     i                       # Generic.
pointer sp                      # Stack.
int     strlen()                # Length of string.
bool	strne()			# Check if strings are not equal.
pointer	sx			# Generic string.

errchk  malloc, salloc, tbcdef, tbcfnd, tbpsta, tbtcre, tbtopn

begin
        call smark (sp)
	call salloc (sx, SZ_LINE, TY_CHAR)

        # If no name is specified, don't bother.
        if (strlen (tname) <= 0)
            t = NULL

        # Else, go on.
        else {

            # Initialize the column definitions.
            call salloc (name, OT_N_COLUMNS*(SZ_COLNAME+1), TY_CHAR)
            call salloc (units, OT_N_COLUMNS*(SZ_COLUNITS+1), TY_CHAR)
            call salloc (fmt, OT_N_COLUMNS*(SZ_COLFMT+1), TY_CHAR)
            call salloc (type, OT_N_COLUMNS, TY_INT)
            call salloc (len, OT_N_COLUMNS, TY_INT)
            
            call strcpy ("file", NAME(OT_FNAME), SZ_COLNAME)
            call strcpy ("group", NAME(OT_GROUP), SZ_COLNAME)
            call strcpy ("shift", NAME(OT_SHIFT), SZ_COLNAME)
            call strcpy ("wave", NAME(OT_WAVE), SZ_COLNAME)
            call strcpy ("wgroup", NAME(OT_WGROUP), SZ_COLNAME)
            call strcpy ("wshift", NAME(OT_WSHIFT), SZ_COLNAME)
            
            call strcpy ("", UNITS(OT_FNAME), SZ_COLUNITS)
            call strcpy ("groups", UNITS(OT_GROUP), SZ_COLUNITS)
            call strcpy ("pixels", UNITS(OT_SHIFT), SZ_COLUNITS)
            call strcpy ("groups", UNITS(OT_WGROUP), SZ_COLUNITS)
            call strcpy ("pixels", UNITS(OT_WSHIFT), SZ_COLUNITS)
            
            call strcpy ("%s", FMT(OT_FNAME), SZ_COLFMT)
            call strcpy ("%d", FMT(OT_GROUP), SZ_COLFMT)
            call strcpy ("%g", FMT(OT_SHIFT), SZ_COLFMT)
            call strcpy ("%s", FMT(OT_WAVE), SZ_COLFMT)
            call strcpy ("%d", FMT(OT_WGROUP), SZ_COLFMT)
            call strcpy ("%d", FMT(OT_WSHIFT), SZ_COLFMT)
            
            TYPE(OT_FNAME) = -SZ_PATHNAME
            TYPE(OT_GROUP) = TY_INT
            TYPE(OT_SHIFT) = TY_DOUBLE
            TYPE(OT_WAVE) = -SZ_PATHNAME
            TYPE(OT_WGROUP) = TY_INT
            TYPE(OT_WSHIFT) = TY_INT
            
            LEN(OT_FNAME) = 1
            LEN(OT_GROUP) = 1
            LEN(OT_SHIFT) = 1
            LEN(OT_WAVE) = 1
            LEN(OT_WGROUP) = 1
            LEN(OT_WSHIFT) = 1

            # Create the output table descriptor.
            call malloc (t, SZ_OT_SZ, TY_STRUCT)
            call malloc (OT_C_PTR(t), OT_N_COLUMNS, TY_POINTER)

            # If saving wavelengths, include the columns.  If not, don't.
            if (savewave)
                n_col = OT_N_COLUMNS
            else
                n_col = OT_N_COL_NOWAVE
            OT_SAVEWAVE(t) = btoi (savewave)

            # If the table exists, find the columns we are looking for.
            # If any of the columns don't exist.  Then abort.
            ifnoerr (OT_TP(t) = tbtopn (tname, READ_WRITE, NULL)) {

		# Check that the zero point reference is the same.
		call tbhgtt (OT_TP(t), "zeropt", Memc[sx], SZ_LINE)
		if (strlen (zero) > 0) {
		    if (strne (zero, Memc[sx])) {
			call eprintf ("poffsets: Specified zero point %s doesn't match zero point\n")
			call pargstr (zero)
			call eprintf ("poffsets: %s specified in table!\n")
			call pargstr (Memc[sx])
			call eprintf ("poffsets: Using table specified zero point\n")
			call strcpy (Memc[sx], zero, SZ_LINE)
		    }
		} else
		    call strcpy (Memc[sx], zero, SZ_LINE)
                bx = false
                call tbcfnd (OT_TP(t), Memc[name], OT_C(t,1), n_col)
                do i = 1, n_col
                    if (OT_C(t,i) == NULL) {
                        call eprintf ("poffsets: column %s doesn't exist in %s\n")
                        call pargstr (NAME(i))
                        call pargstr (tname)
                        bx = true
                    }
                if (bx)
                    call error (1, "poffsets: Columns missing from table, aborting")
                
                # Get the last row written.
                OT_LAST(t) = tbpsta (OT_TP(t), TBL_NROWS)
            }
            
            # Else, create the table.
            else {
                OT_TP(t) = tbtopn (tname, NEW_FILE, NULL)
                call tbtcre (OT_TP(t))
                call tbcdef (OT_TP(t), OT_C(t,1), Memc[name], Memc[units],
                             Memc[fmt], Memi[type], Memi[len], n_col)
                bx = false
                do i = 1, n_col
                    if (OT_C(t,i) == NULL) {
                        call eprintf ("poffsets: Could not define column %s in table %s\n")
                        call pargstr (NAME(i))
                        call pargstr (tname)
                        bx = true
                    }
                if (bx)
                    call error (1, "poffsets: Could not define columns, aborting")
                
                OT_LAST(t) = 0
            }
        }

        # That's all folks.
        call sfree (sp)
        return (t)
end
#---------------------------------------------------------------------------
# End of pof_open_output
#---------------------------------------------------------------------------
procedure pof_close_output (t)

pointer t                       # IO: Output table descriptor, NULL on return.

errchk  tbtclo

begin
        if (t != NULL) {
            call tbtclo (OT_TP(t))
            call mfree (OT_C_PTR(t), TY_POINTER)
            call mfree (t, TY_STRUCT)
        }
end
#---------------------------------------------------------------------------
# End of pof_close_output
#---------------------------------------------------------------------------
procedure pof_write_output (t, file, group, shift, wave, wgroup, wshift)

pointer t                       # I:  The output table descriptor.
char    file[ARB]               # I:  The filename of the data shifted.
int     group                   # I:  Group of data shift is for.
double  shift                   # I:  The shift itself.
char    wave[ARB]               # I:  Wavelength file name.
int     wgroup                  # I:  Wavelength group number.
int     wshift                  # I:  Shift for wavelengths.

int     r                       # Current row.

begin
        if (t != NULL) {
            r = OT_LAST(t) + 1
            call tbeptt (OT_TP(t), OT_C(t,OT_FNAME), r, file)
            call tbepti (OT_TP(t), OT_C(t,OT_GROUP), r, group)
            call tbeptd (OT_TP(t), OT_C(t,OT_SHIFT), r, shift)
            if (OT_SAVEWAVE(t) == YES) {
                call tbeptt (OT_TP(t), OT_C(t,OT_WAVE), r, wave)
                call tbepti (OT_TP(t), OT_C(t,OT_WGROUP), r, wgroup)
                call tbepti (OT_TP(t), OT_C(t,OT_WSHIFT), r, wshift)
            }
            OT_LAST(t) = r
        }
end
#---------------------------------------------------------------------------
# End of pof_write_output
#---------------------------------------------------------------------------
procedure pof_zero (t, zero)

pointer t			# I:  Output table descriptor.
char	zero[ARB]		# I:  Zero point file.

begin
	call tbhadt (OT_TP(t), "zeropt", zero)
end
#---------------------------------------------------------------------------
# End of pof_zero
#---------------------------------------------------------------------------
