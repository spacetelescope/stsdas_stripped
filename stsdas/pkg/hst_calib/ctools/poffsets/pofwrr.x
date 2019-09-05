#---------------------------------------------------------------------------
.help pof_write_corr Feb93 source
.ih
NAME
pof_write_corr -- Write correlation results to table.
.ih
USAGE
call pof_write_corr (table, data, length, n_sections, column)
.ih
ARGUMENTS
.fs table (pointer :input)
The table to write to.
.fe
.fs data[length,n_sections] (double :input)
The data to write to the table.  Each row of the data is written to a separate
column of the table.
.fe
.fs length (int :input)
The length of each row of the data.
.fe
.fs n_sections (int :input)
How many rows are in the data.
.fe
.fs column (int :input/output)
The last column written.
.fe
.endhelp
#---------------------------------------------------------------------------
procedure pof_write_corr (table, data, length, n_sections, column)

pointer table                   # I:  The table to write to.
double  data[length,n_sections] # I:  The data to write.
int     length                  # I:  The length of each row.
int     n_sections              # I:  Number of rows.
int     column                  # IO: The last column written.

# Misc.
pointer col                     # Current column pointer.
int     section                 # Current section.
pointer sp                      # Stack pointer.
pointer xstr                    # Generic strings.

begin
        call smark (sp)
        call salloc (xstr, SZ_LINE, TY_CHAR)

        
        do section = 1, n_sections {
            column = column + 1
            call sprintf (Memc[xstr], SZ_LINE, "c%d")
            call pargi (column)

            call tbcdef (table, col, Memc[xstr], "correlation", "", TY_DOUBLE,
                         length, 1)
            call tbcptd (table, col, data[1,section], 1, length)
        }

        call sfree (sp)
end
