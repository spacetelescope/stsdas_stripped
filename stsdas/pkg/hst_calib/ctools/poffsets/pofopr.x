#---------------------------------------------------------------------------
.help pof_open_corr Feb93 source
.ih
NAME
pof_open_corr -- Open and setup the table containing the correlation results.
.ih
USAGE
table = pof_open_corr (name)
.ih
ARGUMENTS
.fs name (char[ARB] :input)
The name of the table to create.
.fe
.ih
RETURNS
Returns a pointer to an STSDAS table descriptor.
.endhelp
#---------------------------------------------------------------------------
pointer procedure pof_open_corr (name)

char    name[ARB]               # I:  Name of the table to open.

# Table.
pointer t                       # The table descriptor.
int     tbtacc()                # Is the table accessable?
pointer tbtopn()                # Open a table.

begin
        if (tbtacc (name) == YES)
            t = tbtopn (name, READ_WRITE, NULL)
        else {
            t = tbtopn (name, NEW_FILE, NULL)
            call tbtcre (t)
        }

        return (t)
end
#---------------------------------------------------------------------------
# End of pof_open_corr
#---------------------------------------------------------------------------
