include	<clio.h>
include <ctype.h>
include "calpar.h"

#---------------------------------------------------------------------------
.help cp Sep92 source
.ih
NAME
cp_open         -- Create a pset contents descriptor.
cp_read         -- Read the contents of a pset into the contents descriptor.
cp_write        -- Write the contents of arrays into the pset.
cp_close        -- Close a pset contents descriptor.

cp_getpar       -- Retrieve the parameters names from the pset.
cp_extend       -- Inlarge the pset descriptor.
.ih
USAGE
contents = cp_open (pset_name)
call cp_read (contents)
call cp_write (contents)
call cp_close (contents)

call cp_getpar (contents)
call cp_extend (contents)
.ih
DESCRIPTION
The cp family of routines is used to conveniently handle generic
parameter sets.  cp_open takes a pset name and returns a descriptor
containing the parameters in the pset and their values.  cp_read is
available to re-read the contents of a pset, such as to get new values
after epar had been run on the pset.  cp_write writes the current
values in the descriptor back to the associated pset.  cp_close closes
the associated pset and frees allocated memory.  cp_extend is an
internal routine to the cp family for memory management.  cp_getpar is
an internal routine to populate the contents structure with the
parameters found in the specified pset.
.ih
ARGUMENTS
.fs pset_name (char[ARB])
The name of the pset to open.
.fe
.fs contents (pointer)
Descriptor pointing to the arrays containing the pset parameters
and current values of the pset.
.fe
.ih
BUGS
Dependency on the output of the "dparam" task.  If "dparam" should ever
change, cp_getpar will break in a monumentally disasterous way.
.endhelp
#---------------------------------------------------------------------------
pointer procedure cp_open (name)

char    name[ARB]               # I:  The name of the pset to open.

# Declarations.
pointer contents                # The contents descriptor.

# Function prototypes.
pointer clopset()

errchk  clopset, cp_getpar, cp_read, malloc

begin
        call malloc (contents, CP_SIZE, TY_STRUCT)
        call malloc (CP_PAR_PTR(contents), CP_GROWTH*CP_PAR_SIZE, TY_CHAR)
        call malloc (CP_PNAME_PTR(contents), CP_PNAME_SIZE, TY_CHAR)
        call malloc (CP_VALUE_PTR(contents), CP_GROWTH*CP_VALUE_SIZE, TY_CHAR)

        # Open the associated pset.
        CP_PSET(contents) = clopset (name)
        call strcpy (name, CP_PNAME(contents), CP_PNAME_SIZE)

        # Define current maximum number of parameters allowed in this
        # structure.  This can be modified with cp_extend.
        CP_MAXPARAMS(contents) = CP_GROWTH

        # Retrieve the parameters named in the pset.
        call cp_getpar (contents)
        
        # Read in the values of the pset.
        call cp_read (contents)

        return (contents)
end
#---------------------------------------------------------------------------
# End of cp_alloc
#---------------------------------------------------------------------------
procedure cp_getpar (contents)

pointer contents                # I:  The contents descriptor.

# Declarations
int     i, j                    # Period, space pointers.

pointer file                    # File descriptor containing parameters.
pointer file_name               # Name of the file containing parameters.
pointer sp                      # Stack pointer
pointer tmp_str                 # Generic.

string  parlist_str     "dparam %s > %s"
string  period_str      "."
string  root            "tmp$pset"
string  space_str       " "

# Function prototypes.
int     getlline(), open(), stridx()

errchk  close, cp_extend, flush, fprint, getlline, open, salloc, smark

begin
        call smark (sp)
        call salloc (file_name, SZ_LINE, TY_CHAR)
        call salloc (tmp_str, SZ_LINE, TY_CHAR)
        
        # Retrieve the parameter names of the parameter set.
        # Use the task "dparam" to dump the contents of the pset out to
        # the standard output.  This routine is not used to get the values.
        # The values are retrieved in cp_read using standard IRAF routines.
        # WARNING: This routine will break if the output of dparam every
        # changes.
        call mktemp (root, Memc[file_name], SZ_LINE)
        call sprintf (Memc[tmp_str], SZ_LINE, parlist_str)
        call pargstr (CP_PNAME(contents))
        call pargstr (Memc[file_name])
        call clcmdw (Memc[tmp_str])

        # Retrieve the parameter names.  The format of the dparam output is
        # as follows:  'task.parameter = "value"'.  This will retrieve the
        # 'parameter', located between a period and a space.
        file = open (Memc[file_name], READ_ONLY, TEXT_FILE)
        CP_NPARAMS(contents) = 0
	while (getlline (file, Memc[tmp_str], SZ_COMMAND) != EOF) {
            i = stridx (period_str, Memc[tmp_str])
            j = stridx (space_str, Memc[tmp_str])
            if (i > 0 && j > 0 && j > i) {
                if (CP_NPARAMS(contents) == CP_MAXPARAMS(contents))
                    call cp_extend (contents)
                CP_NPARAMS(contents) = CP_NPARAMS(contents) + 1
                call strcpy (Memc[tmp_str+i],
                             CP_PAR(contents,CP_NPARAMS(contents)),
                             min (j-i-1, CP_PAR_SIZE))
            }
        }

        # That's all folks.
        call close (file)
        call delete (Memc[file_name])
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of cp_getpar
#---------------------------------------------------------------------------
procedure cp_read (contents)

pointer contents        # I: The contents of the pset.

# Declarations.
int     i               # Parameter counter.

errchk  clgpseta

begin
        do i = 1, CP_NPARAMS(contents)
            call clgpseta (CP_PSET(contents), CP_PAR(contents,i),
                           CP_VALUE(contents,i), CP_VALUE_SIZE)
end
#---------------------------------------------------------------------------
# End of cp_read
#---------------------------------------------------------------------------
procedure cp_write (contents)

pointer contents        # I:  The contents to write to it.

# Declarations.
int     i               # Generic.

errchk  clppseta

begin
        do i = 1, CP_NPARAMS(contents)
            call clppseta (CP_PSET(contents), CP_PAR(contents,i), 
                           CP_VALUE(contents,i))
end
#---------------------------------------------------------------------------
# End of cp_write
#---------------------------------------------------------------------------
procedure cp_close (contents)

pointer contents        # IO: The contents descriptor.

errchk  clcpset, mfree

begin
        call clcpset (CP_PSET(contents))
        
        call mfree (CP_VALUE_PTR(contents), TY_CHAR)
        call mfree (CP_PNAME_PTR(contents), TY_CHAR)
        call mfree (CP_PAR_PTR(contents), TY_CHAR)
        call mfree (contents, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of cp_free
#---------------------------------------------------------------------------
procedure cp_extend (contents)

pointer contents        # I:  The contents descriptor.

errchk  realloc

begin
        CP_MAXPARAMS(contents) = CP_MAXPARAMS(contents) + CP_GROWTH
        call realloc (CP_PAR_PTR(contents), CP_MAXPARAMS(contents)*CP_PAR_SIZE,
                      TY_CHAR)
        call realloc (CP_VALUE_PTR(contents),
                      CP_MAXPARAMS(contents)*CP_VALUE_SIZE, TY_CHAR)
end
#---------------------------------------------------------------------------
# End of cp_extend
#---------------------------------------------------------------------------
procedure cp_print (contents)

pointer contents                # I:  The pset descriptor.

# Declarations
int     i                       # Generic.

begin
        call printf ("There are %d parameters in pset %s.\n")
        call pargi (CP_NPARAMS(contents))
        call pargstr (CP_PNAME(contents))
        
        do i = 1, CP_NPARAMS(contents) {
            call printf ("%s = %s\n")
            call pargstr (CP_PAR(contents,i))
            call pargstr (CP_VALUE(contents,i))
        }
end
#---------------------------------------------------------------------------
# End of cp_print
#---------------------------------------------------------------------------
