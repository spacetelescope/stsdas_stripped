#---------------------------------------------------------------------------
.help pof_change_ext Apr93 source
.ih
NAME
pof_change_ext -- Put the specified extension on a file name.
.endhelp
#---------------------------------------------------------------------------
procedure pof_change_ext (in_name, newext, out_name, max_size)

char    in_name[ARB]            # I:  Original input name.
char    newext[ARB]             # I:  Extension to replace old one.
char    out_name[max_size]      # O:  File name with new extension.
int     max_size                # I:  Maximum size out_name.

# Misc.
pointer dir                     # Directory part of pathname.
int     index                   # Group index in pathname.
int     ix                      # Generic.
pointer root                    # Root part of pathname.
pointer section                 # Section part of pathname.
pointer sp                      # Stack pointer.
pointer xs                      # Generic string.

begin
        call smark (sp)
        call salloc (dir, SZ_LINE, TY_CHAR)
        call salloc (root, SZ_LINE, TY_CHAR)
        call salloc (section, SZ_LINE, TY_CHAR)
        call salloc (xs, SZ_LINE, TY_CHAR)

        # Parse the file name COMPLETELY.
        call pof_parse (in_name, Memc[dir], SZ_LINE, Memc[root], SZ_LINE,
                        Memc[xs], SZ_LINE, index, ix, Memc[section], SZ_LINE)

        # Now put everything back together but with the new extension.
        call strcpy (Memc[dir], out_name, max_size)
        call strcat (Memc[root], out_name, max_size)
        call strcat (".", out_name, max_size)
        call strcat (newext, out_name, max_size)
        if (index > 0) {
            call sprintf (Memc[xs], SZ_LINE, "[%d]")
            call pargi (index)
            call strcat (Memc[xs], out_name, max_size)
        }
        call strcat (Memc[section], out_name, max_size)

        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of pof_change_ext
#---------------------------------------------------------------------------
procedure pof_parse (input, dir, dir_size, root, root_size, ext, ext_size,
                     cl_index, cl_size, section, section_size)

char    input[ARB]              # I:  Input pathname
char    dir[dir_size]           # O:  Directory part of pathname.
int     dir_size                # I:  Max size of dir.
char    root[root_size]         # O:  Root part of pathname.
int     root_size
char    ext[ext_size]           # O:  Extension part of pathname.
int     ext_size
int     cl_index                # O:  The cluster index.
int     cl_size                 # O:  The cluster size.
char    section[section_size]   # O:  The section part of pathname.
int     section_size

# Declarations
int     i                       # Generic.
int     len_dir                 # Length of the directory spec.

pointer cluster                 # Cluster.
pointer ksection                # Kernel section.
pointer last_period             # Pointer to the last period.
pointer new_cluster             # Cluster without the directory spec.
pointer ptr                     # Pointer into strings.
pointer sp                      # Stack pointer.

string  wildcards       "*?"

# Function prototypes.
int     fnldir(), stridxs()
bool    streq()

begin
        
        call smark(sp)
        call salloc (cluster, SZ_LINE, TY_CHAR)
        call salloc (ksection, SZ_LINE, TY_CHAR)
        
        # Parse the name with the (illegal) call imparse.
        call imparse (input, Memc[cluster], SZ_LINE, Memc[ksection],
                      SZ_LINE, section, section_size, cl_index, cl_size)

        # Further parse the the cluster name into directory, root, and extension.
        # Wildcards are a problem.  The above only deals with fully qualified
        # pathnames, not templates.  But, it seems it could be done.  Scan
        # the directory for wildcards and try to parse out a bit more.  The
        # assumption made is that directories cannot be wildcarded.
        root[1] = EOS
        ext[1] = EOS
        len_dir = fnldir (Memc[cluster], dir, dir_size)
        i = stridxs (wildcards, dir)
        if (i > 0) {
            dir[i] = EOS
            len_dir = fnldir (dir, dir, dir_size)
        }

        # Now there is just root and extension.  Check to see if root is just
        # the relative directory names.  If so, append them to the directory
        # specification.
        new_cluster = cluster + len_dir
        if (streq (Memc[new_cluster], ".") || streq (Memc[new_cluster], "..")) {
            call strcat (Memc[new_cluster], dir, dir_size)
            call strcat ("/", dir, dir_size)
        }

        # Else, find the extension.  This is just the last found "." in the
        # specification.
        else {
            last_period = NULL
            ptr = new_cluster
            while (Memc[ptr] != EOS) {
                if ( Memc[ptr] == '.')
                    last_period = ptr
                ptr = ptr + 1
            }
            if (last_period == NULL) {
                call strcpy (Memc[new_cluster], root, root_size)
                ext[1] = EOS
            } else {
                Memc[last_period] = EOS
                call strcpy (Memc[new_cluster], root, root_size )
                Memc[last_period] = '.'
                call strcpy (Memc[last_period], ext, ext_size)
            }
        }

        # That's all folks.
        call sfree(sp)
        
end
