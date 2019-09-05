include <error.h>
include <tbset.h>
include "groupmod.h"

# What are the default columns if none are specified.
define  DEFAULT_NAME_COLUMN     1
define  DEFAULT_TYPE_COLUMN     2
define  DEFAULT_INIT_COLUMN     3
define  DEFAULT_COMM_COLUMN     4

#---------------------------------------------------------------------------
.help grm_open_kw Jul92 source
.ih
NAME
grm_open_kw    -- Open the keyword table.
grm_close_kw   -- Close and deallocate table structures.
grm_decode     -- Decode type strings into SPP types.
.endhelp
#---------------------------------------------------------------------------
pointer procedure grm_open_kw (input, op, keyname_colname, keytype_colname,
                               keyinit_colname, keycomm_colname)

char    input[ARB]              # I:  The name and column names of the table.
int     op                      # I:  The operation in use.
char    keyname_colname         # Column containing the keyword names.
char    keytype_colname         # Column containing the keyword types.
char    keyinit_colname         # Column of initialization values.
char    keycomm_colname         # Column of comment values.

# Declarations.
int     actual                  # Number of fully specified keywords.
int     row                     # Current row of the table.

pointer comm_col                # Comment column pointer.
pointer init_col                # Initialization column pointer.
pointer kw                      # Keywork table structure.
pointer name_col                # Column for keyword names.
pointer sp                      # STack pointer.
pointer table                   # Keyword table descriptor.
pointer tmp_string              # Generic string.
pointer type_col                # Column for keyword types.

# Function prototypes.
int     strlen(), tbpsta()
bool    strne()
pointer tbcnum(), tbtopn()

begin
        call smark(sp)
        call salloc (tmp_string, SZ_LINE, TY_CHAR)

        # Open the table.
        table = tbtopn (input, READ_ONLY, 0)

        # Allocate the keyword structure.
        call malloc (kw, SZ_KW_STRUCT, TY_STRUCT)
        KW_OP(kw) = op
        KW_N_KEYWORDS(kw) = tbpsta (table, TBL_NROWS)
        call malloc (KW_NAME_PTR(kw), KW_N_KEYWORDS(kw)*(SZ_KEYWORD+1), TY_CHAR)
        call amovkc (EOS, KW_NAME(kw,1), KW_N_KEYWORDS(kw)*(SZ_KEYWORD+1))
        if (op == OP_ADD) {
            call malloc (KW_DTYPE_PTR(kw), KW_N_KEYWORDS(kw), TY_INT)
            call malloc (KW_PLEN_PTR(kw), KW_N_KEYWORDS(kw), TY_INT)
            call malloc (KW_INIT_PTR(kw), KW_N_KEYWORDS(kw)*(MAXPLEN+1),
                         TY_CHAR)
            call amovkc (EOS, KW_INIT(kw,1), KW_N_KEYWORDS(kw)*(MAXPLEN+1))
            call malloc (KW_COMM_PTR(kw), KW_N_KEYWORDS(kw)*(MAXPLEN+1),
                         TY_CHAR)
            call amovkc (EOS, KW_COMM(kw,1), KW_N_KEYWORDS(kw)*(MAXPLEN+1))
        }

        # Get the columns containing the information.
        if (strlen (keyname_colname) == 0)
            name_col = tbcnum (table, DEFAULT_NAME_COLUMN)
        else
            call tbcfnd (table, keyname_colname, name_col, 1)

        if (op == OP_ADD) {
            if (strlen (keytype_colname) == 0)
                type_col = tbcnum (table, DEFAULT_TYPE_COLUMN)
            else
                call tbcfnd (table, keytype_colname,
                             type_col, 1)

            if (strne (keyinit_colname, "INDEF"))
                if (strlen (keyinit_colname) == 0)
                    init_col = tbcnum (table, DEFAULT_INIT_COLUMN)
                else
                    call tbcfnd (table, keyinit_colname,
                                 init_col, 1)
            else
                init_col = NULL
            
            if (strne (keycomm_colname, "INDEF"))
                if (strlen (keycomm_colname) == 0)
                    comm_col = tbcnum (table, DEFAULT_COMM_COLUMN)
                else
                    call tbcfnd (table, keycomm_colname,
                                 comm_col, 1)
            else
                comm_col = NULL

        } else {
            type_col = NULL
            init_col = NULL
            comm_col = NULL
        }
        
        # Check to make sure all necessary information is given.
        if (name_col == NULL)
            call error (1, "Could not find column for keyword names")
        if (op == OP_ADD && type_col == NULL)
            call error (1, "Could not find column for keyword types")

        # Now read the table and decode the type strings.
        actual = 0
        do row = 1, KW_N_KEYWORDS(kw) {
            actual = actual + 1

            # If adding, first try to decode the type.  If this does not
            # succeed, don't try the name.
            if (op == OP_ADD) {
                call tbegtt (table, type_col, row, Memc[tmp_string], SZ_LINE)
                iferr (call grm_decode (Memc[tmp_string], KW_DTYPE(kw,actual),
                                        KW_PLEN(kw,actual))) {
                    call erract (EA_WARN)
                    actual = actual -1
                    next
                }

                # Get initialization value.  If no column exists, don't worry,
                # just initialize to the empty string.   Note that this was
                # done already when the buffer was allocated.
                if (init_col != NULL)
                    call tbegtt (table, init_col, row, KW_INIT(kw,actual),
                                 MAXPLEN)

                # Get comment value.  If no column exists, don't worry,
                # just initialize to the empty string.   Note that this was
                # done already when the buffer was allocated.
                if (comm_col != NULL)
                    call tbegtt (table, comm_col, row, KW_COMM(kw,actual),
                                 MAXPLEN)
            }

            # Get the keyword name.
            call tbegtt (table, name_col, row, Memc[tmp_string], SZ_LINE)
            if (strlen (Memc[tmp_string]) > SZ_KEYNAME) {
                call eprintf ("Keyword %s is too long, maximum of %d characters only...ignoring\n")
                call pargstr (Memc[tmp_string])
                call pargi (SZ_KEYNAME)
                actual = actual - 1
                next
            }
            call strupr (Memc[tmp_string])
            call strcpy (Memc[tmp_string], KW_NAME(kw,actual), SZ_KEYNAME)
        }
        
        KW_N_KEYWORDS(kw) = actual
        if (KW_N_KEYWORDS(kw) <= 0)
            call error (1, "No valid keywords found")
        
        # That's all folks.
        call tbtclo (table)
        call sfree(sp)
        return (kw)
        
end
#---------------------------------------------------------------------------
# End of grm_open_kw
#---------------------------------------------------------------------------
procedure grm_close_kw(kw)

pointer kw                      # IO: The keword table structure.

begin

        if (KW_OP(kw) == OP_ADD) {
            call mfree (KW_COMM_PTR(kw), TY_CHAR)
            call mfree (KW_INIT_PTR(kw), TY_CHAR)
            call mfree (KW_PLEN_PTR(kw), TY_INT)
            call mfree (KW_DTYPE_PTR(kw), TY_INT)
        }
        call mfree (KW_NAME_PTR(kw), TY_CHAR)
        call mfree (kw, TY_STRUCT)
        
end
#---------------------------------------------------------------------------
# End of grm_close_kw
#---------------------------------------------------------------------------
procedure grm_decode (input, dtype, plen)

char    input[ARB]              # I:  The string containing type/length info.
int     dtype                   # O:  The SPP datatype encoded.
int     plen                    # O:  Number of elements of the specified type.

# Declarations.
int     star                    # Index of the '*' in the string.

pointer sp                      # Stack pointer.
pointer tmp_string              # Temporary string handler.

# Function prototypes.
int     ctoi(), strdic(), stridx(), strlen()

begin
        call smark (sp)
        call salloc (tmp_string, SZ_LINE, TY_CHAR)

        # Look for the '*' seperator and find the length.
        star = stridx ("*", input)
        if (star == 0) {
            plen = 1
            star = strlen(input) + 1
        } else
            if (ctoi (input, star+1, plen) == 0) {
                call sprintf (Memc[tmp_string], SZ_LINE, "Could not decode size field of keytype %s")
                call pargstr (input)
                call error (1, Memc[tmp_string])
            }

        # Check on the specified types.
        call strcpy (input, Memc[tmp_string], star-1)
        call strupr (Memc[tmp_string])
        dtype = strdic (Memc[tmp_string], Memc[tmp_string], SZ_LINE, TYPE_DICT)
        if (dtype == 0) {
            call sprintf (Memc[tmp_string], SZ_LINE, "Unknown data type %s...ignoring\n")
            call pargstr (input)
            call error (1, Memc[tmp_string])
        }
        
        # Decode to the appropriate SPP data type.
        switch (dtype) {
        case GRM_LONG:      dtype = TY_LONG
        case GRM_INT:       dtype = TY_INT
        case GRM_REAL:      dtype = TY_REAL
        case GRM_DOUBLE:    dtype = TY_DOUBLE
        case GRM_CHAR:
            dtype = TY_CHAR
            plen = plen + mod (plen, 2)
        case GRM_BOOL:      dtype = TY_BOOL
        }

        # Check on the size.  Cannot be too large.
        if (plen > MAXPLEN) {
            call sprintf (Memc[tmp_string], SZ_LINE, "Type %s is too large, try smaller size")
            call pargstr (input)
            call error (1, Memc[tmp_string])
        }

        # That's all folks.
        call sfree (sp)
end
