include "calpar.h"

#---------------------------------------------------------------------------
.help modcal Sep92 source
.ih
NAME
modcal -- Edit the parameters in the calibration psets.
.ih
USAGE
task modcal = t_modcal
.ih
DESCRIPTION
Keep track of modifications made to the calibration psets and blank those
parameters that have not been changed.  This is used in conjunction
with the task chcalpar to modify large numbers of headers without changing
the headers too drastically.
.endhelp
#---------------------------------------------------------------------------

procedure t_modcal

# Declarations.
int     answer                  # Answer to continue edit question.
int     i                       # Generic.

bool    edit                    # True to continue editing the pset.

pointer new                     # New values of the pset.
pointer old                     # Old values of the pset.
pointer sp                      # Stack pointer.
pointer tmp_str                 # Generic string.

# Function prototypes
int     strdic(), strlen()
bool    streq()
pointer cp_open()

string  accept_str      "accept"
string  keywords_str    "keywords"
string  err_nopset_str  "modcal: no calibration pset is specified"
string  result_str      "result"
string  space           " "

begin
        
        call smark (sp)
        call salloc (tmp_str, SZ_LINE, TY_CHAR)
        
        # Get and open the pset.
        call clgstr (keywords_str, Memc[tmp_str], SZ_LINE)
        if (strlen (Memc[tmp_str]) == 0)
            call error (1, err_nopset_str)
        old = cp_open (Memc[tmp_str])
        
        # Keep modifying the parameters until the user is happy.
        edit = true
        while (edit) {
            
            # Let the user modify the current set of parameters.
            call clepset (CP_PSET(old))
            
            # Read in the modify parameters.
            new = cp_open(CP_PNAME(old))
            
            # Compare the modified parameters with the original set.  Those that
            # are not different, set to empty strings.
            do i = 1, CP_NPARAMS(old) {
                if (strdic (CP_PAR(old,i), Memc[tmp_str], SZ_LINE, BAD_PARS) > 0)
                    next
                if (streq (CP_VALUE(old,i), CP_VALUE(new,i)))
                    call strcpy (space, CP_VALUE(new,i), CP_VALUE_SIZE)
            }
            
            # Write back the parameters and free the space.
            call cp_write (new)
            
            # Show the parameters again.
            call clepset (CP_PSET(new))
            
            # Find out whether the changes are acceptable, re-edit them, or just
            # abort.
            call clgstr (accept_str, Memc[tmp_str], SZ_LINE)
            answer = strdic (Memc[tmp_str], Memc[tmp_str], SZ_LINE,
                             ACCEPT_DICT)
            if (answer == ACCEPT_NO) {
                call cp_write (old)
            } else if (answer == ACCEPT_ABORT) {
                call cp_write (old)
                edit = false
            } else
                edit = false
            call cp_close (new)
            
        }

        # Write the answer back to accept.
        call clpstr (result_str, Memc[tmp_str])
        
        # That's all folks.
        call cp_close (old)
        call sfree (sp)
        
end
#---------------------------------------------------------------------------
# End of t_modcal
#---------------------------------------------------------------------------
