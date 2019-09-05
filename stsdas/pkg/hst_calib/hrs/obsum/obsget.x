#---------------------------------------------------------------------------
.help obs_get_next Jun92 source
.ih
NAME
obs_get_next -- Determine which file of those opened has the earliest time.
.ih
USAGE
int = obs_get_next (time, n)
.ih
ARGUMENTS
.fs time (double[n])
I:  Array of times.
.fe
.fs n (int)
I:  Number of times in the array.
.fe
.endhelp
#---------------------------------------------------------------------------
int procedure obs_get_next (time, n)

double  time[n]                 # I:  Array of times.
int     n                       # I:  Number of times in array.

# Declarations.
double  current_time            # Time that is the lowest.

int     current                 # Current lowest value.
int     i                       # Generic.

begin
        
        # Find first defined time.
        current = INDEFI
        do i = 1, n
            if (!IS_INDEFD(time[i])) {
                current = i
                break
            }

        # Make sure at least one time is defined.  This should never happen,
        # but again, this is a program.
        if (IS_INDEFI(current))
            call error (1, "internal error- cannot get next time, all indef")

        # Now find the time that is the smallest.
        current_time = time[current]
        do i = current+1, n
            if (time[i] < current_time) {
                current = i
                current_time = time[i]
            }

        # That's all folks.
        return (current)
        
end
#---------------------------------------------------------------------------
# End of obs_get_next
#---------------------------------------------------------------------------
