#---------------------------------------------------------------------------
.help obs_aindef Jun92 source
.ih
NAME
obs_aindef -- TRUE if all the times in the array are INDEF.
.ih
USAGE
bool = obs_aindef (input, n)
.ih
ARGUMENTS
.fs input (double[n])
Array of double values.
.fe
.fs n (int)
Number of values in the array.
.fe
.endhelp
#---------------------------------------------------------------------------
bool procedure obs_aindef (input, n)

double  input[n]                # I:  The input array.
int     n                       # I:  Size of array.

# Declarations.
int     i                       # Generic.
bool    result                  # TRUE if all values are INDEF.

begin
        result = TRUE
        do i = 1, n
            if (!IS_INDEFD(input[i])) {
                result = false
                break
            }

        return (result)
end
#---------------------------------------------------------------------------
# End of obs_aindef
#---------------------------------------------------------------------------
