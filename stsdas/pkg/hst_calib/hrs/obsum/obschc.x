#---------------------------------------------------------------------------
.help obs_char_alloc Jun92 source
.ih
NAME
obs_char_alloc -- Allocate an array of character strings.
.ih
USAGE
call obs_char_alloc (size, n_strings, ptr)
.ih
ARGUMENTS
.fs size (int)
I:  The size of each character string.
.fe
.fs n_strings (int)
I:  The number of strings to allocate.
.fe
.fs ptr (pointer)
O:  The pointer to the array of strings.
.fe
.endhelp
#---------------------------------------------------------------------------

procedure obs_char_alloc (size, n_strings, ptr)

int     size                    # I:  Size of strings.
int     n_strings               # I:  The number of strings.
pointer ptr                     # O:  The array.

# Declarations.
int     i                       # Generic.

begin
        call calloc (ptr, n_strings, TY_POINTER)
        do i = 0, n_strings-1
            call calloc (Memi[ptr+i], size, TY_CHAR)
end
#---------------------------------------------------------------------------
# End of obs_char_alloc
#---------------------------------------------------------------------------
.help obs_char_free Jun92 source
.ih
NAME
obs_char_free -- Free memory of an array of strings allocated with obs_char_alloc
.ih
USAGE
call obs_char_free (ptr, n_strings)
.ih
ARGUMENTS
.fs ptr (pointer)
IO: The pointer to the array of strings.
.fe
.fs n_strings (int)
I:  The number of strings in the array.
.fe
.endhelp
#---------------------------------------------------------------------------
procedure obs_char_free (ptr, n_strings)

pointer ptr             # IO: Pointer to the array.
int     n_strings       # I:  The number of strings to deallocate.

# Declarations
int     i               # Generic.

begin
        do i = 0, n_strings-1
            call mfree (Memi[ptr+i], TY_CHAR)
        call mfree (ptr, TY_POINTER)
end
#---------------------------------------------------------------------------
# End of obs_char_free
#---------------------------------------------------------------------------
