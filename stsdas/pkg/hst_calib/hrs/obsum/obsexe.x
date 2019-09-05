# Define how large the output array grows each time.
define  SIZE_INCR       10

#---------------------------------------------------------------------------
.help obs_ext_parse Jun92 source
.ih
NAME
obs_ext_parse -- Take each word from a string and place into an array.
.ih
USAGE
call obs_ext_parse (input, max_size, ptr, n_strings)
.ih
ARGUMENTS
.fs input (char[ARB])
I:  The input string containing space/comma seperated words.
.fe
.fs max_size (int)
I:  The size of each string in the output array of strings.
.fe
.fs ptr (pointer)
O:  The output array of strings.
.fe
.fs n_strings (int)
O:  The number of strings in the output array.
.fe
.endhelp
#---------------------------------------------------------------------------
procedure obs_ext_parse (input, max_size, ptr, n_strings)

char    input[ARB]              # I:  The input string.
int     max_size                # I:  Maximum size of strings in array.
pointer ptr                     # O:  The array of strings.
int     n_strings               # O:  Number of strings in the output array.

# Declarations.
int     current_size            # Current size of the output array.
int     input_ptr               # Pointer into the input string.

pointer c_input                 # Converted input string.
pointer sp                      # Stack pointer.
pointer word                    # Individual words from the input.

# Function prototypes.
int     ctowrd()

begin

        call smark(sp)
        call salloc (c_input, SZ_LINE, TY_CHAR)
        call salloc (word, SZ_LINE, TY_CHAR)

        # Convert the input list from comma-seperated to space-seperated.
        call obs_change_string (input, ",", " ", Memc[c_input], SZ_LINE)

        # For each word in the string, extract it and place it in the array.
        current_size = 0
        n_strings = 0
        input_ptr = 1
        while (ctowrd (Memc[c_input], input_ptr, Memc[word], SZ_LINE) != 0) {
            n_strings = n_strings + 1
            if (n_strings > current_size) {
                current_size = current_size + SIZE_INCR
                call realloc (ptr, current_size, TY_POINTER)
            }
            call calloc (Memi[ptr+n_strings-1], max_size, TY_CHAR)
            call strcpy (Memc[word], Memc[Memi[ptr+n_strings-1]], max_size)
        }

        # That's all folks.
        call sfree(sp)
end
#---------------------------------------------------------------------------
# End of obs_ext_parse
#---------------------------------------------------------------------------
# obs_change_string - Replace a string  with the indicated string.
#---------------------------------------------------------------------------

procedure obs_change_string( input, old, new, output, max )

char input[ARB]   # I:  The input string.
char old[ARB]     # I:  The string segment to be replaced.
char new[ARB]     # I:  The string to replace the old with.
char output[ARB]  # O:  The modified input string.
int  max          # I:  The maximun length of the output string.

# Declarations.
int after         # Next character position after match.
int first         # First character position of matched string.
int ilen          # Length of input.
int ip            # Pointer into input.
int last          # Last character position of matched string.
int old_len       # Length of old.
int op            # Pointer into output.

# Function declarations.
int gstrcpy(), strlen(), gstrmatch()

begin

  # Initialize the string pointers.
  ip = 1
  op = 1
  ilen = strlen( input )
  old_len = strlen( old )

  # Keep going until either the input string has been completely copied
  # or the output string is full.
  while( ip < ( ilen + 1 ) && op < ( max + 1 ) ) {

    # Search for the old string.
    after = gstrmatch( input[ip], old, first, last )

    # If the string is not found, then copy the rest of the input to the
    # output.
    if( after == 0 ) {
      call strcpy( input[ip], output[op], max - op + 1 )
      ip = ilen + 1
    }

    # The old string is found, copy the input up to the old string
    # and replace the old string.
    else {
      first = min( first - 1, max - op + 1 )
      call obs_strncpy( input[ip], first, output[op] )
      ip = ip + last
      op = op + first
      op = op + gstrcpy( new, output[op], max - op + 1 )
    }

  }

end
#---------------------------------------------------------------------------
# End of obs_change_string
#---------------------------------------------------------------------------
# obs_strncpy - Counted character copy.
#---------------------------------------------------------------------------

procedure obs_strncpy( input, n_chars, output )

char input[ARB]   # I:  The input string to copy to the output.
int  n_chars      # I:  The number of characters to copy.
char output[ARB]  # O:  The output string.

# Declarations.
int i             # Index.

begin

  for( i = 1; i <= n_chars; i = i + 1 )
    output[i] = input[i]

end
#---------------------------------------------------------------------------
# End of obs_strncpy.
#---------------------------------------------------------------------------
