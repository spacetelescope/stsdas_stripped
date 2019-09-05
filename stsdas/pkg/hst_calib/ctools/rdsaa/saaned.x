include	<ctype.h>
include "adb.h"
include "rdsaa.h"

# Define where the record type is in the input line.
define RT_POS   71

#---------------------------------------------------------------------------
.help saa_records       Jun92   tools
.in
NAME
saa_next_record -- Return next record from the SVDF file
saa_alloc       -- Allocate saa record structure.
saa_dealloc     -- Deallocate saa record structure.
.endhelp
#---------------------------------------------------------------------------

pointer procedure saa_alloc

# Declarations.
pointer saa_ptr                 # Pointer to structure.

begin
        call malloc (saa_ptr, SAA_RECORD_SIZE, TY_STRUCT)

        call malloc (FILE_TYPE_PTR(saa_ptr), SZ_LINE, TY_CHAR)
        call malloc (CREATE_DATE_PTR(saa_ptr), SZ_LINE, TY_CHAR)
        call malloc (DESCR_PTR(saa_ptr), SZ_LINE, TY_CHAR)
        call malloc (CONTACT_PTR(saa_ptr), SZ_LINE, TY_CHAR)
        call malloc (TNUMBER_PTR(saa_ptr), SZ_LINE, TY_CHAR)
        call malloc (COMMENT_PTR(saa_ptr), SZ_LINE, TY_CHAR)

        return (saa_ptr)
        
end
#---------------------------------------------------------------------------
# End of saa_alloc
#---------------------------------------------------------------------------

procedure saa_dealloc (saa_ptr)

pointer saa_ptr         # IO: Pointer to saa record structure.

begin

        call mfree (FILE_TYPE_PTR(saa_ptr), TY_CHAR)
        call mfree (CREATE_DATE_PTR(saa_ptr), TY_CHAR)
        call mfree (DESCR_PTR(saa_ptr), TY_CHAR)
        call mfree (CONTACT_PTR(saa_ptr), TY_CHAR)
        call mfree (TNUMBER_PTR(saa_ptr), TY_CHAR)
        call mfree (COMMENT_PTR(saa_ptr), TY_CHAR)

        call mfree (saa_ptr, TY_STRUCT)
        
end
#---------------------------------------------------------------------------
# End of saa_dealloc
#---------------------------------------------------------------------------

int procedure saa_next_record (fd, saa_ptr)

int     fd              # I:  File descriptor of the PDB SVDF file.
pointer saa_ptr         # IO: saa record descriptor.

# Declarations.
int     i, j            # Generics.
int     length          # Length of next field.
int     n_fields        # Number of fields read.
int     start           # Next character of input line to read.
int     status          # File/operation status, returned.

char    fmt[SZ_LINE, N_TYPES]        # Chars in each field of each record.

pointer field           # Next field in input line.
pointer field_desc      # String containing # chars to read from input.
pointer field_value     # String representation of the current field.
pointer in_line         # Input line.
pointer sp              # Stack pointer.
pointer tmp_string      # Temporary string holder.

equivalence (fmt[1,FD], fmtFD)
equivalence (fmt[1,GC], fmtGC)
equivalence (fmt[1,VD], fmtVD)

string  fmtFD           "4,4,6,6,25,16,10,2,7"
string  fmtGC           "71,2,7"
string  fmtVD           "2,8,14,14,1,10,22,2,7"
string  short_line      "File is not a PDB file- line is too short"
string  too_many_fields "Too many fields in record- Not a PDB SVDF file"
string  unknown_record  "Unknown record type- Not an SVDF PDB file!"

# Function prototypes.
real    ctor()
int     adb_gettok(), ctoi(), getlline(), gstrcpy(), strdic(), strlen()
pointer saa_alloc()

begin

        # Check to see if a descriptor has been allocated.  If not, allocate
        # one.
        if (saa_ptr == NULL)
            saa_ptr = saa_alloc()

        call smark (sp)
        call salloc (field_desc, SZ_LINE, TY_CHAR)
        call salloc (field_value, SZ_LINE, TY_CHAR)
        call salloc (in_line, SZ_LINE, TY_CHAR)
        call salloc (tmp_string, SZ_LINE, TY_CHAR)

        # Get next line from the file.  If not end of file, decode the string.
        status = getlline (fd, Memc[in_line], SZ_LINE)
        if (status != EOF) {

            # Check that line is long enough.
            if (strlen (Memc[in_line]) < SZ_PDB_RECORD)
                call error (1, short_line)

            # Determine what type of record this is.
            call strcpy (Memc[in_line+RT_POS], Memc[tmp_string], SZ_RT)
            RECORD_TYPE(saa_ptr) = strdic (Memc[tmp_string], Memc[tmp_string],
                                           SZ_RT, TYPE_DICT)
            if (RECORD_TYPE(saa_ptr) < 1 || RECORD_TYPE(saa_ptr) > N_TYPES)
                call error (1, unknown_record)

            # Mark out all the fields by record stype.
            field = in_line
            n_fields = 1
            start = 1
            while (adb_gettok( fmt[1,RECORD_TYPE(saa_ptr)], start,
                               Memc[field_desc], SZ_LINE) > 0 ) {

                # Retrieve the field.
                i = 1
                j = ctoi (Memc[field_desc], i, length)
                field = field + gstrcpy (Memc[field], Memc[field_value], length)

                # Now, depending on which field number this is and which
                # record type we are dealing with, decode the field.
                switch (n_fields) {
                case 1:
                    switch (RECORD_TYPE(saa_ptr)) {
                    case FD:
                        i = 1
                        j = ctoi (Memc[field_value], i, FILE_NUMBER(saa_ptr))
                    case GC:
                        call strcpy( Memc[field_value], COMMENT(saa_ptr),
                                     SZ_LINE)
                    case VD:
                        i = 1
                        j = ctoi (Memc[field_value], i, MODEL_ID(saa_ptr))
                    }

                case 2:
                    switch (RECORD_TYPE(saa_ptr)) {
                    case FD:
                        call strcpy( Memc[field_value], FILE_TYPE(saa_ptr),
                                     SZ_LINE)
                    case VD:
                        i = 1
                        j = ctoi (Memc[field_value], i, VERTEX(saa_ptr))
                    }

                case 3:
                    switch (RECORD_TYPE(saa_ptr)) {
                    case FD:
                        i = 1
                        j = ctoi (Memc[field_value], i, N_RECORDS(saa_ptr))
                    case GC:
                        i = 1
                        j = ctoi (Memc[field_value], i, RECORD(saa_ptr))
                    case VD:
                        i = 1
                        j = ctor (Memc[field_value], i, LATITUDE(saa_ptr))
                    }

                case 4:
                    switch (RECORD_TYPE(saa_ptr)) {
                    case FD:
                        call strcpy( Memc[field_value], CREATE_DATE(saa_ptr),
                                     SZ_LINE)
                    case VD:
                        i = 1
                        j = ctor (Memc[field_value], i, LONGITUDE(saa_ptr))
                    }

                case 5:
                    switch (RECORD_TYPE(saa_ptr)) {
                    case FD:
                        call strcpy( Memc[field_value], DESCR(saa_ptr),
                                     SZ_LINE)
                    case VD:
                        if (Memc[field_value] == 'Y')
                            ACTIVE_FLAG(saa_ptr) = YES
                        else
                            ACTIVE_FLAG(saa_ptr) = NO
                    }

                case 6:
                    switch (RECORD_TYPE(saa_ptr)) {
                    case FD:
                        call strcpy( Memc[field_value], CONTACT(saa_ptr),
                                     SZ_LINE)
                    case VD:
                        i = 1
                        j = ctoi (Memc[field_value], i, FLUX(saa_ptr))
                    }
                        
                case 7:
                    call strcpy( Memc[field_value], TNUMBER(saa_ptr),
                                 SZ_LINE)

                case 8:
                    ;
                case 9:
                    i = 1
                    j = ctoi (Memc[field_value], i, RECORD(saa_ptr))
                    
                default:
                    call error (1, too_many_fields)
                }

                # Increment field counter.
                n_fields = n_fields + 1
                
            }

        }

        # Return the status
        return (status)
        
end
#---------------------------------------------------------------------------
# End of saa_next_record
#---------------------------------------------------------------------------
# ADB_GETTOK -- Retrieve next comma or whitespace delimeted token from string

# This procedure is used to parse character strings containing tokens 
# separated by whitespace or commas. A token is any group of contiguous
# characters which are neither whitespace or commas. The definition of 
# whitespace used by this procedure is anomalous, it includes any character
# whose integer value is less than or equal to a blank. In particular,
# newlines are considered whitespace. The number of characters in the token
# is returned as the function value. If no tokens were found, the returned
# value will be zero.
#
# B.Simon	09 Dec 88	Original

int procedure adb_gettok (str, ic, token, maxch)

char	str[ARB]	#  i: String containing tokens
int	ic		# io: Index of starting character
char	token[ARB]	#  o: Token string
int	maxch		#  i: Maximum length of token string
#--
char	ch
int	jc

begin
	# Skip leading whitespace or commas. Don't go past string terminator.

	for (ch = str[ic]; IS_DELIM(ch); ch = str[ic]) {
	    if (ch == EOS)
		break
	    ic = ic + 1
	}

	# Copy characters to token. End when maxch is reached, or
	# when commas, whitespace, or EOS is found

	for (jc = 1; jc <= maxch; jc = jc + 1) {

	    if (IS_DELIM(ch))
		break

	    # Copy character into token, converting to lower case

	    if (IS_UPPER(ch))
		token[jc] = TO_LOWER(ch)
	    else
		token[jc] = ch

	    ic = ic + 1
	    ch = str[ic]
	}
	token[jc] = EOS

	# If loop is terminated because of maxch, eat remaining characters
	# in field

	while (NOT_DELIM(ch)) {
	    ic = ic + 1
	    ch = str[ic]
	}

	# Return number of characters in token

	return (jc - 1)

end
