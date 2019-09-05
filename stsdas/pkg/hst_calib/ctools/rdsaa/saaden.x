include <tbset.h>
include "rdsaa.h"

#---------------------------------------------------------------------------
.help saa_description Jun92 tools
.ih
NAME
saa_description -- Write the description to the table header.
.endhelp
#---------------------------------------------------------------------------

procedure saa_description (saa_ptr, table)

pointer saa_ptr         # I:  The SVDF descriptor.
pointer table           # I:  The output table descriptor.

# Declarations

string  file_number     "FILENUM"
string  file_type       "FILETYPE"
string  create_date     "CREDAT"
string  descr           "DESCRIP"
string  contact         "CONTACT"
string  tnumber         "PHONE"

begin

        # Check to make sure this is the FD record from the SVDF.
        if (RECORD_TYPE(saa_ptr) != FD)
            call error (1, "Need the FD record in write table user parameters")

        # Write the various FD fields to the user parameter section of
        # the table.
        call tbhadi (table, file_number, FILE_NUMBER(saa_ptr))
        call tbhadt (table, file_type, FILE_TYPE(saa_ptr))
        call tbhadt (table, create_date, CREATE_DATE(saa_ptr))
        call tbhadt (table, descr, DESCR(saa_ptr))
        call tbhadt (table, contact, CONTACT(saa_ptr))
        call tbhadt (table, tnumber, TNUMBER(saa_ptr))
        
end
#---------------------------------------------------------------------------
# End of saa_description
#---------------------------------------------------------------------------
