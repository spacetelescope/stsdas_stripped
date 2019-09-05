include "rdsaa.h"

#---------------------------------------------------------------------------
.help saa_rdsaa Jun92 tools
.ih
NAME
saa_rdsaa -- Convert the PDB SAA Vertex Description File to a table.
.endhelp
#---------------------------------------------------------------------------

procedure saa_rdsaa

# Declarations.
int     current_model           # Number of current model being entered.

pointer lat_col, long_col       # Column descriptors for latitude, longitude.
pointer pdb                     # File descriptor for the PDB file.
pointer saa_record              # SVDF record descriptor.
pointer table                   # Table descriptor for the table version.

string  empty_pdb       "File is empty!"
string  not_svdf        "Input is not a PDB SVDF file!"

include "rdsaa_params.com"

# Function Prototypes.
int     saa_next_record()
bool    tbtacc()
pointer open(), tbtopn()

begin
        # Open the PDB file
        pdb = open (input, READ_ONLY, TEXT_FILE)

        # Open the table.
        if (tbtacc (output))
            table = tbtopn (output, READ_WRITE, 0)
        else {
            table = tbtopn (output, NEW_FILE, 0)
            call tbtcre (table)
        }

        # Make sure there are enough header parameters for the maximum
        # number of models allows.
        call tbtchs (table, MAX_MODELS, -1, -1, -1)

        # Get the PDB file description and update the table.
        if ( saa_next_record (pdb, saa_record) == EOF )
            call error( 1, empty_pdb )
        else if ( RECORD_TYPE(saa_record) != FD )
            call error (1, not_svdf )
        else
            call saa_description (saa_record, table)

        # Now read the rest of the file, populating the table with good info.
        current_model = INDEFI
        while ( saa_next_record (pdb, saa_record) != EOF) {

            # Get record type and operate according to type.
            switch (RECORD_TYPE(saa_record)) {
            case VD:
                if (IS_INDEFI(model) || MODEL_ID(saa_record) == model)
                    call saa_vertex( saa_record, table, current_model,
                                     lat_col, long_col)

            case GC:
                ;

            default:
                call error (1, not_svdf )
            }       

        }

        # That's all folks.
        call saa_dealloc (saa_record)
        call tbtclo (table)
        call close (pdb)
        
end
#---------------------------------------------------------------------------
# End of saa_rdsaa
#---------------------------------------------------------------------------
