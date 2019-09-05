include "rdsaa.h"

#---------------------------------------------------------------------------
.help   saa_vertex      Jun92   tools
.ih
NAME
saa_vertex      -- Convert the VD record to table information.
.endhelp
#---------------------------------------------------------------------------

procedure saa_vertex(saa_ptr, table, current_model, lat_col, long_col)

pointer saa_ptr                 # I:  Record descriptor.
pointer table                   # I:  Table descriptor.
int     current_model           # IO: Current model being added.
pointer lat_col                 # IO: Latitude column pointer.
pointer long_col                # IO: Longitude column pointer.

# Declarations.
pointer sp                      # Stack pointer.
pointer tmp_string              # Temporary string.

string  active_flag     "AFLAG_%02.2d"
string  latitude        "LATITUDE_%02.2d"
string  longitude       "LONGITUDE_%02.2d"

begin

        call smark (sp)
        call salloc( tmp_string, SZ_LINE, TY_CHAR)
        
        # See if the model has changed.  If so, then create the necessary
        # columns and user parameters.  All model dependent column and user
        # parameter names are of the form:  GGGGG_MM where GGGGG is the
        # generic name of the column/parameter and MM is the 2 digit model
        # number.
        if (MODEL_ID(saa_ptr) != current_model) {
            current_model = MODEL_ID(saa_ptr)

            # Define the active flag for this model.
            call sprintf (Memc[tmp_string], SZ_LINE, active_flag)
            call pargi (current_model)
            call tbhadb (table, Memc[tmp_string], ACTIVE_FLAG(saa_ptr))

            # Create the columns for this model.
            call sprintf (Memc[tmp_string], SZ_LINE, latitude)
            call pargi (current_model)
            call tbcfnd (table, Memc[tmp_string], lat_col, 1)
            if (lat_col == NULL)
                call tbcdef (table, lat_col, Memc[tmp_string], "degrees",
                             "14.8g", TY_REAL, 1, 1)
            
            call sprintf (Memc[tmp_string], SZ_LINE, longitude)
            call pargi (current_model)
            call tbcfnd (table, Memc[tmp_string], long_col, 1)
            if (long_col == NULL)
                call tbcdef (table, long_col, Memc[tmp_string], "degrees",
                             "14.8g", TY_REAL, 1, 1)
            
        }

        # Write the columns.
        call tbeptr (table, lat_col, VERTEX(saa_ptr), LATITUDE(saa_ptr))
        call tbeptr (table, long_col, VERTEX(saa_ptr), LONGITUDE(saa_ptr))

        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of saa_vertex
#---------------------------------------------------------------------------
