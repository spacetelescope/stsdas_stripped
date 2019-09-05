include <tbset.h>

#---------------------------------------------------------------------------
.help   hst_saa_plot    Jun92   stplot
.ih
NAME
hst_saa_plot    -- Plot a contour of an SAA model on the world.
.endhelp
#---------------------------------------------------------------------------

procedure hst_saa_plot (table_name, model, gp)

char    table_name[ARB]         # I:  Name of table that contains SAA models.
int     model                   # I:  The model to plot.
pointer gp                      # I:  The graphics descriptor.

# Declarations
int     n_rows                  # Total number of rows from the table.
int     n_points                # Number of actual points to plot.

pointer lat_col                 # Latitude column pointer.
pointer long_col                # Longitude column pointer.
pointer null_flag               # True if value for that row is undefined.
pointer sp                      # Stack pointer.
pointer table                   # Table descriptor.
pointer tmp_string              # Temporary string.
pointer x                       # X coordinate array.
pointer y                       # Y coordinate array.

string  latitude        "LATITUDE_%02.2d"
string  longitude       "LONGITUDE_%02.2d"
string  no_column       "Table does not contain information for model %d"

# Function prototypes.
int     tbpsta()
pointer tbtopn()

begin
        call smark (sp)
        call salloc( tmp_string, SZ_LINE, TY_CHAR)

        # Open the table.
        table = tbtopn (table_name, READ_ONLY, 0)

        # Construct column names and retrieve the column pointers.
        call sprintf (Memc[tmp_string], SZ_LINE, latitude)
        call pargi (model)
        call tbcfnd (table, Memc[tmp_string], lat_col, 1)
        
        call sprintf (Memc[tmp_string], SZ_LINE, longitude)
        call pargi (model)
        call tbcfnd (table, Memc[tmp_string], long_col, 1)

        if (lat_col == NULL || long_col == NULL){
            call sprintf (Memc[tmp_string], SZ_LINE, no_column)
            call pargi (model)
            call error (1, Memc[tmp_string])
        }
        
        # Retrieve the number of rows in the table and allocate arrays.
        # Arrays are one more than the possible number of points to set
        # the return point of the contours.
        n_rows = tbpsta (table, TBL_NROWS)
        call malloc (x, n_rows+1, TY_REAL)
        call malloc (y, n_rows+1, TY_REAL)
        call malloc (null_flag, n_rows, TY_BOOL)

        # Get the columns.
        call tbcgtr (table, long_col, Memr[x], Memb[null_flag], 1, n_rows)
        call tbcgtr (table, lat_col, Memr[y], Memb[null_flag], 1, n_rows)

        # Construct the vectors for the SAA model.  Stop at either an
        # undefined value or until all rows have been read.
        for ( n_points = 0; n_points < n_rows; n_points = n_points + 1) {
            if (Memb[null_flag+n_points])
                break
            call mwcart (Memr[x+n_points], Memr[y+n_points],
                         Memr[x+n_points], Memr[y+n_points])
        }

        # Set last point to first point.
        Memr[x+n_points] = Memr[x]
        Memr[y+n_points] = Memr[y]
        n_points = n_points+1

        # Plot the bugger.
        call gpline (gp, Memr[x], Memr[y], n_points)

        # That's all folks.
        call mfree (null_flag, TY_BOOL)
        call mfree (y, TY_REAL)
        call mfree (x, TY_REAL)
        call tbtclo (table)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of hst_saa_plot
#---------------------------------------------------------------------------
