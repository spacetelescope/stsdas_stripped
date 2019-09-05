include <tbset.h>

# Define the rotation between the geocentric and geomagnetic systems.
define  THETAX  11.4
define  THETAY  0.0
define  THETAZ  19.3

# Define latitude and longitude.
define  LONGITUDE       1
define  LATITUDE        2
define  N_DIM           2

#---------------------------------------------------------------------------
.help   t_geo2mag       Jun92   hrs
.ih
NAME
t_geo2mag       -- Transform to/from geocentric to geomagnetic coordinates.
.endhelp
#---------------------------------------------------------------------------
procedure t_geo2mag

# Declarations.
double  lat, lng                # Input latitude, longitude
double  new_lat, new_lng        # Output latitude, longitude.
double  thetax, thetay, thetaz  # Rotation between geocentric and geomagnetic.

int     i                       # Generic.
int     lat_datatype            # Datatype of input latitude column.
int     lat_lendata             # Length of elements for latitude column.
int     long_datatype           # Datatype of input longitude column.
int     long_lendata            # Length of elements for longitude column.
int     n_rows                  # Number of rows to loop through.
int     row                     # Current row.

bool    copy                    # True to make a copy of the input table.
bool    geo2mag                 # True to go from geocentric to geomagnetic.
bool    replace                 # True to replace data in input table.

pointer geo_trans               # Spherical transformation descriptor.
pointer in_table                # Table descriptor for input table.
pointer input                   # Input table name.
pointer in_column[N_DIM]        # Input column descriptors.
pointer lat_col_fmt             # Column format for latitude.
pointer lat_col_units           # Column units for latitude.
pointer latitude                # Input column name for latitude.
pointer long_col_fmt            # Column format for longitude.
pointer long_col_units          # Column units for longitude.
pointer longitude               # Input column name for longitude.
pointer out_column[N_DIM]       # Output column descriptors.
pointer out_table               # Table descriptor for output table.
pointer outlat                  # Output column name for latitude.
pointer outlong                 # Output column name for longitude.
pointer output                  # Output table name.
pointer sp                      # Stack pointer.
pointer tmp_string              # Temporary string.

# Function prototypes.
int     strlen(), tbpsta()
bool    clgetb()
pointer spt_initd(), tbtopn()

begin
        call smark(sp)
        call salloc (input, SZ_LINE, TY_CHAR)
        call salloc (lat_col_fmt, SZ_LINE, TY_CHAR)
        call salloc (lat_col_units, SZ_LINE, TY_CHAR)
        call salloc (latitude, SZ_LINE, TY_CHAR)
        call salloc (longitude, SZ_LINE, TY_CHAR)
        call salloc (long_col_fmt, SZ_LINE, TY_CHAR)
        call salloc (long_col_units, SZ_LINE, TY_CHAR)
        call salloc (outlat, SZ_LINE, TY_CHAR)
        call salloc (outlong, SZ_LINE, TY_CHAR)
        call salloc (output, SZ_LINE, TY_CHAR)
        call salloc (tmp_string, SZ_LINE, TY_CHAR)

        # Get the input specifications.
        call clgstr ("input", Memc[input], SZ_LINE)
        call clgstr ("longitude", Memc[longitude], SZ_LINE)
        call clgstr ("latitude", Memc[latitude], SZ_LINE)
        replace = clgetb ("replace")
        copy = clgetb ("copy")
        call clgstr ("outlong", Memc[outlong], SZ_LINE)
        call clgstr ("outlat", Memc[outlat], SZ_LINE)
        geo2mag = clgetb ("geo2mag")

        # Open tables according to whether replacing, copying, or creating data.
        if (replace) {
            in_table = tbtopn (Memc[input], READ_WRITE, 0)
            out_table = in_table
        } else {
            call clgstr ("output", Memc[output], SZ_LINE)
            if (copy) {
                call tbtcpy (Memc[input], Memc[output])
                in_table = tbtopn (Memc[output], READ_WRITE, 0)
                out_table = in_table
            } else {
                in_table = tbtopn (Memc[input], READ_ONLY, 0)
                out_table = tbtopn (Memc[output], NEW_FILE, 0)
                call tbtcre (out_table)
            }
        }
        
        # Locate input columns.
        call tbcfnd (in_table, Memc[longitude], in_column[LONGITUDE], 1)
        if (in_column[LONGITUDE] == NULL) {
            call sprintf (Memc[tmp_string], SZ_LINE,
                          "In_column %s is not in table %s")
            call pargstr (Memc[longitude])
            call pargstr (Memc[input])
        }

        call tbcfnd (in_table, Memc[latitude], in_column[LATITUDE], 1)
        if (in_column[LATITUDE] == NULL) {
            call sprintf (Memc[tmp_string], SZ_LINE,
                          "In_column %s is not in table %s")
            call pargstr (Memc[latitude])
            call pargstr (Memc[input])
        }

        # Find format for input in_columns to use when creating output columns.
        call tbcinf (in_column[LONGITUDE], i, Memc[tmp_string],
                     Memc[long_col_units], Memc[long_col_fmt],
                     long_datatype, long_lendata, i)
        call tbcinf (in_column[LATITUDE], i, Memc[tmp_string],
                     Memc[lat_col_units], Memc[lat_col_fmt], lat_datatype,
                     lat_lendata, i)

        # Get the columns to write to.
        if (strlen (Memc[outlong]) == 0)
            call strcpy (Memc[longitude], Memc[outlong], SZ_LINE)
        call tbcfnd (out_table, Memc[outlong], out_column[LONGITUDE], 1)
        if (out_column[LONGITUDE] == NULL)
            call tbcdef (out_table, out_column[LONGITUDE], Memc[outlong],
                         Memc[long_col_units], Memc[long_col_fmt],
                         long_datatype, long_lendata, 1)

        if (strlen (Memc[outlat]) == 0)
            call strcpy (Memc[latitude], Memc[outlat], SZ_LINE)
        call tbcfnd (out_table, Memc[outlat], out_column[LATITUDE], 1)
        if (out_column[LATITUDE] == NULL)
            call tbcdef (out_table, out_column[LATITUDE], Memc[outlat],
                         Memc[lat_col_units], Memc[lat_col_fmt],
                         lat_datatype, lat_lendata, 1)

        # Determine the transformation from geocentric to geomagnetic.
        thetax = THETAX
        thetay = THETAY
        thetaz = THETAZ
        geo_trans = spt_initd (thetax, thetay, thetaz)

        # For each row of data, convert from input to output.
        n_rows = tbpsta (in_table, TBL_NROWS)
        do row = 1, n_rows {
            call tbegtd (in_table, in_column[LONGITUDE], row, lng)
            call tbegtd (in_table, in_column[LATITUDE], row, lat)

            if (geo2mag)
                call spt_forwardd (geo_trans, lng, lat, new_lng, new_lat)
            else
                call spt_backwardd (geo_trans, lng, lat, new_lng, new_lat)

            call tbeptd (out_table, out_column[LONGITUDE], row, new_lng)
            call tbeptd (out_table, out_column[LATITUDE], row, new_lat)
        }

        # That's all folks.
        if (in_table == out_table)
            call tbtclo (in_table)
        else {
            call tbtclo (in_table)
            call tbtclo (out_table)
        }
        call spt_close (geo_trans)
        call sfree(sp)
end
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
