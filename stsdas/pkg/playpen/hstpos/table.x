include <tbset.h>
include "table.h"

# TABLE -- Routines to handle I/O to the table for HSTPOS.

# OPEN_TABLE -- Open/Create the table.

procedure open_table( table_name )

char    table_name[ARB]               # Name of the table to open.

# Declarations.
int     ip                            # Generic index.

pointer sp                            # Stack pointer.
pointer tmp                           # Temporary string.

include "table.com"

# Function prototypes.
int     access(), strlen(), tbpsta(), tbtacc()
pointer tbtopn()

begin

   if( strlen( table_name ) <= 0 )
      table = NULL
   else {

      call smark(sp)
      call salloc( tmp, SZ_LINE, TY_CHAR )

      # Iniitalize the column names.
      call strcpy( ROOT_COLUMN_NAME, col_names[1,ROOT],      SZ_LINE )
      call strcpy( TIME_COLUMN_NAME, col_names[1,TIME],      SZ_LINE )
      call strcpy( LONG_COLUMN_NAME, col_names[1,LONGITUDE], SZ_LINE )
      call strcpy( LAT_COLUMN_NAME,  col_names[1,LATITUDE],  SZ_LINE )
   
      if( access( table_name, 0, 0 ) == YES ) {
         if( tbtacc( table_name ) == YES ) {
            table = tbtopn( table_name, READ_WRITE, 0 )
            call tbcfnd( table, col_names, columns, MAX_COLUMNS )
            
            # Check to make sure all the columns exist.  If one doesn't die
            # off.
            for( ip= 1; ip < MAX_COLUMNS; ip = ip + 1 )
               if( columns[ip] == NULL ) {
                  call sprintf( Memc[tmp], SZ_LINE,
                                "column %s doesn't exist in table %s" )
                  call pargstr( col_names[1,ip] )
                  call pargstr( table_name )
                  call error( 1, Memc[tmp] )
               }
            
            # Determine the next row to append.
            row = tbpsta( table, TBL_NROWS ) + 1
            
         } else {
            call sprintf( Memc[tmp], SZ_LINE, "file %s exists but isn't a table" )
            call pargstr( table_name )
            call error( 1, Memc[tmp] )
         }
      } else {
      
         table = tbtopn( table_name, NEW_FILE, 0 )
      
         call tbcdef( table, columns[ROOT], col_names[1,ROOT],
                      "", "%20s", -20, 1, 1 )
         call tbcdef( table, columns[TIME], col_names[1,TIME],
                      "", "%20.12f", TY_DOUBLE, 1, 1 )
         call tbcdef( table, columns[LONGITUDE], col_names[1,LONGITUDE],
                      "", "%10.0h", TY_REAL, 1, 1 )
         call tbcdef( table, columns[LATITUDE], col_names[1,LATITUDE],
                      "", "%10.0h", TY_REAL, 1, 1 )
         
         call tbtcre( table )
         row = 1
      }
      
      call sfree(sp)

   }

end

# WRITE_TABLE -- Write the appropriate information to the table.

procedure write_table( filename, group, time, longitude, latitude )

char    filename[ARB]        # The file name.
int     group                # Group to append to file name.
                             # If 0, don't append it.
double  time                 # Time in MJD.
real    longitude, latitude  # Position in degrees.

# Declarations.
int     dir_length           # Length of the directory name.

pointer cluster              # Filename without section/group.
pointer image                # Filename without section.
pointer sp                   # Stack pointer.
pointer tmpstr               # Temporary string.

include "table.com"

# Function prototypes.
int     fnldir(), strlen()

begin

   if( table != NULL ) {
      
      call smark(sp)
      call salloc( cluster, SZ_LINE, TY_CHAR )
      call salloc( image, SZ_LINE, TY_CHAR )
      call salloc( tmpstr, SZ_LINE, TY_CHAR )

      call imgcluster( filename, Memc[cluster], SZ_LINE )
      call imgimage( filename, Memc[image], SZ_LINE )
      if( group > 0 &&
          strlen( Memc[cluster] ) == strlen( Memc[image] ) ) {
         call sprintf( Memc[tmpstr], SZ_LINE, "[%d]" )
         call pargi( group )
         call strcat( Memc[tmpstr], Memc[image], SZ_LINE )
      }
      dir_length = fnldir( Memc[cluster], Memc[tmpstr], SZ_LINE )
      call tbrptt( table, columns[ROOT], Memc[image + dir_length],
                   strlen( Memc[image] ) - dir_length, 1, row )

      call tbrptd( table, columns[TIME],      time,      1, row )
      call tbrptr( table, columns[LONGITUDE], longitude, 1, row )
      call tbrptr( table, columns[LATITUDE],  latitude,  1, row )

      row = row + 1

      call sfree(sp)

   }
   
end

# CLOSE_TABLE -- Close the table.

procedure close_table()

# Declarations
include "table.com"

begin
   call tbtclo( table )
end
