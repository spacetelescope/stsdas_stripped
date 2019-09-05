# MKFNAME -- Make a file name given root, extension, and group number.

procedure mkfname( root, type, group, mxgroup, filename, maxch )

char   root[ARB]       # i: file name root
char   type[ARB]       # i: file type (extension)
int    group           # i: group specification
int    mxgroup         # i: total groups in image
char   filename[ARB]   # o: output file name
int    maxch           # i: max chars in filename

int    strlen()
char   gstr[7]

begin

   # Put root into output file name
   call strcpy( root, filename, maxch )

   # Append file type
   if (strlen(type) > 0 ) {
      call strcat( ".", filename, maxch )
      call strcat( type, filename, maxch )
   }
   
   # Append the group specification
   if (group > 0) {
       call mkgstr (group, mxgroup, gstr, 7)
       call strcat ( gstr, filename, maxch)
   }

end
