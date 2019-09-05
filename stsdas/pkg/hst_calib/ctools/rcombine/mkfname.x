# MKFNAME -- Make a file name given root, group, section, and type

procedure mkfname( root, group, section, type, filename, maxch )

char   root[ARB]       # i: file name root
char   group[ARB]      # i: group specification
char   section[ARB]    # i: section specification
char   type[ARB]       # i: file type
char   filename[ARB]   # o: output file name
int    maxch           # i: max chars in filename

int    i, rtlen
char   dot

int    strlen()

begin

   # Get the index of the end of the actual root name.  Skip the first
   # character which might be a . if current directory (./) is specified
   dot = '.'

   #rtlen = stridx( dot, root[2] )
   # JC changes this 9/20/96 to avoid a bug happens for the case of ../file.ext
   # search the dot backward
   rtlen = 0
   do i = strlen(root), 1, -1 {
	if (root[i] == dot) {
	    rtlen = i - 1
	    break
	}
   }
   if ( rtlen <= 0 )
      rtlen = strlen( root )

   # Put root into output file name
   call strcpy( root, filename, rtlen )

   # Append file type
   if (strlen(type) > 0 ) {
      call strcat( ".", filename, maxch )
      call strcat( type, filename, maxch )
   }
   
   # Append the group specification
   if ( strlen(group) > 0 )
      call strcat( group, filename, maxch )

   # Append image section specification
   if ( strlen( section ) > 0 )
      call strcat( section, filename, maxch )

end
