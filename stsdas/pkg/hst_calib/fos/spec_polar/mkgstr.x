define MXGCH 5

# MKGSTR -- Make a string that specifies the group for a multi-group image
#           e.g. [7] or [3/10]

procedure mkgstr( igroup, mxgroup, gstr, maxch )

int     igroup       # i: group number
int     mxgroup      # i: total number of groups in cluster,
                     # 0=> just put group, e.g [7] not [3/10]
char    gstr[ARB]    # o: output group string
int     maxch        # i: max chars n gstr

int     nchar
int     itoc()
char    gch[MXGCH]

begin

   call strcpy ("[", gstr, maxch )
   nchar = itoc( igroup, gch, MXGCH )
   call strcat( gch, gstr, maxch )
   if ( mxgroup > 0 ) {
      nchar = itoc( mxgroup, gch, MXGCH )
      call strcat( "/", gstr, maxch )
      call strcat( gch, gstr, maxch )
   }

   call strcat( "]", gstr, maxch )
   
end
