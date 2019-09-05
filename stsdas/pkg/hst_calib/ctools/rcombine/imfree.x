# IMFREE -- Unmap image pointers but check if they are mapped first

procedure imfree( im )

pointer   im

begin

   if ( im != NULL )
      call imunmap( im )
   
end
