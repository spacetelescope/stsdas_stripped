define MAXRANGES 1 # maximum number of ranges to be parsed
define MAXWP    16 # maximum number of waveplate positions allowed

# BADWP -- Parse a list of bad waveplate positions.  Return a logical
#          array BADFRM which has an element set to true if that
#          frame should not be processed.

procedure badwp(badfrm, istat)

bool    badfrm[ARB]            # o: array of bad frame flags
int     istat                  # o: status flag

#--

pointer sp, wpstr
int     ranges[3,MAXRANGES+2], nval, istat1, ic, number
int     decode_ranges(), get_next_number()

string  badval "Waveplate position is outside of range"

begin

   istat = 0
   
   # Allocate temporary memory
   call smark(sp)
   call salloc( wpstr, SZ_FNAME, TY_CHAR)
   
   # Get the parameter string from the cl
   call clgstr("badwp", Memc[wpstr], SZ_FNAME)

   # Initialize the array of flags
   for (ic=1; ic<=16; ic=ic+1)
      badfrm[ic] = false

   # Parse the range string
   istat1 = decode_ranges(Memc[wpstr], ranges, MAXRANGES+2, nval)

   # If the first number in the range is 0 then there are no bad
   # positions.  Clean up and return.
   if (ranges[1,1] == 0) {
      call sfree(sp)
      istat = 0
      return
   }

   # Fill the array of flags with the appropriate value
   number = 0
   while(get_next_number(ranges, number) != EOF ) {

      # a number between 1 and MAXWP indicates a bad frame
      if (number >= 1 && number <= MAXWP)
	 badfrm[number] = true

      # Otherwise we're done
      else
	 break
   }

   call sfree(sp)

end
