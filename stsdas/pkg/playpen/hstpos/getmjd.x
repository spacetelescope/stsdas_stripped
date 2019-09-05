include <ctype.h>
include <ctotok.h>
include "convtime.h"

# GETMJD -- Get mjd for pkttime from shp
#
#
# S. Hulbert, Oct 91    Original

procedure getmjd (pkttime, mjd)

char	pkttime[ARB]	#I: pkttime string
double	mjd		#O: modified julian date
char	strmjd[SZ_FNAME]
int	indx, num

int	ctod()

begin
   
#   # look for first character of the month in the string
#   ip = 0
#   do i = 1, strlen( pkttime ) {
#      if (IS_ALPHA(pkttime[i])) {
#         ip = i
#         break
#      }
#   }
#   
#   # if we don't find a character just convert the string to a double
#   # if we find a character go and decode it into mjd
#   indx = 1
#   if (ip != 0) {
#      call convtime (pkttime, strmjd, SZ_FNAME)
#      num = ctod (strmjd, indx, mjd)
#   } else {
#      num = ctod (pkttime, indx, mjd)
#   }

   # First try converting the string of the form "DD-MMM-YY HH:MM:SS".
   # If this fails, assume that the string is a pure number.
   indx = 1
   iferr( call convtime( pkttime, strmjd, SZ_FNAME ) )
      num = ctod( pkttime, indx, mjd )
   else
      num = ctod( strmjd, indx, mjd )
   
end
