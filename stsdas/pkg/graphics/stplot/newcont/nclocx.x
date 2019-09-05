include <gset.h>
include "newcont.h"

# We need some goto labels.
define label_01_ 1
define label_12_ 12
define label_24_ 24

# nc_local_minmax - Find local minima and maxima.
#
# Description
#   This routine finds relative minimums and maximums.  A relative minimum
#   (or maximum) is defined to be the lowest (or highest) point within
#   a certain neighborhood of the point.  The neighborhood used here
#   is + or - mn in the x direction and + or - nm in the y direction.
#
#   NOTE: This is taken from the NCAR CONREC routine from NCARGRAPHICS
#   version 1.0 now in the public domain.
#
# Author
#   David Kennison
#
# History
#   23Jan91 - Taken from the CONREC routine, rewritten into SPP and the
#             IRAF GIO interface.  Removed code dealing with "special
#             values".
#---------------------------------------------------------------------------

procedure nc_local_minmax ( z,l,mm,nn )

real z[l, nn]   # I:  The array to find the local minima/maxima.
int  l          # I:  The declared size of Z in the X dimension.
int  mm         # I:  The number of columns to use (in the X dimension).
int  nn         # I:  The number of rows to use (in the Y dimension).

# Declarations
include "newcont_global.h"

pointer format        # Output number string.
pointer sp            # Stack pointer.

real aa, an, x, y
real old_text_size    # Original text size.

int ii, ik, im, ip, is, it, jk, jp, js, jt, m, mm1, mn, n, nm, nm1
int old_text_quality  # Original text quality

# Function definitions
real gstatr()
int  gstati()

begin

  # Initialize
  call smark( sp )
  call salloc( format, SZ_LINE, TY_CHAR )

  m = mm
  n = nn
  mn = min0(15,max0(2,ifix(float(m)/8.)))
  nm = min0(15,max0(2,ifix(float(n)/8.)))
  nm1 = n-1
  mm1 = m-1

  # Save text quality and size and set to high quality and small size.
  old_text_size = gstatr( gp, G_TXSIZE )
  old_text_quality = gstati( gp, G_TXQUALITY )
  call gsetr(gp, G_TXSIZE, SMALL_SIZE )
  call gseti(gp, G_TXQUALITY, GT_HIGH )

  # line loop follows - the complete two-dimensional test for a minimum or
  # maximum of the field is only performed for points which are minima or
  # maxima along some line - finding these candidates is made efficient by
  # using a count of consecutive increases or decreases of the function
  # along the line
  do jp=2,nm1 {

    ip = 1
    im = 1

    # control returns to statement 10 as long as the function is increasing
    # along the line - we seek a possible maximum
    #
label_01_
    repeat {
      ip = ip+1
      aa = an
      if (ip == mm1)
        break
      an = z(ip+1,jp)
      if( aa - an < 0. ) 
        im = im+1
      else if( aa - an == 0. )
        im = 0
      else
        break
    }

    # function decreased - test for maximum on line
    #
    if (im < mn) {
      is = max0(1,ip-mn)
      it = ip-im-1
      if (is <= it)
        do ii=is,it
          if (aa <= z(ii,jp))
            goto label_12_
    }
    is = ip+2
    it = min0(m,ip+mn)
    if (is <= it)
      do  ii=is,it
        if (aa <= z(ii,jp)) 
          go to label_12_

    # we have maximum on line - do two-dimensional test for maximum of field
    js = max0(1,jp-nm)
    jt = min0(n,jp+nm)
    is = max0(1,ip-mn)
    it = min0(m,ip+mn)
    do  jk=js,jt {
      if (jk == jp)
        next
      do ik=is,it
        if (z(ik,jk) >= aa ) 
          go to label_12_
    }

    # We made it.  This is a maximum.  Write it out.
    x = float(ip)
    y = float(jp)
    call gtext( gp, x, y, "h", "h=c;v=b" )
    call sprintf( Memc[format], SZ_LINE, "%g" )
    call pargr( aa )
    call gtext( gp, x, y, Memc[format], "h=c;v=t" )

label_12_
    im = 1
    if ( ip - mm1 >= 0. )
      next

    # control returns to statement 20 as long as the function is decreasing
    # along the line - we seek a possible minimum
    repeat {
      ip = ip+1
      aa = an
     if (ip == mm1)
       break
     an = z(ip+1,jp)
     if( aa - an > 0. )
       im = im+1
     else if( aa - an == 0. )
       im = 0
     else
       break
    }

    # function increased - test for minimum on line
    if (im < mn) {
      is = max0(1,ip-mn)
      it = ip-im-1
      if (is <= it)
      do ii=is,it
        if (aa >= z(ii,jp)) 
          go to label_24_
    }
    is = ip + 1
    it = min0(m,ip+mn)
    if (is <= it)
     do ii=is,it
        if (aa >= z(ii,jp))
          goto label_24_

    # we have minimum on line - do two-dimensional test for minimum of field
    js = max0(1,jp-nm)
    jt = min0(n,jp+nm)
    is = max0(1,ip-mn)
    it = min0(m,ip+mn)
    do jk=js,jt {
      if (jk == jp)
        next
      do ik=is,it
        if (z(ik,jk) <= aa ) 
          goto label_24_
    }

    # We have a low point, mark it.
    x = float(ip)
    y = float(jp)
    call gtext( gp, x, y, "l", "h=c;v=b" )
    call sprintf( Memc[format], SZ_LINE, "%g" )
    call pargr( aa )
    call gtext( gp, x, y, Memc[format], "h=c;v=t" )

label_24_
    im = 1
    if (ip-mm1 < 0. ) 
      goto label_01_

  }

  # Reset text quality and size.
  call gsetr( gp, G_TXSIZE, old_text_size )
  call gseti( gp, G_TXQUALITY, old_text_quality )
  call sfree( sp )

end	
#---------------------------------------------------------------------------
# End of nc_local_minmax
#---------------------------------------------------------------------------
