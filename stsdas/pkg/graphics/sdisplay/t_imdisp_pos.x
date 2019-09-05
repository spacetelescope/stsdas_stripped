include <imhdr.h>

# t_imdisp_pos - Determine where an image is in the image display
#
# Description
#   Because of lack of communication between GIO and the image display tasks,
#   this task is used to determine the viewport coordinates necessary to 
#   specify a viewport on the image display such that it covers the image.
#
# History
#   6Jun91 - Created by Jonathan D. Eisenhamer, STScI.
#   5Dec91 - Removed dependency on the restricted header file display.h. jde
#
# Bugs
#   - Uses an internal and non-standard interface to the display.
#     If images.tv.display and the image display interface changes, this
#     will break.  However, if such a change occurs, it will hopefully
#     be to merge GIO and image display into one coherent task.
#---------------------------------------------------------------------------

procedure t_imdisp_pos

# Declarations
real fb_left, fb_bottom   # Bottom left corner of display.
real fb_right, fb_top     # Upper right corner of display.
real vl, vr, vb, vt       # Viewport corners.
real x, y                 # Generic reals.

int axis[2]               # Which axis refers to what.
int fb_xdim, fb_ydim      # Display size.
int frame                 # Frame the image is displayed in.
int i                     # Generic.
int j                     # Generic.
int key                   # Key hit to read image display.
int lv[IM_MAXDIM]         # Logical coordinates of the image.
int pv1[IM_MAXDIM]
int pv2[IM_MAXDIM]        # The physical coordinates of the image.
int wcs                   # The display WCS being used.

char strval[1]            # Return string from cursor read.

pointer ds                # Display descriptor.
pointer im                # The image descriptor.
pointer image             # Image name.
pointer iw                # Display WCS descriptor.
pointer section           # The section of the image actually displayed.
pointer sp                # Stack pointer.
pointer tmp_image         # Temporary image name.

# Function prototypes.
int imdrcur()
pointer imd_mapframe(), immap(), iw_open()

begin

  # Suck some memory
  call smark( sp )
  call salloc( image, SZ_FNAME, TY_CHAR )
  call salloc( tmp_image, SZ_FNAME, TY_CHAR )
  call salloc( section, SZ_LINE, TY_CHAR )

  # Access the image.
  call clgstr( "image", Memc[image], SZ_FNAME )
  im = immap( Memc[image], READ_ONLY, 0 )

  # Determine which frame is being displayed.
  if (imdrcur ("stdimage", x, y, wcs, key, strval, 1, 0, NO) >= 0)
    frame = max (1, wcs / 100)
  else
    frame = 1

  # Open the display device.
  ds = imd_mapframe( frame, READ_ONLY, YES )
  iw = iw_open( ds, frame, Memc[tmp_image], SZ_FNAME, x )
  if( Memc[tmp_image] == EOS || x == ERR ) {
    call eprintf( "imdisp_pos: No image is located in the display.\n" )

  } else {

    # Get the display size.
    fb_xdim = IM_LEN(ds, 1 ) - 1
    fb_ydim = IM_LEN(ds, 2 ) - 1
  
    # Get the physical image coordinates of the two opposite corners of
    # the image section being displayed.  This ignores questions of images
    # with dimensionality greater than 2, which are better dealt with
    # elsewhere.
  
    lv[1] = 1;  lv[2] = 1
    call imaplv (im, lv, pv1, 2)
  
    lv[1] = 2;  lv[2] = 2
    call imaplv (im, lv, pv2, 2)
  
    # Determine which physical axes contribute to the image section.
    i = 1
    axis[1] = 1;  axis[2] = 2
    do j = 1, IM_MAXDIM
      if (pv1[j] != pv2[j]) {
        axis[i] = j
        i = i + 1
      }
  
    # Determine where the image is located in the frame buffer.
    lv[1] = 1; lv[2] = 1
    call imaplv( im, lv, pv1, 2 )
    x = real( pv1[axis[1]] )
    y = real( pv1[axis[2]] )
    call iw_im2fb( iw, x, y, fb_left, fb_bottom )
  
    lv[1] = IM_LEN(im, 1); lv[2] = IM_LEN(im, 2)
    call imaplv( im, lv, pv2, 2 )
    x = real( pv2[axis[1]] )
    y = real( pv2[axis[2]] )
    call iw_im2fb( iw , x, y, fb_right, fb_top )
  
    # Now set the viewport coordinates.
    vl = max( 0., ( fb_left - 1. ) / fb_xdim )
    vr = min( 1., ( fb_right - 1. ) / fb_xdim )
    vb = max( 0., ( fb_bottom - 1. ) / fb_ydim )
    vt = min( 1., ( fb_top - 1.) / fb_ydim )
  
    # Write the parameter back out.
    call clputr( "left", vl )
    call clputr( "right", vr )
    call clputr( "bottom", vb )
    call clputr( "top", vt )
  
    # If the image is too large for the display, write out what section
    # of the image is actually displayed.
    call imgimage( Memc[image], Memc[tmp_image], SZ_FNAME )
    call iw_fb2im( iw, 1., 1., vl, vb )
    x = real( fb_xdim + 1)
    y = real( fb_ydim + 1)
    call iw_fb2im( iw, x, y, vr, vt )
  
    if( fb_left < 1. )
      lv[1] = int( vl + .5 )
    else
      lv[1] = pv1[axis[1]]
    if( fb_right > x )
      lv[2] = int( vr + .5 )
    else
      lv[2] = pv2[axis[1]]
    if( fb_bottom < 1. )
      lv[3] = int( vb + .5 )
    else
      lv[3] = pv1[axis[2]]
    if( fb_top > y )
      lv[4] = int( vt + .5 )
    else
      lv[4] = pv2[axis[2]]
  
    call sprintf( Memc[section], SZ_FNAME, "%s[%d:%d,%d:%d]" )
    call pargstr( Memc[tmp_image] )
    call pargi( lv[1] )
    call pargi( lv[2] )
    call pargi( lv[3] )
    call pargi( lv[4] )
    call clpstr( "section", Memc[section] )

  }

  # That's all.  Unsuck memory.
  call iw_close( iw )
  call imunmap( ds )
  call imunmap( im )
  call sfree( sp )

end
#---------------------------------------------------------------------------
# End of t_imdisp_pos
#---------------------------------------------------------------------------
