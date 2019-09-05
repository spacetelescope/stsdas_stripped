#---------------------------------------------------------------------------
.help i2g_display 29Apr92 develop
.ih
NAME
i2g_display -- Write the image to the graphics device.
.endhelp
#---------------------------------------------------------------------------

procedure i2g_display( gp, image, nx, ny, left, right, bottom, top, preserve )

pointer gp                      # I:  The graphics descriptor.
short   image[nx,ny]            # I:  The image (scaled) to display.
int     nx, ny                  # I:  The dimensions of the image.
real    left, right
real    bottom, top             # I:  The viewport to place the image in.
bool    preserve                # I:  TRUE to preserve image aspect ratio.

# Declarations.
real    display_ratio           # The display's aspect ratio.
real    image_ratio             # Aspect ratio of the image.
real    new_left, new_right
real    new_bottom, new_top     # The viewport to set to.
real    x, y                    # General x and y sizes.

# Function prototype
real    ggetr()

begin

   # Set the viewport depending on aspect ratios.
   if( !preserve )
      call gsview( gp, left, right, bottom, top )
   else {

      # Get the display aspect ratio.
      display_ratio = ggetr( gp, "ar" )
      if( display_ratio == 0.0 ) {
         x = ggetr( gp, "xr" )
         y = ggetr( gp, "yr" )
         if( x != 0.0 )
            display_ratio = y / x
         else
            display_ratio = 1.
      }

      # Get the image ratio.
      image_ratio = real(ny) / real(nx)

      # Determine viewport.
      display_ratio = display_ratio / image_ratio
      if( display_ratio < 1. ) {
         new_bottom = bottom
         new_top = top
         x = 1. - ( (top - bottom ) * display_ratio )
         new_left = x / 2.
         new_right = 1. - new_left
      } else {
         display_ratio = 1. / display_ratio
         new_left = left
         new_right = right
         x = right - left
         y = 1. - ( ( right - left ) * display_ratio )
         new_bottom = y / 2.
         new_top = 1. - new_bottom
      }
      call gsview( gp, new_left, new_right, new_bottom, new_top )
   }

   # Write it out.
   call gpcell( gp, image, nx, ny, 0., 0., 1., 1. )

end
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
