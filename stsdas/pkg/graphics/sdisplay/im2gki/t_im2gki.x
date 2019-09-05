include <imhdr.h>
include "im2gki.h"
include <psiescape.h>

#---------------------------------------------------------------------------
.help im2gki 29Apr92 develop
.ih
NAME
im2gki -- Write an image to a graphics device
.endhelp
#---------------------------------------------------------------------------

procedure t_im2gki

# Declarations
real    left, right
real    bottom, top             # Viewport definition

int     array_size              # Size of the allocated working arrays.
int     current_image           # Image counter.
int     function                # Function to scale by.
int     length                  # Length of file name.
int     n_elements              # Length of the image data array.
int     nimages                 # Number of images in list.

bool    preserve                # TRUE to preserve image aspect.

pointer device                  # Graphics device.
pointer gp                      # Graphics device descriptor.
pointer im                      # Image descriptor.
pointer in                      # The image data array.
pointer input                   # Input file specifications.
pointer list                    # Image file list.
pointer outr, outs              # Real/Short working arrays.
pointer sp                      # Stack pointer.
pointer rlut, glut, blut        # Image lookup tables.

string  func_dict FUNCTION_DICTIONARY

# Function prototypes
real    clgetr()
int     imtgetim(), imtlen(), strdic(), strlen()
bool    clgetb()
pointer gopen(), imgs2r(), immap(), imtopen()

begin

   call smark(sp)
   call salloc( device, SZ_LINE, TY_CHAR )
   call salloc( input, SZ_LINE, TY_CHAR )
   
   # Read and confirm that there are inputs.
   call clgstr( "input", Memc[input], SZ_LINE )
   list = imtopen( Memc[input] )
   nimages = imtlen( list )
   if( nimages == 0 )
      call error( 1, "im2gki: No input images specified!" )

   # Open the graphics device.
   call clgstr( "device", Memc[device], SZ_LINE )
   left = clgetr( "left" )
   right = clgetr( "right" )
   bottom = clgetr( "bottom" )
   top = clgetr( "top" )
   preserve = clgetb( "preserve" )
   
   if( clgetb( "append" ) )
      gp = gopen( Memc[device], APPEND, STDGRAPH )
   else
      gp = gopen( Memc[device], NEW_FILE, STDGRAPH )
   call gswind( gp, 0., 1., 0., 1. )
      
   # Get and set the colormap if specified.
   call clgstr( "cmap", Memc[input], SZ_LINE )
   if( strlen( Memc[input] ) > 0 ) {
      call salloc( rlut, PS_IMAGE_LUT_SIZE, TY_SHORT )
      call salloc( glut, PS_IMAGE_LUT_SIZE, TY_SHORT )
      call salloc( blut, PS_IMAGE_LUT_SIZE, TY_SHORT )
      call sp_read_colormap( Memc[input], Mems[rlut], Mems[glut], Mems[blut],
                             PS_IMAGE_LUT_SIZE )
   } else
      rlut = NULL

   # Get the desired function.
   call clgstr( "function", Memc[input], SZ_LINE )
   function = strdic( Memc[input], Memc[input], SZ_LINE, func_dict )

   # Read each input image, scale if necessary, and graph it.
   array_size = 0
   do current_image = 1, nimages {

      # If this is not the first image, then page the output.
      if( current_image > 1 )
         call gclear( gp )

      # Write out the lookup tables if specified.
      if( rlut != NULL ) {
         call gescape( gp, PS_IMAGE_RED_LUT, Mems[rlut], PS_IMAGE_LUT_SIZE )
         call gescape( gp, PS_IMAGE_GREEN_LUT, Mems[glut], PS_IMAGE_LUT_SIZE )
         call gescape( gp, PS_IMAGE_BLUE_LUT, Mems[blut], PS_IMAGE_LUT_SIZE )
      }
   
      # Open image.
      length = imtgetim( list, Memc[input], SZ_LINE )
      im = immap( Memc[input], READ_ONLY, 0 ) 
      n_elements = IM_LEN(im,1) * IM_LEN(im, 2)
      in = imgs2r( im, 1, IM_LEN(im,1), 1, IM_LEN(im,2) )

      # Allocate working arrays.
      if( array_size < n_elements ) {
         if( array_size > 0 ) {
            call mfree( outr, TY_REAL )
            call mfree( outs, TY_SHORT)
         }
         call calloc( outr, n_elements, TY_REAL )
         call calloc( outs, n_elements, TY_SHORT )
         array_size = n_elements
      }

      # Scale and convert data types.
      call i2g_scale( Memr[in], n_elements, function, clgetb( "zscale" ),
                      clgetr( "low" ), clgetr( "high" ), 0.,
                      real( PS_MAX_LUT_VALUE ), Memr[outr] )
      call achtrs( Memr[outr], Mems[outs], n_elements )

      # Display the image.
      call i2g_display( gp, Mems[outs], IM_LEN(im,1), IM_LEN(im,2),
                        left, right, bottom, top, preserve )

      # Close image.
      call imunmap( im )
      
   }

   # That's all folks.
   if( array_size > 0 ) {
      call mfree( outr, TY_REAL )
      call mfree( outs, TY_SHORT )
   }
   call gclose( gp )
   call imtclose( list )
   call sfree(sp)

end
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
