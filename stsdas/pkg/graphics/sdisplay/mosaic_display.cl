# mosaic_display -- Display a list of images simultaneously on an image
#		display (all within the same frame buffer).
#
# Description --
#	This script takes a list of file names, presumably image files,
#	and displays all images within a single frame buffer of an image
#	display with the display task.  The parameters are as follows:
#
#	images - The list of images to display.
#	ncols,nrows - If specified, these parameters define the geometry
#		in which the images will be displayed.  If either/both
#		parameters are not specified, then the script determines
#		the smallest sized square necessary to display all the
#		specified images.  If there are not enough images to fill
#		the square, the right end of the last row will remain
#		empty.
#	frame - The image display's frame buffer in which the images
#		are to appear.
#
# Image List format
#	If the image list passed to this script is just simply a list
#	of images, the provided defaults for DISPLAY will be used to
#	display each of the images.  However, if different transfer functions
#	are desired, the necessary DISPLAY parameters can be placed on the
#	the line with the image name in a comma-seperated list.  The parameters
#	apply only to that specific image; the parameters are not learned.
#	An example list would be:
#
#		dev$pix
#		dev$pix,contrast=.5
#		dev$pix,zscale=no
#		dev$pix,nsample_line=50
#		dev$pix, zscale=no, zrange=no, z1=10, z2=1000
#		dev$pix,ztrans="log"
#
#	where each image would be displayed with the appropriate 
#	scaling.
#
# History --
#	17Oct90 - Created Jonathan D. Eisenhamer @ STScI mostly copied from
#		an example in _An Introductory User's Guide to IRAF Scripts_
#	18Oct90 - Added parameter for the frame buffer specification.
#		Also added ability to put DISPLAY parameters on the same
#		line as each image name. jde
#	19Oct90 - Set so that none of the parameters of DISPLAY are
#		permenantly set. jde
#	 2Nov90 - Fixed the way an input file is parsed to allow section
#		specifications along with display parameters. jde
#---------------------------------------------------------------------------

procedure mosaic_display (images)

string images {prompt="Images to display",mode="al"}
int ncols = INDEF {prompt="Number of columns",mode="hl"}
int nrows = INDEF {prompt="Number of rows",mode="hl"}
int frame = 1 {prompt="Image frame buffer",mode="hl"}

struct *imglist {mode="h"}

begin
        # Declarations
        real max_c_position = 1.	# Display's max coord position in cols
        real max_r_position = 1.	# Display's max coord position in rows
        real min_r_position = 0.	# Display's min coord position in rows
        
        string imgfile		        # Temporary file list.
        string iimg			# Temporary file list name.
        string inline			# Input line from file list.
        string dis_com, tdis_com	# Command to display the image.
        real rsize			# Size of necessary square (one side)
        real incols, inrows		# #'s columns/rows to be filled
        real c_center, c_incr, r_center, r_incr # Where each image gets placed.
        int comma			# Position of comma (if any).
        int i				# Index of right bracket in input string.
        int nimages			# # of images to display.
        int right_bracket		# Position of right-most right bracket.
        bool erase=yes		        # Erase the buffer.
        
        # Check that the necessary packages are loaded.
        if ( ! defpac("images") || ! defpac("tv") ) {
            print( "ERROR, you need to be in images.tv!")
            bye
        }
        
        # Get the query parameters
        iimg = images
        
        # Expand the image template into a text file list.
        imgfile = mktemp("tmp$img")
        sections (iimg, option="fullname", >imgfile)
        nimages = sections.nimages
        
        # If the rows and columns are not assigned, then calculate the best
        # square.
        if ( ncols == INDEF || nrows == INDEF ) {
            
            # Calculate the size of the square necessary that the number
            # of images will fill.
            rsize = sqrt( nimages )
            incols = int(rsize)
            if ( frac(rsize) > 0 )
                incols += 1
            inrows = int( rsize + .5)
        }
	
        # Else, just use the specified values.
        else {
            incols = ncols
            inrows = nrows
        }
        if ( incols * inrows < nimages )
            print ("Warning, not enough columns/rows for the number of images specified!")
        
        # Calculate the increment.  Round so the final command isn't too large.
        c_incr = max_c_position / incols
        i = c_incr * 100
        c_incr = i / 100.
        r_incr = max_r_position / inrows
        i = r_incr * 100
        r_incr = i / 100.
        
        # Build the constant part of the display command.  All the parameters
        # used in this procedure for the display command are placed on the
        # command line so any of the modifications are not learned.
        tdis_com = "frame="//frame//",fill=yes,xsize="//c_incr//",ysize="//r_incr
        
        # Now display the images.
        imglist = imgfile
        for ( r_center = 1 - (r_incr / 2.); r_center > min_r_position; r_center -= r_incr ) {
            for ( c_center = c_incr / 2.; c_center < max_c_position; c_center += c_incr ) {
                
                # Get the next image name.  If we have run out,
                # just stop.
                if ( fscan (imglist, inline) == EOF)
                    bye
                
                # See if there are display parameters on the
                # line.  This is done by looking for a comma
                # seperator.  If there isn't one, it is
                # assumed that the only thing on the command
                # line is an image name.  If there is, the
                # first item before the comma is assumed
                # to be the image name and the rest of the
                # line is assumed to be keyword parameter
                # definitions for display. A complication
                # arises here when sections are specified.  Thus,
                # we first need to skip over any specifications.
                # We do this by looking for a right-most right
                # bracket, ']'.
                i = stridx( "]", inline )
                right_bracket = 0
                while ( i != 0 ) {
                    right_bracket = right_bracket + i
                    i = stridx( "]", substr( inline,right_bracket + 1,
                                             strlen( inline ) ) )
                }
                comma = stridx (",", substr( inline, right_bracket + 1,
                                             strlen( inline ) ) )
                if ( comma == 0 )
                    dis_com = "display(\""//inline//"\""
                else
                    dis_com = "display(\""//
                              substr( inline, 1, right_bracket + comma-1 )//
                              "\""//
                              substr(inline, right_bracket + comma,
                                     strlen(inline))
                
                # Construct the command, set parameters and
                # display the image.
                dis_com = dis_com//",xcenter="//c_center//
                          ",ycenter="//r_center//
                          ",erase="//erase//","//tdis_com//")"
                # print( "dis_com = ", dis_com )
                print( dis_com ) | cl
                
                erase = no
            }
        }
        
        # Clean up
        delete ( imgfile, ver-, >& "dev$null")
        
end
