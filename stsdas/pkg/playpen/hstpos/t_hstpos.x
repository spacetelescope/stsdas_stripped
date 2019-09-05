include <ctype.h>
include <error.h>
include <gset.h>
include <imhdr.h>
include <imio.h>
include <syserr.h>
include	"orbdat.h"
include "filetype.h"

define IMSET_STIS	3
define IMSET_NIC	5
define IMSET_DEF	1

# T_HSTPOS -- Calculate HST position using orbital data from
#		a shp header file for a set of times
#
# S. Hulbert 	Jul91	Original
#
# W. Hack	Apr97	Added capability of working with FITS(STIS/NICMOS)
#			data files, and search for 'shp.fits' and 'spt.fits'
#			files for the orbit file.
#
procedure t_hstpos ()

double	mjd                     # Time.

real    datamin, datamax        # Minimum, maximum of an image.
real    latint                  # Interval between geomagnetic latitudes.
real	lng, lat                # Position in degrees of the 'scope.
real	szmarker                # Size of graphics marker.
real	x, y                    # Graphics position of the 'scope.

int	ftype                   # Type of file-image or text.
int     group                   # Current group being plotted.
int     in_file_len             # Length of input file name.
int     marker_type             # Type of marker.
int 	mode                    # Mode to open graphics device.
int     model                   # SAA Model to plot.
int     n_latitudes             # Number of north/south latitudes to draw.
int     ngroup                  # Number of groups to be plotted.
int     orb_file_len            # Length of orbit file name.
int	imset			# Number of Extensions in each IMSET
int	imfits

bool    all_groups              # TRUE if all groups should be plotted.
bool	append                  # TRUE to append the plot.
bool    plot                    # TRUE to produce plots of positions.


pointer fd                      # File descriptor of text time file.
pointer	gp                      # Graphics descriptor.
pointer	imin                    # Image descriptor for time image.
pointer imorb                   # Image descriptor for orbit image.
pointer in_file                 # Current input file for times.
pointer in_list                 # List of input files for times.
pointer ip                      # Buffer pointer/index.
pointer last_orbit              # The last orbit file used.
pointer orb                     # Orbit descriptor.
pointer	orb_file                # Current file for orbit params.
pointer orb_list                # List of files for orbit params.
pointer	pkttime                 # The time in mjd format.
pointer saa_table_name          # Table name that contains the SAA models.
pointer sp                      # Stack pointer.
pointer timekey                 # Header keyword to get time from.
pointer tmp                     # Temporary string buffer.
pointer tmp2                    # Second temporary string buffer.
pointer tmpacc                    # Second temporary string buffer.
pointer	instr
pointer	imexp			# Pointer for FITS IMSET

# Function prototypes.
real	clgetr()
int	clgeti(), getline(), imtgetim(), open()
int	filetype(), imtlen(), strlen()
bool	clgetb()
pointer	immap(), imtopenp(), gopen()
bool	streq()
int	access(), imaccess()

begin
        
        call smark (sp)
        
        call salloc (orb_file, SZ_LINE, TY_CHAR)
        call salloc (in_file, SZ_LINE, TY_CHAR)
        call salloc (pkttime, SZ_LINE, TY_CHAR)
        call salloc (timekey, SZ_LINE, TY_CHAR)
        call salloc( last_orbit, SZ_LINE, TY_CHAR )
        call salloc( tmp, SZ_LINE, TY_CHAR )
        call salloc( tmp2, SZ_LINE, TY_CHAR )
        call salloc( tmpacc, SZ_LINE, TY_CHAR )
	call salloc( instr, SZ_LINE, TY_CHAR )
        
        # Open the input list of files and the standard header packet of files.
        in_list = imtopenp( "input" )
        orb_list = imtopenp( "orbit" )
        
        # If both lists are empty then error out- need some type of input.
        if( imtlen( in_list ) == 0 && imtlen( orb_list ) == 0 )
            call error( 1, "Need to specify input and/or orbit files" )
        
        # If multiple orbit files are specified, it should probably match the
        # number of input files, if they are more than zero.
        if( imtlen( orb_list ) > 1 &&
            imtlen( in_list ) > 0 &&
            imtlen( orb_list ) != imtlen( in_list ) )
            call error( 1, "# orbit files should equal # of input files or 1" )
        
        # Get which header keyword is going to be used to retrieve the time from.
        call clgstr ("timekey", Memc[timekey], SZ_LINE)
        
        # Get whether all groups of a multi-group image should be operated on.
        all_groups = clgetb( "allgroups" )
        
        # Open the table.
        call clgstr( "position", Memc[tmp], SZ_LINE)
        call open_table( Memc[tmp] )
        
        # See if plots are to be generated.
        plot = clgetb( "plot" )
        
        # open the graphics device
        if( plot ) {
            call clgstr ("device", Memc[tmp], SZ_LINE)
            mode = NEW_FILE
            append = clgetb("append")
            if (append)
                mode = APPEND
            gp = gopen (Memc[tmp], mode, STDGRAPH)
            
            # plot the world, saa contours, and geomagnetic lines if desired.
            if (!append) {
                call mwp (gp)

                if (clgetb ("geomag")) {
                    latint = clgetr ("latint")
                    n_latitudes = clgeti ("n_latitudes")
                    call hst_geomag (gp, latint, n_latitudes)
                }

                model = clgeti ("model")
                if (!IS_INDEFI(model)) {
                    call salloc (saa_table_name, SZ_LINE, TY_CHAR)
                    call clgstr ("saa", Memc[saa_table_name], SZ_LINE)
                    call hst_saa_plot (Memc[saa_table_name], model, gp)
                }
            }
            
            
            # get plotting mark for HST positions
            call clgstr ("marker", Memc[tmp], SZ_LINE)
            szmarker = clgetr ("szmarker")
            call init_marker (Memc[tmp], marker_type)
            
        }
        
        # Calculate positions and optionally plot the data.
        # Warning: Ugly conditions!  Read in both the input list and the orbit
        # list. If both exist, use the orbit list for orbit parameters and use
        # the file list for times.  If there is an orbit file but not an input file
        # take all information from the orbit file.  If there is an input file
        # and no orbit file, try to construct the standard header packet file name
        # and look for it.  Continue until both lists are exhausted.
        Memc[last_orbit] = EOS
        while( true ) {
            
            # If both lists are empty or when we are at end of lists, break out.
            in_file_len = imtgetim( in_list, Memc[in_file], SZ_LINE )
            orb_file_len = imtgetim( orb_list, Memc[orb_file], SZ_LINE )
            if( in_file_len == EOF && orb_file_len == EOF )
                break
   
            # If there isn't a new orbit file, recall the last orbit file used.
            # else, reset the last orbit file.
            if( orb_file_len <= 0 )
                call strcpy( Memc[last_orbit], Memc[orb_file], SZ_LINE )
            else
                call strcpy( Memc[orb_file], Memc[last_orbit], SZ_LINE )
            
            # If there is no input file, get all the information from the orbit
            # file.
            if( in_file_len <= 0 ) 
                call strcpy( Memc[orb_file], Memc[in_file], SZ_LINE )
            
            # Else If there is an input file and no orbit file, try to construct the
            # shp file name and use that as the orbit file.
            else if( strlen( Memc[last_orbit] ) == 0 ) {
                
                call imgcluster( Memc[in_file], Memc[tmp], SZ_LINE )
                call fnldir( Memc[tmp], Memc[orb_file], SZ_LINE )
                call fnroot( Memc[tmp], Memc[tmp], SZ_LINE )
		#	call eprintf("Rootname (from fnroot) is %s\n")
		#		call pargstr(Memc[tmp])
                call strcat( Memc[tmp], Memc[orb_file], SZ_LINE )
                call strcat( ".shh", Memc[orb_file], SZ_LINE )

		if (access(Memc[orb_file], 0 , 0) == NO) {
		    # Couldn't find GEIS .shh file, so we are dealing with	
		    # FITS files...
	  	    # Let's start by looking for "_" in rootname...
		    if(orb_file_len < 2) {
			orb_file_len = strlen(Memc[tmp]) - 3
		    } else {
		    	orb_file_len = orb_file_len - 3
		    }
		    
		    call strcpy( Memc[tmp], Memc[orb_file], orb_file_len )
                    call strcat( "shp.fits", Memc[orb_file], SZ_LINE )
		    if (access(Memc[orb_file], 0, 0) == NO ){
		    	call strcpy( Memc[tmp], Memc[orb_file], orb_file_len )
        	        call strcat( "spt.fits", Memc[orb_file], SZ_LINE )
			if (access(Memc[orb_file], 0, 0) == NO ){
				call eprintf("Warning: Could NOT find suitable orbit file... continuing.\n")
				call strcpy(NULL, Memc[orb_file], 1)
			}
		    }
	  	    call strcat("[0]",Memc[orb_file], SZ_LINE)
		}
	        #finished determining orbit file name...
            }
            
            # At this point, an input file name and an orbit file name have been
            # obtained.  First, retreive the orbital information.
            if( plot )
                call gdeactivate( gp, 0 )
            iferr( imorb = immap( Memc[orb_file], READ_ONLY, 0) ) {
                call eprintf ( "ERROR: unable to open orbit image %s\n...continuing\n" )
                call pargstr (Memc[orb_file])
                next
            }
            if( plot )
                call greactivate( gp, 0 )
            
            # get the orbital parameters
            call getorb (imorb, orb)
            
            # Close the orbit file.
            call imunmap (imorb)
            
            # Now get the time.  Note, this file can be a text file of times and
            # not an image.
            
            # get the filetype
            ftype = filetype(Memc[in_file])
            
            # we have an ascii file
            if (ftype == LIST_FILE) {
                
                # open text file
                if( plot )
                    call gdeactivate( gp, 0 )
                fd = open (Memc[in_file], READ_ONLY, TEXT_FILE)
                if( plot )
                    call greactivate( gp, 0 )
                
                # get the next line in the text file
                while (getline (fd, Memc[tmp]) != EOF) {
                    
                    # Skip whitespace
                    for (ip=tmp; IS_WHITE(Memc[ip]); ip=ip+1)
                        ;
                    
                    # Skip comment lines and blank lines.
                    if (Memc[ip] == '#' || Memc[ip] == '\n' || 
                        Memc[ip] == EOS)
                        next
                    
                    # Get time
                    call sscan (Memc[ip])
                    call gargstr (Memc[pkttime], SZ_LINE)
                    
                    # get mjd corresponding to pkttime in shh
                    call getmjd (Memc[pkttime], mjd)
                    
                    # get longitude and latitude at mjd
                    call getpos (orb, mjd, lng, lat)
                    
                    # plot marker
                    if( plot ) {
                        call mwcart (lng, lat, x, y)
                        call gmark (gp, x, y, marker_type, szmarker, szmarker)
                    }
                    
                    # Write table.
                    call write_table( Memc[in_file], 0, mjd, lng, lat )
                    
                }
            }
            
            # here we have an image
            else if (ftype == IMAGE_FILE) {

                call imgcluster( Memc[in_file], Memc[tmp], SZ_LINE )
		in_file_len = in_file_len - 4

		call strcpy(Memc[tmp+in_file_len], Memc[tmp2], 4)
                call fnroot( Memc[tmp], Memc[tmp], SZ_LINE )

		#call fit_access (Memc[tmp],Memc[tmp2], 0, imfits)
		call strcpy (Memc[tmp], Memc[tmpacc], SZ_LINE)
		call strcat("[sci,1]",Memc[tmpacc], SZ_LINE]

		if (imaccess(Memc[tmpacc], 0) == YES ) {
			imfits = YES
		} else {
			imfits = NO
		}

            # map the input
                if( plot )
                    call gdeactivate( gp, 0 )

		# We are working with a GEIS image...
		# Open image here...
		if (imfits == YES) {
		    call strcpy (Memc[in_file], Memc[tmpacc], SZ_LINE)
		    call strcat("[0]",Memc[tmpacc], SZ_LINE]
                    iferr (imin = immap (Memc[tmpacc], READ_ONLY, 0)) {
                    	call eprintf( "ERROR: unable to open input image %s.\n...continuing\n" )
                    	call pargstr (Memc[tmpacc])
                    	next
                    }
		} else {
                    iferr (imin = immap (Memc[in_file], READ_ONLY, 0)) {
                    	call eprintf( "ERROR: unable to open input image %s.\n...continuing\n" )
                    	call pargstr (Memc[in_file])
                    	next
                    }
		}		
                if( plot )
                    call greactivate( gp, 0 )
                
                # Determine the range of groups/IMSETs to operate on.
                if( all_groups ) {
                    
                    # Check to see if a group wasn't specified.
                    call imgimage( Memc[in_file], Memc[tmp], SZ_LINE )
                    call imgcluster( Memc[in_file], Memc[tmp2], SZ_LINE )
                    if( strlen( Memc[tmp] ) != strlen( Memc[tmp2] ) )
                        ngroup = 1
                    else
                        ngroup = IM_CLSIZE(imin)
                } else
                    ngroup = 1


		if (ngroup < 1) ngroup = 1

		iferr( call imgstr(imin,"INSTRUME", Memc[instr], SZ_LINE) ) {
			call eprintf("Warning: Couldn't read instrument name.\n...continuing\n")
			imset = IMSET_DEF
			next
		}

		# Set the number of extensions in each IMSET of the file
		if ( streq(Memc[instr],"STIS") ) imset = IMSET_STIS
		else if (streq(Memc[instr],"NICMOS") ) imset = IMSET_NIC
		else 	imset = IMSET_DEF

	      # Now Let's work with the files correctly, whether they are
	      # FITS or GEIS...
 	      if (imfits == NO) {

                # We are working with GEIS images, so loop on the groups.
                do group = 1, ngroup {
                    
                    # Get the next group.  If doing only one group, then don't
                    # bother opening it.  This either means that the image is
                    # not multigroup, multigroup wasn't requested, or the group
                    # was specified on the command line.
                    if( ngroup > 1 )
                        iferr( call gf_opengr( imin, group, datamin, datamax, 0 ) ) {
                            call eprintf( "ERROR: changing to group %d in input image %s" )
                            call pargi( group )
                            call pargstr( Memc[in_file] )
                            next
                        }
                    
                    # read pkttime from header
                    iferr( call imgstr( imin, Memc[timekey], Memc[pkttime],
                                        SZ_LINE ) ) {
                        call eprintf( "ERROR: cannot get time from image %s.\n...continuing.\n" )
                        call pargstr( Memc[in_file] )
                        next
                    }
 
                    # get mjd corresponding to pkttime in shh
                    call getmjd (Memc[pkttime], mjd)

                    # get longitude and latitude at mjd
                    call getpos (orb, mjd, lng, lat)
                    
                    # plot marker
                    if( plot ) {
                        call mwcart (lng, lat, x, y)
                        call gmark (gp, x, y, marker_type, szmarker, szmarker)
                    }
                    
                    # Write table.
                    if( ngroup > 1 )
                        call write_table( Memc[in_file], group, mjd, lng, lat )
                    else
                        call write_table( Memc[in_file], 0, mjd, lng, lat )
                    
                }
	      } else {
		# We are working with  FITS images...		

		# Loop through the IMSETs...
                do group = 1, ngroup, imset {
                    
                    # Get the next group.  If doing only one group, then don't
                    # bother opening it.  This either means that the image is
                    # not multigroup, multigroup wasn't requested, or the group
                    # was specified on the command line.
                    if( ngroup > 1 ) {
                        # Build filename string to include group number...
			call imgcluster( Memc[in_file], Memc[tmp], SZ_LINE )
			call sprintf(Memc[tmp],SZ_LINE,"%s[%d]")
				call pargstr(Memc[tmp])
				call pargi(group)
		    } else call strcpy(Memc[in_file], Memc[tmp], SZ_LINE)

		# Open image here...
                    iferr (imexp = immap (Memc[tmp], READ_ONLY, 0)) {
                    	call eprintf( "ERROR: unable to open input image %s.\n...continuing\n" )
                    	call pargstr (Memc[tmp])
                    	next
                    }

                if( plot )
                    call greactivate( gp, 0 )

                    # read pkttime from header
                    iferr( call imgstr( imexp, Memc[timekey], Memc[pkttime],
                                        SZ_LINE ) ) {
                        call eprintf( "ERROR: cannot get time from image %s.\n...continuing.\n" )
                        call pargstr( Memc[in_file] )
                        next
                    }
       	          
		    # get mjd corresponding to pkttime in shh
     	            call getmjd (Memc[pkttime], mjd)

                    # get longitude and latitude at mjd
                    call getpos (orb, mjd, lng, lat)
                    
                    # plot marker
                    if( plot ) {
                        call mwcart (lng, lat, x, y)
                        call gmark (gp, x, y, marker_type, szmarker, szmarker)
                    }
                    
                    # Write table.
                    if( ngroup > 1 )
                        call write_table( Memc[in_file], group, mjd, lng, lat )
                    else
                        call write_table( Memc[in_file], 0, mjd, lng, lat )
                    
                }

		call imunmap(imexp)
	      #Finished with FITS image
	      }
                
                # Close the image.
                call imunmap (imin)
                
            } else {
                call eprintf( "ERROR: cannot open input image %s.\nPlease check to make sure image extensions are valid... continuing.\n" )
                call pargstr( Memc[in_file] )
            }
            
	   #call eprintf("Making another loop...\n")
        }
        
        call mfree (orb, TY_DOUBLE)
        if( plot )
            call gclose (gp)
        call close_table
        call sfree (sp)
        
end
