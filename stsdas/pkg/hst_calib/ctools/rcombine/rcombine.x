include <imhdr.h>
include "rcombine.h"

# RCOMBINE -- Combine spectral groups from rapid readout mode.  Propagate the
#             errors and the data quality information correctly, if requested.
#
# 15-June-1993 Modified by HA Bushouse to accept any image type (not just
# .c1h) as input.  Propagation of error (.c2) and data quality (.cq) images
# was also made optional instead of automatic.  If the input image is NOT
# type c1, then the setting of the errors parameter is ignored (i.e. the
# .c2 files are not propagated).
#
# 05-Nov-1993 HA Bushouse: Modified rcombine.h to increase value of
# MAXRANGES from 1 to 50 to allow specification of list of random groups
# such as "1,3,6,11".
#
# 21-Nov-1994 HA Bushouse: Fixed bug in calculating "denom" when user has
# specified a value for "nbin". Also added "imflush" calls to fix buffering
# problem that was leading to incorrect values in output image.
#
# 14-Jan-1998 MD De La Pena: Removed call to iki_parse; replaced with calls
# to imgcluster and fextn in order to obtain the image extension.
#

procedure rcombine( )

pointer sp, input, output, op, groups, list, im, image, root, section, extn
pointer outroot, osection, gstr, tmp, outc1h, outc2h, outcqh, im1out
pointer im2out, imqout, inc1h, inc2h, incqh, im1in, im2in, imqin, ogstr
pointer outfile, indir, hstr
pointer imtopen(), imtgetim(), immap(), imgl1r(), impl1r()

int     nbin, range[3,MAXRANGES+2], nval, ngroup, rndup, ngroup_out, icount
int     igroup, ogroup, mxgrp, mode, template, remainder, istat, rtlen, ibin
int     clgeti(), decode_ranges(), imaccf(), imgeti()
int     get_next_number(), imtlen(), isdirectory(), fnldir()
int     fnextn(), nextn

real    denom

bool    first, errors, dataq
bool    imgetb(), streq(), clgetb()

# error function to handle sqrt of invalid argument
real    errfcn() # Error function to deal with sqrt of invalid argument
extern  errfcn

string  nulstr ""
string  history "File created from %s using operation = %s, groups = \
%s, nbin = %d."

begin

   # Allocate temporary memory
   call smark( sp )
   call salloc( input, SZ_PATHNAME, TY_CHAR )
   call salloc( output, SZ_PATHNAME, TY_CHAR )
   call salloc( outfile, SZ_PATHNAME, TY_CHAR )
   call salloc( indir, SZ_PATHNAME, TY_CHAR )
   call salloc( op, SZ_FNAME, TY_CHAR )
   call salloc( groups, SZ_FNAME, TY_CHAR )
   call salloc( image, SZ_PATHNAME, TY_CHAR )
   call salloc( root, SZ_FNAME, TY_CHAR )
   call salloc( extn, SZ_FNAME, TY_CHAR)
   call salloc( section, SZ_FNAME, TY_CHAR )
   call salloc( outroot, SZ_FNAME, TY_CHAR )
   call salloc( osection, SZ_FNAME, TY_CHAR )
   call salloc( gstr, SZ_GSTR, TY_CHAR )
   call salloc( ogstr, SZ_GSTR, TY_CHAR )
   call salloc( tmp, SZ_MAX_SPEC, TY_REAL )
   call salloc( inc1h, SZ_PATHNAME, TY_CHAR )
   call salloc( inc2h, SZ_PATHNAME, TY_CHAR )
   call salloc( incqh, SZ_PATHNAME, TY_CHAR )
   call salloc( outc1h, SZ_PATHNAME, TY_CHAR )
   call salloc( outc2h, SZ_PATHNAME, TY_CHAR )
   call salloc( outcqh, SZ_PATHNAME, TY_CHAR )
   call salloc( hstr, SZ_LINE, TY_CHAR )

   # Initialize imhdr pointer to NULL
   im = NULL
   im1out = NULL
   im2out = NULL
   imqout = NULL
   im1in = NULL
   im2in = NULL
   imqin = NULL
   

   # Get input parameters
   call clgstr( "input", Memc[input], SZ_PATHNAME )
   call clgstr( "output", Memc[outfile], SZ_PATHNAME )
   call clgstr( "operation", Memc[op], SZ_FNAME )
   call clgstr( "groups", Memc[groups], SZ_FNAME )
   ibin = clgeti( "nbin" )
   dataq  = clgetb( "dataqual")

   # Open input template
   list = imtopen( Memc[input] )

   # If more than one input file was specified, check that output file is
   # a directory
   if ( imtlen( list ) > 1 )
      if ( isdirectory( Memc[outfile], Memc[output], SZ_PATHNAME) == 0 )
	 call error( 1, "Output file name must be a directory if input template is used")
   
   # Loop over file names specified in template
   while( imtgetim( list, Memc[image], SZ_PATHNAME) != EOF ) {

   # Parse the input image name and determine if this is a .c1 image
   call imgcluster (Memc[image], Memc[root], SZ_FNAME)
   nextn = fnextn  (Memc[root], Memc[extn], SZ_FNAME)
   if (streq (Memc[extn], "c1h")) errors = clgetb( "errors" )
       else errors = false
   call mkfname( Memc[image], "", "", Memc[extn], Memc[image], SZ_PATHNAME)

      # Decode the range specification in the groups parameter
      # decode_ranges requires max_ranges to be at least 2+number of
      # clusters (groups)
      if ( decode_ranges( Memc[groups], range, MAXRANGES+2, nval ) == ERR )
	 call error ( 1, "Invalid range of groups specified" )

      # Get group information from image
      im = immap( Memc[image], READ_ONLY, 0 )
      if (imaccf( im, "GROUPS") == NO )
         call error (1, "Image not in group format")
      if (!imgetb( im, "GROUPS"))
         call error (1, "Image not in group format")
      if (imaccf(im, "GCOUNT") == NO )
         call error (1, "Image not in group format")
      ngroup = imgeti( im, "GCOUNT" )

      # If nbin was set to INDEF this means combine all groups into one.
      # Therefore the number of groups to bin into one output group should
      # be the total number of input groups that are to be processes (nval)
      # unless nval = INDEFI which happens when all groups are to be
      # processed.  In the latter case nbin should be set to the number of
      # input groups = ngroup.
      if (nval >= MAX_VAL)
	 nval = (ngroup - range[1,1])/range[3,1] + 1
      if (IS_INDEFI (ibin))
	 nbin = nval
      else
	 nbin = ibin

      # Get the root and image section for current image
      call imgcluster( Memc[image], Memc[root], SZ_FNAME )
      call imgsection( Memc[image], Memc[section], SZ_FNAME )

      # If output file name is a directory, append the input file name
      # to the output, otherwise just use output file name
      if ( isdirectory( Memc[outfile], Memc[output], SZ_PATHNAME) > 0 ){
	 rtlen = fnldir( Memc[root], Memc[indir], SZ_PATHNAME )
	 call strcat( Memc[root+rtlen], Memc[output], SZ_PATHNAME )
      } else {
	 call strcpy( Memc[outfile], Memc[output], SZ_PATHNAME )
      }

      # If nbin does not divide evenly into nval, then we need one more
      # output group than is given by nval/nbin (integer division)
      rndup = 0
      if ( mod( nval, nbin ) > 0 )
	 rndup = 1
      ngroup_out = nval / nbin + rndup

      # Get the output root name
      call imgcluster( Memc[output], Memc[outroot], SZ_FNAME )

      # Loop over input groups, count groups to get output correct.
      # Set first to true so output images get opened the first time through
      icount = 0
      ogroup = 0
      igroup = 0
      first = true
      repeat {
         istat = get_next_number( range, igroup )
	 call mkgstr( igroup, 0, Memc[gstr], SZ_GSTR )
	 icount = icount + 1

	 # Open current input images unless we have exceeded the maximum
	 # number of groups to be processed
	 if ( (igroup <= ngroup) && (icount <= nval) ) {
	    call mkfname ( Memc[root], Memc[gstr], Memc[section], Memc[extn],
			   Memc[inc1h], SZ_PATHNAME )
	    im1in = immap( Memc[inc1h], READ_ONLY, 0 )

	    if (errors) {
		call mkfname ( Memc[root], Memc[gstr], Memc[section], "c2h",
			       Memc[inc2h], SZ_PATHNAME )
		im2in = immap( Memc[inc2h], READ_ONLY, 0 )
	    }
	    if (dataq) {
		call mkfname ( Memc[root], Memc[gstr], Memc[section], "cqh",
			       Memc[incqh], SZ_PATHNAME )
		imqin = immap( Memc[incqh], READ_ONLY, 0 )
	    }
	 }
	 
	 # Open new output images (i.e. the next group) and zero the output
	 # arrays if we have processed nbin input groups, or if it is the
	 # first time through or if we have processed all groups.
	 remainder = mod(icount-1, nbin)

	 if ( remainder == 0 || first || (igroup > ngroup)) {

	    # If this is not the first pass through then we have to
	    # do some book keeping, and then close the output images
	    # before opening new ones
	    if (!first){

	       if (errors) {
		   call asqrr( Memr[imgl1r(im2out)], Memr[impl1r(im2out)],
			       IM_LEN(im2out,1), errfcn )
		   call imflush( im2out )
	       }

	       if ( streq("average",Memc[op] ) ){
		  # When averaging, divide by icount if icount groups were
		  # processed, else divide by the number of groups left
		  if ( remainder == 0 )
		     #denom = icount - 1 # Bug fix 21-Nov-94 H.A.B.
		     denom = nbin
		  else
		     denom = remainder
		  
		  call adivkr( Memr[imgl1r(im1out)], denom,
			       Memr[impl1r(im1out)], IM_LEN(im1out,1))
		  call imflush( im1out )

		  if (errors) {
		      call adivkr( Memr[imgl1r(im2out)], denom,
				   Memr[impl1r(im2out)], IM_LEN(im2out,1))
		      call imflush( im2out )
		  }
	       }
		  
	       call imfree( im1out )
	       if (errors) call imfree( im2out )
	       if (dataq)  call imfree( imqout )
	    }

	    # If icount (current output group) is greater than 
	    # nval then we have processed all of the groups and we
	    # should clean up memory and exit. 
	    if ( icount > nval ){
	       call imfree( im )
	       break
	    }

	    first = false

	    # Calculate the current output group (increment ogroup if nbin
	    # input groups have been processed), make file names and open
	    # images. If the current output group = 1, then we have to create
	    # the file name specifying the maximum number of groups
	    if ( remainder == 0 )
	       ogroup = ogroup + 1
	    if ( ogroup == 1 ){
	       mxgrp = ngroup_out
	       mode = NEW_COPY
	       template = im
	    } else {
	       mxgrp = 0
	       mode = READ_WRITE
	       template = 0
	    }

	    call mkgstr( ogroup, mxgrp, Memc[ogstr], SZ_GSTR )

	    call mkfname( Memc[outroot], Memc[ogstr], Memc[osection],Memc[extn],
			  Memc[outc1h], SZ_PATHNAME )
	    im1out = immap( Memc[outc1h], mode, template )

	    if (errors) {
		call mkfname( Memc[outroot], Memc[ogstr], Memc[osection], "c2h",
			      Memc[outc2h], SZ_PATHNAME )
		im2out = immap( Memc[outc2h], mode, template )
	    }
	    if (dataq) {
		call mkfname( Memc[outroot], Memc[ogstr], Memc[osection], "cqh",
			      Memc[outcqh], SZ_PATHNAME )
		imqout = immap( Memc[outcqh], mode, template )
	    }

	    # The c2h and cqh files were opened as copies of the c1h file.
	    # This means the values of the FILETYPE keyword must be changed
	    # for the c2h and cqh and the BUNIT keyword for the cqh.
	    # Also add History lines to the c1h, c2h and cqh files.
	    # Only need to do all this on the first pass.
	    if (ogroup == 1) {
		call sprintf( Memc[hstr], SZ_LINE, history )
		call pargstr( Memc[input] )
		call pargstr( Memc[op])
		call pargstr( Memc[groups])
		call pargi( nbin )
		call imputh( im1out, "HISTORY", Memc[hstr])
		if (errors) {
		    call impstr( im2out, "FILETYPE", "ERR" )
		    call imputh( im2out, "HISTORY", Memc[hstr])
		}
		if (dataq) {
		    call impstr( imqout, "FILETYPE", "CDQ" )
		    call impstr( imqout, "BUNIT", "" )
		    call imputh( imqout, "HISTORY", Memc[hstr])
		}
	    }
	    
	    # Zero the arrays
	    call aclrr( Memr[impl1r(im1out)], IM_LEN(im1out,1) )
	    call imflush( im1out )
	    if (errors) {
		call aclrr( Memr[impl1r(im2out)], IM_LEN(im2out,1) )
		call aclrr( Memr[tmp], IM_LEN(im2out,1) )
		call imflush( im2out )
	    }
	    if (dataq) {
		call aclrr( Memr[impl1r(imqout)], IM_LEN(imqout,1) )
		call imflush( imqout )
	    }
	    
	 }

	 call aaddr( Memr[imgl1r(im1in)], Memr[imgl1r(im1out)],
		     Memr[impl1r(im1out)], IM_LEN(im1in,1) )
	 call imflush( im1out )

	 if (errors) {
	     call amulr( Memr[imgl1r(im2in)], Memr[imgl1r(im2in)],
			 Memr[tmp], IM_LEN(im2in,1) )
	     call aaddr( Memr[tmp], Memr[imgl1r(im2out)],
			 Memr[impl1r(im2out)], IM_LEN(im2in,1) )
	     call imflush( im2out )
	 }
	 if (dataq) {
	     call amaxr( Memr[imgl1r(imqin)], Memr[imgl1r(imqout)],
			 Memr[impl1r(imqout)], IM_LEN(imqin,1) )
	     call imflush( imqout )
	 }
	 
	 call imfree( im1in )
	 if (errors) call imfree( im2in )
	 if (dataq)  call imfree( imqin )
      }
      
   }
   call imfree( im1out )
   if (errors) call imfree( im2out )
   if (dataq)  call imfree( imqout )
   call imfree( im )
   call imtclose( list )
   call sfree( sp )

end


# ERRFCN -- Error function to deal with sqrt of invalid argument

real procedure errfcn( x )

real x

begin
   return INDEFR
end
