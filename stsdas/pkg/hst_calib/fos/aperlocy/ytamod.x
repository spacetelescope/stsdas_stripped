include	<imhdr.h>
include <tbset.h>
include	"colnames.h"

procedure t_ytamod()
 
#  Module name: YTAMOD
#
#  Keyphrase:
#  ----------
#       FOS aperture location and sizes
#  Description:
#  ------------
#       This routine computes the aperture locations and sizes
#       in an FOS map.
#
#  FORTRAN name: YTAMOD.for
#
#  Keywords of accessed files and tables:
#  --------------------------------------
#  Name                         I/O     Description / Comments
#       input                   I       FOS Y-map (file template)
#       table                   O       output table of edge locations
#                                       and sizes.  Columns in the table
#                                       are:
#                                               aper_id - aperture name
#                                               aper_pos - upper, lower, center
#                                               left_edge
#                                               right_edge
#                                               upper_edge
#                                               lower_edge
#                                               x_center
#                                               y_center
#                                               x_centroid
#                                               y_centroid
#                                               x_crosscor
#                                               y_crosscor
#                                               area
#                                               flux
#       ybases                  O       vector of y-positions observed
#       profile                 O       Vector of y-profile
#
#  CL parameters
#  -------------
#       tabstat         output table status (write or append)
#       aperpos         specification of which aperture of pair is present
#                       both, upper, or lower
#       ymiddle         y-position of midpoint between apertures
#       yfirst          starting y-position to use
#       ylast           last y-position to use
#       d1              first diode to use
#       d2              last diode to use
#       twidthx         cross correlation template width in in sample units
#                               for the x-directions
#       twidthy         cross correlation template width for the y-direction
#
#  Subroutines Called:
#  -------------------
#  CDBS:
#       ygptrn, ygmode, tabinv, tainfo
#  SDAS:
#
#  Others:
#
#
#  History:
#  --------
#  Version      Date        Author          Description
#       1       Sept 87   D. Lindler    Designed and coded
#	2	AUG 88	  D. Lindler	Added time to output table
#					Corrected upper/lower reversal
#					changed to use YPOS group param.
#	3	April 94  H. Bushouse	Converted Fortran to SPP; added
#					capability to read 2-d images; added
#					automatic computation of twidx & twidy
#					if set to INDEF on input.
#	3.1	Oct 94    H. Bushouse	Added choice of output units via new
#					"pixels" parameter.
#-------------------------------------------------------------------------------
#
#  HEADER KEYWORD VALUES
#
%	character*5	detf
%	character*3	fgwaf
%	character*3	aperf
%	character*6	aprpf
%	character*1	polrf
char	det_spp[5]
char	fgwa_spp[3]
char	aper_spp[3]
char	aperps[6]
char	time[24]
int	passdr
int	xsteps,ovrscn,fchnl,nchnl,ybase,yspace
#
# FILE I/O PARAMETERS
#
pointer	idin, idout
int	naxis, nrows, npix, nlines, ngrps
#
# OUTPUT TABLE column pointers
#
pointer	cid[15]
#
# CL PARAMETERS
#
bool	pix
int	twidx,twidy
char	input[SZ_FNAME],output[SZ_FNAME]
char	tbstat[SZ_LINE]
double	yfirst,ylast,ymid,d1,d2
#
# INPUT AREA TO PROCESS
#
int	area[4]
#
# COMPUTED RESULTS
#
double	edges[4],ecent[2],ctroid[2],ccent[2],flux
#
# OTHERS
#
int		istat,i,iaper,naper,s1,s2,l1,l2,l3
bool		paired
double		a,rpos,lookup
double	clgetd()
int	clgeti(), immap(), imgeti(), imgl1d(), imgl2d(), tbtopn(), tbpsta()
bool	streq(), clgetb()
real	datamin, datamax
#
# IMAGE DEFINITION
#
pointer	image, ypos
#
#  DATA DECLARATIONS
#
data area/4*1/
#
#---------------------------------------------------------------------
begin

# GET INPUT CL PARAMETERS
 
	call clgstr("input",input, SZ_FNAME)
        call clgstr("table",output,SZ_FNAME) 
        call clgstr("tabstat",tbstat,SZ_LINE) 
        yfirst = clgetd("yfirst")
	ymid   = clgetd("ymiddle")
	ylast  = clgetd("ylast")
	d1 = clgetd("d1")
	d2 = clgetd("d2")
	call clgstr("aperpos",aperps,6)
	twidx = clgeti("twidthx")
	twidy = clgeti("twidthy")
	pix = clgetb("pixels")
 
#	Open input image
 
	idin = immap(input, READ_ONLY, 0)
	naxis = IM_NDIM(idin)
	ngrps = imgeti( idin, "GCOUNT" )
	npix  = IM_LEN(idin, 1)

	if (naxis == 1) {
	    if (ngrps == 1)
		call error(1, "Input image is not two-dimensional")
	    nlines = ngrps
	} else if (naxis == 2) {
	    if (ngrps > 1)
		call error(1, "Input image is not two-dimensional")
	    nlines = IM_LEN(idin, 2)
	} else {
	    call error(1, "Input image is not two-dimensional")
	}

	call malloc( image, npix*nlines, TY_DOUBLE )
	call malloc( ypos, nlines, TY_DOUBLE)

#	Get header keywords

        call ygmode(idin,detf,fgwaf,aperf,aprpf,polrf,passdr,istat)
        if (istat != 0)
	    call error(1,"ERROR reading input image keywords")
	call f77upk(detf, det_spp, 5)
	call f77upk(fgwaf, fgwa_spp, 3)
	call f77upk(aperf, aper_spp, 3)

        call yxptrn(idin,fchnl,nchnl,xsteps,ovrscn,ybase,yspace,istat)
        if (istat != 0)
	    call error(1,"ERROR reading input image keywords")

	if (twidx == INDEFI) twidx = xsteps+1
	if (twidy == INDEFI) twidy = nint( 256./yspace )

	if (pix) {
	    fchnl  = 1
	    xsteps = 1
	    ybase  = 1
	    yspace = 1
	}

# GET OBSERVATION TIME

	call imgstr(idin,"FPKTTIME",time,24)
 
#	Read image data

	if (naxis == 1) {
	    do i = 1, nlines {
	       call gf_opengr(idin, i, datamin, datamax, 0)
	       call amovd( Memd[imgl1d(idin)], Memd[image+(i-1)*npix], npix)
	       Memd[ypos+i-1] = ybase + (i-1)*yspace
	    }
	} else {
	    do i = 1, nlines {
	       call amovd( Memd[imgl2d(idin,i)], Memd[image+(i-1)*npix], npix)
	       Memd[ypos+i-1] = ybase + (i-1)*yspace
	    }
	}
	
	call imunmap (idin)

#--------------------------------------------------------------------
# SET UP TO DO USEFUL THINGS
#
        naper=1
        paired=false
        if (streq(aper_spp,pair_1)) paired=true
        if (streq(aper_spp,pair_2)) paired=true
        if (streq(aper_spp,pair_3)) paired=true
        if (streq(aper_spp,pair_4)) paired=true

        if (.not.paired) call strcpy("single", aperps, 6)
        if (paired && streq(aperps,"both")) {
                call strcpy("lower", aperps, 6)
                naper=2
        }

# DEFAULT AREA
 
        if (yfirst==0.0) yfirst = Memd[ypos]
        if (ylast==0.0)  ylast  = Memd[ypos+nlines-1]
        if (ymid==0.0)   ymid   = (yfirst+ylast)/2.0
 
# CONVERT AREA TO IMAGE LINE AND SAMPLE COORDINATES
 
        call tabinv(Memd[ypos],nlines,yfirst,rpos)
        l1=rpos
        call tabinv(Memd[ypos],nlines,ymid,rpos)
        l2=rpos
        call tabinv(Memd[ypos],nlines,ylast,rpos)
        l3=rpos
        s1=(d1-fchnl)*xsteps+1
        s2=(d2-fchnl+1)*xsteps-1
 
# MAKE SURE THEY'RE IN THE IMAGE
 
        if (s1 < 1)    s1=1
        if (s2 > npix) s2=npix
        if (l1 < 1)    l1=1
        if (l3 > nlines) l3=nlines
        if ( (s1 >= s2) || (l1 >= l3) || 
             ((naper==2) && ((l2<=l1) || (l2>=l3)) ) ) {  
		call error(1, "Invalid image region specified")
        }         

# CHECK FOR VALID TEMPLATE WIDTHS
 
        if ( (twidx >= (s2-s1-3)) || 
             ((naper == 1) && (twidy >= (l3-l1-3))) || 
             ((naper == 2) && (twidy >= (l2-l1-3))) || 
             ((naper == 2) && (twidy >= (l3-l2-3)))) {   
		call error(1, "twidx or twidy too big for image size")
	}

#-----------------------------------------------------------------------
#
# CREATE TABLE IF TBSTAT NE 'APPEND'
#
        if (.not.streq(tbstat,"append")) {
	    if (pix) {
		call strcpy ("pixels", cunt3, 6)
		call strcpy ("pixels", cunt4, 6)
		call strcpy ("pixels", cunt5, 6)
		call strcpy ("pixels", cunt6, 6)
		call strcpy ("pixels", cunt7, 6)
		call strcpy ("pixels", cunt8, 6)
		call strcpy ("pixels", cunt9, 6)
		call strcpy ("pixels", cunt10, 6)
		call strcpy ("pixels", cunt11, 6)
		call strcpy ("pixels", cunt12, 6)
	    } else {
		call strcpy ("diodes", cunt3, 6)
		call strcpy ("diodes", cunt4, 6)
		call strcpy ("ybases", cunt5, 6)
		call strcpy ("ybases", cunt6, 6)
		call strcpy ("diodes", cunt7, 6)
		call strcpy ("ybases", cunt8, 6)
		call strcpy ("diodes", cunt9, 6)
		call strcpy ("ybases", cunt10, 6)
		call strcpy ("diodes", cunt11, 6)
		call strcpy ("ybases", cunt12, 6)
	    }
	    idout = tbtopn( output, NEW_FILE, NULL )
	    call tbpset( idout, TBL_ALLROWS, 4 )
	    call tbpset( idout, TBL_ROWLEN,  15 )
	    call tbpset( idout, TBL_MAXCOLS, 15 )
	    call tbcdef( idout,cid[1],cnam1,cunt1,cfrm1,ctyp1,1,1)
	    call tbcdef( idout,cid[2],cnam2,cunt2,cfrm2,ctyp2,1,1)
	    call tbcdef( idout,cid[3],cnam3,cunt3,cfrm3,ctyp3,1,1)
	    call tbcdef( idout,cid[4],cnam4,cunt4,cfrm4,ctyp4,1,1)
	    call tbcdef( idout,cid[5],cnam5,cunt5,cfrm5,ctyp5,1,1)
	    call tbcdef( idout,cid[6],cnam6,cunt6,cfrm6,ctyp6,1,1)
	    call tbcdef( idout,cid[7],cnam7,cunt7,cfrm7,ctyp7,1,1)
	    call tbcdef( idout,cid[8],cnam8,cunt8,cfrm8,ctyp8,1,1)
	    call tbcdef( idout,cid[9],cnam9,cunt9,cfrm9,ctyp9,1,1)
	    call tbcdef( idout,cid[10],cnam10,cunt10,cfrm10,ctyp10,1,1)
	    call tbcdef( idout,cid[11],cnam11,cunt11,cfrm11,ctyp11,1,1)
	    call tbcdef( idout,cid[12],cnam12,cunt12,cfrm12,ctyp12,1,1)
	    call tbcdef( idout,cid[13],cnam13,cunt13,cfrm13,ctyp13,1,1)
	    call tbcdef( idout,cid[14],cnam14,cunt14,cfrm14,ctyp14,1,1)
	    call tbcdef( idout,cid[15],cnam15,cunt15,cfrm15,ctyp15,1,1)
	    call tbtcre (idout)
            nrows=0
        } else {
 
# OPEN EXISTING TABLE
 
	    idout = tbtopn( output, READ_WRITE, NULL )
	    call tbcfnd( idout, cnam1,  cid[1],  1 )
	    call tbcfnd( idout, cnam2,  cid[2],  1 )
	    call tbcfnd( idout, cnam3,  cid[3],  1 )
	    call tbcfnd( idout, cnam4,  cid[4],  1 )
	    call tbcfnd( idout, cnam5,  cid[5],  1 )
	    call tbcfnd( idout, cnam6,  cid[6],  1 )
	    call tbcfnd( idout, cnam7,  cid[7],  1 )
	    call tbcfnd( idout, cnam8,  cid[8],  1 )
	    call tbcfnd( idout, cnam9,  cid[9],  1 )
	    call tbcfnd( idout, cnam10, cid[10], 1 )
	    call tbcfnd( idout, cnam11, cid[11], 1 )
	    call tbcfnd( idout, cnam12, cid[12], 1 )
	    call tbcfnd( idout, cnam13, cid[13], 1 )
	    call tbcfnd( idout, cnam14, cid[14], 1 )
	    call tbcfnd( idout, cnam15, cid[15], 1 )
	    nrows = tbpsta( idout, TBL_NROWS )
        }
 
# PROCESS EACH APERATURE
 
        do iaper = 1, naper {
 
# INSERT REGION FOR APERTURE IN AREA
 
           area[1]=s1
           area[2]=s2
           area[3]=l1
           area[4]=l3
           if ((naper == 2) && (iaper == 1)) area[4]=l2
           if (iaper == 2) {   
               area[3]=l2
               call strcpy ("upper", aperps, 6)
           }     
	
           call tainfo(Memd[image],npix,nlines,twidx,twidy,area,
                       edges,ctroid,ecent,ccent,flux,istat)
           if (istat != 0) 
	       call error(1, "ERROR processing image")
 
# WRITE INFO TO THE OUTPUT TABLES
 
           nrows = nrows + 1
	   call tbrptt( idout, cid[15], time,    24, 1, nrows )
	   call tbrptt( idout, cid[1],  aper_spp, 6, 1, nrows )
	   call tbrptt( idout, cid[2],  aperps,   6, 1, nrows )
           if (edges[1] != 0.0) 
	       call tbrptd(idout,cid[3],fchnl+(edges[1]-1)/xsteps,1,nrows)
           if (edges[2] != 0.0) 
	       call tbrptd(idout,cid[4],fchnl+(edges[2]-1)/xsteps,1,nrows)
           if (edges[4] != 0.0) 
	       call tbrptd(idout,cid[5],lookup(Memd[ypos],nlines,edges[4]),
			   1,nrows)
           if (edges[3] != 0.0) 
	       call tbrptd(idout,cid[6],lookup(Memd[ypos],nlines,edges[3]),
			   1,nrows)
           if (ecent[1] != 0.0) 
	       call tbrptd(idout,cid[7],fchnl+(ecent[1]-1)/xsteps,1,nrows)
           if (ecent[2] != 0.0) 
	       call tbrptd(idout,cid[8],lookup(Memd[ypos],nlines,ecent[2]),
			   1,nrows)
           if (ctroid[1] != 0.0) 
	       call tbrptd(idout,cid[9],fchnl+(ctroid[1]-1)/xsteps,1,nrows)
           if (ctroid[2] != 0.0) 
	       call tbrptd(idout,cid[10],lookup(Memd[ypos],nlines,ctroid[2]),
			   1,nrows)
           if (ccent[1] != 0.0) 
	       call tbrptd(idout,cid[11],fchnl+(ccent[1]-1)/xsteps,1,nrows)
           if (ccent[2] != 0.0) 
	       call tbrptd(idout,cid[12],lookup(Memd[ypos],nlines,ccent[2]),
			   1,nrows)
           if ( (edges[1] != 0.0) && (edges[2] != 0.0) &&
                (edges[3] != 0.0) && (edges[4] != 0.0) ) {   
                      a=(edges[2]-edges[1])/xsteps
                      a=a*(lookup(Memd(ypos),nlines,edges[4])-
                           lookup(Memd(ypos),nlines,edges[3]))
                      call tbrptd(idout,cid[13],a,1,nrows)
           }
           call tbrptd(idout,cid[14],flux,1,nrows)
	   call tbhadt(idout,"detector",det_spp)
	   call tbhadt(idout,"fgwa_id",fgwa_spp)
	}

	call tbtclo( idout )
 
# DONE
	call mfree( image, TY_DOUBLE )
	call mfree( ypos,  TY_DOUBLE )
 
        end
