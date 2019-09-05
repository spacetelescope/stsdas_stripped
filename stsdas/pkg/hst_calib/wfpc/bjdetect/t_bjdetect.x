include <imhdr.h>

procedure t_bjdetect()

char	fname[SZ_FNAME]
real	thresh
int	nbins
int	ncomp
pointer	tpin, imdes
int	x1, x2, dim_y
int	nchar
int     grp, cl_index, cl_size
int	ngrp
real	xmin, xmax
int	n
char	outfmt[SZ_LINE]
char    cluster[SZ_PATHNAME]
char    sect[SZ_FNAME]
char    ksect[SZ_FNAME]

int	clgeti()
real	clgetr()
pointer	imtopenp()
pointer	gf_map()
int	imtlen()
int	imtgetim()
int	gf_gcount()

begin
	tpin = imtopenp ("input") 
	thresh = clgetr ("thresh")
	nbins = clgeti ("nbins")
	ncomp = clgeti ("ncomp")
	x1 = clgeti ("x1")
	x2 = clgeti ("x2")
	call clgstr ("out_format", outfmt, SZ_LINE)

	# loop all images
        do n = 1, imtlen(tpin) {

            # read the next input image name in the template list
            nchar = imtgetim (tpin, fname, SZ_FNAME)

            # find out the group spec
            call imparse (fname, cluster, SZ_PATHNAME, ksect, SZ_FNAME,
                                sect, SZ_FNAME, cl_index, cl_size)

	    # open image
	    imdes = gf_map (fname, READ_ONLY, 0)
            dim_y = IM_LEN (imdes, 2)

            # if no group specification, do all groups/extensions.
            if (cl_index <= 0) {
                iferr(ngrp = gf_gcount(imdes)) ngrp = 1
            } else ngrp = 1

	    do grp = 1, ngrp {
		if (grp > 1)
	            call gf_opengr (imdes, grp, xmin, xmax, 0)
	        call bjdet_do (imdes, NULL, thresh, nbins, ncomp, x1, x2, dim_y,
				fname, grp, ngrp, outfmt)
	    }

	    # close image
	    call gf_unmap (imdes)
	}
end
