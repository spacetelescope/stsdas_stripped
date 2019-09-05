define 	ER_OOPS		501	# some error during processing

include <imio.h>
include <iraf77.h>

# GTROOT -- Parse image name and return root.
#
# S. Hulbert,  30-Dec-1991  

procedure gtroot (f77in, f77rt, istat)

%	character*(*)  f77in   
%	character*(*)  f77rt   
int     istat			# error code return   

char	image[SZ_PATHNAME], cluster[SZ_PATHNAME]
char	extn[SZ_FNAME], root[SZ_PATHNAME]
int	iclust, iextn, iroot

int	strlen(), fnextn()

begin

	istat = ER_OK

	iferr {
            # Convert input image name to SPP string
            call f77upk (f77in, image, SZ_PATHNAME)

	    # parse image name to get cluster
	    call imgcluster (image, cluster, SZ_PATHNAME)

	    # get length of cluster
	    iclust = strlen(cluster)

	    # look for extension in cluster
	    iextn = fnextn(cluster, extn, SZ_FNAME)

	    # compute length of root
	    if (iextn == NULL) 
	        iroot = iclust
	    else
	        iroot = iclust - iextn - 1

	    # copy everything except the extension to output 'root'
	    call strcpy (cluster, root, iroot)	

	    # convert output root name to fortran string
 	    call f77pak (root, f77rt, SZ_FNAME)

	} then {
	    istat = ER_OOPS
	}

	return

end
