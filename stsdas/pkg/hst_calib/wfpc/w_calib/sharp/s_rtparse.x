include	<ctype.h>

# rtparse -- Parse an image name into the cluster name, cluster index,
# cluster size
#
#	Syntax:		cluster[cl_index/cl_size]
#
# where all fields are optional except the cluster name.  In the limiting case
# (cl_size = 1) the cluster name and image name are the same.  CL_INDEX and
# CL_SIZE must be simple positive decimal integer constants, if given.  The
# [ character must be escaped to be included in the filename of the cluster.
#

procedure rtparse (root, cluster, sz_cluster, cl_index, cl_size)

char	root[ARB]		# full image specification
char	cluster[ARB]		# receives cluster name
int	sz_cluster		# max chars in cluster name
int	cl_index		# receives cluster index (default 0)
int	cl_size			# receives cluster size (default 0)

int	ip, op, lbrack, ch, n
errchk	syserrs

begin
	ip = 1
	op = 1

	# Extract cluster name.  The first (unescaped) [ marks the start of
	# either the cl_index subscript or a section field.

	for (ch=root[ip];  ch != EOS && ch != '[';  ch=root[ip]) {
	    if (ch == '\\' && root[ip+1] == '[') {
		cluster[op] = '\\'
		op = op + 1
		cluster[op] = '['
		ip = ip + 1
	    } else
		cluster[op] = ch

	    op = min (sz_cluster, op + 1)
	    ip = ip + 1
	}

	cluster[op] = EOS
	lbrack      = ip
	cl_index    = 0
	cl_size     = 0

	if (ch == EOS) {
	    return
	}

	# If we have a [...] field, it is a cl_index subscript.  
 	# A cl_index subscript is anything with the syntax [ddd] 
	# or [ddd/ddd]

	ip = ip + 1
	n = 0

	for (ch=root[ip];  ch != EOS;  ch=root[ip]) {
	    if (IS_DIGIT(ch)) {
		n = (n * 10) + TO_INTEG(ch)
	    } else if (ch == '/') {
		cl_index = max (n, 1)
		n = 0
	    } else if (ch == ']') {
		ip = ip + 1
		break
	    } else {
		# Not a cl_index subscript; must be a section.
		ip = lbrack
		n  = 0
		break
	    }
	    ip = ip + 1
	}

	if (cl_index == 0)
	    cl_index = n
	else
	    cl_size = n
end
