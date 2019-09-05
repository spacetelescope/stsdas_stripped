define  HDR             "HDR$"          # stands for header directory
define  STRLEN_HDR      4

# GPIXFNAME -- Convert a logical pixfile name into a physical pathname.
# Dec 1990 Nelson Zarate. From oif_gpixfname. It will not produce
# VMS dependent output; rather UNIX type.
# 
procedure gpixfname (pixfile, hdrfile, path, maxch)

char	pixfile[ARB]		# pixfile name
char	hdrfile[ARB]		# header file name (gives hdr directory)
char	path[maxch]		# receives pathname
int	maxch

int	nchars
pointer	sp, fname
int	strncmp(), fnldir()

begin
	# Merely return pathname if not case "HDR$".
	if (strncmp (pixfile, HDR, STRLEN_HDR) != 0) {
	    call fpathname (pixfile, path, maxch)
	    return
	}

	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Get host pathname of pixel file directory.
	nchars = fnldir (hdrfile, Memc[fname], SZ_PATHNAME)
	call fpathname (Memc[fname], path, maxch)
	# Fold in any subdirectories from the pixfile name.
	# (as in HDR$pixels/).

	call strcat (pixfile[5], path, maxch)

	call sfree (sp)
end
