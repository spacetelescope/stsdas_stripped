include	"zt.h"

# Memory management
define	Sx			Memc[sx]

#---------------------------------------------------------------------------
.help zt.x 3Apr95 source
.ih
NAME
zt.x -- Routines to manipulate the ZT Keys object.
.endhelp
#---------------------------------------------------------------------------
pointer procedure zt_k_alloc (list)

char	list[ARB]		# I:  List of keywords to accumulate.

# Declarations.
pointer	k			# Keys object.
int	strlen()		# Length of string.
int	word_count()		# Number of words in string.

string	ghrs_keys "zriuta,zriutb,zdett1,zdett2,zdebtf,zdebtr,zpabt1,zpabt2,zmebt1,zmebt2,zfiat,zfibt,zfict,zcst,zsct1,zsct2,zhvpst1,zhvpst2,zdt11,zdt12,zdrt,zobbt"

errchk	calloc, malloc

begin
	call malloc (k, ZT_K_SZ, TY_STRUCT)

	# If list is empty, use GHRS default
	if (strlen (list) <= 0) {
	    ZT_K_KEY_SZ(k) = strlen (ghrs_keys)
	    call malloc (ZT_K_KEY_PTR(k), ZT_K_KEY_SZ(k), TY_CHAR)
	    call strcpy (ghrs_keys, ZT_K_KEY(k), ZT_K_KEY_SZ(k))
	    ZT_K_NKEY(k) = word_count (ZT_K_KEY(k))
	    
	# If the first character is "@", then read in the file
	# containing keywords.
	} else if (list[1] == '@')
	    call zt_k_rkeys (k, list[2])

	# Else, the string is the list of keys.
	else {
	    ZT_K_KEY_SZ(k) = strlen (list)
	    call malloc (ZT_K_KEY_PTR(k), ZT_K_KEY_SZ(k), TY_CHAR)
	    call strcpy (list, ZT_K_KEY(k), ZT_K_KEY_SZ(k))
	    ZT_K_NKEY(k) = word_count (ZT_K_KEY(k))
	}

	# Allocate accumulation arrays
	call calloc (ZT_K_A_PTR(k), ZT_K_NKEY(k), TY_DOUBLE)
	call calloc (ZT_K_NA_PTR(k), ZT_K_NKEY(k), TY_INT)

	# If no keys, abort.
	if (ZT_K_NKEY(k) <= 0) {
	    iferr (call zt_k_free (k))
		;
	    call error (1, "zt_k_alloc: no keywords provided")
	}

	# That's all folks.
	return (k)
end
#---------------------------------------------------------------------------
# End of zt_k_alloc
#---------------------------------------------------------------------------
procedure zt_k_free (k)

pointer	k			# IO: Keys object; NULL on return.

errchk	mfree

begin
	call mfree (ZT_K_NA_PTR(k), TY_INT)
	call mfree (ZT_K_A_PTR(k), TY_DOUBLE)
	call mfree (ZT_K_KEY_PTR(k), TY_CHAR)

	call mfree (k, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of zt_k_free
#---------------------------------------------------------------------------
procedure zt_k_rkeys (k, fname)

pointer	k			# I:  Keys object.
char	fname[ARB]		# I:  File to read from.

# Declarations
int	fd			# File descriptor.
bool	first			# TRUE if first word read.
int	fscan()			# Read from file.
int	open()			# Open a file.
pointer	sb			# String buffer.
pointer	sb_open()		# Create a string buffer.
pointer	sb_string()		# Retrieve buffer as a string.
pointer	sp			# Stack pointer.
int	strlen()		# Length of string.
pointer	sx			# Generic string.
int	word_count()		# Count words in list.

errchk	close, open
errchk	salloc, sfree, smark
errchk	sb_close, sb_open, sb_string

begin
	call smark (sp)
	call salloc (sx, SZ_LINE, TY_CHAR)
	
	sb = sb_open()
	
	# Open file.
	fd = open (fname, READ_ONLY, TEXT_FILE)

	# Read from file, appending each word to the string list.
	first = true
	while (fscan (fd) != EOF) {
	    call gargwrd (Sx, SZ_LINE)
	    if (first)
		first = false
	    else
		call sb_cat (sb, ",")
	    call sb_cat (sb, Sx)
	}

	# Set the string list to the string buffer.
	ZT_K_KEY_PTR(k) = sb_string (sb)
	ZT_K_KEY_SZ(k) = strlen (ZT_K_KEY_PTR(k))
	ZT_K_NKEY(k) = word_count (ZT_K_KEY(k))

	# That's all folks.
	call close (fd)
	call sb_close (sb)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of zt_k_rkeys
#---------------------------------------------------------------------------
procedure zt_k_accum (k, im)

pointer	k			# I:  Keys object.
pointer	im			# I:  Image descriptor.

# Declarations
int	cp			# Pointer in key array.
double	imgetd ()		# Get double-valued keyword header parameter.
int	nkey			# Key number being read.
pointer	sp			# Stack pointer.
pointer	sx			# Generic string.
double	v			# Value of keyword parameter.
int	word_fetch()		# Get next word from string.

errchk	 salloc, sfree, smark

begin
	call smark (sp)
	call salloc (sx, SZ_LINE, TY_CHAR)
	
	nkey = 0
	cp = 1
	while (word_fetch (ZT_K_KEY(k), cp, Sx, SZ_LINE) > 0) {
	    nkey = nkey + 1
	    ifnoerr (v = imgetd (im, Sx)) {
		ZT_K_NA(k,nkey) = ZT_K_NA(k,nkey) + 1
		ZT_K_A(k,nkey) = ZT_K_A(k,nkey) + v
	    }
	}

	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of zt_k_accum
#---------------------------------------------------------------------------
