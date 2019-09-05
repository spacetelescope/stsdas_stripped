include	<imio.h>
include	<imhdr.h>

# EHEADER -- Edit an image header interactively
#
# B.Simon	01-Jun-87	Original

procedure t_eheader ()

#--
char	blank, newline
int	junk, reclen, nchars, nline, iline
pointer	sp, image, efile, ecmd, eline, ext
pointer	ua, im, ed, hd, ip

data	blank, newline	/ ' ', '\n' /

int	open(), stridx(), stropen(), getline(), gstrcpy()
int	strlen(), strncmp(), fnextn()
pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (efile, SZ_FNAME, TY_CHAR)
	call salloc (ecmd, SZ_LINE, TY_CHAR)
	call salloc (eline, SZ_LINE, TY_CHAR)
	call salloc (ext, SZ_FNAME, TY_CHAR)

	call clgstr ("image", Memc[image], SZ_FNAME)
	junk = fnextn (Memc[image], Memc[ext], SZ_FNAME)

	# Open the image

	im = immap (Memc[image], READ_WRITE, NULL)
	ua = IM_USERAREA(im)

	# If the image is blocked, save the length of the header record

	reclen = 0
	if (IM_UABLOCKED(im) == YES) {
	    reclen = stridx(newline, Memc[ua])
	    nline = strlen (Memc[ua])  / reclen
	}

	# Create a temporary file and copy the user area into it

	call mktemp ("tmp$", Memc[efile], SZ_FNAME)
	ed = open (Memc[efile], NEW_FILE, TEXT_FILE)
	hd = stropen (Memc[ua], ARB, READ_ONLY)
	call fcopyo (hd, ed)
	call close (ed)
	call close (hd)

	# Edit the temporary file

	call sprintf (Memc[ecmd], SZ_LINE, "edit \"%s\" ")
	    call pargstr (Memc[efile])
	iferr {
	    call clcmdw (Memc[ecmd])
	    ed = open (Memc[efile], READ_ONLY, TEXT_FILE)
	} then {
	    call delete (Memc[efile])
	    return
	}

	# Copy the edited file back into the user area,
	# reblocking where necessary

	ip = ua
	iline = 0

	repeat {
	    nchars = getline (ed, Memc[eline])
	    iline = iline + 1
	    if (nchars == EOF)
		break
	    if (IM_UABLOCKED(im) == YES && nchars != reclen) {
		for ( ; nchars < reclen; nchars = nchars + 1)
		    Memc[eline+nchars-1] = blank

		Memc[eline+reclen-1] = newline
		Memc[eline+reclen] = EOS
	    }
	    ip = ip + gstrcpy(Memc[eline], Memc[ip], SZ_LINE)
	}

	# If this is a fits file, add blank lines to user area
	# when the new file is shorter than the old

	if (reclen > 0 && strncmp (Memc[ext], "fit", 3) == 0) {
	    call amovkc (blank, Memc[eline], reclen-1)
	    Memc[eline+reclen-1] = newline
	    Memc[eline+reclen] = EOS

	    for ( ; iline < nline; iline = iline + 1) {
		ip = ip + gstrcpy(Memc[eline], Memc[ip], SZ_LINE)
	    }
	}

	Memc[ip] = EOS

	
	call imunmap (im)

	# Close and delete all versions of the temporary file

	call close (ed)
	call deletefg (Memc[efile], YES, NO)

	call sfree (sp)
end


