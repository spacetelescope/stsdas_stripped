# IMFILL -- Set the fill value in an image according to a mask

procedure imfill ()

#--
pointer	image		# image name
pointer	mask		# mask name
pointer	expr		# a boolean expression telling when to set fill value
double	value		# the fill value

include "imfill.com"

double	nullval
int	imgtype, imglen, imglines, msklen, msklines, exptype
pointer	sp, img, msk, code

data	nullval  / 0.0 /
string	badsize  "Image and mask are not the same size"

extern	get_mskvar
double	clgetd()
pointer	opn_fullimg(), old_mask(), vex_compile(), rd_mask()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (mask, SZ_FNAME, TY_CHAR)
	call salloc (expr, SZ_FNAME, TY_CHAR)

	# Read task parameters

	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("mask", Memc[mask], SZ_FNAME)
	call clgstr ("expr", Memc[expr], SZ_FNAME)
	value = clgetd ("value")

	# Open image and mask

	img = opn_fullimg (Memc[image], READ_WRITE)
	msk = old_mask (Memc[mask], READ_ONLY)
	call typ_fullimg (img, imgtype)

	# Make sure image and mask are same size

	call sz_fullimg (img, imglen, imglines)
	call sz_mask (msk, msklen, msklines)

	if ((imglen != msklen) || (imglines != msklines))
	    call error (1, badsize)

	# Initialize common block

	mask_len = msklen
	mask_var[1] = EOS

	# Compile the expression

	code = vex_compile (Memc[expr])

	# Process each line in the image

	repeat {
	    # Read next line from mask

	    mask_line = rd_mask (msk)
	    if (mask_line == NULL)
		break

	    call vex_eval (code, get_mskvar, nullval, exptype)
	    call upd_imgline (img, code, value, imglen, imgtype, exptype)
	}

	# Close the image

	call cls_fullimg (img)
	call cls_mask (msk)

	# Free dynamic memory

	call vex_free (code)
	call sfree (sp)
end
