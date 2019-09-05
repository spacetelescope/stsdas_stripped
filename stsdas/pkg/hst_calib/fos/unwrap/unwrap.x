include <imio.h>
include <imhdr.h>

define	LEN_EXTN	3		# file extension length

# T_UNWRAP -- Remove the "wrap-around" from FOS spectra which exceed
# the internal counter ceiling of 65535 counts
#
# Steve Hulbert, 31Jan90
# 
# 25Jul94 - Howard Bushouse - Fixed problem with null output file name extension

procedure t_unwrap ()

char	input[SZ_FNAME]			# overflowed image
char	output[SZ_FNAME]		# output image
real	thresh				# threshold

pointer	im_in, im_out, buf_in, buf_out, sp
int     ngroups, naxis1, group, pix
int	in_count[IM_MAXDIM], out_count[IM_MAXDIM]
int	immap(), imgeti(), imgnlr(), impnlr(), imaccf()
#int	status
real	ceiling, wrap, datamax, datamin
real	clgetr()
char	root[SZ_FNAME], root2[SZ_FNAME]
char	extn[LEN_EXTN], extn2[LEN_EXTN]

string	msg1 "Group %d: Data previously processed or paired-pulse corrected.\n"
string  msg2 "Group %d: Unwrapping INCOMPLETE.\n"
string	msg3 "Group %d: BAD wrap at ends of spectrum or ERROR in wrap correction.\n"

data	ceiling/65535.0/

begin

	call clgstr ("input", input, SZ_FNAME)
	im_in  = immap (input, READ_ONLY, 0)

	call clgstr ("output", output, SZ_FNAME)

	thresh = clgetr ("thresh")

	if (imaccf(im_in, "GCOUNT") == YES) {
	    ngroups = imgeti (im_in, "GCOUNT") 
	} else {
	    call imunmap (im_in)
	    call error (1, "Cannot find GCOUNT keyword in input image")
	}
	naxis1 = IM_LEN (im_in, 1) 

	# copy input and use a template for output

	call iki_parse (input, root, extn)
	call iki_parse (output, root2, extn2)
	if (extn2[1]==EOS) call iki_mkfname (root2, extn, output, SZ_FNAME)
	#call stf_copy (root,extn,root2,extn2,status)
	call imcopy (input, output)
	im_out = immap (output, WRITE_ONLY, 0)

	call smark (sp)
	call salloc (buf_in, naxis1, TY_REAL)
	call salloc (buf_out, naxis1, TY_REAL)

	do group = 1, ngroups {

	   wrap = 0.0

	   call amovkl (long(1), in_count, IM_MAXDIM)
	   call amovkl (long(1), out_count, IM_MAXDIM)

	   # point to new group in both images
 
	   call gf_opengr (im_in, group, datamin, datamax, 0)
	   call gf_opengr (im_out, group, datamin, datamax, 0)

	   while (imgnlr (im_in, buf_in, in_count) != EOF &&
		  impnlr (im_out, buf_out, out_count) != EOF) {

		# copy first pixel (= 0 offset) from input to output

		Memr[buf_out] = Memr[buf_in]

		do pix = 1, naxis1-1 {

		   if (Memr[buf_in + pix] > ceiling) { 
			call printf (msg1)
			   call pargl (group)
			call printf (msg2)
			   call pargl (group)
			call flush (STDOUT)
			break
		   } else if ((Memr[buf_in + pix - 1] - Memr[buf_in + pix]) >
				thresh) 
			wrap = wrap + ceiling
		   else if ((Memr[buf_in + pix] - Memr[buf_in + pix - 1]) >
				thresh) 
			wrap = wrap - ceiling

		   Memr[buf_out + pix] = Memr[buf_in + pix] + wrap
		}

	   }

	   if (wrap < 0.0 || wrap > 0.0) {
		call printf (msg3)
		   call pargl (group)
		call flush (STDOUT)
	   } 

	}

	call sfree (sp)

	call imunmap (im_in)
	call imunmap (im_out)
	
end
