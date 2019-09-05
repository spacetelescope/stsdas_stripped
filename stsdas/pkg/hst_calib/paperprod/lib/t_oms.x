# get the proper extension number for the jitter file according to the OPERATE
# date

procedure t_oms ()

char	input[SZ_FNAME]		# input/output jitter file name

begin
	# read input file name
	call clgstr ("input", input, SZ_FNAME)

	# attach the extension if necessary
	call pp_oms (input)

	# write the output file name
	call clpstr ("output", input)
end

procedure pp_oms (input)

char	input[ARB]		# input/output jitter file name

char	infile[SZ_FNAME]
char	str[SZ_LINE]
int	j, k
real	ver
pointer	ip

pointer	immap()
int	ctor()

begin
	call strcpy (input, infile, SZ_FNAME)

	# extension [0] must be attached to the file name, otherwise error will
	# occur if the extension has no data.
	call strcat ("[0]", infile, SZ_FNAME)

	# open the input image
	iferr (ip = immap (infile, READ_ONLY, 0)) {
	    return
	} else {
	
	    #ifnoerr (call imgstr (ip, "OMS_VER", str, SZ_LINE)) {
	    # change from OMS_VER to OPERATE because OPUS roll BACK the 
	    # version number from 19.xx to 8.0, 1/5/98 JCH.
	    ifnoerr (call imgstr (ip, "OPERATE", str, SZ_LINE)) {
	        k = 1
	        j = ctor (str, k, ver)
	        #if (ver >= 19.0) call strcat ("[1]", input, SZ_FNAME)

		# Use Feb 2 1997 as the corresponding date for 19.0
	        if (ver >= 1997.033) call strcat ("[1]", input, SZ_FNAME)
	        else call strcat ("[0]", input, SZ_FNAME)
	    }

	    # close input image
	    call imunmap (ip)
	}
end
