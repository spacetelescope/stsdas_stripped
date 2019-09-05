# convert to FITS format

define	SZ_EXT	10		# file extension size
define	NFILES	20		# max number of file extensions

procedure pp_fits ()

char	input[SZ_LINE]		# input file list name
char	output[SZ_LINE]		# output file name
char	tmproot[SZ_LINE]	# temporary file root name
char	instrume[SZ_LINE]	# instrument name

char	name1[SZ_LINE]		# file name
char	name2[SZ_LINE]		# file name
char	root[SZ_LINE]		# root name
char	newroot[SZ_LINE]	# new root name
int	infd, outfd		# input/output file pointer
char	ext1[SZ_EXT, NFILES]
char	ext2[SZ_EXT, NFILES]
char	command[SZ_LINE]
char	str[SZ_LINE]
int	nextn, nconv, i

int	open()
int	getline()
int	access()
int	strlen()
bool	streq()

begin
	# read input, output, and tmproot
	call clgstr ("input", input, SZ_LINE)
	call clgstr ("output", output, SZ_LINE)
	call clgstr ("tmproot", tmproot, SZ_LINE)
	call clgstr ("instrume", instrume, SZ_LINE)

	# open the input and output file
	infd = open (input, READ_ONLY, TEXT_FILE)
	outfd = open (output, NEW_FILE, TEXT_FILE)

	# decide which extensions are to be converted
	if (streq (instrume, "FOS")) {
	    call strcpy ("_shf.fits", ext1[1,1], SZ_EXT)
	    call strcpy (".shh", ext2[1,1], SZ_EXT)

	    call strcpy ("_c0f.fits", ext1[1,2], SZ_EXT)
	    call strcpy (".c0h", ext2[1,2], SZ_EXT)

	    call strcpy ("_c1f.fits", ext1[1,3], SZ_EXT)
	    call strcpy (".c1h", ext2[1,3], SZ_EXT)

	    call strcpy ("_c5f.fits", ext1[1,4], SZ_EXT)
	    call strcpy (".c5h", ext2[1,4], SZ_EXT)

	    call strcpy ("_d0f.fits", ext1[1,5], SZ_EXT)
	    call strcpy (".d0h", ext2[1,5], SZ_EXT)

	    call strcpy ("_trl.fits", ext1[1,6], SZ_EXT)
	    call strcpy (".trl", ext2[1,6], SZ_EXT)

	    call strcpy ("_pdq.fits", ext1[1,7], SZ_EXT)
	    call strcpy (".pdq", ext2[1,7], SZ_EXT)

	    call strcpy ("_c7f.fits", ext1[1,8], SZ_EXT)
	    call strcpy (".c7h", ext2[1,8], SZ_EXT)

	    # do not convert jitter files to GEIS anymore 2/19/97
	    call strcpy ("_jif.fits", ext1[1,9], SZ_EXT)
	    call strcpy ("_jif.fits", ext2[1,9], SZ_EXT)

	    call strcpy ("_jit.fits", ext1[1,10], SZ_EXT)
	    call strcpy ("_jit.fits", ext2[1,10], SZ_EXT)

	    nextn = 10

	} else if (streq (instrume, "HRS") || streq (instrume, "GHRS")) {
	    call strcpy ("_shf.fits", ext1[1,1], SZ_EXT)
	    call strcpy (".shh", ext2[1,1], SZ_EXT)

	    call strcpy ("_d0f.fits", ext1[1,2], SZ_EXT)
	    call strcpy (".d0h", ext2[1,2], SZ_EXT)

	    call strcpy ("_d1f.fits", ext1[1,3], SZ_EXT)
	    call strcpy (".d1h", ext2[1,3], SZ_EXT)

	    call strcpy ("_c0f.fits", ext1[1,4], SZ_EXT)
	    call strcpy (".c0h", ext2[1,4], SZ_EXT)

	    call strcpy ("_c1f.fits", ext1[1,5], SZ_EXT)
	    call strcpy (".c1h", ext2[1,5], SZ_EXT)

	    call strcpy ("_trl.fits", ext1[1,6], SZ_EXT)
	    call strcpy (".trl", ext2[1,6], SZ_EXT)

	    call strcpy ("_ulf.fits", ext1[1,7], SZ_EXT)
	    call strcpy (".ulh", ext2[1,7], SZ_EXT)

	    call strcpy ("_pdq.fits", ext1[1,8], SZ_EXT)
	    call strcpy (".pdq", ext2[1,8], SZ_EXT)

	    # do not convert jitter files to GEIS anymore 2/19/97
	    call strcpy ("_jif.fits", ext1[1,9], SZ_EXT)
	    call strcpy ("_jif.fits", ext2[1,9], SZ_EXT)

	    call strcpy ("_jit.fits", ext1[1,10], SZ_EXT)
	    call strcpy ("_jit.fits", ext2[1,10], SZ_EXT)

	    nextn = 10

	} else if (streq (instrume, "WFPC2")) {
	    call strcpy ("_shf.fits", ext1[1,1], SZ_EXT)
	    call strcpy (".shh", ext2[1,1], SZ_EXT)

	    call strcpy ("_d0f.fits", ext1[1,2], SZ_EXT)
	    call strcpy (".d0h", ext2[1,2], SZ_EXT)

	    call strcpy ("_c0f.fits", ext1[1,3], SZ_EXT)
	    call strcpy (".c0h", ext2[1,3], SZ_EXT)

	    call strcpy ("_pdq.fits", ext1[1,4], SZ_EXT)
	    call strcpy (".pdq", ext2[1,4], SZ_EXT)

	    # do not convert jitter files to GEIS anymore 2/19/97
	    call strcpy ("_jif.fits", ext1[1,5], SZ_EXT)
	    call strcpy ("_jif.fits", ext2[1,5], SZ_EXT)

	    call strcpy ("_jit.fits", ext1[1,6], SZ_EXT)
	    call strcpy ("_jit.fits", ext2[1,6], SZ_EXT)

	    nextn = 6

	} else if (streq (instrume, "FOC")) {
	    call strcpy ("_shf.fits", ext1[1,1], SZ_EXT)
	    call strcpy (".shh", ext2[1,1], SZ_EXT)

	    call strcpy ("_d0f.fits", ext1[1,2], SZ_EXT)
	    call strcpy (".d0h", ext2[1,2], SZ_EXT)

	    call strcpy ("_c0f.fits", ext1[1,3], SZ_EXT)
	    call strcpy (".c0h", ext2[1,3], SZ_EXT)

	    call strcpy ("_c1f.fits", ext1[1,4], SZ_EXT)
	    call strcpy (".c1h", ext2[1,4], SZ_EXT)

	    call strcpy ("_pdq.fits", ext1[1,5], SZ_EXT)
	    call strcpy (".pdq", ext2[1,5], SZ_EXT)

	    # do not convert jitter files to GEIS anymore 2/19/97
	    call strcpy ("_jif.fits", ext1[1,6], SZ_EXT)
	    call strcpy ("_jif.fits", ext2[1,6], SZ_EXT)

	    call strcpy ("_jit.fits", ext1[1,7], SZ_EXT)
	    call strcpy ("_jit.fits", ext2[1,7], SZ_EXT)

	    nextn = 7

	} else {
	    nextn = 0
	}

	nconv = 0

	while (getline(infd, root) != EOF) {

	    # get rid of the carrige return
	    root[strlen(root)] = EOS

	    # get rid of the trailing blanks if any
	    do i = strlen(root), 1, -1 {
		if (root[i] != ' ') {
		    root[i+1] = EOS
		    break
		}
	    }

	    # attach the tmproot to the original root name
	    call sprintf (newroot, SZ_LINE, "%s%s")
		call pargstr (tmproot)
		call pargstr (root)

	    # write to the output table
	    call sprintf (str, SZ_LINE, "%s\n")
		call pargstr (newroot)
	    call putline (outfd, str)

	    # do the strfits for each root name
	    # for jitter files, do not convert, just copy to the tmp
	    do i = 1, nextn {
	        call sprintf (name1, SZ_LINE, "%s%s")
		    call pargstr (root)
		    call pargstr (ext1[1,i])
	        call sprintf (name2, SZ_LINE, "%s%s")
		    call pargstr (newroot)
		    call pargstr (ext2[1,i])


		if (streq(ext2[1,i], "_jif.fits") || 
					streq(ext2[1,i], "_jit.fits")) {
                    name1[strlen(root)] = 'j'
                    name2[strlen(newroot)] = 'j'
		}
	
		if (access (name1, 0, 0) == YES) {
		    if (streq(ext2[1,i], "_jif.fits") || 
					streq(ext2[1,i], "_jit.fits")) {
   		        call sprintf (command, SZ_LINE, 
					"copy %s %s mode=h >& dev$null")
                       	    call pargstr (name1)
                       	    call pargstr (name2)
                        call clcmdw (command)
	   	    } else {

		        # use CL command "strfits"
   		        call sprintf (command, SZ_LINE, "strfits %s \"\" %s oldirafname=no mode=h >& dev$null")
                       	    call pargstr (name1)
                       	    call pargstr (name2)
                        call clcmdw (command)
		        nconv = nconv + 1
		    }   
		}
	    }
	}

	if (nconv == 0) {
	    call printf ("WARNING: No FITS file is converted.\n")
	}
		
	# close the input/output file
	call close (infd)
	call close (outfd)
end
