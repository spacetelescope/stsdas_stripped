################################################################################
#										
#  U_PEDIGREE -- Check the keywords PEDIGREE and DESCRIP of WFPC2's reference 
#		 file and determine what to do according to the value of 
#		 PEDIGREE
#										
#  History:								
#	06 Dec 94 by JCHsu	Initial implementation				

procedure u_pedigree (inim, outim, infile, sw, s1, s2)

#  Calling arguments:
pointer	inim			# input (reference) file pointer
pointer	outim			# output (science) file pointer
char	infile[ARB]		# input (reference) file name
bool	sw			# the relevant calibration switch
char	s1[ARB], s2[ARB]

#  Local variables:
char	pedigree[SZ_LINE]
char	descrip[SZ_LINE]
char	text[5], str[SZ_LINE]

#  Functions used:
bool	streq()
#-------------------------------------------------------------------------------
begin
	# read the value of the keyword PEDIGREE and DESCRIP
	iferr (call imgstr (inim, "DESCRIP", descrip, SZ_LINE))
	    descrip[1] = EOS
	iferr (call imgstr (inim, "PEDIGREE", pedigree, SZ_LINE)) {
	    
	    # if PEDIGREE does not exist, proceed the calibration
	    call printf ("  %s has no PEDIGREE keyword\n")
		call pargstr (infile)
	} else {

	    # if PEDIGREE is DUMMY, skip the calibration by resetting the 
	    # calibration switch
	    call strcpy (pedigree, text, 5)
	    if (streq (text, "DUMMY")) {
		sw = false
		call gf_unmap (inim)
		call gf_ipstr (outim, s2, "SKIPPED")
		call sprintf (str, SZ_LINE, "%s=%s  %s=SKIPPED")
		    call pargstr (s1)
		    call pargstr (infile)	
		    call pargstr (s2)
		call gf_iputh (outim, "HISTORY", str)
		call printf ("%s\n")
		    call pargstr (str)
	    } else {
		call sprintf (str, SZ_LINE, "%s=%s  %s=COMPLETED")
		    call pargstr (s1)
		    call pargstr (infile)	
		    call pargstr (s2)
		call gf_iputh (outim, "HISTORY", str)
		call printf ("%s\n")
		    call pargstr (str)
	    }
	    call sprintf (str, SZ_LINE, "PEDIGREE=%s")
		call pargstr (pedigree)
	    call gf_iputh (outim, "HISTORY", str)
	    call printf ("%s\n")
		call pargstr (str)

	    call sprintf (str, SZ_LINE, "DESCRIP=%s")
		call pargstr (descrip)
	    call gf_iputh (outim, "HISTORY", str)
	    call printf ("%s\n")
		call pargstr (str)
	}
end
