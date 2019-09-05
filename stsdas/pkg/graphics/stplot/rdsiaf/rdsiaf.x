#* HISTORY *
#* B.Simon	24-Feb-89	Original
#* B.Simon	18-Jun-92	Rewritten to produce an sdas table

# RDSIAF -- Load the CDBS Database from the Science Instrument Aperture File
 
procedure rdsiaf ()

#--
pointer	input		# SIAF file name
pointer	output		# SDAS table name

int	fd
pointer	sp, tp, record, pdbdate

string	nullfile "SIAF file is empty"

int	open(), getline(), getaper()
pointer	tbtopn()

begin
	# Allocate dynamic memory for input parameters

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	call salloc (record, SZ_LINE, TY_CHAR)
	call salloc (pdbdate, SZ_FNAME, TY_CHAR)

	# Read task parameters

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)

	# Open the project database SIAF file

	fd = open (Memc[input], READ_ONLY, TEXT_FILE)

	if (getline (fd, Memc[record]) == EOF)
	    call error (1, nullfile)

	call strcpy (Memc[record+14], Memc[pdbdate], 6)

	# Open the output table

	tp = tbtopn (Memc[output], NEW_FILE, NULL)
	call siaftab (tp)

	# Loop over apertures

	while (getaper (fd, tp, Memc[pdbdate]) != EOF)
	    ;

	# Close files and free memory

	call close (fd)
	call tbtclo (tp)
	call sfree (sp)

end
