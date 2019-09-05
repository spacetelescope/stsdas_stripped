define  SZ_PED          40      # pedigree length

# retrieve pedigrees of reference files from history

procedure pp_pedigree (im, ref, ped, nref)

pointer	im
char	ref[SZ_PED, ARB]
char	ped[SZ_PED, ARB]
int	nref

pointer	idb
pointer	recpt
char	dummy[81]
int	ualen
int	iext

int	strmatch()
pointer	idb_open()
int	idb_nextcard()
bool	is_hist()

begin
	nref = 0
	idb = idb_open (im, ualen)

	# read each card
	while (idb_nextcard(idb, recpt) != EOF) {
	    if (is_hist(recpt)) {
		call strcpy (Memc[recpt], dummy, 80)

		# if the history string contains "CORR="
		# extension name, get the pedigree from next history line. 
		iext = strmatch (dummy, "CORR=")
		if (iext != 0) {
		    nref = nref + 1
		    call ref_name (dummy, ref[1,nref])
		    if (idb_nextcard (idb, recpt) == EOF) {
			ped[1,nref] = EOS
		    } else {
			call strcpy (Memc[recpt], dummy, 80)
			# If next line is another 'CORR=' line, then 
			# don't use it, since it has no Pedigree information
			if ( strmatch(dummy, "CORR=") == 0) {
			    iext = strmatch (dummy, "PEDIGREE=")
			    if ( iext == 0 ) { iext = 10 } else {iext=iext-1}
		    	    call strcpy (Memc[recpt+iext], ped[1,nref], 40)
			} else { 
			nref = nref - 1
			}
		    }
		}
	    }
	}
	call idb_close (idb)
end

# IS_HIST: detect a HISTORY record

bool procedure is_hist (recpt)

pointer	recpt		# record pointer

char	dummy[8]

bool	streq()

begin
	call strcpy (Memc[recpt], dummy, 7)
	return (streq (dummy, "HISTORY"))
end

procedure ref_name (str, ref)

char	str[ARB]
char	ref[ARB]

int	i, j

begin
	do i = 11, 80 
	    if (str[i] == '=') break
	
	if (i == 80) {
	    ref[1] = EOS
	} else {
	    do j = i+1, 80 {
	   	if (str[j] != ' ')
		    ref[j-i] = str[j]
		else {
		    ref[j-i] = EOS
		    break
		}
	    }
	}
end
