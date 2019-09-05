# Maximum number of comments to be parsed from QUALITY keyword
define	NUM_EVAL	10

# This routine checks the QUALITY comment from the PDQ file, breaks it into
#	its individual comments (OK, NO-EVAL,UNDEREXPOSED,...) and checks 
#	for the presence of comments not found in the 'evalcheck' array.
#	The evalcheck array is built for each instrument and contains those
#	comments recognized as not being a problem.
#
#	Initial version - 14 Aug 1997 WJH
#
bool procedure qual_check (qual, evalstr)

char	qual[ARB]
char	evalstr[SZ_LINE]		# String containing 'OK' statements

char	evalcheck [SZ_LINE, NUM_EVAL]	# List of 'OK' statements
int	ncheck				# Num. of 'OK' statements

char	qualeval[SZ_LINE, NUM_EVAL]	# List of comments in QUALITY keyword
int	neval				# Num. of quality comments

bool	alright, found

int	ne, nc
bool	streq()

begin
	alright = true
	call qual_parse(qual, qualeval, neval)
	call qual_parse(evalstr, evalcheck, ncheck)

	# Check each QUALITY keyword against each known OK condition
	do ne = 1,neval {
		found = false
		do nc = 1,ncheck {
			# compare QUALITY value with list of OK statements
			if( streq(qualeval[1,ne],evalcheck[1,nc]) ) {
				# we found a match, so it is OK 
				found = true
				break
			}
		}	
		# Set alright to 'false' if at least one is found not to be OK
		if (!found) alright = false
	}
	return alright
end

# parse quality comment from PDQ file (text file)
# This routine takes the QUALITY comment and breaks it up into its 
#	individual statements: OK, NO-TLM, NO-EVAL,...

procedure qual_parse (qual, qualeval, neval)

char	qual[ARB]
char	qualeval[SZ_LINE, ARB]
int	neval			# number of quality comments

char	buf[SZ_LINE]

int	ctowrd()
int	stridx()
int	strlen()
int	ip, idx, idc, len


begin
	neval = 0
	ip = 1

	while (ctowrd(qual, ip, buf, SZ_LINE) > 0 && neval <= NUM_EVAL) {
		neval = neval + 1

		#Look for any existing semi-colons or commas in comment string
		idx = stridx(";", buf)
		idc = stridx(",", buf)
		len = strlen(buf)

		if (idx == 0 && idc == 0) {
			# There are no semi-colons in string
			call strcpy (buf, qualeval[1,neval], SZ_LINE)
		} else {
			# Copy everything up to the semi-colon at end
			call strcpy (buf, qualeval[1,neval], len-1)
		}

	}
end


