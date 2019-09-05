include	<fset.h>
include	<mach.h>
include	"hstephem.h"

define	SZ_EPHEM	7	# number of ephemeris quantities for each epoch
define	LEN_ENTRY	72	# number of SPP characters for each ephemeris
				# entry
define	SZ_DAY		1440	# number of minutes per day


#  t_hstephem -- Perform ST ephemeris conversion
#
#  Description:
#  ------------
#
#  Date		Author		Description
#  ----		------		-----------
#  29-Jun-1992  J.-C. Hsu	design and coding
#  20-Nov-1995  J.-C. Hsu	enhancement to handle not just two-day
#				ephemeris
#  19-Sep-2005  Phil Hodge	subtract mjd_first from values written to
#				the TIME (formerly MJD) column, to be
#				consistent with the current odelaytime
#------------------------------------------------------------------------------

procedure t_hstephem ()

pointer	fin 			# file template pointer
char	outtable[SZ_FNAME]	# output table name

char	fdata[SZ_FNAME]
int	fd			# file descriptor
int	i, j, n, nfiles, nf, nrow, ndays
int	sz_record
short	buf
pointer	raw, raw2, state
int	nchar
double	ephem[SZ_EPHEM]
double	mjd, mjd_old, mjd_first
real	interval, interval_old
pointer	tp			# output table descripter
pointer	colidn[NCOL]		# column descripters
	
pointer	imtopenp()
int	open()
int	read()
int	imtlen()
int	imtgetim()
long	fstatl()
#==============================================================================
begin

	# open input file template
	fin = imtopenp ("infile")
	call clgstr ("outtable", outtable, SZ_FNAME)

	# check that the output file can not be empty
	nfiles = imtlen (fin)
	if (nfiles == 0)
	    call error (1, "no input file")
	if (outtable[1] == EOS)
	    call error (1, "blank output file name")

	# allocate array space
	call malloc (raw, LEN_ENTRY+1, TY_CHAR)
	call malloc (raw2, LEN_ENTRY+1, TY_CHAR)
	call malloc (state, SZ_EPHEM, TY_DOUBLE)

	# open the output table
	call ephem_out (outtable, tp, colidn)

	# loop all input files
	nf = 0
	nrow = 1
	do n = 1, nfiles {
	    nchar = imtgetim (fin, fdata, SZ_FNAME)

	    # open the input file
	    iferr (fd = open (fdata, READ_ONLY, BINARY_FILE)) {
		call printf ("Warning: Input file '%s' does not exist\n")
		    call pargstr (fdata)
		next
	    }

	    # make the following change so the task can handle orbit file of
	    # not just two-day ephemeris.  JC Hsu 11/20/95
	    # determine how many day's worth of data rae in this file by
	    # looping throught number of days and pick the one has sz_record
	    # equal to 144 or 145.
	    do ndays = 1, 400 {

	        # decide how many bytes are in each entry
		# fstatl returns the file size in number of characters.
	        sz_record = nint ( real(fstatl(fd, F_FILESIZE))*real(SZB_CHAR)/
				  real(ndays*SZ_DAY+1) )
		if (sz_record == 144 || sz_record == 145) break
		if (sz_record < 140) break
	    }

	    # skip the file with illegal length
	    if (sz_record < 140) {
		call printf (
		  "Warning: Input file '%s' does not have the correct length\n")
		    call pargstr (fdata)
		next
	    }

	    # each entry of ephemeris has 144 OR 145 bytes, depending on
	    # whether there is an extra line feed at the end of each entry.
	    # Since SPP can only read TWO bytes at a time, we read LEN_ENTRY
	    # (=72) two-bytes first, and if there is line feed, read one extra
	    # two-byte word at even entries.  The first byte
	    # is a (regular not SPP) character which we skip, followed by
	    # 8 bytes of time code, and 6 eight-byte state vector components.

	    nf = nf + 1
	    j = 1
	    repeat {
		if (read (fd, Memc[raw], LEN_ENTRY) != LEN_ENTRY) break
		if (sz_record == 145 && j/2*2 == j) {
		    if (read (fd, Memc[raw+LEN_ENTRY], 1) != 1) break

		    # skip the first TWO bytes
		    call bytmov (Memc[raw], 3, Memc[raw2], 1, 8*SZ_EPHEM)
		} else {

		    # skip the first ONE byte
		    call bytmov (Memc[raw], 2, Memc[raw2], 1, 8*SZ_EPHEM)
		}

		# decode the time code
		mjd = 0.d0
		do i = 1, 8 {
		    buf = 0

		    # move one byte at a time to the SECOND byte of a short
		    call bytmov (Memc[raw2+(i-1)/2], mod(i-1,2)+1, buf, 2, 1)

		    # when bytmov from to to short, need to byte swap because
		    # we mean to move the byte to the least significant byte
		    # of the short integer
		    # (this whole byte swap business is caused by the fact that
		    # IEEE counts bytes from left to right, but systems with
		    # BYTE_SWAP = YES, such as VAX, count bytes from right to
		    # left).
		    if (BYTE_SWAP2 == YES)
			call bswap2 (buf, 1, buf, 1, 2)
		    mjd = mjd + buf * 256.**(i-1)
		}

		mjd = mjd / 8.64d11

		# if there are more than one input files, they have to be
	 	# contiguous, i.e. the first row of the second file must be
		# identical to the last row of the first file etc.
		if (j == 1 && nf > 1) {
		    if (mjd != mjd_old)
			call error (1, "input files are not contiguous")
		    else
			j = j + 1
		    next
		}

		# keep the first epoch, interval, and the previous epoch
		if (j == 1 && nf == 1)
		    mjd_first = mjd
		else
		    interval = mjd - mjd_old
		if (j > 2 || nf != 1) {
		    if (abs(interval-interval_old) > 2*EPSILON) {
			call printf (
			  "data not evenly spaced at file '%s' row %d\n")
			    call pargstr(fdata)
			    call pargi(j)
			    call flush (STDOUT)
			call error (1, "")
		    }
		}

		# the raw data is in VAX floating format, convert to IEEE
		# format, then use ieevupkd (in osb$) to convert to the
		# native floating format.
		call vax2ieeed (Memc[raw2+4], Memd[state], SZ_EPHEM-1)
		call ieevupkd (Memd[state], ephem, SZ_EPHEM-1)

	    	# write one row to the output table
	    	call tbrptd (tp, colidn[ID_MJD], mjd-mjd_first, 1, nrow)
	    	call tbrptd (tp, colidn[ID_X], ephem[1], 1, nrow)
	    	call tbrptd (tp, colidn[ID_Y], ephem[2], 1, nrow)
	    	call tbrptd (tp, colidn[ID_Z], ephem[3], 1, nrow)
	    	call tbrptd (tp, colidn[ID_VX], ephem[4], 1, nrow)
	    	call tbrptd (tp, colidn[ID_VY], ephem[5], 1, nrow)
	    	call tbrptd (tp, colidn[ID_VZ], ephem[6], 1, nrow)

		nrow = nrow + 1
		mjd_old = mjd
		interval_old = interval
		j = j + 1
	    }

	    call tbhadt (tp, "HISTORY", fdata)

	    # close input file
	    call close (fd)
	}

	# write header parameters to the output table
	call tbhadd (tp, "FIRSTMJD", mjd_first)

	# close file template
	call imtclose (fin)

	# close output table
	call tbtclo (tp)

	# release memory
	call mfree (raw, TY_CHAR)
	call mfree (raw2, TY_CHAR)
	call mfree (state, TY_DOUBLE)
end
