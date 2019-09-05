include <mach.h>	# for SZB_CHAR, same on both machines
include <tbset.h>
include "tconvert.h"

define	SZ_PACKED_REC	(SZ_PARREC/SZB_CHAR)

# tvs_sub -- subroutines for copying
#
# Phil Hodge, 21-Sep-1989  Subroutine created.

# tvs_size_info -- copy size-information record
# This routine seeks BOF in both the input and output files.  Subsequent
# sequential reads and writes by other routines should not need to seek.

procedure tvs_size_info (itp, ifd, ofd)

pointer itp			# i: pointer to descriptor for input table
int	ifd			# i: fd for input table file
int	ofd			# i: fd for output table file
#--
int	sizinfo[LEN_SIZINFO]	# size information record
int	read()

begin
	call seek (ifd, BOF)
	if (read (ifd, sizinfo, SZ_SIZINFO) < SZ_SIZINFO)
	    call error (1, "tvs_siz_info:  table data file is empty")

	# Swap bytes in-place in sizinfo.
	call bswap4 (sizinfo, 1, sizinfo, 1, SZ_SIZINFO*SZB_CHAR)

	# Write first record of output table.
	call seek (ofd, BOF)
	call write (ofd, sizinfo, SZ_SIZINFO)
end


# tvs_header -- copy the header (user) parameters
# All of the space allocated for header parameters is copied, not just
# the parameters that have actually been written.

procedure tvs_header (itp, ifd, ofd)

pointer itp			# i: pointer to descriptor for input table
int	ifd			# i: fd for input table file
int	ofd			# i: fd for output table file
#--
char	parrec[SZ_PACKED_REC]	# parameter record (packed chars)
int	maxpar			# number of par for which space is allocated
int	par			# loop index for parameter number
int	read(), tbpsta()

begin
	maxpar = tbpsta (itp, TBL_MAXPAR)

	# Read each header parameter (or unfilled space), and write it to
	# the output table.  No need to byte swap.
	do par = 1, maxpar {
	    if (read (ifd, parrec, SZ_PACKED_REC) < SZ_PACKED_REC)
		call error (1, "tvs_header:  error reading table")
	    call write (ofd, parrec, SZ_PACKED_REC)
	}
end


# tvs_col_descr -- copy the column descriptors
# All of the space allocated for column descriptors is copied, not just
# the descriptors for columns that have actually been defined.

procedure tvs_col_descr (itp, ifd, ofd)

pointer itp			# i: pointer to descriptor for input table
int	ifd			# i: fd for input table file
int	ofd			# i: fd for output table file
#--
int	coldescr[LEN_COLSTRUCT]	# the column descriptor
int	ncols			# number of columns that have been defined
int	maxcols			# number of col for which space is allocated
int	col			# loop index for column number
int	read(), tbpsta()

begin
	ncols = tbpsta (itp, TBL_NCOLS)
	maxcols = tbpsta (itp, TBL_MAXCOLS)

	# Read each column descriptor, swap bytes in-place in coldescr,
	# (only the integer portion, the first 4 longwords), and write
	# to the output file.
	do col = 1, ncols {
	    if (read (ifd, coldescr, SZ_COLSTRUCT) < SZ_COLSTRUCT)
		call error (1, "tvs_col_descr:  error reading table")
	    call bswap4 (coldescr, 1, coldescr, 1, 4*SZ_INT*SZB_CHAR)
	    call write (ofd, coldescr, SZ_COLSTRUCT)
	}

	# Copy space that has been allocated for column descriptors but
	# not used.
	do col = ncols+1, maxcols {
	    if (read (ifd, coldescr, SZ_COLSTRUCT) < SZ_COLSTRUCT)
		call error (1, "tvs_col_descr:  error reading table")
	    call write (ofd, coldescr, SZ_COLSTRUCT)
	}
end
