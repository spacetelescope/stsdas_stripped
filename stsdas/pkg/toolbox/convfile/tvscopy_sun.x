include <mach.h>	# defines BYTE_SWAP4
include <tbset.h>
include "tconvert.h"

# tvs_copy -- change machine format of a table
# This routine copies a table, converting from one machine format to
# another; e.g. VAX to Sun or Sun to VAX.
# There are two copies of this file, with minor differences between them.
# One version is for a Sun, and the other version is for a VAX.  The portion
# of the code that differs between the two versions (there is only one such
# section) is indicated by a comment beginning with "# ***".
#
# Note that since we use open instead of tbtopn to open the output table,
# the file name outtable must be complete, i.e. must include ".tab".
#
# *** This version is for a Sun.
#
# Phil Hodge, 21-Sep-1989  Subroutine created.
# Phil Hodge, 25-Jun-1996  Include open in the errchk statement.

procedure tvs_copy (intable, outtable, idirection)

char	intable[ARB]		# i: name of table in one format
char	outtable[ARB]		# i: name of table in the other format
int	idirection		# i: default, VAX-->Sun, Sun-->VAX
#--
pointer itp		# pointer to descriptor for input table
int	ifd		# fd for input table file
int	ofd		# fd for output table file
int	direction	# local copy of idirection:  VAX-->Sun, Sun-->VAX
int	byte_swap	# YES if we must swap bytes when opening table.
int	table_type	# row or column ordered table?
int	open(), tbpsta()
errchk	open, tbtopns, tvs_dir

begin
	# Determine the direction of conversion from the names of the tables
	# if the direction was not explicitly specified.  Whichever table name
	# contains an '!' is assumed to be on the remote machine.  byte_swap
	# is set to YES if we must swap bytes when opening the input table.

	call tvs_dir (intable, outtable, idirection, direction, byte_swap)

	# Open the input table and optionally swap bytes;
	# open a file for the output table.
	call tbtopns (intable, byte_swap, itp, ifd)
	ofd = open (outtable, NEW_FILE, BINARY_FILE)

	# Copy the size-information record.  This routine seeks BOF in the
	# output file.  Subsequent I/O routines should not need to seek.
	call tvs_size_info (itp, ifd, ofd)

	# Copy the header parameters.
	call tvs_header (itp, ifd, ofd)

	# Copy the column descriptors.
	call tvs_col_descr (itp, ifd, ofd)

	table_type = tbpsta (itp, TBL_WHTYPE)

	# Copy the data.
	if (direction == VAX_SUN) {

	    if (table_type == TBL_TYPE_S_ROW)
		call tvs_data_r (itp, ifd, ofd)
	    else if (table_type == TBL_TYPE_S_COL)
		call tvs_data_c (itp, ifd, ofd)
	    else
		call error (1, "tvs_copy:  invalid table type")

	} else if (direction == SUN_VAX) {

	    if (table_type == TBL_TYPE_S_ROW)
		call tsv_data_r (itp, ifd, ofd)
	    else if (table_type == TBL_TYPE_S_COL)
		call tsv_data_c (itp, ifd, ofd)
	    else
		call error (1, "tvs_copy:  invalid table type")
	}

	# Close the output and input tables.
	call close (ofd)
	call tbtclo (itp)
end

# tvs_dir -- get direction and byte_swap flag
# If the direction of conversion was specified, this routine copies that
# value to the output.  If not, this routine looks at the names of the
# input and output tables to determine which is on a remote node.
# The byte-swap flag is also set:  YES if the host machine and the
# input table have different byte-swap characteristics; NO otherwise.
# Assumes VAX and DecStation are byte-swapped, Sun is not.
# It is the size-information record and the integer portions of the
# column descriptors that are byte-swapped, and this is done in the
# table-descriptor structure, not the input table itself.

procedure tvs_dir (intable, outtable, idirection, direction, byte_swap)

char	intable[ARB]		# i: name of table in one format
char	outtable[ARB]		# i: name of table in the other format
int	idirection		# i: default, VAX-->Sun, Sun-->VAX
int	direction		# o: VAX-->Sun, Sun-->VAX
int	byte_swap		# o: YES if we must swap bytes for input table
#--
int	node_test		# for determining direction of conversion
char	bang			# '!'
int	stridx()

begin
	if (idirection == TVS_DEFAULT) {

	    bang = '!'
	    node_test = 0
	    if (stridx (bang, intable) > 0)
		node_test = node_test + 1
	    if (stridx (bang, outtable) > 0)
		node_test = node_test + 2

	    if (node_test == 0) {
		call eprintf ("intable = %s; outtable = %s\n")
		    call pargstr (intable)
		    call pargstr (outtable)
		call error (1,
	"must give DIRECTION explicitly if neither table has a node name")

	    } else if (node_test == 3) {

		call eprintf ("intable = %s; outtable = %s\n")
		    call pargstr (intable)
		    call pargstr (outtable)
		call error (1,
	"must give DIRECTION explicitly if both tables have node names")

# *** The remote node is a VAX:
	    } else if (node_test == 1) {
		direction = VAX_SUN		# input table is remote format
	    } else if (node_test == 2) {
		direction = SUN_VAX		# input table is local format
	    }

	} else {
	    direction = idirection	# a value was specified explicitly
	}

	# Determine whether we must swap bytes when opening the input table.
	if (direction == SUN_VAX || direction == SUN_DEC) {
	    # Input table is not byte-swapped.
	    if (BYTE_SWAP4 == YES)
		# but host machine is byte-swapped
		byte_swap = YES
	    else
		byte_swap = NO
	} else {
	    # Input table is byte-swapped.
	    if (BYTE_SWAP4 == YES)
		# host machine is also byte-swapped
		byte_swap = NO
	    else
		byte_swap = YES
	}
end
