include <tbset.h>
include	<od.h>
include "sa_defines.h"

# Error messages.
define  ERRMALLOC            "SPECALIGN: No memory available for allocation."

# Default values.
define	DEFAULT_GOFF	0.0d0			# Default granularity offset.

# Memory management.
define	DATA		Memd[data+(($1-1)*len)] # Index into data array.
define	GOFF		Memd[goff+$1-1]		# Index into goff array.
define	GROUP		Memi[group+$1-1]	# Index into group array.
define	NULB		Memb[$1+$2-1]		# Index into null arrays.
define	SHIFT		Memd[shift+$1-1]	# Index into shift array.
define	WDATA		Memd[wdata+(($1-1)*len)] # Index into data array.
define	WGROUP		Memi[wgroup+$1-1]	# Index into group array.
define	WSHIFT		Memi[wshift+$1-1]	# Index into the wavelength.

#---------------------------------------------------------------------------
.help sa_read Jan97 source
.ih
NAME
sa_read -- Read the data.
.ih
USAGE
call sa_read (data, wdata, goff, shift, wshift, template_out, 
              template_wave, rsfunc, n_shifts, len, olen, wlen, 
              glen)
.ih
ARGUMENTS
.ls data (O: pointer)
DOUBLE pointer to an array, dimensioned [len,n_shifts], which
contain the input spectral data.
.le
.ls wdata (O: pointer)
DOUBLE pointer to an array, dimensioned [len,n_shifts], which contains
the wavelength tables.	If no wavelengths are found in the input
table, NULL is returned.
.le
.ls goff (O: pointer)
DOUBLE pointer to an array, dimensioned [n_shifts], which contains the
pixel offsets for the granularity.
.le
.ls shift (O: pointer)
DOUBLE pointer to an array, dimensioned [n_shifts], which contains the
pixel offsets of each of the spectral data.
.le
.ls wshift (O: pointer)
INT pointer to an array, dimensioned [n_shifts], which contains the 
wavelength shifts.
.le
.ls template_out (O: pointer)
STRUCT pointer which points to the OD descriptor of the first file
from the input table.  This is used to create the output files and
must remain open until the outputs are closed.
.le
.ls template_wave (O: pointer)
STRUCT pointer which points to the OD descriptor of the first file
from the input wavelength table.  This is used to create the output
wavelength files and must remain open until the outputs are closed.
.le
.ls rsfunc (I: integer)
Resampling function option for the spectral array.
.le
.ls n_shifts (O: int)
The number of shifted spectra that has been read.
.le
.ls len (O: int)
The length of the spectral data.
.le
.ls olen (O: int)
The length of the composite output spectrum.
.le
.ls wlen (O: int)
Lenght of wavelength solution.
.le
.ls glen (O: int)
The length of the resulting granularity spectrum.
.le
.ih
DESCRIPTION
NOTE: This routine, though not top level to a task, reads from task
parameters.  This routine reads the defined columns from the input table.
In addition, the named input files found in the table are read into
memory.
.endhelp
#---------------------------------------------------------------------------
procedure sa_read (data, wdata, goff, shift, wshift, template_out, 
                   template_wave, rsfunc, n_shifts, len, olen, wlen, glen)

# Passed parameters.
pointer data			# O:  Double array n_shifts x len of data.
pointer wdata			# O:  Double array n_shifts x len of wavelens.
pointer goff			# O:  Double array n_shifts of granular shifts.
pointer shift			# O:  Double array n_shifts of spectral shifts.
pointer	wshift			# O:  Int array n_shifts of wavelength shifts.
pointer template_out		# O:  File to use as template for output/gran.
pointer	template_wave		# O:  File to use for output wavelength.
int     rsfunc                  # I:  Resampling option for spectral and gran arrays.
int	n_shifts		# O:  The number of shifts present.
int	len			# O:  Length of data and granularity.
int	olen			# O:  Length of output data.
int	wlen			# O:  Length of wavelength.
int	glen			# O:  Size of granularity data array.

# Table I/O.
pointer file_col		# Column desc. for the file names.
pointer group			# Contents of the group columns.
pointer in			# Table descriptor.
int	ns			# Current number of real shifts.
pointer null_goff		# Which values in the goff column are null.
pointer null_group		# Which values in the group column are null.
pointer null_shift		# Which values in the shift column are null.
pointer null_wgroup		# Which values in the wgroup column are null.
pointer	null_wshift		# Which values in the wshift column are null.
int	tbpsta()		# Get table information.
pointer tbtopn()		# Open a table.
pointer wave_col		# Wavelength column
pointer wgroup			# Wavelength group column.

# File I/O.
pointer	d_file			# Data file descriptor.
pointer file_name		# Name of the current input file.
int	n_files			# Number of files read.
pointer od_map()		# Open a file.
pointer	w_file			# Wavelength file descriptor.
pointer wave_name		# Name of the current wavelength file.
bool	wave_wcs		# True if wavelengths come from the WCS.

# Misc.
pointer	adx			# Generic double array.
int	i, ix			# Generic integer.
int     imin, imax              # Minimum/Maximum values of integer array.
double  dmin, dmax              # Minimum/Maximum values of double array.
double  residual                # Remainder of value - int(value).
double  tmplen                  # Temporary variable to hold length.
pointer px			# Generic pointer.
pointer sp			# Stack pointer.
bool	streq()			# Are strings equal?
int	strlen()		# Get length of string.
bool	strne()			# Are strings not equal?
pointer sx			# Generic string.
pointer	vx, vy			# X-array vector, Y-array vector.
pointer stemp, gtemp            # Temporary pointers for data conversion, if needed.

begin
	call smark (sp)
	call salloc (sx, max(SZ_LINE, SZ_PATHNAME), TY_CHAR)
	
	# Open the table
	call clgstr ("input", Memc[sx], SZ_LINE)
	in = tbtopn (Memc[sx], READ_ONLY, NULL)
	n_shifts = tbpsta (in, TBL_NROWS)
	if (n_shifts <= 0)
	    call error (1, "specalign: input table is empty")

	# Find column that has the file names.
	call clgstr ("file_col", Memc[sx], SZ_LINE)
	call tbcfnd (in, Memc[sx], file_col, 1)
	if (file_col == NULL)
	    call error (1, "specalign: file column does not exist")

	# Read the column containing the group specifiers.
	call clgstr ("group_col", Memc[sx], SZ_LINE)
	call tbcfnd (in, Memc[sx], px, 1)
	if (px == NULL)
	    call error (1, "specalign: group column does not exist")
	call salloc (group, n_shifts, TY_INT)
	call salloc (null_group, n_shifts, TY_BOOL)
	call tbcgti (in, px, GROUP(1), Memb[null_group], 1, n_shifts)

	# Read the column that contains the pixel shifts.
	call clgstr ("shift_col", Memc[sx], SZ_LINE)
	call tbcfnd (in, Memc[sx], px, 1)
	if (px == NULL)
	    call error (1, "specalign: shift column does not exist")
	call malloc (shift, n_shifts, TY_DOUBLE)
        if (shift == NULL) 
            call error (1, ERRMALLOC)
	call salloc (null_shift, n_shifts, TY_BOOL)
	call tbcgtd (in, px, SHIFT(1), Memb[null_shift], 1, n_shifts)

	# Read the column that contains the granularity shifts.
	call malloc (goff, n_shifts, TY_DOUBLE)
        if (goff == NULL) 
            call error (1, ERRMALLOC)
	call salloc (null_goff, n_shifts, TY_BOOL)
	call amovkd (DEFAULT_GOFF, GOFF(1), n_shifts)
	call amovkb (false, NULB(null_goff,1), n_shifts)
	call clgstr ("goff_col", Memc[sx], SZ_LINE)
	if (strlen (Memc[sx]) > 0)
	    call tbcfnd (in, Memc[sx], px, 1)
	    if (px != NULL)
		call tbcgtd (in, px, GOFF(1), Memb[null_goff], 1, n_shifts)

	# Get the column that contains the wavelengths.
	call clgstr ("wave_col", Memc[sx], SZ_LINE)
	call tbcfnd (in, Memc[sx], wave_col, 1)

	# If wavelengths exist, get the group and shift columns.
	wshift = NULL
	if (wave_col != NULL) {
	    call clgstr ("wgroup_col", Memc[sx], SZ_LINE)
	    call tbcfnd (in, Memc[sx], px, 1)
	    if (px == NULL) {
		call sprintf (Memc[sx], SZ_LINE, "specalign: column %s not found")
		call pargstr (Memc[sx])
		call error (1, Memc[sx])
	    }
	    call salloc (wgroup, n_shifts, TY_INT)
	    call salloc (null_wgroup, n_shifts, TY_BOOL)
	    call tbcgti (in, px, WGROUP(1), Memb[null_group], 1, n_shifts)

	    call clgstr ("wshift_col", Memc[sx], SZ_LINE)
	    call tbcfnd (in, Memc[sx], px, 1)
	    if (px == NULL) {
		call sprintf (Memc[sx], SZ_LINE, "specalign: column %s not found")
		call pargstr (Memc[sx])
		call error (1, Memc[sx])
	    }
	    call malloc (wshift, n_shifts, TY_INT)
            if (wshift == NULL) 
                call error (1, ERRMALLOC)
	    call salloc (null_wshift, n_shifts, TY_BOOL)
	    call tbcgti (in, px, WSHIFT(1), Memb[null_wshift], 1, n_shifts)

	}

	# Since this task requires all the data to be in memory, read
	# all the input data.
	call salloc (file_name, SZ_PATHNAME, TY_CHAR)
	call strcpy ("", Memc[file_name], SZ_PATHNAME)
	call salloc (wave_name, SZ_PATHNAME, TY_CHAR)
	call strcpy ("", Memc[wave_name], SZ_PATHNAME)
	ns = 0
	px = NULL
	n_files = 0
	d_file = NULL
	w_file = NULL
	template_out = NULL
	template_wave = NULL
	do i = 1, n_shifts {

	    # Get the next file.
	    call tbegtt (in, file_col, i, Memc[sx], SZ_LINE)
	    if (strne (Memc[sx], Memc[file_name])) {

		# Don't close the previous file if it is the first one.
		if (n_files > 1)
		    call od_unmap (d_file)
		call strcpy (Memc[sx], Memc[file_name], SZ_PATHNAME)
		d_file = od_map (Memc[file_name], READ_ONLY, NULL)
		n_files = n_files + 1
	    }
	    call od_open_group (d_file, GROUP(i))
	    
	    # Get next wavelength file.
	    if (wave_col != NULL) {
		call tbegtt (in, wave_col, i, Memc[sx], SZ_LINE)

		# If an empty value, assume there is no information.
		if (strlen (Memc[sx]) <= 0) {
		    wave_col = NULL
		    wave_wcs = false

		# Else, if the WCS flag is there, read in the WCS information.
		} else if (streq (Memc[sx], "%%IMAGE_WCS%%")){
		    wave_wcs = true
		}

		# Else, open the file.
		else {
		    if (strne (Memc[sx], Memc[wave_name])) {
			if (w_file != template_wave)
			    call od_unmap (w_file)
			call strcpy (Memc[sx], Memc[wave_name], SZ_PATHNAME)
			w_file = od_map (Memc[wave_name], READ_ONLY, NULL)
		    }
		    call od_open_group (w_file, WGROUP(i))
		    wave_wcs = false
		}
	    }
	    
	    # If the first file, then a number of things must occur.
	    if (i == 1) {

		# Get the length of the data.
		len = OD_LEN(d_file)

		# Allocate the data array.
		call malloc (data, n_shifts * len, TY_DOUBLE)
                if (data == NULL) 
                    call error (1, ERRMALLOC)
		if (wave_col != NULL) {
		    call malloc (wdata, n_shifts * len, TY_DOUBLE)
                    if (wdata == NULL) 
                        call error (1, ERRMALLOC)
                }
		else
		    wdata = NULL

		# This first file will be used as a template to create the
		# output and granularity files.	 These files cannot be created
		# now, because the lengths will include the magnitude of
		# the shifts, which is not known yet.
		template_out = d_file
		template_wave = w_file

		# For WCS transformations.
		call salloc (vx, len, TY_DOUBLE)
		call salloc (vy, len, TY_DOUBLE)
		call salloc (adx, len, TY_DOUBLE)
		do ix = 0, len-1 {
		    Memd[vx+ix] = ix+1
		    Memd[vy+ix] = 1.d0
		}
		
	    }

	    # Check that the length of the data are the same.
	    if (len != OD_LEN(d_file)) {
		call eprintf ("specalign: file %s has bad length, skipping...\n")
		call pargstr (Memc[file_name])
		next
	    }

	    # If the shift is undefined, then skip it
	    if (NULB(null_shift,i)) {
		call eprintf ("specalign: shift for %s is undefined, skipping...\n")
		call pargstr (Memc[file_name])
		next
	    }

	    # If the granularity offset is undefined, just default it.
	    if (NULB(null_goff,i))
		GOFF(i) = DEFAULT_GOFF
	    
	    # Read the data.
	    ns = ns + 1
	    call od_get (d_file, DATA(ns))
	    SHIFT(ns) = SHIFT(i)
	    GOFF(ns) = GOFF(i)

	    # Get the wavelengths, if present.
	    if (wave_col != NULL) {
		if (wave_wcs) {
		    if (streq (OD_WSYS(d_file), "multispec"))
			call mw_v2trand (OD_LW(d_file), Memd[vx], Memd[vy],
					 WDATA(ns), Memd[adx], len)
		    else
			call mw_v1trand (OD_LW(d_file), Memd[vx], WDATA(ns),
					 len)
		} else
		    call od_get (w_file, WDATA(ns))
		WSHIFT(ns) = WSHIFT(i)
	    }
	}

	# Close the remaining file, if it isn't the first one..
	if (n_files > 1)
	    call od_unmap (d_file)
	if (wave_col != NULL && w_file != template_wave)
	    call od_unmap (w_file)
	
	# Make sure some data was found.
	if (ns <= 1)
	    call error (1, "specalign: no data to find shifts for")
	n_shifts = ns

        # If RSFUNC is RS_NONE, then only integral shifts are to be used.  This
        # option is compatible with the original version of this program which
        # only offered integral shifting.
        if (rsfunc == RS_NONE) {
            call salloc (stemp, n_shifts, TY_INT)
            call salloc (gtemp, n_shifts, TY_INT)
            do i = 1, n_shifts {
                Memi[stemp+i-1] = nint(SHIFT(i))
                Memi[gtemp+i-1] = nint(GOFF(i))
            }
            call achtid (Memi[stemp], SHIFT(1), n_shifts)
            call achtid (Memi[gtemp], GOFF(1), n_shifts)
        }

	# Make sure all the shifts are positive.  This is so the resulting
	# array indicies are always calculated positive.
	call anegd (SHIFT(1), SHIFT(1), n_shifts)
	call alimd (SHIFT(1), n_shifts, dmin, dmax)
	call asubkd (SHIFT(1), dmin, SHIFT(1), n_shifts)

	# Determine the length of the resulting output spectrum.
        tmplen = len + dmax - dmin
        olen   = int(tmplen)
        if ((tmplen - olen) > RS_MINDIST)
            olen = olen + 1

	# Make sure all the shifts are positive.  This is so the resulting
	# array indicies are always calculated positive.
	call anegd (GOFF(1), GOFF(1), n_shifts)
	call alimd (GOFF(1), n_shifts, dmin, dmax)
	call asubkd (GOFF(1), dmin, GOFF(1), n_shifts)

	# Determine the length of the resulting granularity.
        tmplen = len + dmax - dmin
        glen   = int(tmplen)
        if ((tmplen - glen) > RS_MINDIST)
            glen = glen + 1

        # The length of the granularity must be at least the length of
        # the longest individual shifted spectral row which is len or len+1.
        # Determine if any of the individual spectral shifts will 
        # produce a shifted array longer than the original length of len.
        if (rsfunc != RS_NONE) {
            do i = 1, n_shifts {
                residual = SHIFT(i) - int(SHIFT(i))
                if ((residual > RS_MINDIST) && (glen < (len+1))) {
                    glen = len + 1
                    break
                }
            }
        }

	# Make sure all the shifts are positive.  This is so the resulting
	# array indicies are always calculated positive.
        if (wshift != NULL) {
            call anegi (WSHIFT(1), WSHIFT(1), n_shifts)
            call alimi (WSHIFT(1), n_shifts, imin, imax)
            call asubki (WSHIFT(1), imin, WSHIFT(1), n_shifts)

            # Determine the length of the resulting output spectrum.
	    wlen = len + imax - imin
        } else
            wlen = 0
        
	# Close the table.
	call tbtclo (in)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of sa_read.
#---------------------------------------------------------------------------
procedure amovkb (k, v, n)

bool	k			# I:  The constant to move into the vector.
bool	v[n]			# O:  The vector to fill with constant.
int	n			# I:  Length of the vector.

# Misc.
int	i			# Index.

begin
	do i = 1, n
	    v[i] = k
end
#---------------------------------------------------------------------------
# End of amovkb
#---------------------------------------------------------------------------
