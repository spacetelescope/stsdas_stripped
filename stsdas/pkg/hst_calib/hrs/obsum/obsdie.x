include	"epoch.h"
include	"obsum.h"

#---------------------------------------------------------------------------
.help obs_diode Oct93 source
.ih
NAME
obs_diode -- Print out science diode information.
.endhelp
#---------------------------------------------------------------------------
procedure obs_diode (im, time, primary_group, root, o)

pointer	im[ARB]			# I:  The image descriptor array.
double	time[ARB]		# I:  Time array.
int	primary_group		# I:  The primary group number.
char	root[ARB]		# I:  Rootname.
pointer	o			# I:  The output descriptor.

# Declarations.
double	fday			# Fractional day.

real	asumi()			# Sum of a long array.

int	day, doy, month, year	# Dates.
int	i, j			# Generic.
int	imgeti()		# Get integer header parameter.
int	andi(), shifti()

pointer	date_obs		# Date of observation.
pointer	imgl1i()		# Get line of long data.
pointer	imgl1s()		# Get line of short data.
pointer	sci			# Line of science data.
pointer	sp			# Stack Pointer.
pointer	sx    			# Generic string.
pointer	time_obs		# Time of observation.
pointer	x0h			# Line of extracted data.

string	cmonth	"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC"
begin
	call smark (sp)
	call salloc (date_obs, SZ_LINE, TY_CHAR)
	call salloc (time_obs, SZ_LINE, TY_CHAR)
	call salloc (sx, SZ_LINE, TY_CHAR)
	
	# Convert the packet time to a readable format.
	call to_ymd (time[ZSCI], year, month, day, doy, fday, NEWSTYLE)
	call sprintf (Memc[date_obs], SZ_LINE, "%2d-%3.3s-%4d")
	call pargi (day)
	call pargstr (cmonth[3*(month-1)+1])
	call pargi (year)
	call sprintf (Memc[time_obs], SZ_LINE, "%11.2h")
	call pargd (24. * fday)

	# Read in the data.
	x0h = imgl1s (im[ZX0H])
	sci = imgl1i (im[ZSCI])

	# Format and output the first line.
	call sprintf (Memc[sx], SZ_LINE, " S. C. TIME=%s %s FILLCNT=%4d   ERRCNT=%4d\n")
	call pargstr (Memc[date_obs])
	call pargstr (Memc[time_obs])
	call pargi (imgeti (im[ZSCI], "FILLCNT"))
	call pargi (imgeti (im[ZSCI], "ERRCNT"))
	call obs_print_out (o, Memc[sx])

	# Line 2
	call sprintf (Memc[sx], SZ_LINE, "%s   PFC=%4d                 ZFOBSNUM%5d  BIN%3d\n")
	call pargstr (root)
	call pargi (imgeti (im[ZSCI], "PKTFMT"))
	call pargi (primary_group - 1)
        i = Mems[x0h+23]
        call pargi (andi(i,177777b))
	call obs_print_out (o, Memc[sx])

	# Print out the science data.
	call obs_print_out (o,"  DIODE    -0-    -1-    -2-    -3-    -4-    -5-    -6-    -7-    -8-    -9-\n")
	call sprintf (Memc[sx], SZ_LINE, "      0      -%7d%7d%7d%7d%7d%7d%7d%7d%7d\n")
 	do i = 0, 5 {
	    j = Mems[x0h+i]
	    call pargi (andi(j,177777b))
 	}
	do i = 0, 2
	    call pargi (Memi[sci+i])
	call obs_print_out (o, Memc[sx])
	
	do j = 3, 483, 10 {
	    call sprintf (Memc[sx], SZ_LINE, "%7d%7d%7d%7d%7d%7d%7d%7d%7d%7d%7d\n")
	    call pargi(j+7)
	    do i = j, j+9
		call pargi (Memi[sci+i])
	    call obs_print_out (o, Memc[sx])
	}

	call sprintf (Memc[sx], SZ_LINE, "%7d%7d%7d%7d%7d%7d%7d%7d%7d%7d%7d\n%7d%7d%7d%7d    SCIENCE DIODE (7-506) TOTAL =%9d\n")
	call pargi (500)
	do i = 493, 499
	    call pargi (Memi[sci+i])
 	do i = 6, 8 {
             j = Mems[x0h+i]
             call pargi (andi(j,177777b))
 	}
	call pargi (510)
 	do i = 9, 11 {
             j = Mems[x0h+i]
             call pargi (andi(j,177777b))
 	}
	call pargr (asumi (Memi[sci], 500))
	call obs_print_out (o, Memc[sx])

	# Now the fun part.  Taking apart the bits of words from the
	# X0H file.
	call sprintf (Memc[sx], SZ_LINE, "50MS CNTR= %4d  OTFA=%8d.  ANTICOINC: SUM=%6d, THRESH=%3d, FLG=%1d, EN=%1d\n")
	i = Mems[x0h+12]
	call pargi (andi (shifti(i, -9), 177b))
	j = Mems[x0h+13]
	j = andi(j,65535)
	call pargi (andi(i,511)*65536+j)
 	i = Mems[x0h+14]
        call pargi (andi(i,177777b))
	i = Mems[x0h+15]
	call pargi (andi (shifti (i, -10), 77b))
	call pargi (andi(shifti(i,-9),1))
	call pargi (andi(shifti(i,-8),1))
	call obs_print_out (o, Memc[sx])

	call sprintf (Memc[sx], SZ_LINE, "SYS=%1d  TP: ADDR=%2d, EN=%1d    DWHON=%1d  THLEN=%1d  CARSL: POS=%6d, ERR=%1d, FTL=%1d\n")
	call pargi (andi (i,2))
	i = Mems[x0h+16]
	call pargi (andi (shifti (i, -11), 37b))
	call pargi (andi (shifti (i,-10),1))
	call pargi (andi (shifti (i,-8),1))
	call pargi (andi (shifti (i,-7),1))
	i = Mems[x0h+19]
	call pargi (andi (i, 65535))
	i = Mems[x0h+15]
	call pargi (andi (shifti (i,-5),7b))
	call pargi (andi (shifti (i,-4),1))
	call obs_print_out (o, Memc[sx])

	call sprintf (Memc[sx], SZ_LINE, "SHUTCL=%1d  LAMPS: EN1=%1d, EN2=%1d, SC1=%1d, SC2=%1d, FF=%1d  HOR DFL=%5d  VER DFL=%5d\n")
	call pargi (andi (shifti (i, -2), 3b))
	i= Mems[x0h+16]
	call pargi (andi( shifti (i,-5), 1))
	call pargi (andi( shifti (i,-4), 1))
	call pargi (andi( shifti (i,-3), 1))
	call pargi (andi( shifti (i,-2), 1))
	call pargi (andi( shifti (i,-1), 1))
	call pargs (Mems[x0h+17])
	call pargs (Mems[x0h+18])
	call obs_print_out (o, Memc[sx])

	call sprintf (Memc[sx], SZ_LINE, "TWEAK=%4d  INT TIME=%4d  COADDS=%4d\n\f")
	i = Mems[x0h+20]
	call pargi (andi (shifti (i, -8), 255))
	call pargi (andi (i, 255))
	i = Mems[x0h+22]
	call pargi (andi (i, 65535))
	call obs_print_out (o, Memc[sx])
	
	# That's all folks.
	call sfree (sp)
	
end
#---------------------------------------------------------------------------
# End of obs_diode
#---------------------------------------------------------------------------


