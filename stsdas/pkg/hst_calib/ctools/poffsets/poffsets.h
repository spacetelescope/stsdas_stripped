#---------------------------------------------------------------------------
.help poffsets.h Feb93 source
.ih
NAME
poffsets.h  -- Defines to use with the poffsets task.
.endhelp
#---------------------------------------------------------------------------

#---
# How much of the wavelength arrays should be sampled, i.e. every
# 5 pixel.
define  SAMPLE          5

#---
# Define what methods are used to search for the shift.
# NOTE: The defines must be the same order/value as appears in METHOD_DICT
define  METHOD_DICT     "|wave|max"
define  METHOD_WAVE     1
define  METHOD_MAX      2

#---
# Define where the wavelengths can be found.
# NOTE: The defines must be the same order/value as appears in WSOURCE_DICT
define  WSOURCE_DICT    "|image|wcs|none"
define  WSOURCE_IMAGE   1
define  WSOURCE_WCS     2
define  WSOURCE_NONE    3

#---
# The basic memory structure for poffsets.
define  IN_LIST         Memi[$1]        # Input file list descriptor.
define  ZERO            Memi[$1+1]      # Zero point 1D descriptor.
define  ZERO_WAVE       Memi[$1+2]      # Zero wavelength file.
define  DELTA           Memi[$1+3]      # Delta around guess to search.
define  WAVE_LIST       Memi[$1+4]      # Wavelength list.
define  WINDOW          Memi[$1+5]      # Maximum pixel shift allowed.
define  CORTAB          Memi[$1+6]      # Correlation table output.
define  OUTTAB          Memi[$1+7]      # Output offset table.
define  SECTION         Memi[$1+8]      # Section to split data into to corre.
define  FW              Memi[$1+9]      # Filter width.
define  ZFW             Memi[$1+10]     # Zero-point filter width.
define  NSIG            Memr[$1+11]     # Sigma rejection for sections.
define  VERBOSE         Memi[$1+12]     # YES to print messages.
define  METHOD          Memi[$1+13]     # How to search for shift.
define  USECORR         Memi[$1+15]     # Calculate correlation?
define  INTERACTIVE     Memi[$1+16]     # Examine correlations interact.
define  GP              Memi[$1+17]     # Graphics descriptor.
define  WSOURCE         Memi[$1+18]     # Source of wavlengths.
define  NITER           Memi[$1+19]     # Maximum rejection iterations.
define  POF_SZ_POF      20              # Size of the memory.
#---------------------------------------------------------------------------
# End of poffsets.h
#---------------------------------------------------------------------------
