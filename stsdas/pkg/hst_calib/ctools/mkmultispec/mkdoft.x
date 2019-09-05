include <math/curfit.h>
include <pkg/gtools.h>
include <imhdr.h>
include <imio.h>
include "mkms.h"

#---------------------------------------------------------------------------
.help mk_dofit Mar93 source
.ih
NAME
mk_dofit -- Fit the functions to the wavelength tables.
.ih
USAGE
call mk_dofit (wave, wght, fit, coeff_fmt, interactive)
.ih
ARGUMENTS
.ls wave (pointer :input)
The IO descriptor of the wavelength table.
.le
.ls wght (pointer :input)
The IO descriptr of the weight table.
.le
.ls fit (pointer :input)
The FIT descriptor.
.le
.ls coeff_fmt (char[ARB] :input)
The format to use when writing coefficients to the MWCS descriptor.
.le
.ls interactive (boolean :input/output)
TRUE if performing interactive fitting.
.le
.endhelp
#---------------------------------------------------------------------------
procedure mk_dofit (wave, wght, fit, coeff_fmt, interactive)

pointer wave                    # I:  Wavelength IO descriptor.
pointer wght                    # I:  Weight IO descriptor.
pointer fit                     # I:  The FIT descriptor.
char    coeff_fmt[ARB]          # I:  Format to write the coefficients with.
bool    interactive             # IO: TRUE if doing interactive fitting.

int     cfit                    # Current group/fit operating on.
pointer cv                      # The current CURFIT descriptor.
int     i1, i2, i3, i4          # Generic.
pointer mk_create()             # Create the MWCS.
bool    mk_icg_fitd()           # Do the interactive fitting.
real    rx                      # Generic.
pointer sp                      # Stack pointer.
pointer tmp_str                 # Generic.

begin
        # Memory.
        call smark(sp)
        call salloc (tmp_str, SZ_LINE, TY_CHAR)

        # If fitting to anything other than a TABLE, setup for the fit.
        if (FIT_INIT_FUNC(fit) != MK_TABLE) {
            rx = min (FIT_X(fit,1), FIT_X(fit,FIT_LEN(fit)))
            call ic_putr (FIT_IC(fit), "xmin", rx)
            rx = max (FIT_X(fit,1), FIT_X(fit,FIT_LEN(fit)))
            call ic_putr (FIT_IC(fit), "xmax", rx)        
        }

        # Go through each fit.
        do cfit = 1, FIT_N_MW(fit) {

            # Get the data
            call mk_data (wave, cfit, FIT_Y(fit,1))
            call mk_data (wght, cfit, FIT_W(fit,1))

            # If creating table MULTISPEC headers, copy the wavelengths
            # into an array and set the current CV pointer to that array.
            if (FIT_INIT_FUNC(fit) == MK_TABLE) {
                call malloc (cv, FIT_LEN(fit), TY_DOUBLE)
                call amovd (FIT_Y(fit,1), Memd[cv], FIT_LEN(fit))
            }

            # Else, do interactive fitting.
            else if (interactive) {

                # Create a title.
                if (IO_TYPE(wave) == IMAGE) {
                    call sprintf (Memc[tmp_str], SZ_LINE, "%s[%d/%d]")
                    call pargstr (IO_NAME(wave))
                    call pargi (cfit)
                    call pargi (IO_NGRP(wave))
                } else {
                    call sprintf (Memc[tmp_str], SZ_LINE, "%s")
                    call pargstr (IO_NAME(wave))
                }
                call gt_sets (FIT_GT(fit), GTTITLE, Memc[tmp_str])
                
                # Perform the fit.
                interactive = ! mk_icg_fitd (FIT_IC(fit), FIT_GP(fit),
                                             "cursor", FIT_GT(fit),
                                             cv, FIT_X(fit,1),
                                             FIT_Y(fit,1), FIT_W(fit,1),
                                             FIT_LEN(fit))
            }

            # Else, just do the remaining fits.
            else {
                i1 = YES; i2 = YES; i3 = YES; i4 = YES
                call ic_fitd (FIT_IC(fit), cv, FIT_X(fit,1),
                              FIT_Y(fit,1), FIT_W(fit,1), FIT_LEN(fit),
                              i1, i2, i3, i4)
            }

            # Create the MWCS system.
            FIT_MW(fit,cfit) = mk_create (FIT_LEN(fit), FIT_INIT_FUNC(fit), cv,
                                          FIT_LABEL(fit), FIT_UNITS(fit),
                                          coeff_fmt)
            if (FIT_INIT_FUNC(fit) == MK_TABLE) {
                iferr (call mfree(cv, TY_DOUBLE))
                    ;
            } else {
                iferr (call dcvfree (cv))
                    ;
            }   
        }
        
        # That's all folks.
        call sfree(sp)
end
