include <imhdr.h>
include <math/curfit.h>
include <math/iminterp.h>
include	<od.h>
include "sa_defines.h"

# Where to go when no iterations should be performed.
define  all_done_            10

# Error messages.
define  ERRMALLOC            "SPECALIGN: No memory available for allocation."

# Some initial values.
define  GRAN_INIT_VALUE      1.d0
define  MASK_INIT_VALUE      1.d0

# Shift definitions.
define  RESAMP_FUNC          "|none|linear|spline3|poly3|poly5|"

#---------------------------------------------------------------------------
.help specalign Dec96 source
.ih
NAME
specalign -- Combine spectra that are offset in pixel space.
.ih
DISCUSSION
Refer to the user-level help for functionality.  Programming notes may
appear below.
.endhelp
#---------------------------------------------------------------------------
procedure t_specalign

#
# Version 2.0    M. De La Pena   Dec 1996   Mods for sub-pixel shifting of
#                                           the spectral data via resampling 
#                                           by a chosen method.
#                Phil Hodge      Aug 2007   In the "Calculate the wavelengths"
#                                           section, allocate dwshift to be of
#                                           length n_shifts rather than wlen.

# Shift information.
pointer goff                    # Array of granularity offsets.
int     n_shifts                # Number of shifts, hence spectra, there are.
pointer shift                   # Array of shifts for input spectra.

# Data arrays.
pointer data                    # Array containing the spectral data.
pointer sdata                   # Array containing the shifted spectral data.
pointer sd_weight               # Array of weights for the shifted spectral data.
pointer g_err                   # Granularity error file.
pointer g_err_data              # Granularity standard deviation.
int     glen                    # Granularity length.
pointer gran                    # OD file descriptor for granularity.
pointer gran_data               # Resultant granularity.
pointer gran_mask               # Mask for granularity.
int     len                     # Length of input spectral data.
pointer slen                    # Array of lengths for each of row of shifted data.
pointer o_err                   # Output spectrum error file.
pointer o_err_data              # Final spectrum standard deviation.
int     olen                    # Length of output spectrum.
pointer out                     # OD file descriptor for output spectrum.
pointer out_data                # Resultant combined spectrum.
pointer out_mask                # Mask for output spectrum.

# File i/o.
pointer od_map()                # Open the one-dimensional data file.

# Parameters for the data combining.
int     rsfunc                  # Function to shift/resample spectral data.
double  delta                   # Damping factor between each iteration.
int     func                    # Function to fit to wavelengths.
int     korder                  # Order to preserve while smoothing.
int     ksize                   # Size of the smoothing kernel.
int     niter                   # Number of iterations to perform.
int     nterms                  # Number of terms to wavelength function.

# Wavelengths.
pointer w_out                   # Output wavelength table.
pointer wave                    # Wavelength file descriptor.
pointer wdata                   # Input wavelength data.
pointer swdata                  # Array containing shifted wavelength data.
pointer sw_weight               # Array of weights for the shifted wavelength data.
pointer	w_err			# Error on wavelength.
int	wlen			# Length of wavelength array.
pointer	wshift			# Array of wavelength shifts.
pointer dwshift                 # Double precision version of wshift.

# Misc.
double  clgetd()                # Get double-valued task parameter.
int     clgeti()                # Get integer-valued task parameter.
int     clgwrd()                # Get enumerated string value.
pointer px                      # Generic pointer.
pointer sp                      # Stack pointer.
int     strlen()                # Get length of string.
bool    strne()                 # Are strings not equal?
pointer sx                      # Generic string.
pointer template_out		# File template for output images.
pointer	template_wave		# File template for output wavelengths.

begin
        call smark (sp)
        call salloc (sx, max (SZ_LINE, SZ_PATHNAME), TY_CHAR)

	# Get resampling function to use on the spectral data.
        # The "none" option uses the default of integral shift/co-add/average
        iferr (rsfunc = clgwrd ("resamp_func", Memc[sx], SZ_LINE, RESAMP_FUNC))
	    rsfunc = RS_NONE

        # Get the input table information.
	wdata  = NULL
	wshift = NULL
        call sa_read(data, wdata, goff, shift, wshift, template_out, 
                     template_wave, rsfunc, n_shifts, len, olen, wlen, glen)
      
        # Open the output file.
        call clgstr ("output", Memc[sx], SZ_LINE)
        if (strlen (Memc[sx]) <= 0)
            call error (1, "specalign: no output specified")
        out = od_map (Memc[sx], NEW_COPY, template_out)
        call od_set_len (out, olen)
        call salloc (out_data, olen, TY_DOUBLE)

        # Open error file.
        call clgstr ("spec_err", Memc[sx], SZ_LINE)
        o_err = NULL
        if (strlen (Memc[sx]) > 0)
            o_err = od_map (Memc[sx], NEW_COPY, out)
        call salloc (o_err_data, olen, TY_DOUBLE)

	# Get function to use on the wavelength.  If the function is not
	# defined, then leave the wavelengths in their currently sized
	# table.
        iferr (func = clgwrd ("function", Memc[sx], SZ_LINE, CV_FUNCTIONS))
	    func = 0
        nterms  = clgeti ("nterms")
	
        # Open the wavelength file.  If none is specified, forget about
        # about the wavelength data.  If the image is "WCS", then write
        # the fit to the output header, don't open a wavelength file.
        # This leaves the following conditions:
        #   wdata == NULL                 -- No wavelengths, don't bother
        #   wdata != NULL && wave == NULL -- Write to output WCS.
        #   wdata != NULL && wave != NULL -- Write to wavelength file.
        wave = NULL
        if (wdata != NULL) {
            call clgstr ("wavelength", Memc[sx], SZ_LINE)
            if (strlen (Memc[sx]) <= 0) {
                call mfree (wdata, TY_DOUBLE)
            } else {
                call salloc (w_out, wlen, TY_DOUBLE)
                if (strne (Memc[sx], "WCS")) {
                    wave = od_map (Memc[sx], NEW_COPY, template_wave)
		    call od_set_len (wave, olen)
		}
            }   
        }

        # Retrieve various other parameters.
        niter = clgeti ("niter")
        delta = clgetd ("delta")
        ksize = clgeti ("kernel_size")
        korder = clgeti ("kernel_order")

        # Allocate the 2D array to hold the shifted data.
        call malloc (sdata, n_shifts * olen, TY_DOUBLE)
        if (sdata == NULL) 
            call error (1, ERRMALLOC)

        # Allocate the 2D array to hold the weights for the shifted data.
        call malloc (sd_weight, n_shifts * olen, TY_DOUBLE)
        if (sd_weight == NULL) 
            call error (1, ERRMALLOC)

        # Allocate an array to hold the lengths of individual rows of shifted data.
        call salloc (slen, n_shifts, TY_INT)

        # Create the initial guess for the spectral data by the
        # chosen resampling method.  RS_NONE uses the original 
        # integral shift/co-add/average method.
        call sa_shiftadd (Memd[data], Memd[shift], Memd[sdata],Memd[sd_weight], 
                          Memd[out_data], Memd[o_err_data], n_shifts, len,
                          Memi[slen], olen, rsfunc)

        # If the number of iterations is 0, then this is the result.  No
        # granularity can be calculated, no masks are used, and no other
        # initializations apply.
        if (niter <=0)
            goto all_done_

        # Open the granularity with the same characteristics as
        # the output.
        call clgstr ("granularity", Memc[sx], SZ_LINE)
        if (strlen (Memc[sx]) <= 0) {
            call eprintf ("specalign: no granularity file specified, no granularity will be output\n")
            gran = NULL
        } else {
            gran = od_map (Memc[sx], NEW_COPY, out)
            OD_LEN(gran) = glen
            IM_LEN(OD_FD(gran),1) = glen
        }
        call salloc (gran_data, glen, TY_DOUBLE)

        # Open granularity error file.
        call clgstr ("gran_err", Memc[sx], SZ_LINE)
        g_err = NULL
        if (strlen (Memc[sx]) > 0)
            g_err = od_map (Memc[sx], NEW_COPY, gran)
        call salloc (g_err_data, glen, TY_DOUBLE)
        
        # Get the output spectral initialization.
        call clgstr ("spec_init", Memc[sx], SZ_PATHNAME)
        if (strlen (Memc[sx]) > 0) {
            px = od_map (Memc[sx], READ_ONLY, NULL)
            if (OD_LEN(px) != olen)
                call error (1, "specalign: Initialization not same length of final output")
            call od_get (px, Memd[out_data])
            call od_unmap (px)
        }

        # Get the output mask.
        call salloc (out_mask, olen, TY_DOUBLE)
        call clgstr ("spec_weight", Memc[sx], SZ_PATHNAME)
        if (strlen (Memc[sx]) > 0) {
            px = od_map (Memc[sx], READ_ONLY, NULL)
            if (OD_LEN(px) != olen)
                call error (1, "specalign: Spectrum mask not same length as final output")
            call od_get (px, Memd[out_mask])
            call od_unmap (px)
        } else
            call amovkd (MASK_INIT_VALUE, Memd[out_mask], olen)

        # Get the granularity initialization.
        call clgstr ("gran_init", Memc[sx], SZ_PATHNAME)
        if (strlen (Memc[sx]) > 0) {
            px = od_map (Memc[sx], READ_ONLY, NULL)
            if (OD_LEN(px) != glen)
                call error (1, "specalign: Granularity initialization not same length as final granularity")
            call od_get (px, Memd[gran_data])
            call od_unmap (px)
        } else
            call amovkd (GRAN_INIT_VALUE, Memd[gran_data], glen)

        # Get the granularity mask.
        call salloc (gran_mask, glen, TY_DOUBLE)
        call clgstr ("gran_weight", Memc[sx], SZ_PATHNAME)
        if (strlen (Memc[sx]) > 0) {
            px = od_map (Memc[sx], READ_ONLY, NULL)
            if (OD_LEN(px) != glen)
                call error (1, "specalign: Granularity mask not same length as final granularity")
            call od_get (px, Memd[gran_mask])
            call od_unmap (px)
        } else
            call amovkd (MASK_INIT_VALUE, Memd[gran_mask], glen)

        # Perform the tomography to retrieve the output spectrum
        # and photocathode granularity.
        call sa_comb (Memd[sdata], Memd[sd_weight], delta, Memd[shift], 
                      Memd[goff], Memd[gran_mask], Memd[gran_data], 
                      Memd[g_err_data], Memd[out_mask], Memd[out_data], 
                      Memd[o_err_data], len, Memi[slen], niter, ksize, 
                      korder, n_shifts, glen, olen, rsfunc)

        # Write the granularity error.
        call od_put (g_err, Memd[g_err_data])
        call od_unmap (g_err)

        # Write the granularity.
        call od_put (gran, Memd[gran_data])
        call od_unmap (gran)

        # Come here if no iterations were to be performed.
all_done_

        # Write the output spectrum error.
        call od_put (o_err, Memd[o_err_data])
        call od_unmap (o_err)
            
        # Calculate the wavelengths.
        if (wdata != NULL) {

            # Allocate the 2D array to hold the shifted wavelength data.
            call malloc (swdata, n_shifts * wlen, TY_DOUBLE)
            if (swdata == NULL) 
                call error (1, ERRMALLOC)

            # Allocate the 2D array to hold the weights for the shifted wavelength data.
            call malloc (sw_weight, n_shifts * wlen, TY_DOUBLE)
            if (sw_weight == NULL) 
                call error (1, ERRMALLOC)

	    call salloc (w_err, wlen, TY_DOUBLE)
	    call salloc (dwshift, n_shifts, TY_DOUBLE)
            call achtid (Memi[wshift], Memd[dwshift], n_shifts)
            call sa_shiftadd (Memd[wdata], Memd[dwshift], Memd[swdata], 
                              Memd[sw_weight], Memd[w_out], Memd[w_err], 
                              n_shifts, len, Memi[slen], wlen, RS_NONE)
            call sa_fit (func, nterms, Memd[w_out], wlen, wave, out, olen)
            call od_unmap (wave)
        }
        
        # Write the output spectrum.
        call od_put (out, Memd[out_data])
        call od_unmap (out)

        # That's all folks.
        call od_unmap (template_out)
	call od_unmap (template_wave)
        call mfree (goff, TY_DOUBLE)
        call mfree (shift, TY_DOUBLE)
	if (wshift != NULL)
	    call mfree (wshift, TY_INT)
	if (wdata != NULL) {
	    call mfree (wdata, TY_DOUBLE)
            call mfree (swdata, TY_DOUBLE)
            call mfree (sw_weight, TY_DOUBLE)
        }
        call mfree (data, TY_DOUBLE)
        call mfree (sdata, TY_DOUBLE)
        call mfree (sd_weight, TY_DOUBLE)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_specalign
#---------------------------------------------------------------------------
