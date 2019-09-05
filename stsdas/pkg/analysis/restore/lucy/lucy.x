include <error.h>
include <imhdr.h>
include "lucyfft.h"

# lucy  -- restore an image by the lucy method
# This routine uses the lucy algorithm to deconvolve an image 
# Inputs - input image, psf, model (used as first guess if given)
#          constant noise, conversion from DN to counts, number of iterations
# Output - deconvolved image
# task will work for both one and two dimensional images.  Model must be
# same size as input image.
# Task LUCY created         29-Nov-1990       E.B. Stobie 
# Add background parameter  09-Sep-1992       E.B. Stobie
# Add masking function      30-Mar-1993       E.B. Stobie
# Add acceleration option   20-Sep-1993       E.B. Stobie
# Add turbo option          01-Dec-1993       E.B. Stobie
# In lucy1d, set npts       25-Mar-2010       Phil Hodge
# Ability to mask pixels allows the algorithm to extend beyond the
# bounds of the input image (by specifying xsizeout and ysizeout).
# Add ability to save intermediate results, both to distinct images


procedure turbo_lucy ()

#--

pointer         infile               # input image name
pointer         psffile              # input psf image name
pointer         modfile              # input model image (optional first guess)
pointer         outfile              # output restored image name
pointer         backstr              # input background string
pointer         maskfile             # input mask image
pointer         maskout              # name of mask image to output
pointer         wgtfile              # input weights image
pointer         acceleration         # string to specify accleration method

int             niter                # number of desired iterations
int             nsave                # save intermediate result every nsave iter
int             xout                 # X dimension of output image
int             yout                 # Y dimension of output image
int             goodpix              # good pixels in mask identified by 1 or 0
int             update               # save intermediate results in output image
int             accelerate           # 0=standard, 1=accelerated, 2=turbo

real            adu                  # number of counts per DN
real            noise                # constant noise
real            nsigma               # sigma factor for masking pixels
real            limchisq             # chi-square limit for convergence
real            backval              # constant background value

bool            center               # center input data in output
bool            verbose              # print list of masked pixels, etc.

pointer         sp                   # stack pointer
pointer         dat                  # pointer to data array structure
pointer         rim, pim             # pointers to input, psf images
pointer         lim, mim             # pointers to output, model images
pointer         wim                  # pointer to weights image
pointer         bim, oim             # pointer to background, output mask images
pointer         lbuf                 # row pointer
pointer         pmask                # pointer to mask image

short           one

int             rndim                # number of dimensions of input image
int             rnpts, pnpts         # number of points for input, PSF
int             rnrows, pnrows       # number of rows for input, PSF
int             npts, nrows          # maximum number of points, rows
int             xsize, ysize         # size of output image
int             firstrow             # first row of mask - real data
int             lastrow              # last row of mask - real data
int             bnpix                # size of background array
int             xloc, yloc           # array offsets for data
int             pxloc, pyloc         # array offsets for PSF
int             npix, titer          # total number of pixels
int             i, ii
int             nch, imerr, mch

real            gmean, chisq

double          sumd, sumw

pointer         immap()
pointer         imgl1r(), imgl1s()
pointer         impl1s(), impl2s()
pointer         impl1r(), impl2r()

int             clgeti(), imgeti()
int             ctor(), clgwrd()

real            clgetr()

bool            clgetb()

define          trim         0.0
define          oner         1.0

begin

                call smark (sp)
                call salloc (infile,  SZ_FNAME, TY_CHAR)
                call salloc (psffile, SZ_FNAME, TY_CHAR)
                call salloc (modfile, SZ_FNAME, TY_CHAR)
                call salloc (outfile, SZ_FNAME, TY_CHAR)
                call salloc (backstr, SZ_FNAME, TY_CHAR)
                call salloc (maskfile, SZ_FNAME, TY_CHAR)
                call salloc (maskout, SZ_FNAME, TY_CHAR)
                call salloc (wgtfile, SZ_FNAME, TY_CHAR)
                call salloc (acceleration, SZ_FNAME, TY_CHAR)
 
                call clgstr ("input",  Memc[infile],  SZ_FNAME)
                call clgstr ("psf",    Memc[psffile], SZ_FNAME) 
                call clgstr ("model",  Memc[modfile], SZ_FNAME)
                call clgstr ("background", Memc[backstr], SZ_FNAME)
                call clgstr ("output", Memc[outfile], SZ_FNAME)
                call clgstr ("maskin", Memc[maskfile], SZ_FNAME)
                call clgstr ("weight", Memc[wgtfile], SZ_FNAME)
                call clgstr ("maskout", Memc[maskout], SZ_FNAME)

                niter = clgeti ("niter")
                adu = clgetr ("adu")
                noise = clgetr ("noise")
                limchisq = clgetr("limchisq")
                goodpix = clgeti ("goodpixval")
                nsigma = clgetr ("nsigma")
                xout = clgeti ("xsizeout")
                yout = clgeti ("ysizeout")
                nsave = clgeti ("nsave")
                if (clgetb("center")) center = TRUE
                else center = FALSE
                update = clgeti ("update")
                if (clgetb("verbose")) verbose = TRUE
                else verbose = FALSE

                accelerate = clgwrd ("accel_method", Memc[acceleration], 
                             SZ_FNAME, "|none|standard|turbo") - 1

                one = 1

                rim = immap (Memc[infile], READ_ONLY, 0)
                pim = immap (Memc[psffile], READ_ONLY, 0)

                rndim = IM_NDIM(rim)

#               dimensions of PSF image must match dimensions of input image  
#               if dimension of input image = 1 & dimension of PSF > 1,
#               an error message is given
#               for case of 1-D input image must specify 1-D image section (row)
#               for PSF or integrate PSF to 1-D image

                if (IM_NDIM(pim) != rndim) {
                   call imunmap (rim)
                   call imunmap (pim)
                   call sfree (sp)
                   call eprintf (
             "PSF image dimensionality does not match input image - aborting\n")
                   call erract (EA_ERROR)
                }


                rnpts = IM_LEN(rim,1)
                pnpts = IM_LEN(pim,1)
                if (xout > 0 && xout < rnpts) {
                   call imunmap (rim)
                   call imunmap (pim)
                   call sfree (sp)
                   call eprintf (
       "Error in xsizeout, cannot specify output image smaller than input\n")
                   call erract (EA_ERROR)
                } 
                xsize = max (rnpts, xout) 
                npts  = max(rnpts, pnpts, xout)

                if (rndim == 1) {
                   rnrows = 1
                   pnrows = 1
                   yout = 1
                } else {
                   rnrows = IM_LEN(rim,2)
                   pnrows = IM_LEN(pim,2)
                   if (yout > 0 && yout < rnrows) {
                      call imunmap (rim)
                      call imunmap (pim)
                      call sfree (sp)
                      call eprintf (
         "Error in ysizeout, cannot specify output image smaller than input\n")
                      call erract (EA_ERROR)
                   }  
                }
                ysize = max (rnrows, yout)
                nrows = max(rnrows, pnrows, yout)
                npix = npts * nrows

#               allocate data structures.  arrays are allocated as follows:
#               xdim = max (input(x), psf(x), xsizeout)
#               ydim = max (input(y), psf(y), ysizeout)

                call malloc (dat, LEN_DAT, TY_STRUCT)

                call calloc (PWEIGHT(dat), npix, TY_REAL)
                call calloc (PNORM(dat), npix, TY_REAL)
                call calloc (PLUCY(dat), npix, TY_REAL)

                NPTS(dat) = npts
                NROWS(dat) = nrows
                RNPTS(dat) = rnpts
                RNROWS(dat) = rnrows
                XOUT(dat) = xsize
                YOUT(dat) = ysize

#               move input image to LUCY array (PLUCY) starting at 
#               pixel(xloc, yloc), if center = no, xloc and yloc = 1
#               otherwise xloc and yloc = offset required to center
#               input data in the array 
#               LUCY array used as temporary storage

                if (center) {
                   xloc = (npts - rnpts)/2 + 1
                   yloc = (nrows - rnrows)/2 + 1
                } else {
                   xloc = 1
                   yloc = 1
                }

                if (rndim == 1) {
                   lbuf = imgl1r(rim)
                   call amovr (Memr[lbuf], Memr[PLUCY(dat)+xloc-1],rnpts)
                } else call movimr (rim, Memr[PLUCY(dat)],npts, nrows, rnpts,
                                    rnrows, xloc, yloc)

#               Check background, if image given allocate PBACK
#               size of internal array and center if requested
#               otherwise background array dimensioned to 1 and set equal
#               to backval if specified or 0. if not.

                imerr = 0
                if (Memc[backstr] != EOS) {
                   mch = 1
                   nch = ctor (Memc[backstr], mch, backval)
                   if (nch != 0) {
                       bnpix = 1
                       call calloc (PBACK(dat), bnpix, TY_REAL)
                       Memr[PBACK(dat)]=backval
                    } else {
                      bim = immap(Memc[backstr], READ_ONLY, 0)
                      call calloc (PBACK(dat), npix, TY_REAL)
                      if (IM_NDIM(bim) == rndim) {
                         if (IM_LEN(bim,1) == rnpts) {
                            if (rndim == 1) {
                               lbuf = imgl1r (bim)
                               call amovr (Memr[lbuf], Memr[PBACK(dat)+xloc-1],
                                           rnpts)
                               bnpix = npts
                            } else if (IM_LEN(bim,2) == rnrows) {
                               call movimr (bim, Memr[PBACK(dat)],
                                  npts, nrows, rnpts, rnrows, xloc, yloc)
                               bnpix = npts * nrows
                            } else imerr = 1
                         } else imerr = 1
                      } else imerr = 1
                      if (imerr != 0) {
                         call imunmap (pim)
                         call imunmap (rim)
                         call mfree (PBACK(dat), TY_REAL)
                         call mfree (PLUCY(dat), TY_REAL)
                         call mfree (PNORM(dat), TY_REAL)
                         call mfree (PWEIGHT(dat), TY_REAL)
                         call mfree (dat, TY_STRUCT)
                         call sfree (sp)
                         call eprintf (
          "Background image dimensions do not match input image - aborting \n")
                         call erract (EA_ERROR)
                      }
                      call imunmap (bim)
                   }
                } else {
                   bnpix = 1
                   call calloc (PBACK(dat), bnpix, TY_REAL)
                   Memr[PBACK(dat)] = 0.
               }

#               set up weight array, load if weight image given
#               otherwise initialize pixels in region of input data to 1
#               for all other pixels set to 0.

                imerr = 0
                if (Memc[wgtfile] != EOS) {
                   wim = immap (Memc[wgtfile], READ_ONLY, 0)
                   if (IM_NDIM(wim) == rndim) {
                      if (IM_LEN(wim, 1) == rnpts) {
                         if (rndim == 1) {   
                            lbuf = imgl1r(wim)
                            call amovr (Memr[lbuf], Memr[PWEIGHT(dat)+xloc-1], 
                                        rnpts)
                         } else if (IM_LEN(wim,2)==rnrows)
                              call movimr (wim, Memr[PWEIGHT(dat)], npts, 
                                           nrows, rnpts, rnrows, xloc, yloc)
                         else imerr = 1
                      } else imerr = 1
                   } else imerr = 1
                   call imunmap (wim)
                   if (imerr != 0) {
                      call imunmap (pim)
                      call imunmap (rim)
                      call mfree (PBACK(dat), TY_REAL)
                      call mfree (PLUCY(dat), TY_REAL)
                      call mfree (PNORM(dat), TY_REAL)
                      call mfree (PWEIGHT(dat), TY_REAL)
                      call mfree (dat, TY_STRUCT)
                      call sfree (sp)
                      call eprintf (
               "Weight image dimensions do not match input image -aborting\n")
                      call erract (EA_ERROR)
                   }
                } else {
                    firstrow = yloc
                    lastrow = firstrow + rnrows - 1
                    do i = firstrow, lastrow {
                       call amovkr (oner, Memr[PWEIGHT(dat)+(i-1)*npts+xloc-1], 
                                    rnpts)
                    }
                }

#               set up mask array, load if mask image given
#               otherwise initialize pixels in region of input data to 1
#               all other mask pixels set to 0.

                imerr = 0
                call calloc (pmask, npix, TY_SHORT)
                if (Memc[maskfile] != EOS) {
                   mim = immap (Memc[maskfile], READ_ONLY, 0)
                   if (IM_NDIM(mim) == rndim) {
                      if (IM_LEN(mim, 1) == rnpts) {
                         if (rndim == 1) {   
                            lbuf = imgl1s(mim)
                            call amovs (Mems[lbuf], Mems[pmask+xloc-1], 
                                        rnpts)
                         } else if (IM_LEN(mim,2)==rnrows)
                              call movims (mim, Mems[pmask], npts, nrows,
                                         rnpts, rnrows, xloc, yloc)
                         else imerr = 1
                      } else imerr = 1
                   } else imerr = 1
                   call imunmap (mim)
                   if (imerr != 0) {
                      call imunmap (pim)
                      call imunmap (rim)
                      call mfree (PBACK(dat), TY_REAL)
                      call mfree (PLUCY(dat), TY_REAL)
                      call mfree (PNORM(dat), TY_REAL)
                      call mfree (PWEIGHT(dat), TY_REAL)
                      call mfree (pmask, TY_SHORT)
                      call mfree (dat, TY_STRUCT)
                      call sfree (sp)
                      call eprintf (
               "Mask image dimensions do not match input image -aborting\n")
                      call erract (EA_ERROR)
                   }
                } else {
                    firstrow = yloc
                    lastrow = firstrow + rnrows - 1
                    do i = firstrow, lastrow {
                       call amovks (one, Mems[pmask+(i-1)*npts+xloc-1], 
                                    rnpts)
                    }
                    goodpix = 1
                }


#               If goodpixval = 0, reverse polarity of mask array
#               Check if there are any remaining bad pixels regardless
#               if mask image was input, 
#               pixels < - nsigma * sqrt((noise^2/adu+background)/adu) or
#               pixels < - noise^2/adu + background if nsigma = 0.
#               are automatically masked.  If term under sqrt < 0. result
#               is set to 0.

                if (rndim == 1) call maskd1 (Memr[PLUCY(dat)],Memr[PBACK(dat)],
                                Mems[pmask], npts, rnpts, bnpix, noise, 
                                adu, nsigma, goodpix, center)
                else call maskd2 (Memr[PLUCY(dat)], Memr[PBACK(dat)],
                                Mems[pmask], npts, nrows, rnpts, rnrows,
                                bnpix, noise, adu, nsigma, goodpix, center,
                                verbose)

#               output mask to image (maskout) if specified
#               only write out mask in region of input data

                if (Memc[maskout] != EOS) {
                   oim = immap(Memc[maskout], NEW_COPY,rim)
                   IM_LEN(oim,1) = rnpts
                   IM_NDIM(oim) = rndim
                   IM_PIXTYPE(oim) = TY_SHORT
                   if (rndim == 1) {
                      lbuf = impl1s(oim)
                      call amovs (Mems[pmask+xloc-1], Mems[lbuf], rnpts)
                   } else {
                      IM_LEN(oim,2) = rnrows
                      firstrow = yloc
                      lastrow = firstrow + rnrows -1
                      do i = firstrow, lastrow {                         
                         ii = i - yloc + 1
                         lbuf = impl2s(oim,ii)
                         call amovs (Mems[pmask+(i-1)*npts+xloc-1],
                                     Mems(lbuf], rnpts)
                      }
                   }
                   call imunmap(oim)
                }             

#               Set weight = weight * mask
#               gmean = sum (data*weight)/ sum (weight)
#               if mean used as first guess, it must be greater than 0.0

                sumd = 0.0d0
                sumw = 0.0d0
                do i = 1, npix {
                   Memr[PWEIGHT(dat)+i-1] = Memr[PWEIGHT(dat)+i-1] * 
                       real (Mems[pmask+i-1])
                   sumd = sumd + Memr[PLUCY(dat)+i-1] * Memr[PWEIGHT(dat)+i-1]
                   sumw = sumw + Memr[PWEIGHT(dat)+i-1]
                }
                gmean = sumd / sumw
                if (gmean < 0.) {
                   call printf ("Mean of input image (%g) negative - resetting to 1.0\n")
                     call pargr (gmean)
                   gmean = 1.0
                }
                call mfree (pmask, TY_SHORT)

#               open output image 
#               (size = max((input_x,xsizeout),(input_y,ysizeout))

                lim = immap (Memc[outfile], NEW_COPY, rim)
                IM_LEN(lim,1) = xsize
                IM_NDIM(lim) = rndim
                if (rndim == 1) {
                   lbuf = impl1r(lim)
                   call aclrr (Memr(lbuf),xsize)
                } else {
                   IM_LEN(lim,2) = ysize
                   lbuf = impl2r (lim,1)
                   call aclrr (Memr[lbuf],xsize)
                }
                IM_PIXTYPE(lim) = TY_REAL
                call imunmap (lim)

#               move psf image to psf array (PNORM) centering data in array
#               and trim data < 0. to 0. 

                pxloc = (npts - pnpts)/2 + 1
                if (rndim == 1) {
                   lbuf = imgl1r(pim)
                   call amovr(Memr[lbuf],Memr[PNORM(dat)+pxloc-1],pnpts)
                } else {
                   pyloc = (nrows - pnrows)/2 + 1
                   call movimr (pim, Memr[PNORM(dat)], npts, nrows, pnpts,
                                pnrows, pxloc, pyloc)
                }
                call imunmap (pim)
                call arltr (Memr[PNORM(dat)], npix, trim, trim)

#               if first guess given, load into lucy array
#               image size must match the output image size

                if (center) {
                   xloc = (npts - xsize)/2 + 1
                   yloc = (nrows - ysize)/2 + 1
                } else {
                   xloc = 1
                   yloc = 1
                }

                if (Memc[modfile] != EOS) {
                   imerr = 0
                   mim = immap (Memc[modfile], READ_ONLY, 0)
                   if (IM_NDIM(mim) == rndim) {
                      if (IM_LEN(mim,1) == xsize) {
                         if (rndim == 1) {
                            lbuf = imgl1r (mim)
                            call amovr (Memr[lbuf], Memr[PLUCY(dat)+xloc-1], 
                                        xsize)
                            iferr (titer = imgeti (mim,"NO_ITER")) titer = 0
                         } else if (IM_LEN(mim,2) == ysize) {   
                             call movimr (mim, Memr[PLUCY(dat)], npts, nrows, 
                                          xsize, ysize, xloc, yloc)
                             iferr (titer = imgeti (mim,"NO_ITER")) titer = 0
                         } else imerr = 1
                      } else imerr = 1
                   } else imerr = 1
                   call imunmap (mim)
                   if (imerr != 0) {
                      call imunmap (rim)
                      call mfree (PBACK(dat), TY_REAL)
                      call mfree (PLUCY(dat), TY_REAL)
                      call mfree (PNORM(dat), TY_REAL)
                      call mfree (PWEIGHT(dat), TY_REAL)
                      call mfree (dat, TY_STRUCT)
                      call sfree (sp)
                      call eprintf (
               "Model image dimensions do not match output image - aborting\n")
                      call erract (EA_ERROR)
                   }
                } else {
                   call aclrr (Memr[PLUCY(dat)], npix)
                   firstrow = yloc
                   lastrow = firstrow + ysize - 1
                   do i = firstrow, lastrow {
                      call amovkr (gmean, Memr[PLUCY(dat)+(i-1)*npts], xsize)
                   }
                   titer = 0
                }

 #              call deconvolution routine & store result in output image

          
                if (rndim == 1) call lucy1d(dat, adu, noise, niter, limchisq, 
                      rim, bnpix, nsave, Memc[outfile], titer, chisq, center,
                      update, accelerate, verbose)
                else call lucy2d(dat, adu, noise, niter, limchisq, rim, bnpix, 
                       nsave, Memc[outfile], titer, chisq, center, update, 
                       accelerate, verbose) 


                call imunmap(rim)
                lim = immap (Memc[outfile], READ_WRITE, 0)
                call dumpim (lim, Memr[PLUCY(dat)], npts, nrows, xsize, ysize,
                             rnpts, rnrows, titer, chisq, center) 
                call imunmap(lim)
                call mfree (PBACK(dat), TY_REAL)
                call mfree (PLUCY(dat), TY_REAL)
                call mfree (PNORM(dat), TY_REAL)
                call mfree (PWEIGHT(dat), TY_REAL)
                call mfree (dat, TY_STRUCT)

                call sfree (sp)
end

include "lucyfft.h"

# 1-D deconvolution 

procedure lucy1d (dat, adu, noise, niter, limchisq, rim, bnpts, nsave, 
                  outname, titer, chisq, center, update, accelerate, verbose)

pointer      dat             # pointer to data array structure
pointer      rim             # pointer to input image
int          niter           # number of iterations desired
int          nsave           # save intermediate images every nth time
int          bnpts           # number of points in background array
int          titer           # total iteration count
int          update          # save intermediate images in output image
int          accelerate      # acceleration method
real         adu             # number of counts per DN
real         noise           # constant noise
real         limchisq        # chi-squared limit for convergence
char         outname[SZ_FNAME]    # output file name
bool         center          # center input image 
bool         verbose         # print iterations of C, etc (accelerated modes)

pointer      work            # pointer to fft work area structure
pointer      pxx             # pointer to temporary complex array
pointer      pxr             # pointer to temporary real array
pointer      praw            # pointer to raw data array
pointer      bbuf            # pointer to read buffer
pointer      lim

int          i, amsh, xmax
int          npts, rnpts     # number of points in arrays, input image
int          nrows           # number of rows in arrays (=1)
int          xsize, ysize    # size of output image
int          iter            # iteration counter
int          xloc
int          count           # current iteration number

real         val, bval
real         scal, chisq  
real         convar
real         num, den
real         floor

real         chi2_d1()

char         out[SZ_FNAME]   # output file name for intermediate results

real         ahivr()

pointer      imgl1r()
pointer      immap()

begin

             convar = noise**2 /adu
             npts = NPTS(dat)
             call calloc (praw, npts, TY_REAL) 
             if (bnpts == 1) bval = Memr[PBACK(dat)]
             call ffta1d(dat, work, pxx, pxr, accelerate)
             rnpts = RNPTS(dat)
             nrows = 1
             xsize = XOUT(dat)
             ysize = 1
             if (center) xloc = (npts - rnpts)/2 + 1
             else xloc = 1

#            find maximum value of PSF and shift to pixel (1,1)

             val = ahivr (Memr[PNORM(dat)], npts)
             xmax = 1
             do i = 1, npts {
                if (Memr[PNORM(dat)+i-1] == val) {
                   xmax = i
                   break
                }
             }
             amsh = -xmax + 1
             call rwshift (npts, amsh, Memr[PNORM(dat)], Memr[pxr])
             call lfftf1 (work, Memr[pxr], Memx[PFPSF(dat)], npts)

#            normalize forward transform of psf by first pixel  

             scal = Memx[PFPSF(dat)]
             call advkxr (Memx[PFPSF(dat)], scal, Memx[PFPSF(dat)], npts)

#            ratio = weight
#            PNORM = real{inverse[forward(ratio)*conjugate(forward(psf))]}
#            NORM pixels < floor set to 1.0, LUCY pixels set to 0.0

             call lfftf1 (work, Memr[PWEIGHT(dat)], Memx[pxx], npts)
             call acjmulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npts)
             call lfftb1 (work, Memx[pxx], Memr[PNORM(dat)], npts)

             floor = ahivr (Memr[PNORM(dat)], npts) * 0.001
             do i = 1, npts {
                if (Memr[PNORM(dat)+i-1] < floor) {
                   Memr[PNORM(dat)+i-1] = 1.0
                   Memr[PLUCY(dat)+i-1] = 0.0
                }
             }

#               lucy convolved = real{inverse[forward(lucy)*forward(psf)]}

                call lfftf1 (work, Memr[PLUCY(dat)], Memx[pxx], npts)
                call amulx (Memx(pxx), Memx[PFPSF(dat)],Memx[pxx],npts)
                call lfftb1 (work, Memx[pxx], Memr[PLUCYC(dat)], npts)

#               load input image and compute initial chi_sq

                bbuf = imgl1r(rim)
                call amovr (Memr[bbuf], Memr[praw+xloc-1], rnpts)
                chisq = chi2_d1(Memr[praw],Memr[PLUCYC(dat)],Memr[PBACK(dat)],
                                Memr[PWEIGHT(dat)],npts,bnpts,adu,noise)
                call printf ("Initial chi-sq = %g\n")
                  call pargr(chisq)
                  call flush (STDOUT)

#            main loop done niter times unless convergence reached before then

             do iter = 1, niter {

#               ratio = (input+convar+back) / (lucy convolved+convar+back)

                do i = 1, npts {
                   if (bnpts == npts) bval = Memr[PBACK(dat)+i-1]
                   num = Memr[praw+i-1] + convar + bval
                   den = Memr[PLUCYC(dat)+i-1] + convar + bval
                   if (den == 0.0) den = 1.0
                   Memr[pxr+i-1] = (num / den) * Memr[PWEIGHT(dat)+i-1]
                }

#               sratio = forward(ratio) * conjugate(forward(psf))

                call lfftf1 (work, Memr[pxr], Memx[pxx], npts)
                call acjmulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npts)
                call lfftb1 (work, Memx[pxx], Memr[pxr], npts)

#               lucy = lucy * real{inverse[sratio]}/norm

                call adivr (Memr[pxr], Memr[PNORM(dat)], Memr[pxr], npts)

                if (accelerate > 0) {
                   count = iter
                   call amulr (Memr[PLUCY(dat)], Memr[pxr], Memr[pxr], npts)
                   call asubr (Memr[pxr], Memr[PLUCY(dat)], Memr[pxr], npts)
                   call accel_d1(dat, praw, pxr, pxx, work, bnpts, center,
                                 convar, adu, count, accelerate, verbose)
                } else {
                   call amulr (Memr[PLUCY(dat)], Memr[pxr], Memr[PLUCY(dat)], 
                               npts)
                   call lfftf1 (work, Memr[PLUCY(dat)], Memx[pxx], npts)
                   call amulx (Memx(pxx), Memx[PFPSF(dat)],Memx[pxx],npts)
                   call lfftb1 (work, Memx[pxx], Memr[PLUCYC(dat)], npts)
                }
               
#               compute chi squared

                chisq = chi2_d1(Memr[praw],Memr[PLUCYC(dat)],Memr[PBACK(dat)],
                        Memr[PWEIGHT(dat)],npts,bnpts,adu,noise)

                titer = titer + 1
                if (titer > iter) {call printf ("Total Iterations %d  ")
                                    call pargi (titer)}
                call printf ("Iteration %d : chi-sq = %g\n")
                     call pargi (iter)
                     call pargr (chisq)
                     call flush (STDOUT)

#               Check if the interim image should be saved,
#               in the final output image (update) and
#               as a unique image (nsave)

                if (iter < niter) {
                   if (nsave > 0 && mod(iter,nsave) == 0) {
                      call outdef (titer, outname, out)
                      if (out[1] != EOS) {
                         lim = immap (out, NEW_COPY, rim)
                         IM_LEN(lim,1) = xsize
                         IM_NDIM(lim) = IM_NDIM(rim)
                         IM_PIXTYPE(lim) = TY_REAL
                         call dumpim (lim, Memr[PLUCY(dat)], npts, nrows,
                                   xsize, ysize, rnpts, nrows, titer, chisq, 
                                   center)
                         call imunmap (lim)
                         call printf ("Iteration: %d  updating image %s\n")
                            call pargi(iter)
                            call pargstr(out)
                         call flush (STDOUT)
                      }
                   }
                   if (update > 0 && mod (iter, update) == 0) {
                      lim = immap (outname, READ_WRITE, 0)
                      call dumpim (lim, Memr[PLUCY(dat)], npts, nrows, 
                               xsize, ysize, rnpts, nrows, titer, chisq, 
                               center)
                      call imunmap (lim)
                      call printf ("Iteration: %d  updating image %s\n")
                         call pargi(iter)
                         call pargstr(outname)
                      call flush (STDOUT)
                   }
                }
                if (chisq <= limchisq) {
                   call printf (" chi-sq <= %g, converged\n")
                      call pargr (limchisq)
                   break
                }
             }
             call fftf1d (dat, work, pxx, pxr, accelerate)

end

include "lucyfft.h"

# 2-D deconvolution 

procedure lucy2d (dat, adu, noise, niter, limchisq, rim, bnpix, nsave, 
                  outname, titer, chisq, center, update, accelerate, verbose)

pointer      dat             # pointer to data array structure
pointer      rim             # pointer to input image
int          niter           # number of iterations desired
int          nsave           # save intermediate image every nth iteration
int          bnpix           # size of background array
int          titer           # Total number of interations complete
int          update          # save intermediate results in output image
int          accelerate      # acceleration method
real         adu             # number of counts per DN
real         noise           # constant noise
real         limchisq        # chi-squared limit for convergence
char         outname[SZ_FNAME]  # Name of output image
bool         center          # center input data 
bool         verbose         # print iterations of C, etc.

pointer      work            # pointer to fft work area structure
pointer      pxx             # pointer to temporary complex array
pointer      pxr             # pointer to temporary real array
pointer      lim

int          i, amsh[2]
int          xmax, ymax
int          npts, rnpts     # number of points in arrays, input image
int          nrows, rnrows   # number of rows in arrays, input image
int          xsize, ysize    # size of output image
int          npix, iter
int          xloc, yloc      # array offsets for centering
int          count           # iteration count (for turbo)

real         val, scal
real         bval
real         num, den
real         convar
real         chisq, floor
real         chi2_d2()
real         ahivr()

char         out[SZ_FNAME]   # name of interim output files

pointer      immap()

begin

             convar = noise**2 / adu 
             call ffta2d (dat, work, pxx, pxr, accelerate)
             npts = NPTS(dat)
             nrows = NROWS(dat)
             rnpts = RNPTS(dat)
             rnrows = RNROWS(dat)
             xsize = XOUT(dat)
             ysize = YOUT(dat)
             npix = npts * nrows
             if (center) {
                xloc = (npts - rnpts)/2 + 1
                yloc = (nrows - rnrows)/2 + 1
             } else {
                xloc = 1
                yloc = 1
             }
             if (bnpix == 1) bval = Memr[PBACK(dat)]

#            find maximum value of psf & shift to pixel(1,1)

             call aymax (Memr[PNORM(dat)], npts, nrows, val, xmax, ymax)
             amsh[1] = -xmax + 1
             amsh[2] = -ymax + 1
             call imshift (npts, nrows, amsh, Memr[PNORM(dat)], Memr[pxr])
             call lfftf2 (work, Memr[pxr], Memx[PFPSF(dat)], npts, nrows)

#            normalize forward transform of psf by first pixel 

             scal = Memx[PFPSF(dat)]
             call advkxr (Memx[PFPSF(dat)], scal, Memx[PFPSF(dat)], npix)

#            ratio = weight
#            PNORM = real{inverse[forward(ratio)*conjugate(forward(psf))]}
#            NORM pixels < 0.001  set to 1.0, LUCY pixels set to 0.0

             call lfftf2 (work, Memr[PWEIGHT(dat)], Memx[pxx], npts, nrows)
             call acjmulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npix)
             call lfftb2 (work, Memx[pxx], Memr[PNORM(dat)], npts, nrows)

             floor = ahivr (Memr[PNORM(dat)], npix) * 0.001
             do i = 1, npix {
                if (Memr[PNORM(dat)+i-1] < floor) {
                   Memr[PNORM(dat)+i-1] = 1.0
                   Memr[PLUCY(dat)+i-1] = 0.0
                }
             }

#            lucy convolved = real{inverse[forward(lucy)*forward(psf)]

             call lfftf2 (work, Memr[PLUCY(dat)], Memx[pxx], npts, nrows)
             call amulx (Memx(pxx), Memx[PFPSF(dat)],Memx[pxx],npix)
             call lfftb2 (work, Memx[pxx], Memr[PLUCYC(dat)], npts, nrows)

#            load input image shifting by (xloc,yloc) and compute initial
#            chi^2
                 
             call aclrr (Memr[pxr], npix)
             call movimr (rim, Memr[pxr], npts, nrows, rnpts, rnrows, xloc,
                          yloc)
             chisq = chi2_d2 (Memr[pxr],Memr[PLUCYC(dat)],Memr[PBACK(dat)],
                     Memr[PWEIGHT(dat)],npts,nrows,bnpix, adu, noise)
             call printf ("Initial chi-sq = %g\n")
               call pargr (chisq)
             call flush (STDOUT)

#            main loop done niter times unless convergence reached before then

             do iter = 1, niter {


#               ratio = (input+convar+back) / (lucy convolved+convar+back)

                do i = 1, npix {
                   if (bnpix == npix) bval = Memr[PBACK(dat)+i-1]
                   num = Memr[pxr+i-1] + convar + bval
                   den = Memr[PLUCYC(dat)+i-1] + convar + bval
                   if (den == 0.0) den = 1.0
                   Memr[pxr+i-1] = (num / den) * Memr[PWEIGHT(dat)+i-1]
                }

#               sratio = inverse(forward(ratio) * conjugate(forward(psf))) /
#                        norm

                call lfftf2 (work, Memr[pxr], Memx[pxx], npts, nrows)
                call acjmulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npix)

                call lfftb2 (work, Memx[pxx], Memr[pxr], npts, nrows)
                call adivr (Memr[pxr], Memr[PNORM(dat)], Memr[pxr], npix)

#               using an acceleration algorithm?

                if (accelerate > 0) {
                   count = iter
                   call amulr (Memr[PLUCY(dat)], Memr[pxr], Memr[pxr], npix)
                   call asubr (Memr[pxr], Memr[PLUCY(dat)], Memr[pxr], npix)
                   call accel_d2(rim,dat,pxr,pxx,work,bnpix,center,convar,adu,
                                 count, verbose, accelerate) 
                } else {
                   call amulr (Memr[PLUCY(dat)], Memr[pxr], Memr[PLUCY(dat)], 
                               npix)
                   call lfftf2 (work, Memr[PLUCY(dat)], Memx[pxx], npts, nrows)
                   call amulx (Memx(pxx), Memx[PFPSF(dat)],Memx[pxx],npix)
                   call lfftb2 (work, Memx[pxx], Memr[PLUCYC(dat)], npts, nrows)
                }

#               compute chi squared

                call aclrr (Memr[pxr], npix)
                call movimr (rim, Memr[pxr], npts, nrows, rnpts, rnrows, 
                             xloc, yloc)

                chisq = chi2_d2 (Memr[pxr],Memr[PLUCYC(dat)],Memr[PBACK(dat)],
                        Memr[PWEIGHT(dat)],npts,nrows,bnpix, adu, noise)

                titer = titer + 1
                if (titer > iter) {call printf("Total Iterations %d  ")
                                    call pargi (titer)}
                call printf ("Iteration %d : chi-sq = %g\n")
                     call pargi (iter)
                     call pargr (chisq)
                     call flush (STDOUT) 

#               Check if the interim image should be saved,
#               in the final output image (update) and
#               as a unique image (nsave)

                if (iter < niter) {
                   if (nsave > 0 && mod(iter,nsave) == 0) {
                      call outdef (titer, outname, out)
                      if (out[1] != EOS) {
                         lim = immap (out, NEW_COPY, rim)
                         IM_LEN(lim,1) = xsize
                         IM_LEN(lim,2) = ysize
                         IM_NDIM(lim) = IM_NDIM(rim)
                         IM_PIXTYPE(lim) = TY_REAL
                         call dumpim (lim, Memr[PLUCY(dat)], npts, nrows,
                                 xsize, ysize, rnpts, rnrows, titer, chisq, 
                                 center)
                         call imunmap (lim)
                         call printf ("Iteration: %d  updating image %s\n")
                            call pargi(iter)
                            call pargstr(out)
                         call flush (STDOUT)
                      }
                   }
                   if (update > 0 && mod (iter, update) == 0) {
                      lim = immap (outname, READ_WRITE, 0)
                      call dumpim (lim, Memr[PLUCY(dat)], npts, nrows, 
                               xsize, ysize, rnpts, rnrows, titer, chisq, 
                               center)
                      call imunmap (lim)
                      call printf ("Iteration: %d  updating image %s\n")
                         call pargi(iter)
                         call pargstr(outname)
                      call flush (STDOUT)
                   }
                }
                if (chisq <= limchisq) {
                   call printf (" chi-sq <= %g,  converged\n")
                   call pargr (limchisq) 
                   break
                }
             }
             call fftf2d (dat, work, pxx, pxr, accelerate)

end        

#            find maximum value of a two-dimensional array and its pixel
#            coordinates

procedure aymax (array, npts, nrows, maxval, xmax, ymax)

real         array[npts, nrows]
int          npts
int          nrows

real         maxval
int          xmax
int          ymax
                   
int          i
real         tmpmax
real         ahivr()

begin

             maxval = ahivr (array[1,1],npts)
             ymax = 1
             do i = 2, nrows {
                tmpmax = ahivr (array[1,i], npts)
                if (tmpmax > maxval) {
                   maxval = tmpmax
                   ymax = i
                }
             }
             xmax = 1
             do i = 1, npts {
                if (array[i,ymax] == maxval) {
                   xmax = i
                   break
                }
             }

end

#            shift a two-dimensional array by the amounts amsh[1], amsh[2]

procedure imshift (npts, nrows, amsh, a, b)

int          npts
int          nrows
int          amsh[2]
real         a[npts,nrows]

real         b[npts,nrows]

int          inline, outline

begin

             outline = 1 + amsh[2]
             outline = mod(outline,nrows)
             do inline = 1, nrows {
                if (outline < 1)
                   outline = outline + nrows
                if (outline > nrows)
                   outline = outline - nrows
                call rwshift (npts, amsh[1], a[1,inline], b[1,outline])
                outline = outline + 1
             }

end

#            copy a two-dimensional real image into a two-dimensional real
#            array with xloc and yloc offsets in the array.
#            image size + offsets <= array size (no wraparound) 

procedure movimr (im, array, npts, nrows, inpts, inrows, xloc, yloc)

pointer      im
real         array[npts,nrows]
int          npts
int          nrows
int          inpts
int          inrows
int          xloc
int          yloc
int          npix

pointer      inbuf
int          i
pointer      imgl2r()

begin
             npix = nrows * npts
             call aclrr (array[1,1], npix)
             do i = 1, inrows {
                inbuf = imgl2r (im, i)
                call amovr (Memr[inbuf], array[xloc, yloc+i-1], inpts)
             }

end

#            copy a two-dimensional short image into a two-dimensional 
#            integer array with xloc and yloc offsets in the array.
#            image size + offsets <= array size (no wraparound) 

procedure movims (im, array, npts, nrows, inpts, inrows, xloc, yloc)

pointer      im
short        array[npts,nrows]
int          npts
int          nrows
int          inpts
int          inrows
int          xloc
int          yloc
int          npix

pointer      inbuf
int          i
pointer      imgl2s()

begin
             npix = nrows * npts
             call aclrs (array[1,1], npix)
             do i = 1, inrows {
                inbuf = imgl2s (im, i)
                call amovs (Mems[inbuf], array[xloc, yloc+i-1], inpts)
             }

end

include "lucyfft.h"

#            allocate fft work area and data arrays for one-dimensional case

procedure ffta1d(dat, work, pxx, pxr, accelerate)

pointer      dat              # pointer to data arrays
pointer      work             # pointer to fft work area
pointer      pxx              # pointer to temporary complex array
pointer      pxr              # pointer to temporary real array
int          accelerate       # turbo = 2
int          npts             # number of points in array
int          wsiz             # siz of trig table

begin

             npts = NPTS(dat)
             call malloc (PLUCYC(dat), npts, TY_REAL)
             call malloc (PFPSF(dat), npts, TY_COMPLEX)

             if (accelerate == 2) {
                call malloc (POPHI(dat), npts, TY_REAL)
                call malloc (POPSI(dat), npts, TY_REAL)
             }

             call malloc (work, LEN_WORK, TY_STRUCT)
             wsiz = npts * 4 + 15
             call calloc (TRIGTAB1(work), wsiz, TY_REAL)
             call cffti (npts, Memr[TRIGTAB1(work)])

             call malloc (pxr, npts, TY_REAL)
             call malloc (pxx, npts, TY_COMPLEX)

end

include "lucyfft.h"

#            allocate fft work area and data arrays for two-dimensional case

procedure ffta2d(dat, work, pxx, pxr, accelerate)

pointer      dat                # pointer to data arrays
pointer      work               # pointer to fft work structure
pointer      pxx                # pointer to temporary complex array
pointer      pxr                # pointer to temporary real array
int          accelerate         # turbo = 2

int          npts, nrows        # number of points, rows in array
int          npix               # total number of points in array
int          wsiz               # size of trig table

begin

             npts = NPTS(dat)
             nrows = NROWS(dat)
             npix = npts * nrows

             if (accelerate == 2) {
                call malloc (POPHI(dat), npix, TY_REAL)
                call malloc (POPSI(dat), npix, TY_REAL)
             }

             call malloc (PLUCYC(dat), npix, TY_REAL)
             call malloc (PFPSF(dat), npix, TY_COMPLEX)

             call malloc (work, LEN_WORK, TY_STRUCT)
             wsiz = npts * 4 + 15
             call calloc (TRIGTAB1(work), wsiz, TY_REAL)
             call cffti (npts, Memr[TRIGTAB1(work)])

             wsiz = nrows * 4 + 15
             call calloc (TRIGTAB2(work), wsiz, TY_REAL)
             call cffti (nrows, Memr[TRIGTAB2(work)])

             call malloc (XWORK(work), nrows, TY_COMPLEX)
             call malloc (pxr, npix, TY_REAL)
             call malloc (pxx, npix, TY_COMPLEX)

end

include "lucyfft.h"

#            deallocate fft work area and data arrays for one-dimensional case

procedure fftf1d(dat, work, pxx, pxr, accelerate)

pointer      dat              # pointer to data arrays
pointer      work             # pointer to fft work area
pointer      pxx              # pointer to temporary complex array
pointer      pxr              # pointer to temporary real array
int          accelerate       # turbo = 2

begin
 

             call mfree (pxx, TY_COMPLEX)
             call mfree (pxr, TY_REAL)

#            call mfree (XWORK(work), TY_COMPLEX)
             call mfree (TRIGTAB1(work), TY_REAL)
             call mfree (work, TY_STRUCT)

             call mfree (PFPSF(dat), TY_COMPLEX)
             call mfree (PLUCYC(dat), TY_REAL)

             if (accelerate == 2) {
                call mfree (POPHI(dat), TY_REAL)
                call mfree (POPSI(dat), TY_REAL)
             }
end

include "lucyfft.h"

#            deallocate fft work area and data arrays for two-dimensional case

procedure fftf2d (dat, work, pxx, pxr, accelerate)

pointer      dat              # pointer to data arrays
pointer      work             # pointer to fft work area
pointer      pxx              # pointer to temporary complex array
pointer      pxr              # pointer to temporary real array
int          accelerate       # turbo = 2

begin

             call mfree (pxx, TY_COMPLEX)
             call mfree (pxr, TY_REAL)

             call mfree (XWORK(work), TY_COMPLEX)
             call mfree (TRIGTAB1(work), TY_REAL)
             call mfree (TRIGTAB2(work), TY_REAL)
             call mfree (work, TY_STRUCT)

             call mfree (PFPSF(dat), TY_COMPLEX)
             call mfree (PLUCYC(dat), TY_REAL)

             if (accelerate == 2) {
                call mfree (POPHI(dat), TY_REAL)
                call mfree (POPSI(dat), TY_REAL)
             }
end

include "lucyfft.h"

#            forward fast fourier transform a one dimensional array using
#            NCAR fft routines.  Normalization is the last step.  Input is
#            real, output is complex.

procedure lfftf1 (work, input, output, npts)

pointer      work                  # fft work structure
real         input[npts]           # input array
complex      output[npts]          # output array
int          npts                  # number of points

real         xnpix

begin

#            convert real to complex, transform, and normalize              

             call achtrx (input, output, npts)
             call cfftf (npts, output, Memr[TRIGTAB1(work)])
             xnpix = real(npts)  
             call advkxr (output, xnpix, output, npts)

end
                
include "lucyfft.h"

#            forward fast fourier transform a two dimensional array using
#            NCAR fft routines.  For two dimensional case, each row is 
#            transformed, then each column.  Normalization is the last step.
#            Input is real,  Output is complex

procedure lfftf2 (work, input, output, npts, nrows)

pointer      work                  # fft work structure
real         input[npts,nrows]     # input array 
complex      output[npts,nrows]    # output array
int          npts                  # number of points in row
int          nrows                 # number of rows

int          i,j
real         xnpix

begin

#            transform  rows

             do i = 1, nrows {
                call achtrx (input[1,i], output[1,i], npts)
                call cfftf (npts, output[1,i], Memr[TRIGTAB1(work)])
             }

#            transform columns

             do i = 1, npts {
                do j = 1, nrows {
                   Memx[XWORK(work)+j-1] = output[i,j]
                }
                call cfftf (nrows, Memx[XWORK(work)], Memr[TRIGTAB2(work)])
                do j = 1, nrows {
                   output [i,j] = Memx[XWORK(work)+j-1]
                }
             }

#            Normalize data
 
             xnpix = real(npts*nrows)

             do i = 1, nrows {
                call advkxr(output[1,i],xnpix,output[1,i],npts)
             }

end

include "lucyfft.h"

#            inverse fast fourier transform a one dimensional aray using
#            the NCAR fft routines.  No normalization is done.  Input is
#            complex, output is real.

procedure lfftb1(work, input, output, npts)

pointer      work                # fft work structure
complex      input[npts]         # input array
real         output[npts]        # output array
int          npts                # number of points

begin

#            transform data, then extract real part

             call cfftb (npts, input, Memr[TRIGTAB1(work)])
             call achtxr (input, output, npts)

end

include "lucyfft.h"

#            inverse fast fourier transform a two dimensional array using
#            NCAR fft routines.  For two dimensional case, each row is 
#            transformed, then each column. 
#            Input is complex,  Output is real.

procedure lfftb2 (work, input, output, npts, nrows)

pointer      work                  # fft work structure
complex      input[npts,nrows]     # input array 
real         output[npts,nrows]    # output array
int          npts                  # number of points in row
int          nrows                 # number of rows

int          i,j

begin

#            transform rows

             do i = 1, nrows {
                call cfftb (npts, input[1,i], Memr[TRIGTAB1(work)])
             }

#            transform columns

             do i = 1, npts {
                do j = 1, nrows {
                   Memx[XWORK(work)+j-1] = input[i,j]
                }
                call cfftb (nrows, Memx[XWORK(work)], Memr[TRIGTAB2(work)])
                do j = 1, nrows {
                   input [i,j] = Memx[XWORK(work)+j-1]
                }
             }

#            Extract real part of array

             do i = 1, nrows {
                call achtxr(input[1,i],output[1,i],npts)
             }

end

#            compute and return value of chi-squared for 2-D data
#
#         (adu * weight * (lucy convolved - input image))^2
#         / (adu * weight * (lucy convolved+background) + constant noise^2)

real procedure chi2_d2 (input, luc, back, weight, npts, nrows, bnpix, adu, 
                        noise)

real         input[npts,nrows]   # input data array
real         luc[npts,nrows]     # lucy convolved array
real         back[bnpix]         # backgournd array
real         weight[npts,nrows]  # weight array

int          npts                # number of points in row of input data array
int          nrows               # number of rows in input data array
int          bnpix               # number of elements in background array
real         adu                 # number of electrons per DN
real         noise               # constant noise
real         chisq               # returned chisq value
real         bval

int          i,j                 
int          gpix
int          npix
real         noise2, temp
real         tnum, tden
double       tchi

begin

             tchi = 0.
             gpix = 0
             noise2 = noise**2
             npix = nrows * npts
             if (bnpix == 1) bval = back[1]
             do j = 1, nrows {
                do i = 1, npts {
                   if (bnpix == npix) bval = back[i+(j-1)*npts]
                   if (weight[i,j] > 0.0) {
                      temp = adu * weight[i,j]
                      tnum = (temp * (luc[i,j] - input[i,j])) **2
                      tden = temp * (luc[i,j] + bval) + noise2
                      if (tden != 0.) tchi = tchi + (tnum / tden)
                      gpix = gpix + 1
                   }
                }
             }

             chisq = tchi / real(gpix-1)
             return (chisq)

end

#            compute and return value of chi-squared for 1-D data
#
#            (adu * weight * (lucy convolved - input image)) **2
#            / (adu * weight * (lucy convolved+background) + constant noise^2)

real procedure chi2_d1 (input, luc, back, weight, npts, bnpts, adu, noise)

real         input[npts]         # input data array
real         luc[npts]           # lucy convolved array
real         back[bnpts]         # background array
real         weight[npts]        # weight array

int          npts                # number of points in row of input data array
int          bnpts               # number of elements in background array
real         adu                 # number of electrons per DN
real         noise               # constant noise
real         chisq               # returned chisq value
real         bval

int          i               
int          gpix
real         noise2, temp
real         tnum, tden
double       tchi

begin

             tchi = 0.
             gpix = 0
             noise2 = noise**2
             if (bnpts == 1) bval = back[1]
             do i = 1, npts {
                if (bnpts == npts) bval = back[i]
                if (weight[i] > 0.0) {
                   temp = adu * weight[i]
                   tnum = (temp * (luc[i] - input[i])) **2
                   tden = temp * (luc[i] + bval) + noise2
                   if (tden != 0.) tchi = tchi + (tnum / tden)
                   gpix = gpix + 1
                }
             }

             chisq = tchi / real(gpix-1)
             return (chisq)

end

#            divide a complex array by a real constant

procedure  advkxr (a, b, c, npix)

complex      a[npix]
real         b
complex      c[npix]
int          npix

int          i

begin
             do i = 1, npix {
                c[i] = a[i] / b
             }

end

#            multiply the complex conjugate of one array by another complex 
#            array.  Data treated as one-dimensional.

procedure acjmulx (a, b, c, npix)

complex      a[npix]
complex      b[npix]
complex      c[npix]

int          npix

int          i

begin

             do i = 1, npix {
                c[i] = b[i] * conjg(a[i])
             }

end


# rwshift -- shift a 1-D array
# This routine copies an array from input to output, shifting by an
# integral number of pixels.  The actual arguments corresponding to
# the input and output arrays must be distinct.

procedure rwshift (npts, amsh, in, out)

int	npts		# i: size of arrays
int	amsh		# i: amount of shift
real	in[npts]	# i: input array
real	out[npts]	# o: output array
#--
int	sh		# abs (amsh)
int	n		# npts - abs(amsh)

begin
 
	if (amsh > 0) {

	    n = npts - amsh

	    call amovr (in, out[amsh+1], n)
	    call amovr (in[n+1], out, amsh)

	} else if (amsh < 0) {

	    sh = abs (amsh)
	    n = npts - sh

	    call amovr (in, out[n+1], sh)
	    call amovr (in[sh+1], out, n)

	} else {

	    call amovr (in, out, npts)
	}
end

# procedure to set up mask array for 2-D data, make sure good pixels have 
# value of 1, any pixels below threshold are masked

procedure maskd2 (input, back, mask, npts, nrows, rnpts, rnrows, bnpix,
                   noise, adu, nsigma, goodpix, center, verbose)

int     nrows, npts               # number of rows and points in arrays
int     rnrows, rnpts             # number of rows and points in input image
int     bnpix                     # number of pixels in background array
int     goodpix                   # number of good pixels

bool    center                    # data centered in arrays
bool    verbose                   # print list of masked pixels

real    noise, adu                # input noise and conversion factor
real    nsigma                    # threshold factor

real    input[npts, nrows]        # input data
real    back[bnpix]               # background array 
short   mask[npts, nrows]         # mask array

real    convar, limit             # constant variance, masking threshold
real    bval

int     npix
int     i, j
int     ii, jj
int     begpt, endpt
int     begrow, endrow
int     phead
int	n_negatives
begin

#       Search only over real data points

        phead = 0
        convar = noise**2 / adu
        npix = nrows * npts
        if (center) {
           begpt = (npts - rnpts)/2 + 1
           endpt = begpt + rnpts - 1
           begrow = (nrows - rnrows)/2 + 1
           endrow = begrow + rnrows -1
        } else {
           begpt = 1
           endpt = rnpts
           begrow = 1
           endrow = rnrows
        }
        if (bnpix == 1) bval = back[1]

#       if goodpixval = 0, switch polarity of mask values

        if (goodpix == 0) {
           do j = begrow, endrow {
              do i = begpt, endpt {
                 if (mask[i,j] == 0) mask[i,j] = 1
                 else mask[i,j] = 0
              }
           }
        }

#      compare each pixel in input image to threshold

        n_negatives = 0        
        do j = begrow, endrow {
           do i = begpt, endpt {
              if (mask[i,j] != 0) {
                  if (bnpix == npix) bval = back[i+(j-1)*npts]
                  limit = convar + bval
                  if (nsigma > 0.0) {
                     limit = limit/adu
                     if (limit >= 0.) limit = sqrt(limit) * nsigma
                     else limit = 0.
                  }
                  if (input[i,j] < -limit) {
                    mask[i,j] = 0
		    if (!verbose)
		       n_negatives = n_negatives + 1
	            else {
                       if (phead == 0) {
                           phead = 1
                           call printf (
                             "List of pixels below threshold -%s (-%g sigma)\n")
                             call pargr(limit)
                             call pargr(nsigma)
                       }
                       if (npts > rnpts && (center)) {
                          ii = i - begpt + 1
                          jj = j - begrow + 1
                          call printf ("pixel (%d,%d)  [%d,%d];")
                            call pargi (ii)
                            call pargi (jj)
                            call pargi (i)
                            call pargi (j)
                       } else { call printf ("pixel (%d,%d);")
                                call pargi (i)
                                call pargi (j)
                       }
                       call printf (" value %g\n")
                         call pargr (input[i,j])
                       call flush (STDOUT)
		    }
                  }
              }
           }
        }

        if(!verbose && n_negatives > 0) {
     call printf ("\n**** There are %d negatives pixels in the input image\n")
     call pargi(n_negatives)
}
end

# procedure to set up mask array for 1-D data, make sure good pixels have 
# value of 1, any pixels below threshold are masked

procedure maskd1 (input, back, mask, npts, rnpts, bnpts, noise, adu, 
                    nsigma, goodpix, center)

int     npts, rnpts               # number of points in arrays, input image
int     bnpts                     # number of points in background array
int     goodpix                   # number of good pixels

bool    center                    # data centered in arrays

real    noise, adu                # input noise and conversion factor
real    nsigma                    # threshold factor

real    input[npts]               # input data
real    back[bnpts]               # background array 
short   mask[npts]                # mask array

real    convar, limit             # constant variance, threshold for masking
real    bval

int     i, ii
int     begpt, endpt
int     phead

begin

        phead = 0
        convar = noise**2 / adu
        if (center) {
           begpt = (npts - rnpts)/2 + 1
           endpt = begpt + rnpts -1
        } else {
           begpt = 1
           endpt = rnpts
        }
        if (bnpts == 1) bval = back[1]

#       if goodpixval = 0 reverse polarity of mask values

        if (goodpix == 0) {
           do i = begpt, endpt {
              if (mask[i] == 0) mask[i] = 1
                 else mask[i] = 0
           }
        }

#       compare each pixel with threshold
        
        do i = begpt, endpt {
           if (mask[i] != 0) {
              if (bnpts == npts) bval = back[i]
              else bval = back[1]
              limit = convar + bval
              if (nsigma > 0.0) {
                 limit = limit/adu
                 if (limit >= 0.) limit = sqrt(limit) * nsigma
                 else limit = 0.
              }
              if (input[i] < -limit) {
                  mask[i] = 0
                  if (phead == 0) {
                     phead = 1
                     call printf (
                       "List of pixels below threshold -%s (-%g sigma)\n")
                       call pargr(limit)
                       call pargr(nsigma)
                  }
                  if (npts > rnpts) {
                     ii = i - begpt + 1
                     call printf ("pixel (%d)  [%d];")
                       call pargi (ii)
                       call pargi (i)
                  } else {call printf ("pixel (%d);")
                           call pargi (i) }
                  call printf (" value %g\n")
                    call pargr (input[i])
                  call flush (STDOUT)
              }

           }
        }
end

# procedure to build outname name for intermediate results, i.e.,
# outname_iter#.extension

procedure outdef (iter, outname, out)

int          iter

char         outname[SZ_FNAME]
char         out[SZ_FNAME]

int          ldir, lroot
int          lextn, slen
int          tlen

char         dir[SZ_FNAME], root[SZ_FNAME]
char         ext[SZ_FNAME], siter[5]

int          strlen(), fnroot()
int          fnldir(), fnextn()

begin

              out[1] = EOS
              ldir = fnldir (outname, dir, SZ_FNAME)
              lroot = fnroot (outname, root, SZ_FNAME)
              lextn = fnextn (outname, ext, SZ_FNAME)
              if (lextn > 0) lextn = lextn + 1
              tlen = ldir + lroot + lextn
              slen = strlen (outname)
              if ( tlen != slen) {
                 call printf ("Error in output file specification: %s\n")
                   call pargstr (outname)
              } else {
                 if (ldir > 0) {
                    call strcpy (dir, out, SZ_FNAME)
                    call strcat (root, out, SZ_FNAME)
                 } else call strcpy (root, out, SZ_FNAME)
                 siter[1]=EOS
                 call sprintf (siter, SZ_FNAME, "_%d")
                   call pargi (iter)
                 call strcat (siter, out, SZ_FNAME)
                 if (lextn > 0) {
                    call strcat (".", out, SZ_FNAME)
                    call strcat (ext, out, SZ_FNAME)
                 }
              }

end

#   dumpim - dumps specified array to an image taking into account
#            any offset that may be required, i.e., a subset of the
#            array

procedure dumpim (im, array, npts, nrows, xsize, ysize, rnpts, rnrows, 
                  titer, chisq, center)

pointer       im                # image pointer

int           npts, nrows       # number of points and rows in array
int           xsize, ysize      # number of points and rows in output image
int           rnpts, rnrows     # number of points and rows in output image
int           titer             # total number of iterations performed

real          array[npts,nrows] # array to be output to image
real          chisq             # computed chi squared

bool          center            # is output image centered in array?

int           begpt             # first point in array to dump
int           begrow, endrow    # first and last row in array to dump
int           i, ii
int           imaccf()

real          crpix
real          imgetr()

pointer       lbuf
pointer       impl1r(), impl2r()

begin

              if (xsize < npts && (center)) begpt = (npts - xsize)/2 + 1
              else begpt = 1

              if (ysize < nrows && (center)) {
                 begrow = (nrows - ysize)/2 + 1
                 endrow = begrow + ysize -1
              } else {
                 begrow = 1
                 endrow = ysize
              }

              if ( nrows == 1) {
                 lbuf = impl1r(im)
                 call amovr (array[begpt,1], Memr[lbuf], xsize)
              } else {
                 do i = begrow, endrow {
                    ii = i - begrow + 1
                    lbuf = impl2r(im,ii)
                    call amovr (array[begpt,i], Memr[lbuf], xsize)
                 }
              }

              call imaddi (im, "NO_ITER", titer)
              call imaddr (im, "LUCYCHI2", chisq)

              if (center) {
                 if (xsize > rnpts) {
                    if (imaccf(im, "CRPIX1") == YES) {
                       crpix = imgetr (im, "CRPIX1")
                       crpix = crpix + (xsize - rnpts)/2
                       call imputr (im, "CRPIX1", crpix)
                    }
                 }
                 if (ysize > rnrows) {
                    if (imaccf (im, "CRPIX2") == YES) {
                       crpix = imgetr (im, "CRPIX2")
                       crpix = crpix + (ysize-rnrows)/2
                       call imputr (im, "CRPIX2", crpix)
                    }
                 }
              }

end

#  Accelerated iteration routine
#  accelerate = 1  -> line-search method (Hook and Lucy)
#  accelerate = 2  -> conjugae gradient method ( R.L. White's turbo)

procedure accel_d2 (rim, dat, pdpsi, pxx, work, bnpix, center, convar, adu, 
                    count, verbose, accelerate)

pointer       rim             # pointer to input image
pointer       dat             # pointer to data structure
pointer       pdpsi           # pointer to dpsi array (next lucy - current)
pointer       pxx             # pointer to temporary complex array
pointer       work            # pointer to temporary fft structure

int           bnpix           # number of pixels in background image
int           xloc            # beginning x location in output array
int           yloc            # beginning y location in output array
int           accelerate      # turbo = 2, accelerated = 1, standard = 0
int           count           # iteration count

real          convar          # noise^2/adu
real          adu

bool          center
bool          verbose

pointer       pdphi           # pointer to dphi array (top half of pxx)
pointer       praw            # pointer to input data (bottom half of pxx)

bool          turbo           # use turbo method this iteration?

int           npts, nrows     # number of points and rows in output image
int           rnpts, rnrows   # number of points and rows in input image
int           npix            # total pixels of output image
#int           negpix 
int           pospix
int           i
int           initer          # number initial accelerated iterations (turbo)
int           tcycle          # number iterations in turbo-accelerate cycle

real          cmax, c
real          cmin
real          suma, sumb
real          sumc, gamma
real          weight
real          temp

double        sum1, sum2

real          calcmax()
real          calc_d2()
extern        calc_d2

begin

#             if turbo mode, first initer steps use the standard (Hook&Lucy)
#             algorithm, then each subsequent tcycle group of iterations are
#             of the pattern (tcycle - 1) turbo steps followed by one 
#             standard accelerated step.  initer currently set to 5 and
#             tcycle set to 3.

              initer = 5
              tcycle = 3
              npts = NPTS(dat)
              nrows = NROWS(dat)
              npix = npts * nrows
              rnpts = RNPTS(dat)
              rnrows = RNROWS(dat)
              if (center) {
                 xloc = (npts - rnpts)/2 + 1
                 yloc = (nrows - rnrows)/2 + 1
              } else {
                 xloc = 1
                 yloc = 1
              }
              pdphi = (pxx-1)*2 + 1
              praw = pdphi + npix

#              if (accelerate == 2 && count > initer && 
#                 mod((count-initer),tcycle) > 0) turbo=TRUE
#              else turbo = FALSE

              if (accelerate == 2 && count > 1) turbo = TRUE
              else turbo = FALSE
 
              call lfftf2 (work, Memr[pdpsi], Memx[pxx], npts, nrows)
              call amulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npix)
              call lfftb2 (work, Memx[pxx], Memr[pdphi], npts, nrows)

#             load input data

              call movimr (rim, Memr[praw], npts, nrows, rnpts, rnrows,
                           xloc, yloc)

              if (turbo) {
                 sum1 = 0.0d0
                 sum2 = 0.0d0
                 do i = 1, npix {
                    weight = Memr[PWEIGHT(dat)+i-1] * adu
                    suma = weight * Memr[POPHI(dat)+i-1] * Memr[pdphi+i-1] *
                           (Memr[praw+i-1]+convar)
                    sumb = weight * Memr[POPHI(dat)+i-1] **2 * 
                           (Memr[praw+i-1]+convar) 
                    sumc = (Memr[PLUCY(dat)+i-1] + convar) **2
                    sum1 = sum1 + suma / sumc
                    sum2 = sum2 + sumb / sumc
                 }
                 gamma = - (sum1/sum2)
                 do i = 1, npix {
                    temp = Memr[pdpsi+i-1]
                    Memr[pdpsi+i-1] = Memr[pdpsi+i-1] + gamma * 
                                      Memr[POPSI(dat)+i-1]
                    Memr[POPSI(dat)+i-1] = temp
                    temp = Memr[pdphi+i-1]
                    Memr[pdphi+i-1] = Memr[pdphi+i-1] + gamma *
                                      Memr[POPHI(dat)+i-1]
                    Memr[POPHI(dat)+i-1] = temp
                 }
              }

#             Check if any pixels in the restored image have tiny negative 
#             numbers because of round off errors and set to 0.

              do i = 1, npix {
                 if (Memr[PLUCY(dat)+i-1] < 0.) {
                    call printf ("Negative pixel (%d): %g\n")
                      call pargi (i)
                      call pargr (Memr[PLUCY(dat)+i-1])
                    Memr[PLUCY(dat)+i-1] = 0.
                    Memr[pdpsi+i-1] = 0.
                 }
              }
#              negpix = 0
#              cmax = INDEFR

#              do i = 1, npix {
#                 if (Memr[pdpsi+i-1] < 0.) {
#                    cmax = min (cmax, -(Memr[PLUCY(dat)+i-1]/Memr[pdpsi+i-1]))
#                    negpix = negpix + 1
#                 } 
#              }

#              if (negpix == 0) cmax = 100.

              cmax = calcmax (Memr[PLUCY(dat)], Memr[pdpsi], npix)

#             if (cmax < 1.0) {
              if (cmax < 1.0 && turbo) {	# 1995 Mar 8, from E.B. Stobie
                 turbo = FALSE
                 call amovr (Memr[POPSI(dat)], Memr(pdpsi), npix)
                 call amovr (Memr[POPHI(dat)], Memr(pdphi), npix)
                 cmax = calcmax (Memr[PLUCY(dat)], Memr[pdpsi], npix)
              }

              if (turbo) {
                 pospix = 0
                 cmin = -INDEFR
                 do i = 1, npix {
                    if (Memr[pdpsi+i-1] > 0.) {
                       cmin = max(cmin,-(Memr[PLUCY(dat)+i-1]/Memr[pdpsi+i-1]))
                       pospix = pospix + 1
                    }
                 }
                 if (pospix == 0) cmin = -100.
               } else cmin = 1.

               if (cmin <= 1.0  && cmax >= 1.0) c = 1.0
               else c = (cmin + cmax) / 2.

              c = calc_d2 (Memr[PLUCYC(dat)], Memr[pdphi], Memr[praw], 
                  Memr[PBACK(dat)], Memr[PWEIGHT(dat)], npts, nrows, rnpts,
                  rnrows, center, bnpix, convar, cmax, cmin) 

              if (verbose) {
                 if (turbo) call printf ("Turbo iteration      ")
                 else call printf ("Accelerated iteration")
                 call printf ("  CMIN = %g, CMAX = %g, C = %g\n")
                   call pargr (cmin)
                   call pargr (cmax)
                   call pargr (c)
              }
            
              call amulkr (Memr[pdpsi], c, Memr[pdpsi], npix)
              call amulkr (Memr[pdphi], c, Memr[pdphi], npix)
          
              call aaddr (Memr[PLUCY(dat)], Memr[pdpsi], Memr[PLUCY(dat)],
                          npix)
              call aaddr (Memr[PLUCYC(dat)], Memr[pdphi], Memr[PLUCYC(dat)],
                          npix)

#             if turbo mode, save current delta psi and delta phi

              if (accelerate == 2) {
                 call amovr (Memr[pdpsi], Memr[POPSI(dat)], npix)
                 call amovr (Memr[pdphi], Memr[POPHI(dat)], npix)
              }

end


real procedure calcmax (lucy, dpsi, npix)

real          lucy[npix]
real          dpsi[npix]
int           npix

real          cmax

int           negpix
int           i

begin

              negpix = 0
              cmax = INDEFR

              do i = 1, npix {
                 if (dpsi[i] < 0. ) {
                    cmax = min (cmax, -(lucy[i] / dpsi[i]))
                    negpix = negpix + 1
                 }
              }
         
              if (negpix == 0) cmax = 100.

              return (cmax)

end

real procedure calc_d2 (lucyc, dphi, raw, back, weight, npts, nrows, rnpts,
                     rnrows, center, bnpix, convar, cmax, cmin) 

real          lucyc[npts,nrows]
real          dphi [npts,nrows]
real          raw[npts,nrows]
real          back[bnpix]
real          weight[npts,nrows]

int           npts, nrows
int           rnpts, rnrows
int           bnpix      

real          convar
real          cmax, cmin

bool          center

int           begpt, endpt
int           begrow, endrow
int           npix
int           i, j, k

real          c
real          dc, dclim
real          model, phitwid
real          bval

double        f, dfdc

begin

              c = 1.0
              dclim = 0.01
              npix = nrows * npts
              if (bnpix == 1) bval = back[1]

              if (center) {
                 begpt = (npts - rnpts)/2 + 1
                 endpt = begpt + rnpts - 1
                 begrow = (nrows - rnrows)/2 + 1
                 endrow = begrow + rnrows - 1
              } else {
                 begpt = 1
                 endpt = rnpts
                 begrow = 1
                 endrow = rnrows
              }

#             Use modified Newton's method to find maximum likelihood
#             No more than 20 iterations

              do k = 1, 20 {
                 f = 0.
                 dfdc = 0.
                 do j = begrow, endrow  {
                    do i = begpt, endpt {
                       if (weight[i,j] > 0.0) {
                          if (bnpix == npix) bval = back[i+(j-1)*npts]
                          model = weight[i,j] * (lucyc[i,j]+bval+c*dphi[i,j]) +
                                  convar
                          phitwid = weight[i,j] * (raw[i,j] + bval) + convar
                          f = f + dphi[i,j] * weight[i,j] * ((phitwid/model)-1)
                          dfdc = dfdc + phitwid * ((dphi[i,j]*weight[i,j]) /
                                 model)**2
                       }
                    }
                 }
                 dc = (-f) / (-dfdc)

#                binary division of interval between c and cmax if overshooting
#                cmax or between c and cmin if overshooting cmin

                 if (c+dc >= cmax) dc = (cmax - c)/2.
                 else if (c+dc <= cmin) dc = (cmin - c)/2.
                 c = c+dc
                 if (abs(dc) <= dclim) return (c)
              }
              return (c)

end



procedure accel_d1 (dat, praw, pdpsi, pxx, work, bnpts, center, convar, adu, 
                    count, accelerate, verbose)

pointer       dat             # pointer to data structure
pointer       praw            # pointer to input array
pointer       pdpsi           # pointer to dpsi array (next lucy - current)
pointer       pxx             # pointer to temporary complex array
pointer       work            # pointer to temporary fft structure

int           bnpts           # number of pixels in background image
int           accelerate      # turbo = 2  accelerated = 1
int           count           # iteration count

real          convar          # noise^2/adu
real          adu             # gain (number of electrons per DN)
bool          center
bool          turbo           # use turbo acceleration this iteration?
bool          verbose         # print additional information

pointer       pdphi           # pointer to dphi array

int           npts            # number of points in output image
int           rnpts           # number of points in input image
int           negpix, pospix
int           i
int           initer          # no of initial accelerated iterations (turbo)
int           tcycle          # no of iterations in turbo-accelerate cycle

real          cmax, c
real          cmin
real          suma, sumb
real          sumc, gamma
real          weight

real          calc_d1()

double        sum1, sum2

begin

              npts = NPTS(dat)
              rnpts = RNPTS(dat)

              call calloc (pdphi, npts, TY_REAL)

              initer = 5
              tcycle = 3

              if (accelerate == 2 && count > initer && 
                  mod((count-initer),tcycle) > 0) turbo = true
              else turbo = false

              call lfftf1 (work, Memr[pdpsi], Memx[pxx], npts)
              call amulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npts)
              call lfftb1 (work, Memx[pxx], Memr[pdphi], npts)

              if (turbo) {
                 sum1 = 0.0d0
                 sum2 = 0.0d0
                 do i = 1, npts {
                    weight = Memr[PWEIGHT(dat)+i-1] * adu
                    suma = weight * Memr[POPHI(dat)+i-1] * Memr[pdphi+i-1] *
                           (Memr[praw+i-1]+convar)
                    sumb = weight * Memr[POPHI(dat)+i-1] ** 2 *
                           (Memr[praw+i-1]+convar)
                    sumc = (Memr[PLUCY(dat)+i-1] + convar) ** 2
                    sum1 = sum1 + suma/sumc
                    sum2 = sum2 + sumb/sumc
                 }
                 gamma = - (sum1/sum2)
                 do i = 1, npts {
                    Memr[pdpsi+i-1] = Memr[pdpsi+i-1] + gamma *
                                      Memr[POPSI(dat)+i-1]
                    Memr[pdphi+i-1] = Memr[pdphi+i-1] + gamma *
                                      Memr[POPHI(dat)+i-1]
                 }
              }

#             Check if any pixels in restored image are small negative
#             numbers because of round-off and reset to 0.

              do i = 1, npts {
                 if (Memr[PLUCY(dat)+i-1] < 0.) {
                    call printf ("Negative pixel (%d): %g\n")
                      call pargi (i)
                      call pargr (Memr[PLUCY(dat)+i-1])
                    Memr[PLUCY(dat)+i-1] = 0.
                    Memr[pdpsi+i-1] = 0.
                 }
              }
          
              negpix = 0
              cmax = INDEFR

              do i = 1, npts {
                 if (Memr[pdpsi+i-1) < 0.) {
                    cmax = min (cmax, -(Memr[PLUCY(dat)+i-1]/Memr[pdpsi+i-1]))
                    negpix = negpix + 1
                 } 
              }

              if (negpix == 0) cmax = 100.

              if (turbo) {
                 pospix = 0
                 cmin = -INDEFR
                 do i = 1, npts {
                    if (Memr[pdpsi+i-1] > 0.) {
                       cmin=max(cmin,-(Memr[PLUCY(dat)+i-1]/Memr[pdpsi+i-1]))
                       pospix = pospix + 1
                    }
                 }
                 if (pospix == 0) cmin = -100.
               } else cmin = 1.

               if (cmin <= 1.0 && cmax >= 1.0) c = 1.0
               else c = (cmin + cmax) / 2.

              c = calc_d1 (Memr[PLUCYC(dat)], Memr[pdphi], Memr[praw], 
                  Memr[PBACK(dat)], Memr[PWEIGHT(dat)], npts, rnpts, center, 
                  bnpts, convar, cmax, cmin) 

              if (verbose) {
                 if (turbo) call printf ("Turbo iteration      ")
                 else call printf ("Accelerated iteration")
                 call printf ("  CMIN = %g, CMAX = %g, C = %g\n")
                   call pargr (cmin)
                   call pargr (cmax)
                   call pargr (c)
              }

              call amulkr (Memr[pdpsi], c, Memr[pdpsi], npts)
              call amulkr (Memr[pdphi], c, Memr[pdphi], npts)
          
              call aaddr (Memr[PLUCY(dat)], Memr[pdpsi], Memr[PLUCY(dat)],
                          npts)
              call aaddr (Memr[PLUCYC(dat)], Memr[pdphi], Memr[PLUCYC(dat)],
                          npts)

#             if turbo mode, save current delta phi and delta psi in
#             POPHI and POPSI

              if (accelerate == 2) {
                 call amovr (Memr[pdpsi], Memr[POPSI(dat)], npts)
                 call amovr (Memr[pdphi], Memr[POPHI(dat)], npts)
              }

end

real procedure calc_d1 (lucyc, dphi, raw, back, weight, npts, rnpts, center, 
                        bnpts, convar, cmax, cmin) 

real          lucyc[npts]
real          dphi [npts]
real          raw[npts]
real          back[bnpts]
real          weight[npts]

int           npts, rnpts
int           bnpts      

real          convar
real          cmin, cmax

bool          center

int           begpt, endpt
int           i, k

real          c
real          dc, dclim
real          model, phitwid
real          bval

double        f, dfdc

begin

              c = 1.0
              dclim = 0.01
              if (bnpts == 1) bval = back[1]

              if (center) {
                 begpt = (npts - rnpts)/2 + 1
                 endpt = begpt + rnpts - 1
              } else {
                 begpt = 1
                 endpt = rnpts
              }

#             Use modfied Newton's method to find maximum likelihood
#             Do no more than 20 iterations

              do k = 1, 20 {
                 f = 0.
                 dfdc = 0.
                 do i = begpt, endpt {
                    if (weight[i] > 0.0) {
                       if (bnpts == npts) bval = back[i]                 
                       model = weight[i] * (lucyc[i]+bval+c*dphi[i]) + convar
                       phitwid = weight[i] * (raw[i] + bval) + convar
                       f = f + dphi[i] * weight[i] * ((phitwid/model)-1)
                       dfdc = dfdc + phitwid * ((dphi[i]*weight[i])/model)**2
                    }
                 }
                 dc = (-f) / (-dfdc)

#                binary division of interval between c and cmax if 
#                overshooting cmax or between cmin and c if overshooting
#                cmin

                 if (c+dc >= cmax) dc = (cmax - c)/2.
                 else if (c+dc <= cmin) dc = (cmin - c)/2.
                 c = c+dc
                 call printf ("Iteration %d, c = %g, dc = %g\n")
                   call pargi (k)
                   call pargr (c)
                   call pargr (dc)
                 
                 if (abs(dc) <= dclim) return (c)
              }
              return (c)

end


