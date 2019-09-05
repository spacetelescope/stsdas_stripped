include <error.h>
include <imhdr.h>
include "janfft.h"

# jansson  -- restore an image by the jansson method
# This routine uses the jansson algorithm to deconvolve an image 
# Inputs - raw image, psf, model (used as first guess if given)
#          constant noise, conversion from DN to counts, number of iterations
# Output - deconvolved image
# task will work for both one and two dimensional images.  Model must be
# same size as raw image.
# E.B. Stobie   30-SEP-1991    Task jansson created.

procedure jansson ()

#--

pointer         infile               # input raw image name
pointer         psffile              # inpute psf image name
pointer         modfile              # input model image (optional first guess)
pointer         outfile              # output restored image name

int             niter                # number of desired iterations

int             irow                 # row to be printed

real            adu                  # number of counts per DN
real            noise                # constant noise
real            dfac                 # scale factor for correction
real            cutoff               # 
real            limchisq             # chi-square limit for convergence

pointer         sp                   # stack pointer
pointer         dat                  # pointer to data array structure
pointer         rim, pim             # pointers to input, psf images
pointer         jim, mim             # pointers to output, model images
pointer         jbuf                 # row pointer

int             rndim                # number of dimensions of raw image
int             rnpts, pnpts         # number of points for raw, PSF
int             rnrows, pnrows       # number of rows for raw, PSF
int             npts, nrows          # maximum number of points, rows
int             xloc, yloc           # array offsets for PSF
int             npix                 # total number of pixels
int             i, maxrow

real            mean, sigma
real            maxval, tmpmax

pointer         immap()
pointer         imgl1r(), imgl2r()
pointer         impl1r(), impl2r() 
int             clgeti()
real            clgetr()
real            ahivr()

define          trim         0.0

begin

                call smark (sp)
                call salloc (infile,  SZ_FNAME, TY_CHAR)
                call salloc (psffile, SZ_FNAME, TY_CHAR)
                call salloc (modfile, SZ_FNAME, TY_CHAR)
                call salloc (outfile, SZ_FNAME, TY_CHAR)
                
                call clgstr ("input",  Memc[infile],  SZ_FNAME)
                call clgstr ("psf",    Memc[psffile], SZ_FNAME) 
                call clgstr ("model",  Memc[modfile], SZ_FNAME)
                call clgstr ("output", Memc[outfile], SZ_FNAME)
 
                niter = clgeti ("niter")
                adu = clgetr ("adu")
                noise = clgetr ("noise")
                dfac = clgetr ("dfac")
                cutoff = clgetr ("cutoff")
                limchisq = clgetr("limchisq")
                if (limchisq <= 0.0) limchisq = 1.0

                irow = clgeti ("irow")

                rim = immap (Memc[infile], READ_ONLY, 0)
                pim = immap (Memc[psffile], READ_ONLY, 0)

                rndim = IM_NDIM(rim)

#               if dimensions of RAW image >= 2, dimensions of PSF  must be >= 2
#               if dimension of RAW image = 1 & dimension of PSF > 1,
#                  the row that contains the maximum (or first maximum) is used

                if (rndim >= 2 & IM_NDIM(pim) < 2) {
                   call imunmap (rim)
                   call imunmap (pim)
                   call sfree (sp)
                   call erract (EA_ERROR)
                }

                rnpts = IM_LEN(rim,1)
                pnpts = IM_LEN(pim,1)
                npts  = max(rnpts, pnpts)

                if (rndim == 1) {
                   rnrows = 1
                   pnrows = 1
                } else {
                   rnrows = IM_LEN(rim,2)
                   pnrows = IM_LEN(pim,2)
                }
                nrows = max(rnrows, pnrows)

#               arrays are allocated as 
#               (max(RAW(npts),PSF(npts)),max(RAW(nrows),PSF(nrows)))

                call malloc (dat, LEN_DAT, TY_STRUCT)
                npix = npts * nrows
                call calloc (PRAW(dat), npix, TY_REAL)
                call calloc (PJAN(dat), npix, TY_REAL)
                call calloc (PJANC(dat), npix, TY_REAL)

                NPTS(dat) = npts
                NROWS(dat) = nrows
                RNPTS(dat) = rnpts
                RNROWS(dat) = rnrows

#               move raw image to raw array (PRAW) starting at pixel(1,1)

                xloc = 1
                yloc = 1
                call movimr (rim, Memr[PRAW(dat)],npts, nrows, rnpts, rnrows,
                             xloc, yloc)
                
#               open output image (size of original raw image)

                jim = immap (Memc[outfile], NEW_COPY, rim)
                IM_LEN(jim,1) = rnpts
                IM_NDIM(jim) = min (rndim, 2)
                if (rndim == 1) {
                   jbuf = impl1r(jim)
                   call aclrr (Memr(jbuf),rnpts)
                } else {
                   IM_LEN(jim,2) = rnrows
                   jbuf = impl2r (jim,1)
                   call aclrr (Memr[jbuf],rnpts)
                }
                IM_PIXTYPE(jim) = TY_REAL
                call imunmap (jim)
                call imunmap (rim)

#               move psf image to temp psf array (PJANC(dat)) centering data in 
#                array and trim data < 0. to 0.

                xloc = (npts - pnpts)/2 + 1
                if (xloc < 1) xloc = 1
                if (rndim == 1) {
                   if (IM_NDIM(pim) == 1) {
                      jbuf = imgl1r(pim)
                      call amovr(Memr[jbuf],Memr[PJANC(dat)+xloc-1],pnpts)
                   } else {
                      jbuf = imgl2r (pim, 1)
                      maxval = ahivr (Memr[jbuf], pnpts)
                      maxrow = 1
                      do i = 2, IM_LEN(pim,2) {
                         jbuf = imgl2r (pim, i)
                         tmpmax = ahivr (Memr[jbuf], pnpts)
                         if (tmpmax > maxval) {
                            maxval = tmpmax
                            maxrow = i
                         }
                      }
                      jbuf = imgl2r (pim, maxrow)
                      call amovr (Memr[jbuf], Memr[PJANC(dat)+xloc-1], pnpts)
                      call printf (
                       "Maximum %g found in row %d of 2-dimensional psf\n")
                        call pargr (maxval)
                        call pargi (maxrow)
                      call printf (" Row %d of psf used in jan deconvolution\n")
                        call pargi (maxrow)
                   }
                } else {
                   yloc = (nrows - pnrows)/2 + 1
                   if (yloc < 1) yloc = 1
                   call movimr (pim, Memr[PJANC(dat)], npts, nrows, pnpts,
                                pnrows, xloc, yloc)
                }
                call imunmap (pim)
                call arltr (Memr[PJANC(dat)], npix, trim, trim)

#               if first guess given, load into jan array

                if (Memc[modfile] == EOS) {
                   call aavgr (Memr[PRAW(dat)],npix, mean, sigma)
                   call amovkr (mean, Memr[PJAN(dat)], npix)
                }
                else {
                   mim = immap (Memc[modfile], READ_ONLY, 0)
                   if (rndim == 1) {
                      jbuf = imgl1r (mim)
                      call amovr (Memr[jbuf], Memr[PJAN(dat)], rnpts)
                   } else {
                      xloc = 1
                      yloc = 1
                      call movimr (mim, Memr[PJAN(dat)], npts, nrows, rnpts,
                                   rnrows, xloc, yloc)
                   }
                   call imunmap(mim)
                }  

 #              call deconvolution routine & store result in jansson image

                if (rndim == 1) {
                   call jan1d(dat, adu, noise, dfac, cutoff, niter, limchisq)
                   jim = immap (Memc[outfile], READ_WRITE, 0)
                   jbuf = impl1r(jim)
                   call amovr (Memr[PJAN(dat)], Memr(jbuf), rnpts)
                } else {
                   call jan2d(dat, adu, noise, dfac, cutoff, niter, limchisq, 
                              irow)
                   jim = immap (Memc[outfile], READ_WRITE, 0)
                   do i = 1, rnrows {
                      jbuf = impl2r(jim,i)
                      call amovr(Memr[PJAN(dat)+(i-1)*npts], Memr(jbuf),
                                 rnpts)
                   }
                }
 
                call imunmap(jim)
                call mfree (PJANC(dat), TY_REAL)
                call mfree (PJAN(dat), TY_REAL)
                call mfree (PRAW(dat), TY_REAL)
                call mfree (dat, TY_STRUCT)

                call sfree (sp)
end

include "janfft.h"

# 1-D deconvolution 

procedure jan1d (dat, adu, noise, dfac, cutoff, niter, limchisq)

pointer      dat             # pointer to data array structure
real         adu             # number of counts per DN
real         noise           # constant noise
real         dfac            #
real         cutoff          #
int          niter           # number of iterations desired
real         limchisq        # chi-squared limit for convergence

pointer      work            # pointer to fft work area structure
pointer      pxx             # pointer to temporary complex array
pointer      pxr             # pointer to temporary real array

int          i, amsh, xmax
int          npts, rnpts
int          nrows, iter

real         val, scal, var
real         diff, xdiff, corr
real         chisq, lchisq
real         tnum, tden

double       tchi

real         ahivr()

begin

             call jfta1d(dat, work, pxx, pxr)
             npts = NPTS(dat)
             nrows = NROWS(dat)
             rnpts = RNPTS(dat)

#            find maximum value of PSF and shift to pixel (1,1)

             val = ahivr (Memr[PJANC(dat)], npts)
             xmax = 1
             do i = 1, npts {
                if (Memr[PJANC(dat)+i-1] == val) {
                   xmax = i
                   break
                }
             }
             amsh = -xmax + 1
             call rwshift (npts, amsh, Memr[PJANC(dat)], Memr[pxr])
             call jfftf1 (work, Memr[pxr], Memx[PFPSF(dat)], npts)

#            normalize forward transform of psf by first pixel  

             scal = Memx[PFPSF(dat)]
             call advkxr (Memx[PFPSF(dat)], scal, Memx[PFPSF(dat)], npts)

             call jfftf1 (work, Memr[PJAN(dat)], Memx[pxx], npts)
             call amulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npts)
             call jfftb1 (work, Memx[pxx], Memr[PJANC(dat)], npts)

             tchi = 0.
             do i = 1, npts {
                tnum = adu**2 * (Memr[PJANC(dat)+i-1) - Memr[PRAW(dat)+i-1])**2
                tden = adu * Memr[PJANC(dat)+i-1] + noise**2
                tchi = tchi + tnum / tden
             }
             lchisq = tchi / real(npts-1)
             call printf (" Initial chisq = %g \n")
                call pargr (lchisq)

#            main loop done niter times unless convergence reached before then

             do iter = 1, niter {

#               save current jan image in temporary array (pxr)
                call amovr (Memr[PJAN(dat)], Memr[pxr], npts)

                do i = 1, npts {
                   xdiff = Memr[PRAW(dat)+i-1] - Memr[PJANC(dat)+i-1]
                   var = adu * Memr[PJANC(dat)+i-1] + noise**2
                   diff = dfac * xdiff**2 / var
                   if ( diff > 1.0 ) diff = 1.0
                   corr = Memr[PJAN(dat)+i-1] / cutoff
                   if ( corr > 1.0) corr = 1.0
                   Memr[PJAN(dat)+i-1] = Memr[PJAN(dat)+i-1] + corr*diff*xdiff
                }
                call jfftf1 (work, Memr[PJAN(dat)], Memx[pxx], npts)
                call amulx (Memx[pxx], Memx[PFPSF(dat)], Memx[pxx], npts)
                call jfftb1 (work, Memx[pxx], Memr[PJANC(dat)], npts)
                    
#               compute chi squared

                tchi = 0.
                do i = 1, npts {
                   tnum = adu**2 * (Memr[PJANC(dat)+i-1]-Memr[PRAW(dat)+i-1])**2
                   tden = adu * Memr[PJANC(dat)+i-1] + noise**2
                   tchi = tchi + tnum/tden
                }
                chisq = tchi / real(npts-1)
                call printf ("Iteration %d : chi-sq = %g\n")
                     call pargi (iter)
                     call pargr (chisq)
                     call flush (STDOUT)
                if (chisq <= limchisq) {
                   call printf (" chi-sq <= %g, converged\n")
                      call pargr (limchisq)
                   break
                } else if (chisq > lchisq) {
                   call printf ("Chisq minimum %g reached, converged \n")
                      call pargr (lchisq)
                   call amovr (Memr[pxr], Memr[PJAN(dat)], npts)
                   break
                }
             }
             call jftf1d (dat, work, pxx, pxr)

end

include "janfft.h"

# 2-D deconvolution 

procedure jan2d (dat, adu, noise, dfac, cutoff, niter, limchisq, irow)

pointer      dat             # pointer to data array structure
real         adu             # number of counts per DN
real         noise           # constant noise
real         dfac            #
real         cutoff          #
int          niter           # number of iterations desired

int          irow            # row to be printed

real         limchisq        # chi-squared limit for convergence

pointer      work            # pointer to fft work area structure
pointer      pxx             # pointer to temporary complex array
pointer      pxr             # pointer to temporary real array

int          i, amsh[2]
int          xmax, ymax
int          npts, rnpts
int          nrows, rnrows
int          npix, iter

real         val, scal
real         diff, corr
real         xdiff, var
real         chisq, lchisq
real         jcalchi2()

begin

             call jfta2d (dat, work, pxx, pxr)
             npts = NPTS(dat)
             nrows = NROWS(dat)
             rnpts = RNPTS(dat)
             rnrows = RNROWS(dat)
             npix = npts * nrows

#            find maximum value of psf & shift to pixel(1,1)

             call aymax (Memr[PJANC(dat)], npts, nrows, val, xmax, ymax)
             amsh[1] = -xmax + 1
             amsh[2] = -ymax + 1
             call imshift (npts, nrows, amsh, Memr[PJANC(dat)], Memr[pxr])
             call jfftf2 (work, Memr[pxr], Memx[PFPSF(dat)], npts, nrows)

#            normalize forward transform of psf by first pixel 

             scal = Memx[PFPSF(dat)]
             call advkxr (Memx[PFPSF(dat)], scal, Memx[PFPSF(dat)], npix)


             call jfftf2 (work, Memr[PJAN(dat)], Memx[pxx], npts, nrows)
             call amulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npix)
             call jfftb2 (work, Memx[pxx], Memr[PJANC(dat)], npts, nrows)

             lchisq = jcalchi2 (Memr[PRAW(dat)], Memr[PJANC(dat)], npts,
                       nrows, rnpts, rnrows, adu, noise)
             call printf (" Initial chi-sq= %g \n")
                call pargr (lchisq)

#            main loop done niter times unless convergence reached before then

             do iter = 1, niter {
 
#               save current jan image in temporary array (pxr)
                call amovr (Memr[PJAN(dat)], Memr[pxr], npts)

#               compute difference and corrective factor over image
#               adjust jan image accordingly

                do i = 1, npix {

                   xdiff = Memr[PRAW(dat)+i-1] - Memr[PJANC(dat)+i-1]
                   var = adu * Memr[PJANC(dat)+i-1] + noise**2
                   diff = dfac * xdiff**2 / var
                   if (diff > 1.0) diff = 1.0
                   corr = Memr[PJAN(dat)+i-1] / cutoff
                   if (corr > 1.0) corr = 1.0
                   Memr[PJAN(dat)+i-1] = Memr[PJAN(dat)+i-1] + corr*xdiff*diff
                   if (irow > 0) call printf (
                      "col=%d  raw=%6g  janc=%6g  diff=%6g  corr=%g  jan=%6g\n")
                       call pargi (irow)
                       call pargr (Memr[PRAW(dat)+i-1])
                       call pargr (Memr[PJANC(dat)+i-1])
                       call pargr(diff)
                       call pargr (corr)
                       call pargr (Memr[PJAN(dat)+i-1])
                }
  
                call jfftf2 (work, Memr[PJAN(dat)], Memx[pxx], npts, nrows)
                call amulx (Memx[PFPSF(dat)], Memx[pxx], Memx[pxx], npix)
                call jfftb2 (work, Memx[pxx], Memr[PJANC(dat)], npts, nrows)

#               compute chi squared

                chisq = jcalchi2 (Memr[PRAW(dat)], Memr[PJANC(dat)], npts,
                                 nrows, rnpts, rnrows, adu, noise)
                call printf ("Iteration %d : chi-sq = %g\n")
                     call pargi (iter)
                     call pargr (chisq)
                     call flush (STDOUT) 
                if (chisq <= limchisq) {
                   call printf (" chi-sq <= %g,  converged\n")
                      call pargr (limchisq) 
                   break
                } else if (chisq > lchisq) {
                  call printf (
                  " Chi-sq minimum %g has been achieved, converged \n")
                      call pargr (lchisq)
                  call amovr (Memr[pxr], Memr[PJAN(dat)], npts)
                  break
                } 
                lchisq = chisq
             }

             call jftf2d (dat, work, pxx, pxr)

end        

include "janfft.h"

#            allocate fft work area and data arrays for one-dimensional case

procedure jfta1d(dat, work, pxx, pxr)

pointer      dat              # pointer to data arrays
pointer      work             # pointer to fft work area
pointer      pxx              # pointer to temporary complex array
pointer      pxr              # pointer to temporary real array

int          npts             # number of points in array
int          wsiz             # siz of trig table

begin

             npts = NPTS(dat)
             call malloc (PFPSF(dat), npts, TY_COMPLEX)

             call malloc (work, LEN_WORK, TY_STRUCT)
             wsiz = npts * 4 + 15
             call calloc (TRIGTAB1(work), wsiz, TY_REAL)
             call cffti (npts, Memr[TRIGTAB1(work)])

             call malloc (pxr, npts, TY_REAL)
             call malloc (pxx, npts, TY_COMPLEX)

end

include "janfft.h"

#            allocate fft work area and data arrays for two-dimensional case

procedure jfta2d(dat, work, pxx, pxr)

pointer      dat                # pointer to data arrays
pointer      work               # pointer to fft work structure
pointer      pxx                # pointer to temporary complex array
pointer      pxr                # pointer to temporary real array

int          npts, nrows        # number of points, rows in array
int          npix               # total number of points in array
int          wsiz               # size of trig table

begin

             npts = NPTS(dat)
             nrows = NROWS(dat)
             npix = npts * nrows

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

include "janfft.h"

#            deallocate fft work area and data arrays for one-dimensional case

procedure jftf1d(dat, work, pxx, pxr)

pointer      dat              # pointer to data arrays
pointer      work             # pointer to fft work area
pointer      pxx              # pointer to temporary complex array
pointer      pxr              # pointer to temporary real array

begin

             call mfree (pxx, TY_COMPLEX)
             call mfree (pxr, TY_REAL)

             call mfree (XWORK(work), TY_COMPLEX)
             call mfree (TRIGTAB1(work), TY_REAL)
             call mfree (work, TY_STRUCT)

             call mfree (PFPSF(dat), TY_COMPLEX)

end

include "janfft.h"

#            deallocate fft work area and data arrays for two-dimensional case

procedure jftf2d (dat, work, pxx, pxr)

pointer      dat              # pointer to data arrays
pointer      work             # pointer to fft work area
pointer      pxx              # pointer to temporary complex array
pointer      pxr              # pointer to temporary real array

begin

             call mfree (pxx, TY_COMPLEX)
             call mfree (pxr, TY_REAL)

             call mfree (XWORK(work), TY_COMPLEX)
             call mfree (TRIGTAB1(work), TY_REAL)
             call mfree (TRIGTAB2(work), TY_REAL)
             call mfree (work, TY_STRUCT)

             call mfree (PFPSF(dat), TY_COMPLEX)

end

include "janfft.h"

#            forward fast fourier transform a one dimensional array using
#            NCAR fft routines.  Normalization is the last step.  Input is
#            real, output is complex.

procedure jfftf1 (work, input, output, npts)

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
                
include "janfft.h"

#            forward fast fourier transform a two dimensional array using
#            NCAR fft routines.  For two dimensional case, each row is 
#            transformed, then each column.  Normalization is the last step.
#            Input is real,  Output is complex

procedure jfftf2 (work, input, output, npts, nrows)

pointer      work                  # fft work structure
real         input[npts,nrows]     # input array 
complex      output[npts,nrows]    # output array
int          npts                  # number of points in row
int          nrows                 # number of rows

int          i,j,npix
real         xnpix

begin

             npix = npts * nrows

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

include "janfft.h"

#            inverse fast fourier transform a one dimensional aray using
#            the NCAR fft routines.  No normalization is done.  Input is
#            complex, output is real.

procedure jfftb1(work, input, output, npts)

pointer      work                # fft work structure
complex      input[npts]         # input array
real         output[npts]        # output array
int          npts                # number of points

begin

#            transform data, then extract real part

             call cfftb (npts, input, Memr[TRIGTAB1(work)])
             call achtxr (input, output, npts)

end

include "janfft.h"

#            inverse fast fourier transform a two dimensional array using
#            NCAR fft routines.  For two dimensional case, each row is 
#            transformed, then each column. 
#            Input is complex,  Output is real.

procedure jfftb2 (work, input, output, npts, nrows)

pointer      work                  # fft work structure
complex      input[npts,nrows]     # input array 
real         output[npts,nrows]    # output array
int          npts                  # number of points in row
int          nrows                 # number of rows

int          i,j,npix

begin

             npix = npts * nrows

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

#            compute and return value of chi-squared
#
#            (adu^2 * (jan convolved - raw image)^2)
#              / (adu * jan convolved + constant noise^2)

real procedure jcalchi2 (raw, jan, npts, nrows, onpts, onrows, adu, noise)

real         raw[npts,nrows]     # raw data array
real         jan[npts,nrows]     # jan convolved array
int          npts                # number of points in row of raw data array
int          nrows               # number of rows in raw data array
int          onpts               # number of points in row of original raw data
int          onrows              # number of rows in original raw data
real         adu                 # number of electrons per DN
real         noise               # constant noise

real         chisq               # returned chisq value

int          i,j                 
real         noise2, adu2
real         tnum, tden, npix
double       tchi

begin

             tchi = 0.
             adu2 = adu**2
             noise2 = noise**2
             npix = real(onpts * onrows) - 1.
             do j = 1, onrows {
                do i = 1, onpts {
                   tnum = adu2 * (jan[i,j] - raw[i,j])**2
                   tden = adu * jan[i,j] + noise2
                   tchi = tchi + tnum / tden
                }
             }

             chisq = tchi / npix
             return (chisq)

end

