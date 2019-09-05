# These series of strfits commands will convert the fits files
# into Geis files in the parent directory. Template with the Group
# parameter information are necesary to recreate the original
# image file. Note that the NICMOS and WFPC2 PSF images stay in FITS
# format; they are simply copied to the scidata directory.
#
# Feed this command to the cl by typing   "cl < read_fits.cl"
#
set imtype=hhh
strfits.oldirafname = no
strfits foc.fits '' ../scidata/foc.hhh  
strfits fos.fits '' ../scidata/fos.hhh 
strfits hsp.fits '' ../scidata/hsp.hhh 
strfits hrs.fits '' ../scidata/hrs.hhh 
strfits cosiaf.fits '' ../scidata/cosiaf.tab
strfits czptnelinec.fits '' ../scidata/czptnelinec.tab
strfits czscoffc.fits '' ../scidata/czscoffc.tab
strfits saa.fits '' ../scidata/saa.tab
strfits de200.fits '' ../scidata/de200.tab
copy de200.fits ../scidata/de200.fits
strfits lftemplate.fits '' ../scidata/lftemplate.hhh
strfits xmatrix.fits '' ../scidata/xmatrix.hhh
strfits ymatrix.fits '' ../scidata/ymatrix.hhh
copy    nicmos_psf.cat  ../scidata/
copy    nic1_psf_*.fits ../scidata/
copy    nic2_psf_*.fits ../scidata/
copy    nic3_psf_*.fits ../scidata/
copy    wfpc2_psf.cat   ../scidata/
copy    wfpc2_1_*.fits  ../scidata/
copy    wfpc2_2_*.fits  ../scidata/
copy    wfpc2_3_*.fits  ../scidata/
copy    wfpc2_4_*.fits  ../scidata/
