#{ FITSIO.CL -- The FITSIO package.

cl < "tables$lib/zzsetenv.def"
package fitsio, bin = tablesbin$

task    catfits,
        strfits,
        stwfits,
        gftoxdim,
        fits2tape,
        xdimtogf = "tables$pkg/fitsio/x_fitsio.e"

clbye()

