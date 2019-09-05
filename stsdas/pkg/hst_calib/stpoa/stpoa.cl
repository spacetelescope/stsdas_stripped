#{ STPOA IRAF layered package
# Version 1.0, Anastasia Alexov, July 2000

cl < "stpoa$lib/zzsetenv.def"

package stpoa, bin = stpoabin$

       set  poa_fos          = "stpoa$poa_fos/"

       task poa_fos.pkg      = "stpoa$poa_fos/poa_fos.cl"

clbye ()

