#{ SPECRES - Package for spectroscopic restoration (Jeremy Walsh)

cl < "specres$lib/zzsetenv.def"
package specres, bin = specresbin$

task specinholucy,
     specpsf,
     specholucy = "specressrc$x_specres.e"

clbye
