#{ IMPOL - Package for imaging polarimetry (Jeremy Walsh)

cl < "impol$lib/zzsetenv.def"
package impol, bin = impolbin$

task polimplot,
     polimodel,
     hstpolima,
     hstpolsim,
     hstpolpoints = "impolsrc$x_impol.e"

clbye
