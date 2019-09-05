#{ IMRES - Package for image restoration utilities

cl < "imres$lib/zzsetenv.def"
package imres, bin = imresbin$

task apomask,
     seeing,
     cplucy = "imressrc$x_imres.e"

clbye
