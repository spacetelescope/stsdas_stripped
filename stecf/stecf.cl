# STECF IRAF layered package
# Version 1.0, Richard Hook, December 1999
# Version 1.1, Richard Hook, November 2000
# Version 1.2, Richard Hook, July 2001
# Version 1.5, Richard Hook, March 2003
# Version 2.0, Richard Hook, May 2010

package stecf

set impol = stecf$impol/
task impol.pkg = impol$impol.cl

set driztools = stecf$driztools/
task driztools.pkg = driztools$driztools.cl

set imres = stecf$imres/
task imres.pkg = imres$imres.cl

set specres = stecf$specres/
task specres.pkg = specres$specres.cl

clbye()
