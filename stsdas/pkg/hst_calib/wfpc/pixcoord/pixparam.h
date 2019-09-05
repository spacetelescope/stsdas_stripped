# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# PIXPARAM.H -- Parameter structure definitions

define	PAR_LENGTH	7
define	PAR_NUMSTR	9
define	PAR_NUMINT	3
define	PAR_MAXSTR	63

define	PAR_STRNAMES	"image,catalog,name,ra,dec,coordfile,tvcmd,label,\
mkcolor"
define	PAR_INTNAMES	"nterm,boxsize,tvframe,mksize"
define	PAR_FLAGNAMES	"im,tv,cat,coord"

define	PAR_BUFFER	Memi[$1]
define	PAR_STRVEC	Memi[$1+1]
define	PAR_INTVEC	Memi[$1+2]
define	PAR_IMFLAG	Memi[$1+3]
define	PAR_TVFLAG	Memi[$1+4]
define	PAR_CATFLAG	Memi[$1+5]
define	PAR_COORDFLAG	Memi[$1+6]

define	PAR_STRPTR	Memi[PAR_STRVEC($1)+$2-1]
define	PAR_INTVAL	Memi[PAR_INTVEC($1)+$2-1]
define	PAR_STRVAL	Memc[PAR_STRPTR($1,$2)]

