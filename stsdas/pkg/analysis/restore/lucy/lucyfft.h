#  include  lucyfft.h   --   include file for STSDAS lucy algorithm
#                            (image restoration)
#                            defines pointers to primary data arrays

define       LEN_DAT          14

define       NPTS             Memi[$1]
define       NROWS            Memi[$1+1]
define       RNPTS            Memi[$1+2]
define       RNROWS           Memi[$1+3]
define       XOUT             Memi[$1+4]
define       YOUT             Memi[$1+5]

define       PWEIGHT          Memi[$1+6]
define       PNORM            Memi[$1+7]
define       PLUCY            Memi[$1+8]
define       PBACK            Memi[$1+9]
define       PLUCYC           Memi[$1+10]
define       POPHI            Memi[$1+11]
define       POPSI            Memi[$1+12]

define       PFPSF            Memi[$1+13]

define       LEN_WORK         3

define       TRIGTAB1         Memi[$1]
define       TRIGTAB2         Memi[$1+1]
define       XWORK            Memi[$1+2]

