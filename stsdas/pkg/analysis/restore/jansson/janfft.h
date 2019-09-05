#  include  janfft.h   --   include file for STSDAS jansson algorithm
#                            (image restoration)
#                            defines pointers to primary data arrays

define       LEN_DAT          8

define       NPTS             Memi[$1]
define       NROWS            Memi[$1+1]
define       RNPTS            Memi[$1+2]
define       RNROWS           Memi[$1+3]

define       PRAW             Memi[$1+4]
define       PJAN             Memi[$1+5]
define       PJANC            Memi[$1+6]

define       PFPSF            Memi[$1+7]




define       LEN_WORK         3

define       TRIGTAB1         Memi[$1]
define       TRIGTAB2         Memi[$1+1]
define       XWORK            Memi[$1+2]
