#! /usr/bin/env python
"""
print_asn:
    Prints out the tabular data from a FITS table.
    This implementation relies on IRAF and TABLES being
    installed.
    
Usage: print_asn.py [options] table

where 'table' is FITS table file and 
'options' are one or more of:
  -s  show row number for each row
  -c  print column names in output
  -a  align columns with headers
  -h  Print this message

Long versions of options:
  -s  --showrow
  -c  --colnames
  -a  --align
  -h  --help
  
Version 0.5, 09-June-2005 (WJH)
"""
from __future__ import division, print_function # confidence high

import os, sys

import pyraf
from pyraf import iraf
from pyraf.iraf import tables

yes = iraf.yes
no = iraf.no


def usage():
    print(__doc__)
    sys.stdout.flush()
    sys.exit()

import getopt
try:
    optlist, args = getopt.getopt(sys.argv[1:], "scah",
        ["showrow", "colnames","align","help"])
    if len(args) > 1:
        print('Error: more than one table specified')
        usage()
    if len(args) == 0 and len(optlist) == 0:
        usage()
except getopt.error as e:
    print(str(e))
    usage()

_out_width = 80
_show_row = no
_column_names = no
_align_cols = no

if optlist:
    for opt, value in optlist:
        if opt in( "-s", "--showrow"):
            _show_row = yes
        elif opt in ("-c", "--colnames"):
            _column_names = yes
        elif opt in ("-a", "--align"):
            _align_cols = yes
        elif opt in ("-h", "--help"):
            usage()
        else:
            print("Program bug, uninterpreted option", opt)
            raise SystemExit
del getopt

iraf.tprint(args[0],prparam=no,prdata=yes,pwidth=_out_width,
            plength=0,showrow=_show_row,orig_row=yes,showhdr=_column_names,
            showunits=yes,columns="",rows="-",option="plain",align=_align_cols,
            sp_col="",lgroup=0,mode="h")

        
