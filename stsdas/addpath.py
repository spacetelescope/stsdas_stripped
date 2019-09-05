""" Inserts the directory containing links 
    to all Python-based tasks in the STSDAS tree to 
    the default Python path.
"""
from __future__ import division # confidence high

import iraf,sys

#Define path to top level Python directory in STSDAS
_path = iraf.osfn('stsdas$python')

# If directory not already in PYTHONPATH,...
if _path not in sys.path:
    # Add the directory to path.
    sys.path.insert(1,_path)
