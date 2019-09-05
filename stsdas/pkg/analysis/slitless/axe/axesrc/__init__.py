"""
"""
from __future__ import absolute_import
__version__ = "2.0"

from .axetasks    import *
from .axesimtasks import *

def straighten_string(in_string):
    if in_string != None:
        in_string = in_string.strip()
        if len(in_string) < 1:
            in_string = None
    return in_string
