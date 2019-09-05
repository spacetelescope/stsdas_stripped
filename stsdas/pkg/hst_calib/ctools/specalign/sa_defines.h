# Definitions used by the SPECALIGN task, Version 2.0, January 1997.

# Minimum value of shift below which only integral shifting is done.
define  RS_MINDIST  0.001

# Shift option definitions.  RESAMPLE_FUNC is defined in the source
# as having the allowed values of: "|none|linear|spline3|poly3|poly5|".
define  RS_NONE             1
define  RS_LINEAR           2
define  RS_SPLINE3          3
define  RS_POLY3            4
define  RS_POLY5            5
