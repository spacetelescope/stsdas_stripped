.help calpar.h Jul97 source
.ih
NAME
calpar.h -- Include file for the calibration parameter routines.
.endhelp
#---------------------------------------------------------------------------

# Define the standard pset names.
define  STD_FOC_PAR     "ckwfoc"
define  STD_FOS_PAR     "ckwfos"
define  STD_HRS_PAR     "ckwhrs"
define  STD_HSP_PAR     "ckwhsp"
define  STD_WFPC2_PAR   "ckwwfp2"
define  STD_WFPC_PAR    "ckwwfpc"
define  STD_NICMOS_PAR  "ckwnicmos"
define  STD_STISCI_PAR  "ckwstis1"
define  STD_STISCS_PAR  "ckwstis2"
define  STD_STISMI_PAR  "ckwstis3"
define  STD_STISMS_PAR  "ckwstis4"
define  STD_ACSCCD_PAR  "ckwacs1"
define  STD_ACSSBC_PAR  "ckwacs2"

# Define detectors and obstypes for ACS.
define  SBC             "sbc"
define  WFC             "wfc"
define  HRC             "hrc"

# Define detectors and obstypes for STIS.
define  CCD             "ccd"
define  FUV_MAMA        "fuv-mama"   
define  NUV_MAMA        "nuv-mama"   
define  SPECTROSCOPIC   "spectroscopic"
define  IMAGING         "imaging"

# Define which instruments are supported.
define  INSTR_DICT      "|foc|fos|hrs|hsp|wfpc|wfpc2|nicmos|stis|acs"
define  NO_INSTR        0
define  FOC             1
define  FOS             2
define  HRS             3
define  HSP             4
define  WFPC            5
define  WFPC2           6
define  NICMOS          7
define  STIS            8
define  ACS             9

# Define the answers to the accept question.  This must match the order
# of the values in the ACCEPT_DICT macro.
define  ACCEPT_DICT     "|no|yes|abort"
define  ACCEPT_NO       1
define  ACCEPT_YES      2
define  ACCEPT_ABORT    3

# Define a set of values that will be converted to uppercase when
# written to the image and converted to lower case when read from an image.
define  UPPER_STR_DIC   "|perform|omit|yes|no|complete|done|real|short|long|t|n|f"

# Define the parameter name that contains the instrument specification.
define  INSTR_PAR       "instrument"
define  DETECTOR_PAR    "detector"
define  OBSTYPE_PAR     "obstype"

# Define the parameters that should not be included in any modifications,
# listings, etc.
define  BAD_PARS        "|instrument|detector|obstype|mode|Version"

# Define the memory structure used to store parameter names and current values
# of those parameters.
define  CP_GROWTH       50              # Number of parameters to grow the memory
define  CP_PNAME_SIZE   SZ_LINE         # Size of the pset name.
define  CP_PAR_SIZE     SZ_LINE         # Maximum size of parameter names.
define  CP_VALUE_SIZE   SZ_LINE         # Maximum size of parameter values.

define  CP_NPARAMS      Memi[$1]        # Number of parameters stored.
define  CP_PAR_PTR      Memi[$1+1]      # The parameter names.
define  CP_PAR          Memc[CP_PAR_PTR($1)+(($2-1)*CP_PAR_SIZE)]
define  CP_VALUE_PTR    Memi[$1+2]      # The parameter values.
define  CP_VALUE        Memc[CP_VALUE_PTR($1)+(($2-1)*CP_VALUE_SIZE)]
define  CP_PSET         Memi[$1+3]      # The pset pointer structure.
define  CP_MAXPARAMS    Memi[$1+4]      # Maximum number of params for this.
define  CP_PNAME_PTR    Memi[$1+5]      # Pset name pointer.
define  CP_PNAME        Memc[CP_PNAME_PTR($1)]
define  CP_SIZE         6               # Size of memory structure.
