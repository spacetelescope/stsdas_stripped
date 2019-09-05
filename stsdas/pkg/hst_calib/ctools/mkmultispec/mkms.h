# Define the length of the attribute strings.
define  ATTR_LEN        68

# Define the generic format for the first line of attributes.
# Note in the length, 5 is added for fudge factor.
define  ATTR_FORMAT     "%10d %10d %3d %25.16g %25.16g %10d %25.16g %12.2f %12.2f"

# Define the available functions.
define	MK_FUNCTIONS	"|chebyshev|legendre|spline3|spline1|table|"
define  MK_CHEBYSHEV    1
define  MK_LEGENDRE     2
define  MK_SPLINE3      3
define  MK_SPLINE1      4
define  MK_TABLE        5

# Define various parameter names.
define  FUNCTION        "function"
define  GROW            "grow"
define  HIGH_IC         "high"
define  HIGH_PAR        "high_reject"
define  LOW_IC          "low"
define  LOW_PAR         "low_reject"
define  MREJ            "markrej"
define  NITER           "niterate"
define  ORDER_IC        "order"
define  ORDER_PAR       "nterms"

#-----
# The following is used to define the list access interface.
# Define how many lists will be maintained.  There are 4 in this case, the
# input, wavelength, weights, and output lists.
define  INPUT           1
define  OUTPUT          2
define  WAVE            3
define  WEIGHT          4
define  N_LISTS         4

# Define various sizes of the structures.
define  FILE_SIZE       SZ_LINE
define  TMPLT_SIZE      SZ_LINE

# The lists structure
define  MK_TMPLT_PTR    Memi[$1]        # Pointer to template names.
define  MK_TEMPLATE     Memc[MK_TMPLT_PTR($1)+(($2-1)*TMPLT_SIZE)]
define  MK_LD_PTR       Memi[$1+1]      # Pointer to list descriptors.
define  MK_LD           Memi[MK_LD_PTR($1)+$2-1]
define  MK_FILE_PTR     Memi[$1+2]      # The current file names.
define  MK_FILE         Memc[MK_FILE_PTR($1)+(($2-1)*FILE_SIZE)]
define  MK_LEN_PTR      Memi[$1+3]      # Number of entries in the lists.
define  MK_LEN          Memi[MK_LEN_PTR($1)+$2-1]
define  MK_NEW_PTR      Memi[$1+4]      # Whether a new file exists or not.
define  MK_NEW          Memi[MK_NEW_PTR($1)+$2-1]
define  MK_SZ_LISTS     5               # Size of the list structure.

#-----
# Below describes the structure and access to the MK_IMIO structure.
define  IO_FD           Memi[$1]        # The image/table descriptor
define  IO_TYPE         Memi[$1+1]      # TABLE/IMAGE flag.
define  IO_CD_PTR       Memi[$1+2]      # Table column descriptor.
define  IO_CD           Memi[IO_CD_PTR($1)+$2-1]
define  IO_LEN          Memi[$1+3]      # Dimension of the data.
define  IO_NGRP         Memi[$1+4]      # Number of groups in image.
define  IO_GRP          Memi[$1+5]      # Current open group.
define  IO_NAME_PTR     Memi[$1+6]      # Specified file name.
define  IO_NAME         Memc[IO_NAME_PTR($1)]
define  MK_SZ_IO        7               # Size of structure.

# The flag of what type of file we are dealing with.
define  TABLE           1
define  IMAGE           2

#-----
# Below describes the structure and access to the fit descriptor.
define  FIT_N_MW        Memi[$1]        # Number of MWCS descriptors.
define  FIT_MAX_MW      Memi[$1+1]      # Maximum possible MWCS' to store.
define  FIT_MW_PTR      Memi[$1+2]      # The MW descriptors of the fits.
define  FIT_MW          Memi[FIT_MW_PTR($1)+$2-1]
define  FIT_LEN         Memi[$1+3]      # The length of the arrays.
define  FIT_X_PTR       Memi[$1+4]      # The X array to fit.
define  FIT_X           Memd[FIT_X_PTR($1)+$2-1]
define  FIT_Y_PTR       Memi[$1+5]      # The Y array to fit.
define  FIT_Y           Memd[FIT_Y_PTR($1)+$2-1]
define  FIT_W_PTR       Memi[$1+6]      # The weight array to fit.
define  FIT_W           Memd[FIT_W_PTR($1)+$2-1]
define  FIT_IC          Memi[$1+7]      # The ICFIT descriptor.
define  FIT_GT          Memi[$1+8]      # The GTOOLS descriptor.
define  FIT_GP          Memi[$1+9]      # The GIO descriptor.
define  FIT_LARGE       Memi[$1+10]     # The largest number of WAT2 keywords.
define  FIT_INIT_FUNC   Memi[$1+11]     # Numeric value of initial function.
define  FIT_LABEL_PTR   Memi[$1+12]     # Label of wavelengths.
define  FIT_LABEL       Memc[FIT_LABEL_PTR($1)]
define  FIT_UNITS_PTR   Memi[$1+13]     # Units of wavelengths.
define  FIT_UNITS       Memc[FIT_UNITS_PTR($1)]
define  MK_SZ_FIT       14              # Number of elements in structure.
