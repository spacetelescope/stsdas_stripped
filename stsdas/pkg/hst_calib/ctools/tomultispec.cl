################################################################################
#
# TOMULTISPEC.CL - IRAF Script to obtain selected rows from a FITS 
# 3-D binary table and convert the data into OIF MULTISPEC format for use 
# with IRAF spectral analysis tasks.  The desired rows must be designated 
# using the 'selectors' syntax appended to the input file name 
# (i.e, stis_x1d.fits[r:row=(2:5,8)]).  Please see the help file for 
# 'selectors' for details on the row/column selector syntax.
#
# Version    Author          Date
# -------------------------------
#   1.0      M. De La Pena   12 August 1997
#   1.1      M. De La Pena   18 August 1997  Mods: Enforce MIN/MAX, fix logic
#                            between NROWS and INTERACTIVE, add code to
#                            determine spectral order number, and temporary
#                            files written to a temporary directory.
#   1.2      M. De La Pena   20 August 1997  Mods: Output from 'tximage' have
#                            temporary names with "hhh" extensions, extended 
#                            missing package warning, check full pathname for 
#                            input file, nrows <= 0.
#   1.3      M. De La Pena   12 January 1998 Mod: Revamped tmp output filenames.
#   1.4      M. De La Pena   20 January 1998 Modified access statement which 
#                            did not understand FITS extension syntax.
#   1.5      M. De La Pena   11 April 2000 Fixed outfile to include specified 
#                            directory, use 'tcopy' instead of 'tscopy'.
#   1.6      P. Hodge/MDD    10 May 2000 Replaced 'tcopy' with 'tinfo' to make
#                            task more robust wrt spectral order numbers.
#
################################################################################

procedure tomultispec (input, output)

file   input       {"",           prompt = "Input 3-D FITS binary table"}
file   output      {"",           prompt = "Output OIF multispec file name"}
string flux_col    {"FLUX",       prompt = "Input column containing fluxes"}
string wave_col    {"WAVELENGTH", prompt = "Input column containing wavelengths"}
string function    {"chebyshev",  prompt = "Function to fit the wavelengths", \
                                  enum   = "|legendre|chebyshev|spline3|\
                                             spline1|table"}
int    nterms      {4, min=1,     prompt = "Number of terms or splice pieces to use for fit"}
string label       {"Wavelength", prompt = "Label for the MULTISPEC system"}
string units       {"Angstroms",  prompt = "Units of the MULTISPEC system"}
real   low_reject  {2.0, min=0.0, prompt = "Low rejection in sigma of fit"}
real   high_reject {4.0, min=0.0, prompt = "High rejection in sigma of fit"}
int    niterate    {0, min=0,     prompt = "Number of rejection iterations"}
real   grow        {1., min=0.,   prompt = "Rejection growing radius in pixels"}
string weight      {"",           prompt = "Weight file name template"}
string format      {"%g",         prompt = "Format for writing coefficients"}
bool   interactive {no,           prompt = "Interactively perform the fitting?"}
string device      {"stdgraph",   prompt = "Graphics device to use for interactive fitting"}
bool   markrej     {yes,          prompt = "Mark rejected points?"}
bool   verbose     {yes,          prompt = "Write out information messages?"}
file   tempdir     {"tmp$",       prompt = "Directory for temporary files"}
struct *dout_list  {              prompt = "Internal script use only"}
string mode        {"al",         prompt = ""}

begin

    # Declarations.
    # Variables to hold input parameters and used locally.
    file   in_table           # Input table name.
    string fcol               # Name of column in table containing flux.
    string wcol               # Name of column in table containing wave.
    string outfile            # Output file name.
    string tmpdir             # Directory for temporary output files.

    # Variables to store input parameters passed to 'mkmultispec' task.
    int    pnterms            
    int    pniterate
    real   plow, phigh, pgrow 
    bool   pmark, pverb, pinteract
    string pfunction
    string plabel
    string punits 
    string pweight 
    string pformat 
    string pdevice 

    # Local variables.
    int    ii                 # Loop counter.
    int    nrows              # Number of rows selected from table.
    int    nread              # Number of values read by 'fscan'.
    bool   inter_opt          # Interactive option for 'mkmultispec'.
    string msg                # Message string.
    string fname              # Input name concatenated with flux column specs.
    string wname              # Input name concatenated with wave column specs.
    string ftemp              # Temporary storage string for file name.
    string etemp              # Temporary storage string for extracted data.
    string root               # Root name of input file.
    string froot, wroot       # Root names of temporary output files.
    string flist_file         # Temporary list of flux filenames.
    string wlist_file         # Temporary list of wavelength filenames.
    string apid_file          # Temporary text file.
    bool   sporder_found      # True if SPORDER column were found.

    # Reassign the input parameter values as necessary.
    in_table = input
    outfile  = output
    tmpdir   = tempdir
    fcol     = flux_col
    wcol     = wave_col

    # Reassign the input parameters to be passed to 'mkmultispec'.
    pfunction = function
    pnterms   = nterms
    plabel    = label
    punits    = units
    plow      = low_reject
    phigh     = high_reject
    pweight   = weight
    pniterate = niterate
    pgrow     = grow
    pformat   = format
    pdevice   = device
    pmark     = markrej
    pinteract = interactive
    pverb     = verbose

    # Check that the necessary tasks/packages are loaded.
    msg = ""
    if (!deftask("fparse"))      msg = msg // " stsdas.toolbox.tools"
    if (!deftask("tximage"))     msg = msg // " tables.ttools"
    if (!deftask("sections"))    msg = msg // " images.imutil"
    if (!deftask("mkmultispec")) msg = msg // " stsdas.hst_calib.ctools"
    if (!deftask("scopy"))       msg = msg // " noao.onedspec"
    if (!deftask("delete"))      msg = msg // " system"
    if (!deftask("access"))      msg = msg // " language"
    if (strlen(msg) > 0) {
	printf ("Please load packages: %s\n", msg)
	bye
    }

    # Make sure the output file name is of imtype '.imh' and that the
    # file does not already exist.
    fparse (input = outfile, verbose = no)
    outfile = fparse.directory // fparse.root // ".imh"
    if (access(outfile))
        error (1, "Output file - " // outfile // " - already exists.")

    # Parse the input table name.
    fparse (input = in_table, verbose = no)

    # Check that the input file exists; strip off any FITS extension number.
    ftemp = fparse.directory // fparse.root // fparse.extension
    if (!access(ftemp))
        error (1, "Input file - " // ftemp // " - not found.")

    # Set up the root names for the temporary output files based upon    
    # the task name and the independent (X aka wave_col) or dependent 
    # (Y aka flux_col) axis.
    froot = mktemp (tmpdir // "tomul_Y_")
    wroot = mktemp (tmpdir // "tomul_X_")

    # Concatenate the column specification to the input table name 
    # using the 'selectors' syntax.
    fname = in_table // "[c:" // fcol // "]"
    wname = in_table // "[c:" // wcol // "]"

    # Extract the specified rows from the 3-D binary FITS table.
    tximage (intable = fname, output = froot // ".hhh", verbose = no)
    tximage (intable = wname, output = wroot // ".hhh", verbose = no)

    # Generate lists of the flux and wavelength file names.
    flist_file = mktemp (tmpdir // "FLXlist")
    sections (froot // "*.hhh", \
              option = "fullname", > flist_file)
    wlist_file = mktemp (tmpdir // "WAVlist")
    sections (wroot // "*.hhh", \
              option = "fullname", > wlist_file)
    nrows = sections.nimages

    # If no rows have been extracted from the input table, error out.
    if (nrows <= 0) {
        error (1, "No rows have been extracted from table, " // fparse.root //
                   fparse.extension // ".")
    }

    # Determine if the FITS table contains a column SPORDER.  If so, this 
    # information can be used later to update the WCS information.
    tinfo (in_table // "[c:SPORDER]", ttout=no)
    sporder_found = (tinfo.ncols == 1)

    # Fill the list directed structure for reading.
    dout_list = flist_file

    # If the SPORDER column were present in the input table, generate the
    # informational text file.
    if (sporder_found) {
        apid_file = mktemp (tmpdir // "APIDlist")
        for (ii = 1; ii <= nrows; ii += 1) {
            tabpar (table  = in_table,  \
                    column = "SPORDER",   \
                    row    = ii)
            nread = fscan (dout_list, etemp)
            print (ii, tabpar.value, " " // etemp, >> apid_file)
        }
    }

    # Only allow interactive graphics in 'mkmultispec' when the user
    # has requested a single row (spectral order) and requested to be
    # interactive.
    inter_opt = no
    if (nrows == 1) {
        if (pinteract)
            inter_opt = yes
    }
    else {
        if (pinteract || (pfunction == "table")) {
            print ("Warning: Multiple rows/spectral orders have been chosen.")
            if (pinteract)
                print ("Interactive fitting of the wavelengths has \
                        been disabled.")
            if (pfunction == "table") {
                print ("Limited space in header could cause problems \
                        for the storage of wavelengths")
                print ("with parameter ""function = table"".")
            }
        }
    }
    
    # Create the individual multispec-style GEIS files.  Note: the 
    # input flux files are overwritten with the new multispec data.
    mkmultispec (input       = "@" // flist_file, \
                 wave        = "@" // wlist_file, \
                 output      = "",                \
                 function    = pfunction,         \
                 nterms      = pnterms,           \
                 weight      = pweight,           \
                 label       = plabel,            \
                 units       = punits,            \
                 low_reject  = plow,              \
                 high_reject = phigh,             \
                 niterate    = pniterate,         \
                 grow        = pgrow,             \
                 format      = pformat,           \
                 interactive = inter_opt,         \
                 device      = pdevice,           \
                 markrej     = pmark,             \
                 cursor      = "",                \
                 verb        = pverb)

    # Ensure that the DATAMIN and DATAMAX keywords are properly updated
    # in each of the GEIS files which have been processed by 'mkmultispec'.
    minmax (images  = "@" // flist_file, \
            force   = yes,               \
            update  = yes,               \
            verbose = no)

    # Copy the individual multispec'ed files into an OIF 2-D image.
    scopy (input     = "@" // flist_file, \
           output    = outfile,           \
           w1        = INDEF,             \
           w2        = INDEF,             \
           apertures = "",                \
           bands     = "",                \
           apmodulus = 0,                 \
           format    = "multispec",       \
           renumber  = yes,               \
           offset    = 0,                 \
           clobber   = no,                \
           merge     = no,                \
           rebin     = yes,               \
           verbose   = no)

    # If the spectral order information is known in 'apid_file', edit
    # the WCS information so that "beam" contains the spectral order number.
    if (sporder_found) {
        sapertures (input = outfile,       \
                    apertures = " ",       \
                    apidtable = apid_file, \
                    wcsreset  = no,        \
                    verbose   = no,        \
                    beam      = INDEF,     \
                    dtype     = INDEF,     \
                    w1        = INDEF,     \
                    dw        = INDEF,     \
                    z         = INDEF,     \
                    aplow     = INDEF,     \
                    aphigh    = INDEF,     \
                    title     = "")
    }
    
    # Notify user of success.
    print (" ")
    print ("The output file - " // outfile // " - has been written.")

    # Clean up.
    dout_list = ""
    imdelete (wroot // "*.hhh", go+, verify-, def+, >& "dev$null")
    imdelete (froot // "*.hhh", go+, verify-, def+, >& "dev$null")
    delete (flist_file, go+, verify-, def+, all+, >& "dev$null")
    delete (wlist_file, go+, verify-, def+, all+, >& "dev$null")
    if (sporder_found) {
        delete (apid_file, go+, verify-, def+, all+, >& "dev$null")
    }

end
