################################################################################
#
# ECHPLOT.CL - IRAF Script written to drive the 'echscript' task and IGI
#              package to streamline creating plots for users.
#
# Version    Author          Date
# -------------------------------
#   1.0      M. De La Pena   08 October 1997
#   1.1      M. De La Pena   26 November 1997 - Reorganized assignment of
#                            inputs, checking for input table done by main
#                            program, removed unnecessary parsing, min/maxwave 
#                            are INDEF.
#   1.2      M. De La Pena   30 January 1998 - Clarified version numbering.
#                            Allows overwrite of output if the environment
#                            variable "clobber" is set to "yes".
#   2.1      M. De La Pena   04 June 1998 - Allow for a null ("") output IGI
#                            script name; triggers "view only" mode.
#
################################################################################

procedure echplot (input, output, plotstyle)

file   input       {"",           prompt = "Input 3-D FITS binary table"}
file   output      {"",           prompt = "Output IGI script name"}
string plotstyle   {"",           prompt = "Plot style ", \
                                  enum   = "|single|multiple|panel|diagnostic"}
string flux_col    {"FLUX",       prompt = "Flux column name"}
string title       {"",           prompt = "Optional plot title"}
real   minwave     {INDEF, min=0.0, prompt = "Minimum wavelength to plot"}
real   maxwave     {INDEF, min=0.0, prompt = "Maximum wavelength to plot"}
string device      {"stdgraph",   prompt = "Graphics device"}
string Version     {"Version 2.1, 04Jun98", prompt = "Installation date"}
string mode        {"al",         prompt = ""}

begin

    # Declarations.
    # Variables to hold input parameters used locally.
    string in_table           # Input table name.
    string outfile            # Output file name.

    # Variables to store input parameters passed to 'echscript' task.
    real   pminwave, pmaxwave
    string pplotstyle
    string pflux_col
    string ptitle
    string pdevice 

    # Local variables.
    string ftemp
    string msg                # Message string.
    string env_clob           # Setting of environment variable "clobber".
    bool   viewonly           # View only mode. No output script produced.

    # Reassign the input parameter values as necessary.
    in_table   = input
    outfile    = output
    pplotstyle = plotstyle
    pflux_col  = flux_col
    ptitle     = title
    pminwave   = minwave
    pmaxwave   = maxwave
    pdevice    = device

    # Check that the necessary tasks/packages are loaded.
    msg = ""
    if (!deftask("echscript"))   msg = msg // " stsdas.hst_calib.stis"
    if (!deftask("access"))      msg = msg // " language"
    if (!deftask("igi"))         msg = msg // " stsdas.graphics.stplot"
    if (strlen(msg) > 0) {
	printf ("Please load packages: %s\n", msg)
	bye
    }

    # Only check that there is no conflict with the output file name
    # if outfile != "".  An outfile = "" triggers the view only mode
    # where the output script will be written to a temporary file and
    # deleted at the end of the task.
    if (outfile != "") {
        viewonly = no

        # Make sure the output file does not already exist.
        env_clob = envget ("clobber")
        if (access(outfile) && env_clob == "no")
            error (1, "Output file - " // outfile // " - already exists.")
    }
    else { 
        outfile  = mktemp ("tmp$echView")
        viewonly = yes
    }
   
    # Call 'echscript' to generate the IGI script.
    echscript (input     = in_table,   \
               output    = outfile,    \
               plotstyle = pplotstyle, \
               flux_col  = pflux_col,  \
               title     = ptitle,     \
               minwave   = pminwave,   \
               maxwave   = pmaxwave,   \
               viewonly  = viewonly)

    # Check that the output file exists before calling IGI to 
    # perform the graphics.
    if (access(outfile)) {
 
        # Call the IGI package and plot to the selected device.
        igi (initcmd  = "",      \
             wlpars   = "",      \
             usewcs   = no,      \
             wcspars  = "",      \
             device   = pdevice, \
             metacode = "",      \
             append   = no,      \
             debug    = no,      \
             cursor   = "", < outfile)

        # Flush the graphics buffer if device != stdgraph.
        if (pdevice != "stdgraph")
            gflush

        # If "viewonly = yes", clean up the temporary output file.
        if (viewonly)
            delete (outfile, ver-, >& "dev$null")
    }

end
