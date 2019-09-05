# PFOS_DISPFIT -- Run the task fos_dispfit to calculate the FOS
# dispersion coeffs for ccs6 ref file. Also performs some checking 
# on the input parameters and existance of files/tools.

procedure pfos_dispfit (input, fittab, output, iterations, relax)

file   input        {"",           prompt = "Input data table with x and wavelengths"}
file   fittab       {"",           prompt = "Input fit table with previous fit or blank"}
file   output       {"",           prompt = "Name of output fit table (FITS)"}
int    iterations   {min=-100000000, max= 100000000, prompt = "Number of iterations to perform"}
real   relax        {              prompt = "Relaxation factor"}
bool   use_params   {no,           prompt = "Use 10 initial param guesses below?"}
int    print        {50, min=1,    prompt = "Print results every N number of iterations"}
string fit_dir      {"wave2x", enum = "wave2x|x2wave", prompt = "Fitting direction: wave2x or x2wave"}
string x_col        {"X",          prompt = "Name of column with x position in input file"}
string wave_col     {"WAVE",       prompt = "Name of column with wavelength in input file"}
string sel_col      {"",           prompt = "Optional name of selection column in input file"}
string newcalc_col  {"",           prompt = "Optional - create new column in input file using fit"}
real   a            {1.,           prompt = "Parameter 'a' initial value"}
string a_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'a' param or fixed"}
real   b            {1.,           prompt = "Parameter 'b' initial value"}
string b_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'b' param or fixed"}
real   c            {1.,           prompt = "Parameter 'c' initial value"}
string c_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'c' param or fixed"}
real   d            {1.,           prompt = "Parameter 'd' initial value"}
string d_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'd' param or fixed"}
real   e            {1.,           prompt = "Parameter 'e' initial value"}
string e_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'e' param or fixed"}
real   o            {1.,           prompt = "Parameter 'a' initial value"}
string o_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'a' param or fixed"}
real   p            {1.,           prompt = "Parameter 'a' initial value"}
string p_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'a' param or fixed"}
real   q            {1.,           prompt = "Parameter 'a' initial value"}
string q_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'a' param or fixed"}
real   r            {1.,           prompt = "Parameter 'a' initial value"}
string r_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'a' param or fixed"}
real   s            {1.,           prompt = "Parameter 'a' initial value"}
string s_mod        {"FIXED", enum = "FIXED|OPEN",  prompt = "Modifiable 'a' param or fixed"}
string mode         {"al",         prompt = ""}

begin

        # Declare local variables.
        string  in_put, fit_tab, out_put, fitdir, x_name, wave_name, sel_name, value, newcol
        int     iter, print_N, ii
        real    rel
        bool    use_pars
        real    a_par, b_par, c_par, d_par, e_par
        string  amod, bmod, cmod, dmod, emod
        real    o_par, p_par, q_par, r_par, s_par
        string  omod, pmod, qmod, rmod, smod

        # read all the tool parameters and assign them to local vars
        in_put     =   input
        fit_tab    =   fittab
        out_put    =   output
        fitdir     =   fit_dir
        x_name     =   x_col
        wave_name  =   wave_col
        sel_name   =   sel_col
        newcol     =   newcalc_col
        iter       =   iterations
        print_N    =   print
        rel        =   relax
        use_pars   =   use_params
        a_par      =   a
        amod       =   a_mod
        b_par      =   b
        bmod       =   b_mod
        c_par      =   c
        cmod       =   c_mod
        d_par      =   d
        dmod       =   d_mod
        e_par      =   e
        emod       =   e_mod
        o_par      =   o
        omod       =   o_mod
        p_par      =   p
        pmod       =   p_mod
        q_par      =   q
        qmod       =   q_mod
        r_par      =   r
        rmod       =   r_mod
        s_par      =   s
        smod       =   s_mod

        # Check if necessary packages are loaded
        if (!defpac("stsdas")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("hst_calib")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("ctools")) {
             print "ERROR: you must first load the stsdas.hst_cal.ctools packages."
             bye
        }
        if (!defpac("fos")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("stpoa")) {
             print "ERROR: you must first load the stpoa.poa_fos packages."
             bye
        }
        if (!defpac("poa_fos")) {
             print "ERROR: you must first load the stpoa.poa_fos packages."
             bye
        }

        # Check if all input files exist
        if (!access (in_put)) {
            print ("ERROR: ", in_put, " file does not exist.")
            bye 
        }
        if ((out_put == "") && (fit_tab == "")) {
            print ("ERROR:  'fittab' and 'output' names are blank.")
            print ("        User must specify at least the output file.")
            bye 
        }
        else if ((out_put == "") || (out_put == fit_tab)) {
            print ("ERROR:  'output' file name must be specified, and cannot")
            print ("        be the same as the fittab name due to clobber issues.")
            bye 
        }

        # when requested number of iterations are less than zero, this means
        # that the fit should continue from the previous fittab, therefore,
        # the fittab must be specified by the user
        if (iter < 0) {
           if ((fit_tab == "") || (!access (fit_tab))) {
              print ("ERROR:  fittab must already exist to continue fitting")
              bye
           }
           copy (fit_tab, out_put)
        }
        # if this is not a continuation of a fit, then if the fittab is
        # specified, use it, otherwise, create a clean table from a template
        else {
           if (fit_tab == "") {
              fit_tab=out_put
              copy ("poa_disp$dispfit_blank.fits", out_put)
              if (use_pars) {
              }
              else {
                 pfos_dispfit.use_params=yes
                 print "WARNING: 'use_params' is being switched on because no previous"
                 print "         fittab was specified; restarting fit using a-s params"
                 use_pars=yes
              }
           }
           else if (!access (fit_tab)) {
              print ("WARNING: ", fit_tab, " does not exist.  Using empty blank table.")
              fit_tab=out_put
              copy ("poa_disp$dispfit_blank.fits", out_put)
              if (use_pars) {
              }
              else {
                 pfos_dispfit.use_params=yes
                 print "WARNING: 'use_params' is being switched on because no previous"
                 print "         fittab was specified; restarting fit using a-s params"
                 use_pars=yes
              }
           }
           # if fit table is specified
           else {
              if (use_pars) {
                  print "Using A-S tool parameter settings as input initial guesses"
              }
              else {
                 print "Using fittab INITAIL_GUESS values as input parameters"
              }
           }
        }
        if (!access (out_put)) {
              copy ("poa_disp$dispfit_blank.fits", out_put)
        }

        # call fos_dispfit
#        print (in_put, fit_tab, out_put, iter, rel, use_pars, print_N, fitdir, x_name, wave_name, sel_name, a_par, amod, b_par, bmod, c_par, cmod, d_par, dmod, e_par, emod, o_par, omod, p_par, pmod, q_par, qmod, r_par, rmod, s_par, smod)

        fos_dispfit(in_put, fit_tab, out_put, iter, rel, use_params=use_pars, print=print_N, fit_dir=fitdir, x_col=x_name, wave_col=wave_name, sel_col=sel_name, newcalc_col=newcol, a=a_par, a_mod=amod, b=b_par, b_mod=bmod, c=c_par, c_mod=cmod, d=d_par, d_mod=dmod, e=e_par, e_mod=emod, o=o_par, o_mod=omod, p=p_par, p_mod=pmod, q=q_par, q_mod=qmod, r=r_par, r_mod=rmod, s=s_par, s_mod=smod) 

end

