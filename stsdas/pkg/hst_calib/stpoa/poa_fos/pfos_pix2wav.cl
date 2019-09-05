# PFOS_PIX2WAV -- Run the task fos_pix2pos and append the pix and wave
# tables into one outputfile. Also performs some checking on the input
# parameters and existance of files/tools.

procedure pfos_pix2wav (input, pixtab, wavtab)

file   input       {"",           prompt = "Rootname of the FOS observation"}
file   pixtab      {"",           prompt = "Input table with pixel positions"}
file   wavtab      {"",           prompt = "Output table with pixel and wavelength positions"}
bool   sub_pix     {yes,          prompt = "Apply the sub-pixel correction from '.poa' file?"}
file   poafile     {"",           prompt = "Rootname of the poa_calfos output '.poa' file"}
int    specnum     {INDEF,        min=-1, prompt = "Spec number for the sub-pixel correction"}
string mode        {"al",         prompt = ""}

begin

        # Declare local variables.
        string  in_name
        string  poa_name
        string  pix_name
        string  wav_name
        string  tmp_name
        string  merge_name
        string  tmp_d0h
        string  pre_proc
        bool    subpix
        int     snum

        in_name    =   input
        poa_name   =   poafile
        pix_name   =   pixtab
        wav_name   =   wavtab
        tmp_name   =   "tmp_"//wav_name
        merge_name =   tmp_name//","//pix_name
        tmp_d0h    =   in_name//".d0h"
        subpix     =   sub_pix
        snum       =   specnum

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
        if (!access (in_name // ".d0h")) {
            print ("ERROR: ", in_name, ".d0h does not exist.")
            bye 
        }
        if (!access (in_name // ".d0d")) {
            print ("ERROR: ", in_name, ".d0d does not exist.  Please")
            print "provide all root FOS data files in current directory."
            bye 
        }
        if (!access (pix_name)) {
            print ("ERROR: ", pix_name, " does not exist.")
            bye
        }
        # set the poa name to the root, if blank string is input
        if (subpix) {
           if (poa_name == "") {
              poa_name=in_name
           }
           if (!access (poa_name // ".poa")) {
              print ("ERROR: file ", poa_name, ".poa does not exist.")
              print "       Please run 'poa_calfos' to create this file."
              bye
           }
        }

        # get the CYCCSFR keyword (as a simple check if pre-proc has been run)
        unlearn keypar
        keypar(tmp_d0h, "CYCCSFR", silent+)
        if (keypar.found) {
           pre_proc=keypar.value
        }
        else {
           pre_proc="nothing"
        }

        if (pre_proc == "nothing")  {
            # exit since the POA processing has not been done
            print ("ERROR: ", in_name, ".d0h is missing CYCCSFR keyword.")
            print "        Your data has not been POA-preprocessed; please run"
            print "        the task 'poa_procfos_all' for POA FOS data processing."
            bye
        }

        # if the specnum = INDEF, set the input to a value of -1 (will use
        # all spectra in the .poa file for sub-pix correction
        if (snum == INDEF) {
           snum=-1
        }
        # zero and negative spectral values are not allowed
        else if (snum <= 0) {
           snum=-1
           print ("WARNING: SPECNUM must be > 0; switching to default where")
           print ("         average of all spectra are used for sub-pix correction")
        }

        # delete the wav table if it already exists
        if (access (wav_name)) {
           delete (wav_name, yes, ver-, default_acti+, allversions+, subfiles-)
        }

        # make sure wavtab is not a blank string
        if (wav_name == "") {
            # exit since the POA processing has not been done
            print ("ERROR: input wavtab parameter cannot be a blank string.")
            bye
        }

        # call fos_pix2wav to get the wavelength table results
        fos_pix2wav (in_name, pix_name, tmp_name, sub_pix=subpix, poafile=poa_name//".poa", specnum=snum)
       
        # if wav table was created (success), the merge it with the pixel table
        if (access (tmp_name)) {
           tmerge (merge_name, wav_name, "merge", allcols+, tbltype="default", allrows=INDEF, extracol=0)
           if (access (wav_name)) {
              print ""
              print ("pfos_pix2wav completed successfully; output wavelengths have been")
              print ("merged with input pixel table; resultant file: ", wav_name)

              # temporary table can now be deleted
              delete (tmp_name, yes, ver-, default_acti+, allversions+, subfiles-)
           }

           # must refresh memory due to some sort of problem in iraf;  unable
           # to run this tool more than once, if flpr is not run inbetween
           flpr
        }
        else {
           print ("ERROR: no results for wave table merge")
           flpr
        }

end

