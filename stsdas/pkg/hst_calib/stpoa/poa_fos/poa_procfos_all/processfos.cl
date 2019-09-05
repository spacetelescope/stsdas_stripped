# PROCESSFOS -- Run the poa fos preprocessor and decide whether
#               to run the POA FOS pipeline or calfos.

procedure processfos (input, output)

file   input          {"",           prompt = "Input FOS root name, or 'file.list' name"}
file   output         {"",           prompt = "Output FOS root name, or blank"}
bool   internal_ref   {yes,          prompt = "Use internal STPOA ref files, or CDBS files"}
bool   force_poa      {no,           prompt = "Force processing for data w/o POA correction?"}
struct *rootlist      {"",           prompt = "Dummy par needed to run on a list; leave blank"}
string mode           {"al",         prompt = ""}

begin

     # Declare local variables.
     string  in_name
     string  out_name
     string  s1
     string  det
     string  file_mode
     string  fgwa
     string  aper
     bool    force
     string  pre_proc
     bool    internal
     int     strlength
     string  sub_string
     string  tmp_name

     in_name    =   input
     out_name   =   output
     internal   =   internal_ref
     force      =   force_poa
     rootlist   =   ""


     # find out if input is a list file;  if yes, then set up to run on list
     strlength=strlen(in_name)
     sub_string=substr(in_name,strlength-4,strlength)
     if (sub_string != ".list") {
        tmp_name = mktemp ("tmplist")
        print (in_name, " ", out_name, > tmp_name)
        rootlist=tmp_name
     }
     else {
        rootlist=in_name
     }
     
     # look over the list
     while (fscan (rootlist, in_name, out_name) != EOF) {
        s1=in_name//".d0h"

        print ""

        # Load necessary packages
        if (!defpac("stsdas")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("hst_calib")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("ctools")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("fos")) {
             print "ERROR: you must first load the stsdas.hst_cal.ctools packages."
             bye
        }

        # get the detector name of the file
        unlearn keypar
        keypar(s1, "DETECTOR", silent+)
        det=keypar.value

        # get the mode of the file
        unlearn keypar
        keypar(s1, "GRNDMODE", silent+)
        file_mode=keypar.value

        # get the fgwa_is of the file
        unlearn keypar
        keypar(s1, "FGWA_ID", silent+)
        fgwa=keypar.value

        # get the aper_id of the file
        unlearn keypar
        keypar(s1, "APER_ID", silent+)
        aper=keypar.value

        # get the CYCCSFR, to check if pre-proc has already been run
        unlearn keypar
        keypar(s1, "CYCCSFR", silent+)
        if (keypar.found) {
           pre_proc=keypar.value
        }
        else {
           pre_proc="nothing"
        }

        if (pre_proc == "nothing")  {
           # will pre-process the data, no matter 
        }
        else if ((pre_proc != "nothing") && (force)){
           print ""
           print "WARNING: Your data seems to already have been POA pre-processed."
           print "         Since 'force_poa=YES', the data will be re-pre-processed."
           print ""
        }
        else {
           print ""
           print "WARNING: Your data seems to already have been POA pre-processed."
           print "         You can simply run poa_calfos on your data now;  or if"
           print "         you really want to re-pre-process, then you must"
           print "         re-run the all-processor while setting 'force_poa=YES'."
           print ""
           return
        }

        # check if valid combination of header keywords
        if ((force) || 
           (( det == "BLUE") && ((file_mode == "SPECTROSCOPY") || 
           (file_mode == "RAPID-READOUT") || (file_mode == "IMAGE")) &&
           ((fgwa == "H13") || (fgwa == "H19") || (fgwa == "H27") || 
           (fgwa == "H40") || (fgwa == "H57") || (fgwa == "L65")  ||
           (fgwa == "L15")) && 
           ((aper == "A-1") || (aper == "A-2") || (aper == "A-3") || 
           (aper == "A-4") || 
           (aper == "B-1") || (aper == "B-2") || (aper == "B-3") || 
           (aper == "B-4"))) ){

           # enter this loop only if valid combination of header keys;  
           # update the header for POA processing, echo info to STDOUT

           # Call poa_preproc_fos
           print ("")
           print ("**Executing POA FOS data pre-processor.")
           print ("")
           if (internal == YES) {
              poa_preproc_fos (input=in_name, internal_ref+, force_poa=force)
           }
           else { 
              poa_preproc_fos (input=in_name, internal_ref-, force_poa=force)
           }
           # Call poa_calfos
           print ("")
           print ("**Executing POA FOS pipeline.")
           print ("")
           poa_calfos (input=in_name, output=out_name)

        }  
        else {

           # enter this loop only if invalid combination of header keys;  
           # echo info to STDOUT, and exit without updating the header

           print ("")
           print ("WARNING: Your dataset ", in_name," cannot be processed by poa_calfos;")
           print ("         this POA software only works on the following FOS datasets:")
           print ("         DETECTOR:  BLUE")
           print ("         GRNDMODE:  SPECTROSCOPY, RAPID-READOUT, IMAGE")
           print ("         FGWA_ID:   H13, H19, H27, H40, H57, L15, L65")
           print ("         APER_ID:   A-1, A-2, A-3, A-4, B-1, B-2, B-3, B-4")
           print ("")
           print ("         Your dataset's keyword values do not match the above criteria:")
           print ("         ",s1, ":  DETECTOR=", det, ", GRNDMODE=", file_mode,",")
           print ("                         FGWA_ID=", fgwa, ", APER_ID=", aper)
           print ("         Running 'calfos' in stsdas.hst_cal.fos instead to process your data.")
           print ("")

           # Call calfos
           print ("**Executing OLD CALFOS pipeline.")
           print ("")
           calfos (input=in_name, output=out_name)
        }
     }   
     # clean up the mktemp file, if it exists
     if (sub_string != ".list") {
        if (access(tmp_name)) {
           del (tmp_name)
        }
     }
     rootlist   =   ""

        # keep the packages that were opened by this script, in order to
        # have access to the calfos calibration variables
        # keep
end

