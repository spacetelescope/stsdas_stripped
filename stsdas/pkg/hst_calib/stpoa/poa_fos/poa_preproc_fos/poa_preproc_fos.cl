# POA_PREPROC_FOS -- Preprocess FOS data before running poa_calfos.
#                    Add new header keywords to the group parameters
#                    of the input FOS data headers.

procedure poa_preproc_fos (input)

file   input          {"",           prompt = "Input FOS root name"}
bool   internal_ref   {yes,          prompt = "Use internal STPOA ref files, or CDBS files"}
bool   force_poa      {no,           prompt = "Force processing for data w/o POA correction?"}
string mode           {"al",         prompt = ""}

begin
        # Declare local variables.
        string  in_name
        string  det
        string  file_mode
        string  fgwa
        string  aper
        string  fl1
        string  fl2
        string  fl1_sub
        string  fl1_new
        string  fl2_sub
        string  fl2_new
        string  off_orig
        string  na="N/A"
        int     i
        int     j
        bool    na1=NO
        bool    na2=NO
        bool    force
        string  ccs6_orig
        string  ccs6_sub
        string  ccs6_new
        string  flat_calfos
        string  flat_poafos
        bool    found_fl1=NO
        bool    found_fl2=NO
        string  pre_proc
        bool    internal
        string  sct_corr

        # set the input params to local variables
        in_name   =   input
        internal  =   internal_ref
        force     =   force_poa

        s1=in_name//".d0h"
        list="pref$old_new_convert.list"
        s3=in_name//".*h"

        # Load necessary packages - exit if no loaded
        if (!defpac("stsdas")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("hst_calib")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("fos")) {
             print "ERROR: you must first load the stsdas.hst_cal.fos packages."
             bye
        }
        if (!defpac("ctools")) {
             print "ERROR: you must first load the stsdas.hst_cal.ctools packages."
             bye
        }

        set imtype = "hhh"

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
           print "         re-run the pre-processor while setting 'force_poa=YES'."
           print ""
           return
        }

        if (force) {
           print ("FORCING POA pre-processing.")
           print ""
        }

        # check if valid combination of header keywords
        if ((force) || 
           (( det == "BLUE") && ((file_mode == "SPECTROSCOPY") || 
           (file_mode == "RAPID-READOUT") || (file_mode == "IMAGE")) &&
           ((fgwa == "H13") || (fgwa == "H19") || (fgwa == "H27") || 
           (fgwa == "H40") || (fgwa == "H57") || (fgwa == "L65")   ||
           (fgwa == "L15")) && 
           ((aper == "A-1") || (aper == "A-2") || (aper == "A-3") || 
           (aper == "A-4") || 
           (aper == "B-1") || (aper == "B-2") || (aper == "B-3") || 
           (aper == "B-4"))) ) {

           # enter this loop only if valid combination of header keys;  
           # update the header for POA processing, echo info to STDOUT

           ####  OFF_CORR Section  ####
           # get the off_corr value from the header
           unlearn keypar
           keypar(s1, "OFF_CORR", silent+)
           off_orig=keypar.value

           ####  SCT_CORR Section  ####
           # get the sct_corr value from the header
           unlearn keypar
           keypar(s1, "SCT_CORR", silent+)
           sct_corr=keypar.value

           ####  WAVELENGTH REF FILE Section  ####

           # get the ccs6 wavelength ref file from the header
           unlearn keypar
           keypar(s1, "CCS6", silent+)
           ccs6_orig=keypar.value
           i=strlen(ccs6_orig)
           ccs6_sub=substr(ccs6_orig,6,i)
           # if the known old name is recognized
           if ((ccs6_sub == "bck10546y.cy6") || (ccs6_sub == "l611655ty.cy6")){
              # check if user wants internal release ref files
              if (internal == YES) {
                 ccs6_new="pwav$l611655ty.cy6"
              }
              # try to use external release ref files
              else {
                 # check to see if CDBS has the file;  if not warn the user
                 # and set to POA location;  if yes, set to CDBS location
                 if (access("ytab$l611655ty.cy6")) {
                    ccs6_new="ytab$l611655ty.cy6"
                 }
                 # unable to access CDBS versions, use internal versions
                 else {
                    print ("WARNING: Parameter 'internal_ref = NO', but CCS6 ref file is not")
                    print ("         available in CDBS location; must use internal version")
                    ccs6_new="pwav$l611655ty.cy6"
                 }
              }
           }
           else if ((ccs6_sub == "e5v11576y.cy6") || (ccs6_sub == "l611655oy.cy6")){
              if (internal == YES) {
                 ccs6_new="pwav$l611655oy.cy6"
              }
              else {
                 # check to see if CDBS has the file;  if not warn the user
                 # and set to POA location;  if yes, set to CDBS location
                 if (access("ytab$l611655oy.cy6")) {
                    ccs6_new="ytab$l611655oy.cy6"
                 }
                 else {
                    print ("WARNING: Parameter 'internal_ref = NO', but CCS6 ref file is not")
                    print ("         available in CDBS location; must use internal version")
                    ccs6_new="pwav$l611655oy.cy6"
                 }
              }
           }
           else if ((ccs6_sub != "l611655ty.cy6") && (ccs6_sub != "l611655oy.cy6")){
              print ("WARNING: CCS6 file ", ccs6_orig," is unrecognised.")
              print ("         The know names are bck10546y.cy6 and e5v11576y.cy6")
              print ("         No substitution for CCS6, POA version will be made.")
              ccs6_new=ccs6_orig
           }
           else {
              ccs6_new=ccs6_orig
           }


           ####  Flat Fields Section  ####

           # get the FL1HFILE of the file
           unlearn keypar
           keypar(s1, "FL1HFILE", silent+)
           fl1=keypar.value
           # get the FL2HFILE of the file
           unlearn keypar
           keypar(s1, "FL2HFILE", silent+)
           fl2=keypar.value

              # first flat
              i=strlen(fl1)
              fl1_sub=substr(fl1,6,i)
              if (fl1_sub != na) {
                  # loop over old calfos names and get new uniq poa names
                  while (fscan (list, flat_calfos, flat_poafos) != EOF) {
                     if ((flat_calfos == fl1_sub) || (flat_poafos == fl1_sub)) {
                        if (internal == YES) {
                           fl1_new="pref$"//flat_poafos
                           found_fl1=YES
                           break  
                        }
                        else {
                           # check to see if CDBS has the file;  if not warn the user
                           # and set to POA location;  if yes, set to CDBS location
                           if (access("yref$"//flat_poafos)) {
                              fl1_new="yref$"//flat_poafos
                              found_fl1=YES
                              break  
                           }
                           else {
                              print ("WARNING: Parameter 'internal_ref = NO', but FL1FILE file is not")
                              print ("         available in CDBS location; must use internal version")
                              fl1_new="pref$"//flat_poafos
                              found_fl1=YES
                              break  
                           }
                        }
                     }
                  }
                  if (found_fl1 == NO) {
                     fl1_new=fl1
                     print ("POA does not have a new version of the flat field: ", fl1)
                     print ("Processing will use original flat field.")
                  }
              }
              else {
                  fl1_new=fl1
                  na1=YES
              }
              # second flat
              list="pref$old_new_convert.list"
              j=strlen(fl2)
              fl2_sub=substr(fl2,6,j)
              if (fl2_sub != na) {
                  # loop over old calfos names and get new uniq poa names
                  while (fscan (list, flat_calfos, flat_poafos) != EOF) {
                     if ((flat_calfos == fl2_sub) || (flat_poafos == fl1_sub)) {
                        if (internal == YES) {
                           fl2_new="pref$"//flat_poafos
                           found_fl2=YES
                           break  
                        }
                        else {
                           # check to see if CDBS has the file;  if not warn the user
                           # and set to POA location;  if yes, set to CDBS location
                           if (access("yref$"//flat_poafos)) {
                              fl2_new="yref$"//flat_poafos
                              found_fl2=YES
                              break  
                           }
                           else {
                              print ("WARNING: Parameter 'internal_ref = NO', but FL2FILE file is not")
                              print ("         available in CDBS location; must use internal version")
                              fl2_new="pref$"//flat_poafos
                              found_fl2=YES
                              break  
                           }
                        }
                     }
                  }
                  if (found_fl2 == NO) {
                     fl2_new=fl2
                     print ("POA does not have a new version of the flat field: ", fl1)
                     print ("Processing will use original flat field.")
                  }
              }
              else {
                 fl2_new=fl2
                 na2=YES
              }

              # check if 'pref$' has the new flat field file
              if ((na1 == NO) && (access (fl1_new))){
              }
              else if (na1 == YES){
                  fl1_new=fl1
              }
              else {
                 print ("POA does not have a new version of the flat field: ", fl1)
                 print ("Processing will use original flat field.")
                 fl1_new=fl1
              }
              if ((na2 == NO) && (access (fl2_new))){
              }
              else if (na2 == YES){
                  fl2_new=fl2
              }
              else {
                 print ("POA does not have a new version of the flat field: ", fl2)
                 print ("Processing will use original flat field.")
                 fl2_new=fl2
              }

           # Setting and action of the POA pre-processor
           print ("")
           print ("POA Pre-Processor is updating the following header keywords:")
           print ("Format:   param=original value    ===>   param=new value")
           print ""
           print ("   OFF_CORR=", off_orig, "   ===>   OFF_CORR=PERFORM")
           print ("   SCT_CORR=", sct_corr, "   ===>   SCT_CORR=OMIT")
           print ("   FL1HFILE=", fl1, "   ===>    FL1HFILE=", fl1_new)
           print ("   FL2HFILE=", fl2, "   ===>    FL2HFILE=", fl2_new)
           print ("   CCS6=", ccs6_orig, "   ===>   CCS6=", ccs6_new)
           print ("")

           # Call header update to change values for all data
           unlearn ckwfos
           ckwfos.fl1hfile=fl1_new
           ckwfos.fl2hfile=fl2_new
           ckwfos.ccs6=ccs6_new

           cl < "stpoa$poa_fos/poa_preproc_fos/ck_off_corr.cl"
           putcal (s3, "ckwfos")

           # add the DGRF and NORAD tables to the headers
           print ""
           print "Adding CCSE1->CCSE9, CCSF ref tables to the raw headers"

           if (internal == YES) {
              hedit (s3, "CYCCSER1", "ref$l5u1623cy.cye", add+, del-, verify-, show-)
              hedit (s3, "CYCCSER2", "ref$l5u16246y.cye", add+, del-, verify-, show-)
              hedit (s3, "CYCCSER3", "ref$l5u1624ay.cye", add+, del-, verify-, show-)
              hedit (s3, "CYCCSER4", "ref$l5u1624ey.cye", add+, del-, verify-, show-)
              hedit (s3, "CYCCSER5", "ref$l5u1624iy.cye", add+, del-, verify-, show-)
              hedit (s3, "CYCCSER6", "ref$l5u1624ny.cye", add+, del-, verify-, show-)
              hedit (s3, "CYCCSER7", "ref$l5u1624qy.cye", add+, del-, verify-, show-)
              hedit (s3, "CYCCSER8", "ref$l5u16250y.cye", add+, del-, verify-, show-)
              hedit (s3, "CYCCSER9", "ref$l5u16255y.cye", add+, del-, verify-, show-)

              hedit (s3, "CYCCSFR", "ref$l5u1739ay.cyf", add+, del-, verify-, show-)
           }
           else {
              # check to see if CDBS has the file;  if not warn the user
              # and set to POA location;  if yes, set to CDBS location
              if (access("ytab$l5u1623cy.cye")) {
                 hedit (s3, "CYCCSER1", "ytab$l5u1623cy.cye", add+, del-, verify-, show-)
              }
              # unable to access CDBS versions, use internal versions
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER1 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER1", "ref$l5u1623cy.cye", add+, del-, verify-, show-)
              }

              if (access("ytab$l5u16246y.cye")) {
                 hedit (s3, "CYCCSER2", "ytab$l5u16246y.cye", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER2 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER2", "ref$l5u16246y.cye", add+, del-, verify-, show-)
              }

              if (access("ytab$l5u1624ay.cye")) {
                 hedit (s3, "CYCCSER3", "ytab$l5u1624ay.cye", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER3 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER3", "ref$l5u1624ay.cye", add+, del-, verify-, show-)
              }

              if (access("ytab$l5u1624ey.cye")) {
                 hedit (s3, "CYCCSER4", "ytab$l5u1624ey.cye", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER4 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER4", "ref$l5u1624ey.cye", add+, del-, verify-, show-)
              }

              if (access("ytab$l5u1624iy.cye")) {
                 hedit (s3, "CYCCSER5", "ytab$l5u1624iy.cye", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER5 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER5", "ref$l5u1624iy.cye", add+, del-, verify-, show-)
              }

              if (access("ytab$l5u1624ny.cye")) {
                 hedit (s3, "CYCCSER6", "ytab$l5u1624ny.cye", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER6 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER6", "ref$l5u1624ny.cye", add+, del-, verify-, show-)
              }

              if (access("ytab$l5u1624qy.cye")) {
                 hedit (s3, "CYCCSER7", "ytab$l5u1624qy.cye", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER7 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER7", "ref$l5u1624qy.cye", add+, del-, verify-, show-)
              }
              
              if (access("ytab$l5u16250y.cye")) {
                 hedit (s3, "CYCCSER8", "ytab$l5u16250y.cye", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER8 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER8", "ref$l5u16250y.cye", add+, del-, verify-, show-)
              }

              if (access("ytab$l5u16255y.cye")) {
                 hedit (s3, "CYCCSER9", "ytab$l5u16255y.cye", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSER9 ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSER9", "ref$l5u16255y.cye", add+, del-, verify-, show-)
              }

              if (access("ytab$l5u1739ay.cyf")) {
                 hedit (s3, "CYCCSFR", "ytab$l5u1739ay.cyf", add+, del-, verify-, show-)
              }
              else {
                 print ("WARNING: Parameter 'internal_ref = NO', but CYCCSFR ref file is")
                 print ("         not available in CDBS location; must use internal version")
                 hedit (s3, "CYCCSFR", "ref$l5u1739ay.cyf", add+, del-, verify-, show-)
              }
              
           }

           unlearn ckwfos
           unlearn hedit
           
        }  else {

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
           print ("         Please use 'calfos' in stsdas.hst_cal.fos to process your data.")
           print ("")
        }
        
        # keep the packages that were opened by this script, in order to
        # have access to the calfos calibration variables
        # keep
end

