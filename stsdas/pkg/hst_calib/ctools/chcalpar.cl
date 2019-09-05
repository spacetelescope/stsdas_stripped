#--
# chcalpar -- Get, edit, and put calibration keyword parameters in images.
#
# BUGS
# The parameter "keywords" should be declared as a pset, but if done so,
# the psets apparently get buffered somewhere and none of the changes
# between the individual tasks ever appear.  Keep this as a string.
#---------------------------------------------------------------------------
procedure chrspar (images)

file 	images          {prompt = "List of images to modify"}

file 	template        {"",prompt = "Image to read header from"}
string  keywords        {"",prompt="Pset to use if not reading from an image"}
bool	add		{yes,prompt="Add keywords if not present in header?"}
bool    verbose         {yes,prompt="Print out files as they are modified?"}
string  Version         {"17Apr2002",min="|25Mar94",prompt="Date of Installation"}

begin
        # Declarations
        file    fx                      # Generic file names.
        string  omode                   # Old mode of keywords parameters.
        file    pimages                 # Parameter 'input'.
        string  pkeywords               # Internal kewyord pset.

        # Get the necessary parameters
        pimages = images
        pkeywords = keywords

        # Get example header from either the template or the first file
        # from the input images list.
        if (strlen (template) > 0)
            fx = template
        else
            fx = pimages
        getcal (fx, pkeywords)

        # Retrieve the parameter set name that contains the header
        # values.  Need to change mode of the getcal.keywords parameter
        # in order to not query the user.
        omode = getcal.keywords.p_mode
        getcal.keywords.p_mode="h"
        pkeywords = getcal.keywords
        getcal.keywords.p_mode=omode
        
        # Edit the calibration parameters.
        modcal (pkeywords)

        # Now that we are done editing, applying the changes to the
        # specified list of images.
        if (strlen (pimages) > 0 && modcal.result == "yes")
            putcal (pimages, pkeywords, add=add, verbose=verbose)
            
end
