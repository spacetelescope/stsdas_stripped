procedure copyftt (outlist)

#  COPYFTT -- 	Copy GSC FITS tables on CD to disk with the correct extension
#		to be used by TABLES routines.  Must have extension FITS not
#		GSC. Get the list of regions, 
#		consisting of a path and region number,from STDIN.  Write a 
#  		list of the resultant FITS table names to STDOUT.
#
#  Revision History:
#	8-May-1997 R.L. Williamson II  Derived from old FTTOSTT script

file	outlist	{prompt="Output list of region tables"}

begin
	file	tabname
	file	fitstab
	file	sdastab
	file	garbage
	file	olist
	file	oftab

	file	path
	bool	first


	garbage = mktemp ("tmp$grb")

	print ("Copying FITS region table(s) to local copy")

	first = yes
	olist = outlist

        while (scan (path, tabname) != EOF) {
            # For each table in the list

            fitstab = path + "/" + tabname + ".gsc"
	    #print (fitstab)
            sdastab = "gsc" + tabname + ".fits"
	    #print (sdastab)

	    if (olist == "STDOUT")
		print (sdastab)

	    else if (first) {
                # Make sure we start from scratch
		print (sdastab, >olist)
                first=no

            } else {
                # Append to tables list
                print (sdastab, >>olist)
	    }
            # Check to see if the local copy exists already
            if (access (sdastab))
                print (sdastab, " exists")
 
            else {
                copy (fitstab, sdastab, >>garbage)
                print (fitstab, " --> ", sdastab)
	    }

        }

	if (access (garbage))
	    delete (garbage)
end
