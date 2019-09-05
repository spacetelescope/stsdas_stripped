procedure stgindx ()

#  STGINDX -- Stage the GSC index table from CD to mag disk and convert
#  from FITS to a STSDAS table.
#
#  12/6/91 replaced loading fitsio package with a test to see if the
#  package is loaded.  ZGL
#
#  8/17/93  Rename output table to get rid of group number appended to
#  root by strfits.
#  
#  5/8/97  Due to change in strfits, gsc FITS tables not readable by
#  strfits.  Changed to use tcopy instead. RLW.

file	cdfile	{"/data/gsc1/tables/regions.tbl", prompt="CD file name"}
file	index	{"scidata$index", prompt="output GSC index table"}

begin
	file	intab
	file	root
	file    tmpfile
	file    tmpfile2
	file	indhhh
	file	outtab

	intab = cdfile
	root = index + ".tab"
	tmpfile = "tmp$index.fits"
	tmpfile2 = tmpfile + "[1]"

	# Make sure the ttools packages are loaded.
	if (!defpac("ttools")) {
	    print( "Error: ttools packages need to be loaded!" )
	    bye
	}

	# Copy FITS index table to scidata directory, changing the extension
	# so that tcopy can read it
        copy (intab, tmpfile)
	tcopy (tmpfile2, root)

	delete (tmpfile)
end
