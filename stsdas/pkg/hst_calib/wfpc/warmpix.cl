# Flag/correct warm pixels of WFPC2 images

# Change the working table from text table to STSDAS table because text 
# tables now can have specific table names and STSDAS table is much faster.
# (JC Hsu, 9-22-1999)

procedure warmpix (input, masks, tables, rej_thresh, fix_thresh, var_thresh)

file input = ""		{prompt = "input image(s)"}
file masks = ""		{prompt = "input mask(s)"}
file tables = ""	{prompt = "input warm pixel table(s)"}
real rej_thresh = 0.1	{prompt = "rejection threshold (DN/sec)", min = 0.}
real fix_thresh = 0.003	{prompt = "fixing threshold (DN/sec)", min = 0.}
real var_thresh = 0.003	{prompt = "variance threshold (DN/sec)", min = 0.}

int fix_dqval = 1024	{prompt = "Data quality value for fixed pixels", min=0}
real rej_val = INDEF	{prompt = "Pixel value for rejected pixels"}
bool runagain = no	{prompt = "Rerun this task on images which ran this task before?"}
bool verbose = yes	{prompt = "print out verbose message?"}

string mode = "al"

begin
	file	infile, inmask, intable
	real	thresh1, thresh2, thresh3
	file	tmptable
	file	tmpfile
	file 	ifile, str
	int	i, j, ntot, nf, ntb

        # load the imgtools package
        if (!defpac("imgtools")) {
            print ("Error: need to load the imgtools package.")
            bye ()
        }

	# added 2/7/2000, JCHsu
	set clobber =  yes

	# assign parameters to local variables
	infile = input
	inmask = masks
	intable = tables
	thresh1 = rej_thresh
	thresh2 = fix_thresh
	thresh3 = var_thresh

	if (infile == "") {
	    print ("Error: No input image.")
	    bye ()
	}
	if (thresh1 < thresh2) {
	    print ("Caution: rej_thresh is less than fix_thresh")
	    #bye ()
	}
 
        countfiles (infile)
        nf = countfiles.output
        countfiles (intable)
        ntb = countfiles.output

	# set up names for scratch table and file
	tmptable = mktemp("warmpix")+".tab"
	tmpfile = mktemp("warmpix")

	# check if the input file has run warmpix task before, by looking for 
	# the string "WARMPIX", this will work for FITS file as well (but not 
	# very pretty).
	ntot = 0
	for (i=1; i<=nf; i+=1) {
	    pickfile (infile, i)
	    ifile = pickfile.output
	    match ("WARMPIX", ifile, > tmpfile)
	    str = "@"+tmpfile
	    countfiles (str)
	    j = countfiles.output
	    if (j != 0) print ("Warning: File ", ifile, " already ran the task WARMPIX before")
	    ntot = ntot + j
	}
	delete (tmpfile)
	if (ntot != 0 && !runagain) bye()

	# merge input tables (into an STSDAS table)
	if (verbose) print ("Copying/merging hot pixel table(s)...")
	tmerge (intable, tmptable, "append", allcols=no, tbltype="row")

	# if the user is using old text tables (with no specific column names),
	# fix the column names
	tchcol (tmptable, "c1", "CHIP",	"", "", >& "dev$null")
	tchcol (tmptable, "c2", "X", 	"", "", >& "dev$null")
	tchcol (tmptable, "c3", "Y", 	"", "", >& "dev$null")
	tchcol (tmptable, "c4", "EPOCH","", "", >& "dev$null")
	tchcol (tmptable, "c5", "DARK", "", "", >& "dev$null")

	# sort the merged table according to the following order: (1) detector, 
	# (2) Y, (3) X, (4) epoch.
	if (ntb > 1) {
	    if (verbose) print ("Sorting hot pixel table(s)...")
	    tsort (tmptable, "CHIP,Y,X,EPOCH", ascend=yes)
	} else {
	    print ("There is only one hot pixel table, skip sorting ...")
	}

	if (verbose) print ("Fixing warm pixels...")
	t_warmpix (infile, inmask, tmptable, thresh1, thresh2, thresh3, 
			fix_dqval=fix_dqval, rej_val=rej_val, verbose=verbose)

	# delete scratch table
	delete (tmptable)
end
