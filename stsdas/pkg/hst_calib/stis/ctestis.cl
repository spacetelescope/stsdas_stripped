procedure ctestis (intab,outtab,netc,skyc,yc)

file	intab	  {"",prompt="Name of input table"}
file	outtab	  {"",prompt="Name of output table"}
string	netc   	  {"",prompt="Column containing Object flux counts"}
string 	skyc   	  {"",prompt="Column containing Background levels"}
string 	yc   	  {"",prompt="Column containing Y positions of objects"}
string	stisimage {"",prompt="(Optional) name of STIS image used for measurements"}
real    mjd    	  {INDEF,prompt="Modified Julian Date of Observation"}
int	nread	  {INDEF,prompt="Number of images summed (e.g., CR-SPLITs)"}
int	ybin	  {INDEF,prompt="Binning Factor in Y"}
int 	gain	  {INDEF,prompt="CCD Gain setting (1 or 4)"}
string	outscol   {"corr_flux",prompt="Output flux column ID (not used for ASCII)"}
string	outmcol   {"corr_dmag",prompt="Output delta(mag) column ID (not used for ASCII)"}
string	outycol   {"corr_dy",prompt="Output delta(y) column ID (not used for ASCII)"}
bool	verbose	  {yes,prompt="Show origin of parameter values?"}
string  Version   {"27Apr11",prompt="Date of Installation"}

# History
# 2006/08/10 P. Goudfrooij - Created task.
# 2011/04/27 V. Dixon - Added correction for Y centroid shift.
#

begin

 string ypos, innet, insky, temptab, exten
 string c_tab, ycol, netcol, skycol, areacol, theintab, tabtyp, outfile
 string colS, colbck, collcts, collbck, colc1, colc2, colcti, r_colcti
 string colcorrflux, c_outscol, coldmag, c_outmcol, inima
 string c_outycol, ctinorm, coldy
   real inmjd, yr
    int biny, ccdgain, nr, numcols, numlcts, numbck, numlbck
    int numcti, numrcti, numc1, numc2, numB, numS, i, numcorrflux
    int numdmag, newcolnum1, newcolnum2
    int numnet, numsky, numyc
    int newcolnum3, numdy, numctinorm
   bool show, gotinmjd, gotnr, gotbiny, gotccdgain
#
  gotinmjd = no ; gotnr = no ; gotbiny = no ; gotccdgain = no
#
#*********************************************************************
# Check if necessary IRAF/STSDAS packages are loaded
#
   if (! defpac ("ttools")) {
	print("Please load 'ttools' package before running this task")
	bye
   }
#
#*********************************************************************
#
  ypos = yc ; innet = netc ; insky = skyc ; inmjd = mjd ; c_tab = intab
  outfile = outtab ; c_outscol = outscol ; c_outmcol = outmcol ; nr = nread
  ccdgain = gain ; ycol = yc ; netcol = netc ; skycol = skyc ; biny = ybin
  inima = stisimage ; show = verbose ; c_outycol = outycol
#
# Get list of columns in input table
#
  if (access ("intabcolumns")){
        del ("intabcolumns", verify-, go_ahead+)}
  tlcol (c_tab, nlist=1, > "intabcolumns")
#
# For "ascii" input table format, make sure the three input columns start
# with a "c" when executing tcalc later on.
#
  tinfo (table=c_tab, ttout-)
  tabtyp = tinfo.tbltype
  if  (substr (tabtyp, 1, 4) == "text"){
	exten = ".dat"
	if (substr (innet, 1, 1) != "c"){
		innet = "c"//innet}
	if (substr (insky, 1, 1) != "c"){
		insky = "c"//insky}
	if (substr (ypos, 1, 1) != "c"){
		ypos = "c"//ypos}
	}
  else if  (substr (tabtyp, 1, 4) == "fits"){
	exten = ".fits"
	}
  else if  (substr (tabtyp, 1, 3) == "sts"){
	exten = ".tab"
	}
  else {
	error (1, "Cannot understand format of input table - I read "//tabtyp)
  }
#
# Create temporary table file name and copy input table into it to work with
#
  temptab = mktemp("ctetempor")
  copy (c_tab, temptab//exten, verbose-)
  c_tab = temptab//exten
#
# Get number of columns in input table (since it might be ASCII format)
# and designate column numbers and names
#
  numcols = int(tinfo.ncols)
  numS    = numcols+1
  numbck  = numcols+2
  numlcts = numcols+3
  numlbck = numcols+4
  numc1   = numcols+5
  numc2   = numcols+6
  numcti  = numcols+7
  numrcti = numcols+8
  numcorrflux = numcols+9
  numdmag = numcols+10
  numctinorm = numcols+11
  numdy = numcols+12
  colS = "c"//numS ; colbck = "c"//numbck ; collcts = "c"//numlcts
  collbck = "c"//numlbck ; colc1 = "c"//numc1 ; colc2 = "c"//numc2
  colcti = "c"//numcti ; r_colcti = "c"//numrcti ; ctinorm = "c"//numctinorm
#
  if (strlen(c_outscol) >= 1 && substr (tabtyp, 1, 4) != "text") {
	colcorrflux = c_outscol
	coldmag = c_outmcol
	coldy = c_outycol
  }
  else {
  	colcorrflux = "c"//numcorrflux
  	coldmag = "c"//numdmag
	coldy = "c"//numdy
  }
#
# If a given user parameter is INDEF, see if STIS image exists, and if so, read
# relevant header keywords into those task parameters.
# Print out info as to where a given input parameter was taken from.
#
  if (real(inmjd) > 0.) gotinmjd = yes
  if (int(nr) >= 1) gotnr = yes
  if (int(biny) >= 1) gotbiny = yes
  if (int(ccdgain) >= 1) gotccdgain = yes
#
  if (strlen(inima) >= 1){
	imgets (inima//"[0]", "nextend")
	if (int(imgets.value) < 3) {
		print ("Your STIS image file ", inima, \
			" does not have nominal STIS file extensions.")
		print ("Will (try to) use `mjd' through `gain' user parameters.")
	}
	else {
		imgets (inima//"[0]", "texpstrt")
		if (real(imgets.value) > 0. && (!gotinmjd)) {
			inmjd = real(imgets.value)
			if (show) print ("Got `mjd' from STIS image")}
		else if (gotinmjd) {
			if (show) print ("Used `mjd' as supplied by user")}
		else {
  	  		del (temptab//"*,intabcolumns", verify-, go_ahead+)
			print ("Cannot find `mjd' in header of STIS image.")
			error (1,"Please supply `mjd' user parameter")
		}
		imgets (inima//"[1]", "ncombine")
		if (int(imgets.value) >= 1 && (!gotnr)) {
			nr = int(imgets.value)
			if (show) print ("Got `nread' from STIS image")}
		else if (gotnr) {
			if (show) print ("Used `nread' as supplied by user")}
		else {
  	  		del (temptab//"*,intabcolumns", verify-, go_ahead+)
			print ("Cannot find `nread' in header of STIS image.")
			error (1,"Please supply `nread' user parameter")
		}
		imgets (inima//"[0]", "binaxis2")
		if (int(imgets.value) >= 1 && (!gotbiny)) {
			biny = int(imgets.value)
			if (show) print ("Got `ybin' from STIS image")}	
		else if (gotbiny) {
			if (show) print ("Used `ybin' as supplied by user")}
		else {
  	  		del (temptab//"*,intabcolumns", verify-, go_ahead+)
			print ("Cannot find `ybin' in header of STIS image.")
			error (1,"Please supply `ybin' user parameter")
		}
		imgets (inima//"[0]", "ccdgain")
		if (int(imgets.value) >= 1 && (!gotccdgain)) {
			ccdgain = int(imgets.value)
			if (show) print ("Got `gain' from STIS image")}
		else if (gotccdgain) {
			if (show) print ("Used `gain' as supplied by user")}
		else {
  	  		del (temptab//"*,intabcolumns", verify-, go_ahead+)
			print ("Cannot find `gain' in header of STIS image")
			error (1,"Please supply `gain' user parameter")
		}
	}
  }
  else {
  	if (show) print ("No STIS image file provided.")
	if (!gotinmjd) print ("ERROR: No `mjd' provided.")
	if (!gotnr) print ("ERROR: No `nread' provided.")
	if (!gotbiny) print ("ERROR: No `ybin' provided.")
	if (!gotccdgain) print ("ERROR: No `gain' provided.")
	if ((!gotinmjd) || (!gotnr) || (!gotbiny) || (!gotccdgain)) {
  	  	del (temptab//"*,intabcolumns", verify-, go_ahead+)
		return}
	else {
		if (show) {
	    		print("Used `mjd', `nread', `ybin', and `gain'", \
			      " as supplied by user.")}
	}
  }
#
# Expression for yr
#
  yr = (inmjd-51765.)/365.25
#
# Calculate columns for relevant parameters and calculate CTE loss
#
  if (ccdgain == 1) {
        tcalc (c_tab, colS, innet//"*1.00/"//nr, datatype="double", colunit="", \
 	        colfmt="")
  	tcalc (c_tab, colbck, "max(0.,"//insky//"*1.00/"//nr//")", \
		datatype="double", colunit="", colfmt="")}
  else if (ccdgain == 4) {
        tcalc (c_tab, colS, innet//"*4.08/"//nr, datatype="double", colunit="", \
 	        colfmt="")
  	tcalc (c_tab, colbck, "max(0.,"//insky//"*4.08/"//nr//")+4.5", \
		datatype="double", colunit="", colfmt="")}
  else {
	error (1, "Sorry, only CCD gain settings 1 and 4 are supported; yours is "//ccdgain)
  }
  tcalc (c_tab, collcts, "log("//colS//")-8.5", datatype="double", colunit="", \
	colfmt="")
  tcalc (c_tab, collbck, "log(sqrt(("//colbck//")**2+1.))-2.", datatype="double", \
	colunit="", colfmt="")
  tcalc (c_tab, colc1, \
    "1.33E-4*exp(-0.54*"//collcts//")*(1.+0.205*"//yr//")", \
    datatype="double", colunit="", colfmt="")
  tcalc (c_tab, colc2, \
    "0.05*exp(-0.82*"//collbck//")+(1.-0.05)*exp(-3.60*("//colbck//"/"//colS//")**0.21)", \
    datatype="double", colunit="", colfmt="")
  tcalc (c_tab, colcti, colc1//"*"//colc2, \
	datatype="double", colunit="", colfmt="")
  tcalc (c_tab, r_colcti, "real("//colcti//")", \
	datatype="real", colunit="", colfmt="g15.8")
  tcalc (c_tab, colcorrflux, \
        innet//"/((1.-"//r_colcti//")**(1024.-"//ypos//"*"//biny//"))", \
  	datatype="real", colunit="Corrected DN", colfmt="f10.3")
  tcalc (c_tab, coldmag, "-2.512*log10("//colcorrflux//"/"//innet//")",
  	datatype="real", colunit="d(mag)", colfmt="f7.3")
  tcalc (c_tab, ctinorm, colcti//"*1.0E4", \
	datatype="double", colunit="", colfmt="")
  tcalc (c_tab, coldy, "-0.025*"//ctinorm//"+(0.78E-3*sqr("//ctinorm//"))",
	datatype="real", colunit="d(ypix)", colfmt="f7.3")
#
# create output table in same format as input, without unnecessary intermediate columns
#
  if (access (outfile)){
        del (outfile, verify-, go_ahead+)}
  tproject (c_tab, outfile, "@intabcolumns,"//colcorrflux//","//coldmag//","//coldy)
  if (show) {
     print ("Output table `", outfile, "' has three new columns:")
     if (strlen(c_outscol) >= 1 && substr (tabtyp, 1, 4) != "text") {
     	print ("  `", colcorrflux, "': Net object counts corrected for CTE loss")
     	print ("  `", coldmag,     "': Delta(mag) to be added to correct for CTE loss")
     	print ("  `", coldy,       "': Delta(y) to be added to measured Y centroid")
  	}
     else {
     	tinfo (table=outfile, ttout-)
     	newcolnum1 = int(tinfo.ncols)-2
     	newcolnum2 = int(tinfo.ncols)-1
     	newcolnum3 = int(tinfo.ncols)
     	print ("  c"//newcolnum1//": Net object counts corrected for CTE loss")
     	print ("  c"//newcolnum2//": Delta(mag) to be added to correct for CTE loss")
     	print ("  c"//newcolnum3//": Delta(y) to be added to measured Y centroid")
     }
  }
#
#***************************************************************************
# Get rid of intermediate files, if any
#
  if (access ("intabcolumns")){
          del ("intabcolumns", verify-, go_ahead+)}
  del (temptab//"*", verify-, go_ahead+)

end
