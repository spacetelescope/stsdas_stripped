procedure ypacqpeak (root, tmproot, igi_list, imtype)

char	root	{prompt="Rootname of observation"}
char	tmproot	{prompt="Rootname for temporary files"}
char	igi_list {prompt="Name of the file containing the igi script names"}
char	imtype	{prompt="Image type of root (FITS or GEIS)"}

begin
	# Declarations
	char	peak_script
	char	script
	char	tmp
	char	ftype
	
	# Create file names.
	tmp = mktemp (tmproot//"ACP")
	script = tmp//".igi"
	peak_script = tmp//"PEAK.igi"
	ftype = imtype

	# Start a new page.
	print ("erase", >> script)
	ypbanner (root, script, ftype)

	# Get the peakup output.
	yppeak (root, ftype, plot=yes, v2v3off=yes, pos_tab="", 
		short_title=yes, igi_script=peak_script,
		device="stdgraph")
	printf ("location 0.2 0.8 0.2 0.85\n", >> script)
	type (peak_script, map_cc=yes, device="text", >> script)

	# Add the script to the list.
	print (script, >> igi_list)
end
