procedure pp_igi (input, device)

char	input	{prompt="File containing list of igi scripts to execute"}
char	device	{"stdgraph",prompt="Device to send igi output"}

char	metacode {"",prompt="Metacode file"}
bool	debug

struct	*in_list

begin
	# Declarations.
	char	igi_script		# Igi script file name.
	char	pdevice			# Graphics device.

	# Get interactive input.
	in_list = input
	pdevice = device

	# Display them all.
	wlpars.major_grid = no
	wlpars.minor_grid = no
	wlpars.label_size = 0.6

	if (metacode == "") {
	    while (fscan (in_list, igi_script) != EOF) {
		if (debug) print ("Processing ", igi_script)
		igi (initcmd="", wlpars="", usewcs=no, wcspars="",
			major_grid=no, minor_grid=no, label_size=0.65,
			axis1_minor=1, axis2_minor=1,
		     	device=pdevice, metacode=metacode, append=no,
		     	debug=no, cursor="", < igi_script)
	 	flprc
	    }
	} else {
	    delete (metacode, verify=no, >& "dev$null")
	    while (fscan (in_list, igi_script) != EOF) {
		if (debug) print ("Processing ", igi_script)
		igi (initcmd="", wlpars="", usewcs=no, wcspars="",
			major_grid=no, minor_grid=no, label_size=0.65,
			axis1_minor=1, axis2_minor=1,
		     	device=pdevice, metacode=metacode, append=no,
		     	debug=no, cursor="", < igi_script, >>G metacode)
		flprc
	    }
	}
end
