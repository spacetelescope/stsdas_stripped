procedure metric (infile, x, y)

file infile = ""	{prompt = "input file"}
string x = ""		{prompt = "input X or table and X column"}
string y = ""		{prompt = "input Y or table and Y column"}
bool hms = yes 		{prompt = "output RA/Dec in sexagisimal format?"}
bool centroid = no	{prompt = "do centroiding?"}
int boxsize = 7		{prompt = "centroiding box size", min = 1}

string ra = ""          {prompt = "output RA"}
string dec = ""         {prompt = "output Dec"}

string mode = "al"

begin

file 	ifile
string	dumx
string  dumy
bool	dumcorr
bool	dumhms
bool	dumcentroid
int	dumboxsize

	# assign CL parameters to local parameters
	ifile = infile
	dumx = x

	# if x is NULL, interactive mode
	if (dumx == "") {
	    dumy = ""
	    display (ifile, mode="h")
	    display.image.p_mode = "h"
	} else
	    dumy = y

	dumhms = hms
	dumcentroid = centroid
	dumboxsize = boxsize

	t_metric (ifile, dumx, dumy, hms = dumhms, centroid = dumcentroid,
			boxsize = dumboxsize)
	ra = t_metric.ra
	dec = t_metric.dec

	if (dumx == "")
	    display.image.p_mode = "a"
end
