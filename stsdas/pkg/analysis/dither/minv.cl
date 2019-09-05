procedure minv(infile,outfile)

# Routine to invert mask of zeros and ones, i.e. to 
# make 0 --> 1 and 1 --> 0.
#
# needed by qzap.cl
#
# MD

string 	infile		{prompt = "Input mask image"}
string 	outfile		{prompt = "Output (inverted) mask image"}

begin

	string inf,outf

# Get query parameters:

	inf = infile
	outf = outfile

	if (inf!=outf) imcopy (inf,outf,ve-)
	imreplace (outf,2,lower=0,upper=0)
	imarith (outf,"-",1,outf)

end

