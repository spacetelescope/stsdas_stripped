procedure msdel (input,delnum,output,nicmos)

# Script to remove an imset from a FITS file.  
#
# EWW - STScI/SSG, 22-MAY-1997  version 1.1
# EWW - STScI/SSG, 02-JUN-1998  version 1.2
# EWW - STScI/SSG, 16-JUL-1998  version 1.3
# EWW - STScI/SSG, 11-MAR-1999  version 1.4
# HAB - STSCI/SSB, 14-MAR-2007  version 1.5: add WFC3/IR support
# 
# This script excludes one (1) entire FITS imset that the user may choose
# through the parameter delnum.  The input file is copied while 
# excluding the selected extension.

string 	input 	{prompt="Input FITS filename "}
int	delnum	{prompt="Input starting extension number of IMSET to remove "}
string 	output 	{prompt="Output FITS filename [will have '.fits' appended ] "}
bool	nicmos	{prompt="Is this NICMOS or WFC3/IR data [Y/N] "}

#mode = "ql"

begin


# Define and initialize internal variables
# Internal variables that have the same name as the procedure parameter
# have a 'P' prepended to them.

string 	in, out, tmpfil, tmpout, mystmp
int 	totnum,Pdelnum,i
string 	stPdelnum,ext,tot
bool nic,mys
int j1,j2,j3,j4,j5

j1=999 # Initialize numbers to value greater than likely
j2=999 # ever to be encountered.  They will all be over-
j3=999 # written, except for when a STIS imset is operated upon
j4=999
j5=999

# create temporary file for use in
# finding the number of extensions
# to define the number of repetitions


in 	= input	
out 	= output//".fits"
nic 	= nicmos

tmpfil = mktemp("F")
tmpout = mktemp("FT")//".fits"

# Fill this file for determining how many SCI extensions are 
# in the input FITS file.

catfits (in,
"*", format_file="mstools$imsdel.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0, > tmpfil) 

match ("{SCI}",
tmpfil, stop=yes, print_file_n=yes, metacharacte=no)

Pdelnum = delnum # fill variable of first extension not to be copied

# For use if extension versions are not in sequential order. (future)
#catfits (in,
#"*", format_file="mstools$imsdel.mip", log_file="", long_header=no,
#short_header=yes, ext_print=yes, offset=0) | match (Pdelnum//":")

tail (tmpfil,
nlines=1) | scan (tot)

totnum = int(tot)

                        # proceed copying each wanted extension
			# Ask if data are NICMOS, if not assume STIS
if (nic) {
	j1 = Pdelnum
	j2=j1+1
	j3=j2+1
	j4=j3+1
	j5=j4+1
         }
else {
	j1 = Pdelnum
        j2=j1+1
        j3=j2+1
      }

# Copy PHDU first
imcopy (in//"[0]", tmpout, verbose=no)

for (i = 1; i <= totnum; i+=1) {
	if (i != j1 && i !=  j2 && i != j3 && i != j4 && i != j5) {
	ext = i
imcopy (in//"["//ext//"][noinherit]",
tmpout//"[inherit,dupnam,append]", verbose=yes)
                                                                  }
                                }
if (in==out){
print ("Keeping the same filename..."//in)

if (access(in)) {
	delete (in,
	go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)
	rename (tmpout, out, field="root")
                }
            }
else {

if (access(out))
	delete (out,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

	rename (tmpout, out, field="all")

    }

print ""
if (access(tmpfil))
	delete (tmpfil)
if (access(tmpout))
	delete (tmpout)
end
