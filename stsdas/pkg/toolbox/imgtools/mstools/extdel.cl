procedure extdel (input,delnum,output)

# Script to remove an extension from a FITS file.  
#
# EWW - STScI/SSG, 22-MAY-1997  version 1.1
# EWW - STScI/SSG, 02-JUN-1998  version 1.2
# EWW - STScI/SSG, 19-JUL-1998  version 1.3
# 
# This script excludes one FITS extension that the user may choose
# through the parameter 'delnum'.  The input file is copied while 
# excluding the selected extension.

string 	input 	{prompt="Input FITS filename "}
int	delnum	{prompt="Input number of FITS extension to remove "}
string 	output 	{prompt="Output FITS filename [will have '.fits' appended ] "}

begin

# Define and initialize internal variables

string 	in, out, tmpfil, tmpout
int 	totnum,Pdelnum,i
string 	stPdelnum,ext

# create temporary file for use in
# finding the number of extensions
# to define the number of repetitions


in 	= input	
out 	= output

tmpfil = mktemp("F")
tmpout = mktemp("FT")//".fits"

catfits (in,
"*", format_file="mstools$format.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0, > tmpfil) 

#catfits (in,
#"*", format_file="", log_file="", long_header=no,
#short_header=yes, ext_print=yes, offset=0, > tmpfil) 

catfits (in,
"*", format_file="mstools$format.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0) 

tail (tmpfil,
nlines=1) | scan (s1)

totnum = int(s1)

if (access(tmpfil))
delete (tmpfil,
yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

Pdelnum= delnum

print ("Input --> ",in,"  extension # --> ",Pdelnum)

                         # proceed copying each wanted extension
                	 # check to see that extension is not
			 # the one the user wanted removed

for (i = 0; i <= totnum; i+=1) {
	ext = i
	if (i != Pdelnum)
imcopy (in//"["//ext//"][noinherit]",
tmpout//"[inherit,dupnam,append]", verbose=no)
                               }
if (in==out){
print (in//" <- ? -> "//out)
print ("Keeping the same filename...")

if (access(in))
delete (in,
yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

rename (tmpout,
out, field="root")
            }
else {

if (access(out))
delete (out,
yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

rename (tmpout,
out, field="root")
     }

end
