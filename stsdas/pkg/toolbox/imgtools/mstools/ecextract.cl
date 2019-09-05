procedure ecextract (input,output,extnam,oops)

# Script to extract a single class of extensions from a FITS file.  
#
# EWW - STScI/SSG, 30-MAY-1997  version 1.5
# EWW - STScI/SSG, 02-JUN-1998  version 1.6
# EWW - STScI/SSG, 16-JUL-1998  version 2.0
# 
# This script selects out one (1) entire class of FITS extentions that may be 
# chosen through the parameter extnam.  

string 	input 	{prompt="Input FITS filename "}
string 	output	{prompt="Output FITS filename [will have '.fits' appended ]"}
string  extnam	{prompt="Enter class of FITS extensions to extract"}
bool 	oops 	{prompt="Proceed in spite of these conditions [Y/N] ? "}
string	mode = "ql"

begin

# Define and initialize internal variables
# A capital 'P' is placed in front of procedure variables of the same name 
# as parameters.

string 	in, out, tmpfil, tmpout
int 	totnum,delnum,i,selrows
string 	stdelnum,ext,tot
bool kill 
string	s1,s2,s3,s4,s5,s6
string tmptab,tmpdif,tmpsel,Pextnam
string keeplis

# create temporary file for use in
# finding the number of extensions
# to define the number of repetitions

in 	= input	
out 	= output

if (in==out){
	print ("")
	print ("WARNING: You have chosen the same filename for your output")
	print ("	as you have for your input!  This will cause a ")
	print ("	loss of data from your input file!")
	print ("")
	kill = oops
	    
	if (!kill)
		bye
            }
tmpfil = mktemp("F")
tmpout = mktemp("FT")//".fits"

# Fill this file for determining how many extensions are 
# in the input FITS file.

catfits (in,
"*", format_file="mstools$imsdel.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0, > tmpfil)

tail (tmpfil,
nlines=1) | scan (tot)

#if (access(tmpfil))
#delete (tmpfil,
#go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

totnum = int(tot)
print ("Total number of FITS extensions in this file ->  ",totnum)

tmptab = mktemp("IMSTMP")//".tab" 
tmpsel = mktemp("SEL")//".tab"
tmpdif = mktemp("DIF")//".tab"
keeplis = mktemp("KEEP")//".dat"

tcreate (tmptab,
"mstools$ecextract.col", tmpfil, uparfile="", nskip=3, nlines=0, nrows=0, 
hist=yes, extrapar=5, tbltype="default", extracol=0)

catfits (in,
"*", format_file="mstools$format.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0) | head n=8

Pextnam = extnam
print ("")
print ("Extension name class to be selected is ->", Pextnam)
print ("")

#tquery (tmptab,
#tmpsel, exp=" Pextnam ?= " "//Pextnam//" "", "", "",
#uniq=no, ascend=yes, casesens=yes)

s1 = Pextnam
print ("s1 ->",s1)

tselect (tmptab,
tmpsel, "Pextnam == """//s1//"""")
 
tprint (tmpsel,
prparam=no, prdata=yes, pwidth=80, plength=200, showrow=no, showhdr=no,
showunits=no, columns="ext", rows="-", option="plain", align=yes, sp_col="",
lgroup=200, > keeplis)

if (access(tmpsel))
tprint (tmpsel,showrow-) | match(Pextnam) | page
else 
print ("no selection table ")

tinfo(tmpsel,
ttout=no, nrows=71, ncols=4, npar=1, rowlen=7., rowused=7., allrows=71,
maxpar=5, maxcols=5, tbltype="row", tblversion=2)
selrows = (tinfo.allrows)

# Test to be sure that you have entered a valid extnam
if (selrows <= 0){
	print ("There were no matches to your input extension name.")
	goto cleanup
	bye
                 }

print ("Number of extensions that are of type ",Pextnam, " -> ",selrows)
print ("")

# Copy the Primary header unit first in all cases
imcopy (in//"[0][noinherit]",
tmpout//"[inherit,dupnam]", verbose=no)

for (i=1; i<= selrows; i+=1){
	tabpar (tmpsel,
	column="ext", row=i, value="", undef=no)
	ext = (tabpar.value) 
	imcopy (in//"["//ext//"][noinherit]",
	tmpout//"[noinherit,dupnam,append]", verbose=no)
                            }
#if (access(out))
delete (out,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

print ("")
print ("Renaming ",tmpout, " with all ",Pextnam," extensions to -> ",out)

rename (tmpout,
out, field="root")

cleanup:
# Clean up temporary files
if (access(tmpfil))
delete (tmpfil,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)
if (access(tmptab))
delete (tmptab,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)
if (access(tmpsel))
delete (tmpsel,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)
if (access(tmpdif))
delete (tmpdif,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)
if (access(keeplis))
delete (keeplis,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)


end
