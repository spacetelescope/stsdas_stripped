procedure ecdel (input,output,extnam)#,doit)

# Script to remove extension classes from a FITS file.  
#
# EWW - STScI/SSG, 23-MAY-1997  version 2.0
# EWW - STScI/SSG, 02-JUN-1998  version 2.1
# 
# This script excludes one (1) entire class of FITS extentions that may be 
# chosen through the parameter extnam.  The input file is copied while 
# excluding the selected extensions.

string 	input 	{prompt="Input FITS filename "}
string 	output 	{prompt="Output FITS filename [will have '.fits' appended ] "}
string  extnam	{prompt="Enter class of FITS extensions to remove "}
#bool	doit	{prompt="Do you want to remove these extensions, (last chance) [Y/N] ? "}
string	mode = "ql"

begin


# Define and initialize internal variables
# Internal parameters have a capital 'P' in front of them

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

if (access("ohpleasedelme"))
	delete ohpleasedelme

in 	= input	
out 	= output

tmpfil = mktemp("F")
tmpout = mktemp("FT")//".fits"

# Fill this file for determining how many extensions are 
# in the input FITS file.
catfits (in,
"*", format_file="mstools$imsdel.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0, > tmpfil)

# Find number of extensions by looking at last line of 'catfits'
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

# Make a table from output of 'catfits'
catfits (in,
"*", format_file="mstools$format.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0) | tcreate (tmptab,
"mstools$ecextract.col", "STDIN", uparfile="", nskip=3, nlines=0, nrows=0, hist=yes,
extrapar=5, tbltype="default", extracol=0)

# Print the first 8 lines of 'catfits' output
catfits (in,
"*", format_file="mstools$format.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0) | head n=8

# Change variable name to match column name.
Pextnam = extnam
print ("")
print ("Extension name class to be removed is ->", Pextnam)

# Select all rows in first table that don't match extension you want
# to discard
tquery (tmptab,
tmpsel, "Pextnam != " // "'" //Pextnam//"'", "ext fitsname Pextnam extver", "",
uniq=no, ascend=yes, casesens=yes)

# Fill a list of extensions that you want to keep.
tprint (tmpsel,
prparam=no, prdata=yes, pwidth=80, plength=200, showrow=no, showhdr=no,
showunits=no, columns="ext", rows="-", option="plain", align=yes, sp_col="",
lgroup=200, > keeplis)

if (access(tmpsel))
tprint (tmpsel,showrow-) | match(Pextnam)
else 
print ("no selection table ")

#kill = doit

#if (kill) {

# Find the number of extensions to keep in table; use in for loop
tinfo(tmpsel,
ttout=no, nrows=71, ncols=4, npar=1, rowlen=7., rowused=7., allrows=71,
maxpar=5, maxcols=5, tbltype="row", tblversion=2)
selrows = (tinfo.allrows)

#if (access(tmpsel))
#tprint (tmpsel,showrow-) | match(Pextnam) 

# show number of extensions left behind after culling
print ("Remaining extensions number -> ",selrows)

for (i=1; i<= selrows; i+=1){
	tabpar (tmpsel,
	column="ext", row=i, value="", undef=no)
	ext = (tabpar.value) 
	imcopy (in//"["//ext//"][noinherit]",
	tmpout//"[noinherit,dupnam,append]", verbose=no)
                            }

#          }
#else
#	bye

if (in==out){
print (in//" <- ? -> "//out)
print ("Keeping the same filename...")
print ("")

if (access(in))
delete (in,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

print ("Renaming ",tmpout, " -> ",in)

rename (tmpout,
out, field="root")
            }
else {
if (access(out))
delete (out,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

print ("Renaming ",tmpout, " -> ",out)

rename (tmpout,
out, field="root")
     }

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

if (access("ohpleasedelme"))
delete ("ohpleasedelme",
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)


end
