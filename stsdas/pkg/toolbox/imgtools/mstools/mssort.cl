procedure mssort (input,output)

# Script to sort FITS file into original type format.
#
# EWW - STScI/SSG, 11-JUL-1997  version 2.2
# EWW - STScI/SSG, 02-JUN-1998  version 2.3
# EWW - STScI/SSG, 19-JUN-1998  version 2.4
# 

string 	input 	{prompt="Input FITS filename "}
string	output	{prompt="Output sorted FITS filename [will have '.fits' appended ]"}
string	mode = "ql"
struct *list

begin

# Define and initialize internal variables

string 	in, out
file	tmpfil, tmpfilt, tmpfiltx
int 	totnum, delnum, i, selrows
string 	tot
string 	tmptab, tmpdif
string 	keeplis, inlis
string 	inn, outn
string 	s1, s2, s3, extver

in 	= input	
out	= output

if (! access(in)) {
	print ("Input file ",in," not found! Bye...")
	bye
                  }

tmpfil = mktemp("tmp$F")

# Fill this file for determining how many extensions are 
# in the input FITS file.

catfits (in,
"*", format_file="mstools$/imsdel.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0, > tmpfil)

tail (tmpfil,
nlines=1) | scan (tot)

tmpfilt  = mktemp("tmp$Ft")
tmpfiltx = mktemp("tmp$FtX")

tail (tmpfil,
nlines=-3, > tmpfilt)

totnum = int(tot)
print ("Total number of FITS extensions in this file ->  ",tot)

tmptab = mktemp("IMSTMP")//".tab" 
tmpdif = mktemp("DIF")//".tab"
keeplis = mktemp("KEEP")//".dat"
inlis = mktemp("IN")//".tab"

catfits (in,
"*", format_file="mstools$format.mip", log_file="", long_header=no,
short_header=yes, ext_print=yes, offset=0) | tcreate (tmptab,
"mstools$ecextract.col", "STDIN", uparfile="", nskip=2, nlines=0, nrows=0, hist=yes,
extrapar=5, tbltype="default", extracol=0)

tcalc (tmptab,
"extver", "extver+1000", datatype="int", colunits="", colfmt="i6")

tcalc (tmptab,"row",eq="rownum",colfmt="i6")

tsort (tmptab,
"extver,extnam", ascend=yes, casesens=no)

tprint (tmptab,
prparam=no, prdata=yes, pwidth=80, plength=200, showrow=no, showhdr=no,
showunits=no, columns="*", rows="-", option="plain", align=yes, sp_col="",
lgroup=4000, > tmpfiltx)

list = tmpfiltx

tcalc (tmptab,"newrow",eq="rownum",colfmt="i6")

tinfo(tmptab,
ttout=no, nrows=71, ncols=4, npar=1, rowlen=7., rowused=7., allrows=71,
maxpar=5, maxcols=5, tbltype="row", tblversion=2)
selrows = (tinfo.allrows)

imcopy (in//"["//0//"][inherit]", 
out, verbose=no)

while (fscan(list,extver,s1,s2,s3,outn) != EOF){
	print (extver,s1,s2,s3,outn)
	outn = int(outn)-1

	imcopy (in//"["//outn//"][noinherit]",
        out//"[dupnam,append]", verbose=no)
                                               }

if (access(tmpfil))
	delete(tmpfil,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

if (access(tmpfilt))
	delete(tmpfilt,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

if (access(tmpfiltx))
	delete(tmpfiltx,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

if (access(tmptab))
	delete(tmptab,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

if (access(keeplis))
	delete(keeplis,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

if (access(inlis))
	delete(inlis,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

if (access(tmpdif))
	delete(tmpdif,
go_ahead=yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

end
