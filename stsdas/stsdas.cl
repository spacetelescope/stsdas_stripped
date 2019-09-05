#{ STSDAS.CL -- The STScI science data analysis suite of packages
# Modified: Ball, 8-dec-1988 to remove telemetry package
# Modified: Willard, 11-Jan-1989 to add problems package
# Modified: Hanisch, 23-Jan-1989 to remove incomplete packages (spectra,
#  timeseries)
# Modified: Ball, 25-Jan-1989 moved to pkg and changed stsdas$ to stsdaspkg$
# Modified: Levay, 26-Jan-1989 rename tvdisplay to vdisplay
# Modified: Willard, 8-Mar-1989 add call to stsdas package zzsetenv and bin
# Modified: Willard, 2-Jun-1989 comment out vdisplay on 'ra'
# Modified: Busko, 12-Feb-1990 to add isophote package
# Modified: Busko, 14-Feb-1990 to add fitting package
# Modified: Richmond, 14-Feb-1990 to add playpen package (for Levay)
# Modified: Simon, 15-Feb-1990 to add testdata package
# Modified: Hsu, 28-Feb-1990 to add timeseries package
# Modified: Levay,  6-Mar-1990 to fix reference to gasp
# Modified: Williamson, 18-Jul-1991 add tim package
# Modified: RAShaw, 26-Jul-1991 to add "examples" task
# Modified: Williamson, 15-Aug-1991 add restore package
# Modified: RAShaw, 5-Sep--1991 to add "describe" task
# Modified: Williamson, 17-Dec-1991 to add contrib package and to take out
# astrometry package
# Modified: Williamson, 12-Jul-1993 restructure packages for 1.3 release
# Modified: Williamson, 12-Feb-1997 move stlocal package to separate package
# Modified: Hack, 15-Mar-2002 added Python support tasks pyexecute and nopyraf

# Set up tasks which report on PyRAF-based tasks
task pyexecute = "stsdas$pyexecute.cl"
# This task simply prints a message stating that a task needs PyRAF to run
task nopyraf = "stsdas$nopyraf.cl"
hidetask pyexecute, nopyraf

# Add Python tree to default Python path, if running PyRAF
pyexecute("stsdas$addpath.py",verbose=no)

cl < "stsdas$/lib/zzsetenv.def"

package	stsdas, bin = stsdasbin$

task	analysis.pkg	= "analysis$analysis.cl"
task	contrib.pkg	= "contrib$contrib.cl"
task	describe	= "stsdas$describe.cl"
task	examples	= "stsdas$examples.cl"
task	fitsio.pkg	= "fitsio$fitsio.cl"
task	graphics.pkg	= "graphics$graphics.cl"
task	hst_calib.pkg	= "hst_calib$hst_calib.cl"
task	playpen.pkg	= "playpen$playpen.cl"
task	problems	= "stsdas$problems.cl"
task	sobsolete.pkg	= "sobsolete$sobsolete.cl"
task	toolbox.pkg	= "toolbox$toolbox.cl"


#Load some of the stsdas packages
cl < stsdas$load.cl
# Print the Welcome banner
if (motd)
type stsdas$stsdas_motd
;
clbye ()
