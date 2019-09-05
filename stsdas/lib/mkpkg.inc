# Global MKPKG definitions for the STSDAS packages.

$set XFLAGS	= "$(XFLAGS) -p stsdas -p tables"
$set XVFLAGS	= "$(XVFLAGS) -p stsdas -p tables"
$set LFLAGS	= "$(LFLAGS) -p stsdas -p tables"


$set USE_CDBS	    = yes
$set USE_CONTRIB    = yes	# Compile and link contrib package?
$set USE_PLAYPEN    = yes
$set USE_SUNVIEW   = yes

# Set up the NO_UNDERSCORE macro that can be used by C programs.
# vms = old VMS systems, _vax = openVMS on a Vax, _alpha = openVMS on the Alpha.
$ifeq (MACH, hp700, rs6000, vms, _vax, _alpha)
    $set XFLAGS = "$(XFLAGS) -DNO_UNDERSCORE"
$else $ifeq (MACH, macosx)
    $set XFLAGS = "$(XFLAGS) -DMACOSX"
$endif
 
$ifeq (MACH, sparc) then
$include "stsdas$lib/mkpkg.sf.SUN4"
$else $ifeq (MACH, ssun) then
$include "stsdas$lib/mkpkg.sf.SUN4"
$else $ifeq (MACH, i386) then
$include "stsdas$lib/mkpkg.sf.I386"
$else $ifeq (MACH, mc68020) then
$include "stsdas$lib/mkpkg.sf.SUN3"
$else $ifeq (MACH, f68881) then
$include "stsdas$lib/mkpkg.sf.SUN3"
$else $ifeq (MACH, mips) then
$include "stsdas$lib/mkpkg.sf.DS3100"
$else $ifeq (MACH, alpha) then
$include "stsdas$lib/mkpkg.sf.OSF1"
$else $ifeq (MACH, hp700) then
$include "stsdas$lib/mkpkg.sf.HP700"
$else $ifeq (MACH, redhat) then
$include "stsdas$lib/mkpkg.sf.RHUX"
$else
$include "stsdas$lib/mkpkg.sf.VMS"
$end

