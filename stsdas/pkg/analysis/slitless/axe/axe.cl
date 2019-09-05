set     axebin        = "stsdasbin$"

package axe, bin = axebin$

pyexecute("axe$axe_verify.py")

pyexecute("axe$fcubeprep_iraf.py", tasknames="fcubeprep")
pyexecute("axe$iolprep_iraf.py",   tasknames="iolprep")
pyexecute("axe$axeprep_iraf.py",   tasknames="axeprep")
pyexecute("axe$axecore_iraf.py",   tasknames="axecore")
pyexecute("axe$drzprep_iraf.py",   tasknames="drzprep")
pyexecute("axe$axedrizzle_iraf.py",tasknames="axedrizzle")
pyexecute("axe$axegps_iraf.py",    tasknames="axegps")
pyexecute("axe$sex2gol_iraf.py",   tasknames="sex2gol")
pyexecute("axe$gol2af_iraf.py",    tasknames="gol2af")
pyexecute("axe$af2pet_iraf.py",    tasknames="af2pet")
pyexecute("axe$backest_iraf.py",   tasknames="backest")
pyexecute("axe$petcont_iraf.py",   tasknames="petcont")
pyexecute("axe$petff_iraf.py",     tasknames="petff")
pyexecute("axe$pet2spc_iraf.py",   tasknames="pet2spc")
pyexecute("axe$stamps_iraf.py",    tasknames="stamps")
pyexecute("axe$drz2pet_iraf.py",   tasknames="drz2pet")

clbye()
keep

