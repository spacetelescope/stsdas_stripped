set     axesimbin        = "stsdasbin$"

package axesim, bin = axesimbin$

pyexecute("axesim$axesim_verify.py")

# Define axesim package here
pyexecute("axesim$prepspectra_iraf.py",tasknames="prepspectra")
pyexecute("axesim$prepimages_iraf.py",tasknames="prepimages")
pyexecute("axesim$simdispim_iraf.py",tasknames="simdispim")
pyexecute("axesim$simdirim_iraf.py",tasknames="simdirim")
pyexecute("axesim$simdata_iraf.py",tasknames="simdata")

clbye()
keep
