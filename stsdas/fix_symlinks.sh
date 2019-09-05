#!/bin/sh
#
# fix symlinks in the stsdas directory
#
# We don't do this very often because the actual symlinks are in the repository.
# This file exists as a note about what they should be, as well as a way to fix
# it if you lose the symlinks.
# 
# 

(
	rm -f bin
	ln -s bin.generic bin
)

(
	cd lib
	rm -f libapplib.a libcvos.a libf77util.a libhstio.a libiraf77.a libsynphot.a
	ln -s ../bin/libapplib.a libapplib.a
	ln -s ../bin/libcvos.a libcvos.a
	ln -s ../bin/libf77util.a libf77util.a
	ln -s ../bin/libhstio.a libhstio.a
	ln -s ../bin/libiraf77.a libiraf77.a
	ln -s ../bin/libsynphot.a libsynphot.a
)

(
	cd python
	rm -f acs 
	ln -s ../pkg/hst_calib/acs acs
	ln -s ../pkg/analysis/slitless/axe axe
	ln -s ../pkg/analysis/slitless/axesim axesim
)

(
	cd data
	rm -f scidata
	ln -s scidata.generic scidata
)

