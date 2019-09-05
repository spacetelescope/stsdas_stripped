#!/bin/csh
#
# Clean up files before a release - delete .e, .o, .a files
# and then reset the Iraf architecture to "generic"
#
# credits to stecf extern package:
# Richard Hook, December 1999

find . -name '*.e' -exec rm {} \;
find . -name '*.o' -exec rm {} \;
find . -name '*.a' -exec rm {} \;

mkpkg generic
