
/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*
**    COMPILE.H  -  Include file for routines calling the Gauss compiler.
**    compile() returns a structure containing information about the files
**    compiled.  If the pointer returned is NULL then no error was found.
**    A non-NULL pointer indicates an error was found and the file name and
**    line number can be found in the structure.
*/


struct ifile {
	char *fname;
	int line_number;
	char *errmess;
};
struct ifile *compile();
extern int verbose;
