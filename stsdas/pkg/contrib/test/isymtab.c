/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#include <iraf.h>

#include "defines.h"
#include "machine.h"

extern SYMBOL *symtable;

initsymtab() /* set up symbol table for the opcodes */
{
	int i,tableptr;

	for(i=0; (opcodes[i].opname)[0]!='\0';i++) {
		tableptr = findsymbol(opcodes[i].opname);
		symtable[tableptr].label = -2;
	}
#ifdef DEBUG4D
	fprintf(stderr, "*** init symbol table\n");
#endif
}
