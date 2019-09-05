/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*
**	MAIN - main routine for Gauss.
**	    
**
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_ctype
#include <iraf.h>

#include "defines.h"
#include "files.h"

extern int cur_fd;         /* information used by compiler */
extern int line_number;
extern FILE *fd_stack[];
extern FILE *fd_out;

extern char *itos(); /* integer to string conversion */

gausst()   /*  gaussfit truncated by SPP */
{
	char ofname[25];
	int errstat = 0;

	strcpy(ofname,getenvstr("model"));
	gaussmain(ofname,ofname); /* execute program */

}
