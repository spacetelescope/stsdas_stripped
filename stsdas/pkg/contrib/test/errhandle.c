/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Print error messages */

#define  import_spp
#define  import_libc
#define  import_stdio
#include <iraf.h>

#include "defines.h"

extern FILE *fp;

error(s)  /* print an error message */
char *s;
{
	printf("%s\n",s);
	if (fp != NULL)
		fprintf(fp,"%s\n",s);
}

fatalerror(s,t) /* print a fatal error message */
char *s;  /* a format string */
char *t;  /* any string */
{
	printf(s,t);
     	printf("\n");
     	if (fp != NULL)
		fprintf(fp,s,t);
	close_all_tables();
	fatale();
	exit(0);
}

fatalerror2(s,t,w) /* print a fatal error message */
char *s;  /* a format string */
int t;  /* any integer */
char *w;  /* any string */
{
	printf(s,t,w);
     	printf("\n");
     	if (fp != NULL)
		fprintf(fp,s,t,w);
	close_all_tables();
	fatale();
	exit(0);
}

fatalerror2c(s,t,w) /* print a fatal error message */
char *s;  /* a format string */
char *t;  /* any string */
char *w;  /* any string */
{
	printf(s,t,w);
	if (fp != NULL)
		fprintf(fp,s,t,w);
	close_all_tables();
	fatale();
	exit(0);
}

warningerror(s,t,d1,d2) /* print a warning error message */
char *s;  /* a format string */
char *t;  /* any string */
int d1,d2;
{
	printf(s,t,d1,d2);
	if (fp != NULL)
		fprintf(fp,s,t,d1,d2);
}
