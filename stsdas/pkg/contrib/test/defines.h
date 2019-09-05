/*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Header file for all files */

#ifndef VAX
#ifndef IBM
#ifndef MAC
#define MAC
#endif
#endif
#endif

#ifndef _DEFINE
#define _DEFINE

#ifdef MAC
#include <MacTypes.h>
#define malloc(x) NewPtr((long)x)  /* definitions of malloc and free (Unix */
#define free(x) DisposPtr(x)       /* standard memory alloction functions)
*/
#else
	typedef char *Ptr;         /* Ptr is a pointer to a char */
#endif

#endif
