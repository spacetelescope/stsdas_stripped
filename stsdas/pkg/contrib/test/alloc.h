/*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#ifdef THINK_C
#include <storage.h>
#define malloc(x) mlalloc((long)x)
#define realloc(x,y) relalloc(x,(long)y)
#else
char *malloc(), *realloc();
#endif
