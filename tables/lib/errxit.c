/* This routine takes an error exit on unix systems 
 * It is called by  exit_handler.x
 *
 *  Nelson Zarate	30-Nov-95	original
 */

#include <stdlib.h>

/* Usual Fortran/C calling convention confusion */

#if defined (__hpux) || defined (_AIX) || defined(vms)
#define ERRXIT errxit
#else
#define ERRXIT errxit_
#endif

void ERRXIT (exit_code) 

int	*exit_code;	/* i: pointer because of Fortran calling conventions */

{ 
	exit (*exit_code);
}
