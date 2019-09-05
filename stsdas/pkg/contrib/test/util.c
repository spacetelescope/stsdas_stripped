#define  import_spp
#define  import_libc
#include <iraf.h>

#ifdef VAX
#define NEGLIM  -1.0e-38
#define POSLIM   1.0e-38 
#endif
#ifdef THINK_C
#define NEGLIM  -1.0e-4932
#define POSLIM   1.0e-4932 
#endif
#ifdef SUN
#define NEGLIM  -1.0e-38
#define POSLIM   1.0e-38 
#endif
#ifdef VMS
#define NEGLIM  -1.0e-38
#define POSLIM   1.0e-38 
#endif


notzero(num)
double num;
{
	if ((num > NEGLIM) && (num < POSLIM))
		return 0;
	else
		return 1;
}
