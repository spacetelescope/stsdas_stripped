#include <stdlib.h>
#if defined (__hpux) || defined (_AIX) 
#define rfexit_ rfexit
#endif
 
void rfexit_ (int *exit_code) 
{ 
  exit (*exit_code);
}
