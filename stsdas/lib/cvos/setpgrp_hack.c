/*
* DO NOT include any system include files here.
*/
void setpgrp_hack( int jobcode) ;


/*

Problem:

1. On some systems, setpgrp() takes parameters and on others it does not.

2. Mac OSX 10.4 and Mac OSX 10.5 are different, but there does not
appear to be a way to tell the difference at compile time.


Solution:

In this file, we do not declare setpgrp(), but we call it with
parameters.  On systems that don't expect parameters, the parameters
will just be additional words on the stack that are ignore.  On
systems that do expect the parameters, they will be there.


*/

void setpgrp_hack( int jobcode ) 
{
	setpgrp(0, jobcode);
}

