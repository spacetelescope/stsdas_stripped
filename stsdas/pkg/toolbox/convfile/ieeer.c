#ifdef __hpux
#define sysv 1
#endif

#ifdef _AIX
#define sysv 1
#endif

#ifdef sysv
#define unix 1
#define vx2sur_ vx2sur
#define su2vxr_ su2vxr
#define v2sr_ v2sr
#define s2vr_ s2vr
#endif

#ifdef unix 

/* I3EPAK -- Routine to convert a VAX array on the SUN into IEEE on the SUN. */

vx2sur_ (a, b, nints)  
/* ieevpakr_ (a, b, nints) */
long	*a;			/* Vax array on the SUN  */
long	*b;			/* output ieee array			*/
int	*nints;			/* number of integers to swap	*/
{
	register char	*ip, *op;
	register int	n,i;
	register unsigned temp;

	ip = (char *)a;
	op = (char *)b;

	if (ip != op) {    /* The input and output arrays are not the same */
	   /* Swap successive 2 byte groups.
	    */
	   for (n = *nints, i=0;  --n >= 0; i++ ) {
	       if (*(a+i) == 0.0) {
	           *(b+i) = 0;
		   op += 4;
		   ip += 4;
		   continue;
	       }
	       *op++ = *(ip+1) - 1; /* op[0] = ip[1]  */
	       *op++ = *ip;	 /* op[1] = ip[0]  */
	       *op++ = *(ip+3);	 /* op[2] = ip[3]  */
	       *op++ = *(ip+2);	 /* op[3] = ip[2]  */
	        ip += 4;
	   }
	} else {
	   for (n = *nints, i=0;  --n >= 0; i++ ) {
	       if (*(a+i) == 0.0) {
	           *(b+i) = 0;
		   op += 4;
		   ip += 4;
		   continue;
	       }
	       temp  = *ip++;	  /* temp  = ip[0]   */
	       *op++ = *ip++ - 1; /* op[0] = ip[1]  */
	       *op++ = temp;	  /* op[1] = temp   */
	       temp  = *ip++;     /* temp  = ip[2]  */
	       *op++ = *ip++;	  /* op[2] = ip[3]  */
	       *op++  = temp;     /* op[3] = temp   */
	   }
	}
}
/* I3EUPK -- Routine to convert a IEEE array on the SUN into VAX on the SUN. */

su2vxr_ (a, b, nints) 
/* ieevpakr_ (a, b, nints) */
long	*a;			/* ieee array on the SUN  */
long	*b;			/* vax output ieee array */
int	*nints;			/* number of integers to swap	*/
{
	register char	*ip, *op;
	register int	n,i;
	register unsigned temp;


	ip = (char *)a;
	op = (char *)b;

	if (ip != op) {     /* The input and output pointers are not the same*/

	   /* Swap successive 2 byte groups.
	    */
	   for (n = *nints, i=0;  --n >= 0; i++ ) {
	       if (*(a+i) == 0.0) {
	           *(b+i) = 0;
		   op += 4;
		   ip += 4;
		   continue;
	       }
	       *op++ = *(ip+1);     /* op[0] = ip[1]  */
	       *op++ = *ip + 1;	    /* op[1] = ip[0]  */
	       *op++ = *(ip+3);	    /* op[2] = ip[3]  */
	       *op++ = *(ip+2);	    /* op[3] = ip[2]  */
	        ip += 4;
	   }
	} else {
	   for (n = *nints, i=0;  --n >= 0; i++ ) {
	       if (*(a+i) == 0.0) {
	           *(b+i) = 0;
		   op += 4;
		   ip += 4;
		   continue;
	       }
	       temp  = *ip++ +1;   /* temp  = ip[0] */
	       *op++ = *ip++;	   /* op[0] = ip[1] */ 
	       *op++ = temp;       /* op[1] = temp  */
	       temp  = *ip++;      /* temp  = ip[2] */
	       *op++ = *ip++;	   /* op[2] = ip[3] */
	       *op++ = temp;       /* op[3] = temp  */
	   }

	}
}
/* IEEUPK -- Convert a sun float to a vax float on the SUN*/
s2vr_ (a)
float	*a;			/* Vax array on the SUN  */
{
	register char	*ip, *op;
	float *b, t;

	b = &t;
	ip = (char *)a;
	op = (char *)b;

	if (*a == 0.0)
	    return;

	*op++ = *(ip+1);         /* op[0] = ip[1]  */
	*op++ = *ip + 1;	 /* op[1] = ip[0]  */
	*op++ = *(ip+3);	 /* op[2] = ip[3]  */
	*op   = *(ip+2);	 /* op[3] = ip[2]  */

	*a = t;
}

/* IEEPAKR -- Convert a Vax float into a SUN float on the SUN  */
v2sr_ (a)
long	*a;			/* Vax array on the SUN  */
{
	register char	*ip, *op;
	long *b, c;

	b = &c;
	ip = (char *)a;
	op = (char *)b;

	if (*a == 0.0)
	    return;

	*op++ = *(ip+1) - 1;     /* op[0] = ip[1]  */
	*op++ = *ip;	         /* op[1] = ip[0]  */
	*op++ = *(ip+3);	 /* op[2] = ip[3]  */
	*op   = *(ip+2);	 /* op[3] = ip[2]  */
	*a = c;

}

#endif
