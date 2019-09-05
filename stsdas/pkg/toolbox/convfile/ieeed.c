/* 	Copyright restrictions apply - see stsdas$copyright.stsdas 
*/ 
#ifdef __hpux
#define sysv 1
#endif

#ifdef _AIX
#define sysv 1
#endif

#ifdef sysv
#define unix 1
#define vx2sud_ vx2sud 
#define su2vxd_ su2vxd 
#define v2sd_ v2sd
#define s2vd_ s2vd
#endif

#ifdef unix 

#include <stdio.h>
/* IEEED -- Routine to convert a VAX array on the SUN into IEEE on the SUN. */

vx2sud_ (a, b, nints)
/* ieevpakd_ (a, b, nints) */

double	*a;		/* natine input array */
double	*b;		/* output array of doubles with the ieee numbers*/
int	*nints;		/* number of elements in the input array*/

{

	register char	*ip, *op;
	register int i,	n;
	short exp;
	unsigned short  swb; 
	double *d, t;
	union  {
		double dv;
		unsigned short sv[4];
		unsigned int l[2];
		}uv;

	ip = (char *)a;
	op = (char *)b;

	if (ip != op) {
	   for (n = *nints;  --n >= 0; ) {
                /* swap bytes first */	   
	      *op++ = *(ip+1);    /* op[0] = ip[1]  */
	      *op++ = *ip;	/* op[1] = ip[0]  */
	      *op++ = *(ip+3);	/* op[2] = ip[3]  */
	      *op++ = *(ip+2);	/* op[3] = ip[2]  */
	      *op++ = *(ip+5);	/* op[4] = ip[5]  */
	      *op++ = *(ip+4);	/* op[5] = ip[4]  */
	      *op++ = *(ip+7);	/* op[6] = ip[7]  */
	      *op++ = *(ip+6);	/* op[7] = ip[6]  */

	      if (*b == 0.0) {
		   a++;
		   b++;
		   ip += 8;
		   continue;
	      }
   
	      uv.dv = *b;
	      exp = ((uv.sv[0] & (unsigned short)0x7f80) >> 7) + 894;
	      i = uv.sv[0] & 0x8000;   /* get sign */
	      swb = (uv.sv[1] & 0x7) << 13; 
	      uv.l[0] >>=3; uv.l[1]>>=3;	
	      uv.sv[0] = (exp <<4 ) | (uv.sv[0] & 0xf);
	      uv.sv[2] = uv.sv[2] | swb;
	      *b++ = i ? -uv.dv : uv.dv; 
	      ip += 8;
	   }
	} else {
	   d = &t;
	   op = (char *)d;
	   for (n = *nints;  --n >= 0; ) {
              /* swap bytes first */	   
	      *op     = *(ip+1);        /* op[0] = ip[1]  */
	      *(op+1) = *ip;	        /* op[1] = ip[0]  */
	      *(op+2) = *(ip+3);	/* op[2] = ip[3]  */
	      *(op+3) = *(ip+2);	/* op[3] = ip[2]  */
	      *(op+4) = *(ip+5);	/* op[4] = ip[5]  */
	      *(op+5) = *(ip+4);	/* op[5] = ip[4]  */
	      *(op+6) = *(ip+7);	/* op[6] = ip[7]  */
	      *(op+7) = *(ip+6);	/* op[7] = ip[6]  */
  
	      if (t == 0.0) {
		  *a++ = 0;
		  ip += 8;
		  continue;
	      }
  
	      uv.dv = t;
	      exp = ((uv.sv[0] &  (unsigned short)0x7f80) >> 7) + 894;
	      i = uv.sv[0] & 0x8000;   /* get sign */
	      swb = (uv.sv[1] & 0x7) << 13; 
	      uv.l[0] >>=3; uv.l[1]>>=3;	
	      uv.sv[0] = (exp <<4 ) | (uv.sv[0] & 0xf);
	      uv.sv[2] = uv.sv[2] | swb;
	      *a++ = i ? -uv.dv : uv.dv; 
	      ip += 8;
	   }
	}
}

su2vxd_ (a, b, nints)
/* ieevupkd_ (a, b, nints) */

double	*a;		/* natine double input array */
double	*b;		/* output array of VAX doubles */
int	*nints;		/* number of elements in the input array*/

{

	register char	*ip, *op;
	register int	n,i;
	short exp;
	unsigned short  swb; 
	char   sb;
	double *d, t;
	union  {
		double dv;
		unsigned short sv[4];
		unsigned int l[2];
		}uv;

	ip = (char *)a;
	op = (char *)b;

	if (ip != op) {
	   for (n = *nints;  --n >= 0; ) {
	      if (*a == 0.0) {
	           *b++ = 0;
		   a++;
		   op += 8;
		   ip += 8;
		   continue;
	      }
	      uv.dv = *a;
	      exp = ((uv.sv[0] &  (unsigned short)0x7ff0) >> 4) - 894; /* Extract and convert */
						    /* exp to bias 128 */
	      i = uv.sv[0] & 0x8000;   /* get sign */
	      swb = (uv.sv[2] &  (unsigned short)0xe000) >> 13;/* Save 1st 3 bits of 3rd word */
	      uv.l[0] <<=3; uv.l[1] <<=3;       /* Now shift everything by 3 */
	      uv.sv[0] = (exp << 7) | (uv.sv[0] & 0x7f); /* restore  exponent*/
	      uv.sv[1] = uv.sv[1] | swb;	     /* Put the 3 bits */
	      *a = i ? -uv.dv : uv.dv; 

              /* swap bytes */	   
	      *op++ = *(ip+1);    /* op[0] = ip[1]  */
	      *op++ = *ip;	   /* op[1] = ip[0]  */
	      *op++ = *(ip+3);	   /* op[2] = ip[3]  */
	      *op++ = *(ip+2);    /* op[3] = ip[2]  */
	      *op++ = *(ip+5);	   /* op[4] = ip[5]  */
	      *op++ = *(ip+4);	   /* op[5] = ip[4]  */
	      *op++ = *(ip+7);	   /* op[6] = ip[7]  */
	      *op++ = *(ip+6);	   /* op[7] = ip[6]  */
	      ip += 8;
	      a++;
	      b++;
	   }
	} else {
	   d = &t;
	   op = (char *)d;
	   for (n = *nints;  --n >= 0; ) {
	      if (*a == 0.0) {
		   a++;
		   ip+=8;
		   continue;
	      }
	      uv.dv = *a;
	      exp = ((uv.sv[0] &  (unsigned short)0x7ff0) >> 4) - 894;
	      i = uv.sv[0] & 0x8000;   /* get sign */
	      swb = (uv.sv[2] &  (unsigned short)0xe000) >> 13;
	      uv.l[0] <<=3; uv.l[1] <<=3; 
	      uv.sv[0] = (exp << 7) | (uv.sv[0] & 0x7f);
	      uv.sv[1] = uv.sv[1] | swb;
	      *a = i ? -uv.dv : uv.dv; 
   
	      *op     = *(ip+1);       /* op[0] = ip[1]  */
	      *(op+1) = *ip;	        /* op[1] = ip[0]  */
	      *(op+2) = *(ip+3);	/* op[2] = ip[3]  */
	      *(op+3) = *(ip+2);	/* op[3] = ip[2]  */
	      *(op+4) = *(ip+5);	/* op[4] = ip[5]  */
	      *(op+5) = *(ip+4);	/* op[5] = ip[4]  */
	      *(op+6) = *(ip+7);	/* op[6] = ip[7]  */
	      *(op+7) = *(ip+6);	/* op[7] = ip[6]  */
	      *a++ = t;
	      ip+=8;
	   }
	}
}
v2sd_ (a)

double	*a;		/* natine input array */

{

	register char	*ip, *op;
	register int	n,i;
	short exp;
	unsigned short  swb; 
	char   sb;
	double *b,dd;
	union  {
		double dv;
		unsigned short sv[4];
		unsigned int l[2];
		}uv;

	b = &dd;
	ip = (char *)a;
	op = (char *)b;

	if (*a == 0.0) 
	   return;

	*op++ = *(ip+1);        /* op[0] = ip[1]  */
	*op++ = *ip;	        /* op[1] = ip[0]  */
	*op++ = *(ip+3);	/* op[2] = ip[3]  */
	*op++ = *(ip+2);	/* op[3] = ip[2]  */
	*op++ = *(ip+5);	/* op[4] = ip[5]  */
	*op++ = *(ip+4);	/* op[5] = ip[4]  */
	*op++ = *(ip+7);	/* op[6] = ip[7]  */
	*op   = *(ip+6);	/* op[7] = ip[6]  */

	uv.dv = dd;
	exp = ((uv.sv[0] &  (unsigned short)0x7f80) >> 7) + 894;  /* Extract and convert exp */
						 /* to bias 1023 */
	i = uv.sv[0] & 0x8000;   /* get sign */
	swb = (uv.sv[1] & 0x7) << 13;  /* Save last 3 bits of 2nd word */
				       /* and put them in the 1st 3 bits */
	uv.l[0] >>=3; uv.l[1]>>=3;     /* Now shift everything by 3 */
	uv.sv[0] = (exp <<4 ) | (uv.sv[0] & 0xf); /* restore  exponent*/
	uv.sv[2] = uv.sv[2] | swb;     /* Put the 3 bits */
	*a = i ? -uv.dv : uv.dv; 
}

s2vd_ (a)

double	*a;		/* natine double input array */

{

	register char	*ip, *op;
	register int	n,i;
	short exp;
	unsigned short  swb; 
	char   sb;
	double *b, dd;
	union  {
		double dv;
		unsigned short sv[4];
		unsigned int l[2];
		}uv;

	b = &dd;
	ip = (char *)a;
	op = (char *)b;

	if (*a == 0.0)
	    return;

	uv.dv = *a;
	exp = ((uv.sv[0] &  (unsigned short)0x7ff0) >> 4) - 894;  /* Extract and convert exp */
						     /* bias 128 */
	i = uv.sv[0] & 0x8000;   /* get sign */
	swb = (uv.sv[2] &  (unsigned short)0xe000) >> 13;  /* Save 1st 3 bits of 3nd word */
	uv.l[0] <<=3; uv.l[1] <<=3;	  /* Now shift everything by 3 */
	uv.sv[0] = (exp << 7) | (uv.sv[0] & 0x7f); /* restore  exponent*/
	uv.sv[1] = uv.sv[1] | swb;	             /* Put the 3 bits */
	*a = i ? -uv.dv : uv.dv; 

             /* swap bytes */	   
	*op++ = *(ip+1);        /* op[0] = ip[1]  */
	*op++ = *ip;	        /* op[1] = ip[0]  */
	*op++ = *(ip+3);	/* op[2] = ip[3]  */
	*op++ = *(ip+2);	/* op[3] = ip[2]  */
	*op++ = *(ip+5);	/* op[4] = ip[5]  */
	*op++ = *(ip+4);	/* op[5] = ip[4]  */
	*op++ = *(ip+7);	/* op[6] = ip[7]  */
	*op   = *(ip+6);	/* op[7] = ip[6]  */
	*a = dd;
}
#endif
