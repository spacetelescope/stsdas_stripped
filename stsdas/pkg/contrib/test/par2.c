/*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#include <iraf.h>

#include "defines.h"
#include "par2.h"
/*#include "unix.h"*/
#include "strings.h"

static TBNAMES tbname;

char *getfitsstr(s)
char *s;
{
  short sppnam[64],sppin[128];
  char string[128],*str, *strpointer, *malloc();
        strupk(s,sppnam,&sxf);
        clgstr(sppnam,sppin,&ote);
        strpak(sppin,string,&ote);
        str = string;
        if ((strpointer = malloc(strlen(str)+1)) != NULL)
           strcpy(strpointer, str);
        return strpointer;  
}                      

parse_dlist(s,ltype)
 char *s;
 int ltype;
 { 
   short  sppnam[64],outstr[64];
   char  fname[64], *p;  
   int i,ilist;
   strupk(s,sppnam,&sxf);    
   i = 0; 
   ilist = clpops(sppnam);
   numfil = clplen(&ilist);
   for (i=0;i <numfil;i++)
        {                  
  	  clgfil (&ilist, outstr, &sxf);
          strpak(outstr,fname,&sxf);
          p = fname;
	  /*printf ("file = %s\n\n", p);   */
          if (ltype == DATA_TABLE)  strcpy(tbname.dptr[i],p);
          if (ltype == PARAM_TABLE) strcpy(tbname.pptr[i],p);
        }    
   clpcls(&ilist);                                           
   if (ltype == DATA_TABLE)	tbname.dfnum = numfil;
   if (ltype == PARAM_TABLE)	tbname.pfnum = numfil;      
   if (ltype == DATA_TABLE)  
{	for (i = 0; i <tbname.dfnum; i++)
		printf("Data File Read: %s \n\n\n",tbname.dptr[i]);
 }  return numfil;
}
              
char *getdatafilename(num)
int num;
{
	return tbname.dptr[num];
}                             

char *getparamfilename(num)
int num;
{
	return tbname.pptr[num];
}                             
                       
getdatafilenum()
{
	return tbname.dfnum;
}

getparamfilenum()
{
	return tbname.pfnum;
}

putfitsstr(s,t)
char *s, *t;
{
  short sppnam[64],sppin[128];
        strupk(s,sppnam,&sxf);
        strupk(t,sppin,&ote);
        clpstr(sppnam,sppin);

}

double getdblval(s)
char *s;
{
  short sppnam[64];
  double dblenum, clgetd();  
        dblenum = 0.0;
        strupk(s,sppnam,&sxf);
        dblenum = clgetd(sppnam);
        /*printf ("%lf\n",dblenum);   */
        return dblenum;
}
 
putdbleval(s,val)
char *s;
double val;
{
   short sppnam[64];  
         strupk(s,sppnam,&sxf);
         clputd(sppnam,&val);
}

int getintval(s)
char *s;
{                             
  short sppnam[64];
  int intnum; 
        intnum = 0;
        strupk(s,sppnam,&sxf);
        intnum = clgeti(sppnam);
        return intnum;
}

putintval(s,val)
char *s;
int val;
{
   short sppnam[64];
         strupk(s,sppnam,&sxf);
         clputi(sppnam,&val);
}
